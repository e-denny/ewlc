/*
 p See LICENSE file for copyright and license details.
 */
#define _POSIX_C_SOURCE 200809L
#include "util.h"
#include "server.h"
#include "client.h"
#include "output.h"
#include "pointer.h"
#include "keyboard.h"
#include "module.h"
#include <emacs-module.h>
#include <getopt.h>
#include <linux/input-event-codes.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>
#include <wayland-client.h>
#include <wayland-server-core.h>
#include <wlr/backend.h>
#include <wlr/render/wlr_renderer.h>
#include <wlr/types/wlr_compositor.h>
#include <wlr/types/wlr_cursor.h>
#include <wlr/types/wlr_data_device.h>
#include <wlr/types/wlr_export_dmabuf_v1.h>
#include <wlr/types/wlr_gamma_control_v1.h>
#include <wlr/types/wlr_input_device.h>
#include <wlr/types/wlr_keyboard.h>
#include <wlr/types/wlr_matrix.h>
#include <wlr/types/wlr_output.h>
#include <wlr/types/wlr_output_layout.h>
#include <wlr/types/wlr_pointer.h>
#include <wlr/types/wlr_primary_selection.h>
#include <wlr/types/wlr_primary_selection_v1.h>
#include <wlr/types/wlr_screencopy_v1.h>
#include <wlr/types/wlr_seat.h>
#include <wlr/types/wlr_viewporter.h>
#include <wlr/types/wlr_xcursor_manager.h>
#include <wlr/types/wlr_xdg_decoration_v1.h>
#include <wlr/types/wlr_xdg_output_v1.h>
#include <wlr/types/wlr_xdg_shell.h>
#include <wlr/util/log.h>
#include <xkbcommon/xkbcommon.h>

#ifdef XWAYLAND
#include <X11/Xlib.h>
#include <wlr/xwayland.h>
#endif

void xdeco_mgr_new_toplevel_decoration_handler(struct wl_listener *listener, void *data)
{
    struct wlr_xdg_toplevel_decoration_v1 *wlr_deco = data;
    struct ewlc_decoration *d = wlr_deco->data = calloc(1, sizeof(*d));
    struct ewlc_server *s;

    s = wl_container_of(listener, s, xdeco_mgr_new_top_level_decoration_listener);
    d->server = s;

    d->deco_request_mode_listener.notify = deco_request_mode_notify;
    wl_signal_add(&wlr_deco->events.request_mode, &d->deco_request_mode_listener);

    d->deco_destroy_listener.notify = deco_destroy_notify;
    wl_signal_add(&wlr_deco->events.destroy, &d->deco_destroy_listener);

    deco_request_mode_notify(&d->deco_request_mode_listener, wlr_deco);
}

void deco_destroy_handler(struct wl_listener *listener, void *data)
{
    struct wlr_xdg_toplevel_decoration_v1 *wlr_deco = data;
    struct ewlc_decoration *d = wlr_deco->data;

    wl_list_remove(&d->deco_destroy_listener.link);
    wl_list_remove(&d->deco_request_mode_listener.link);
    free(d);
}

void deco_request_mode_handler(struct wl_listener *listener, void *data)
{
    struct wlr_xdg_toplevel_decoration_v1 *wlr_deco = data;
    wlr_xdg_toplevel_decoration_v1_set_mode(
        wlr_deco, WLR_XDG_TOPLEVEL_DECORATION_V1_MODE_SERVER_SIDE);
}

void sigchld(int unused)
{
    if (signal(SIGCHLD, sigchld) == SIG_ERR)
        EERROR("can't install SIGCHLD handler");
    while (0 < waitpid(-1, NULL, WNOHANG));
}

#ifdef XWAYLAND

void update_window_type(struct ewlc_client *c)
{
    struct ewlc_server *srv = c->server;
    size_t i;
    for (i = 0; i < c->surface.xwayland->window_type_len; i++)
        if (c->surface.xwayland->window_type[i] ==
                srv->netatom[NetWMWindowTypeDialog] ||
            c->surface.xwayland->window_type[i] ==
                srv->netatom[NetWMWindowTypeSplash] ||
            c->surface.xwayland->window_type[i] ==
                srv->netatom[NetWMWindowTypeToolbar] ||
            c->surface.xwayland->window_type[i] ==
                srv->netatom[NetWMWindowTypeUtility])
            c->is_floating = 1;
}

void xwayland_ready_handler(struct wl_listener *listener, void *data)
{
    struct ewlc_server *srv = wl_container_of(listener, srv, xwayland_ready_listener);
    xcb_connection_t *xc = xcb_connect(srv->xwayland->display_name, NULL);
    int err = xcb_connection_has_error(xc);
    if (err) {
        fprintf(
            stderr,
            "xcb_connect to X server failed with code %d\n. Continuing with "
            "degraded functionality.\n",
            err);
        return;
    }

    /* Collect atoms we are interested in.  If get_atom returns 0, we will
     * not detect that window type. */
    srv->netatom[NetWMWindowTypeDialog] = get_atom(xc, "_NET_WM_WINDOW_TYPE_DIALOG");
    srv->netatom[NetWMWindowTypeSplash] = get_atom(xc, "_NET_WM_WINDOW_TYPE_SPLASH");
    srv->netatom[NetWMWindowTypeUtility] = get_atom(xc, "_NET_WM_WINDOW_TYPE_TOOLBAR");
    srv->netatom[NetWMWindowTypeToolbar] = get_atom(xc, "_NET_WM_WINDOW_TYPE_UTILITY");

    /* assign the one and only seat */
    wlr_xwayland_set_seat(srv->xwayland, srv->seat);

    xcb_disconnect(xc);
}

#endif

emacs_value Fwl_display_create(emacs_env *env, ptrdiff_t nargs,
                               emacs_value args[], void *data)
{
    struct wl_display *d = wl_display_create();
    return env->make_user_ptr(env, NULL, d);
}

emacs_value Fc_set_sigchld(emacs_env *env, ptrdiff_t nargs,
                           emacs_value args[], void *data)
{
    int v = env->extract_integer(env, args[0]);
    sigchld(v);
    return Qt;
}

emacs_value Fewlc_set_backend_listeners(emacs_env *env, ptrdiff_t nargs,
                                        emacs_value args[], void *data)
{
    struct ewlc_server *s = env->get_user_ptr(env, args[0]);
    struct wlr_backend *backend = env->get_user_ptr(env, args[1]);

    s->backend_new_output_listener.notify = backend_new_output_notify;
    wl_signal_add(&backend->events.new_output, &s->backend_new_output_listener);

    return Qt;
}

emacs_value Fwlr_backend_autocreate(emacs_env *env, ptrdiff_t nargs,
                                    emacs_value args[], void *data)
{
    struct wl_display *display = env->get_user_ptr(env, args[0]);
    struct wlr_backend *backend = wlr_backend_autocreate(display, NULL);
    if (backend)
        return env->make_user_ptr(env, NULL, backend);
    return Qnil;
}

emacs_value Fwlr_backend_get_renderer(emacs_env *env, ptrdiff_t nargs,
                                      emacs_value args[], void *data)
{
    struct wlr_backend *backend = env->get_user_ptr(env, args[0]);
    struct wlr_renderer *renderer = wlr_backend_get_renderer(backend);
    return env->make_user_ptr(env, NULL, renderer);
}

emacs_value Fwlr_compositor_create(emacs_env *env, ptrdiff_t nargs,
                                   emacs_value args[], void *data)
{
    struct wl_display *display = env->get_user_ptr(env, args[0]);
    struct wlr_renderer *renderer = env->get_user_ptr(env, args[1]);
    struct wlr_compositor *compositor = wlr_compositor_create(display, renderer);
    return env->make_user_ptr(env, NULL, compositor);
}

emacs_value Fwlr_seat_create(emacs_env *env, ptrdiff_t nargs,
                             emacs_value args[], void *data)
{
    char* name;
    ptrdiff_t len = 0;
    struct wl_display *display = env->get_user_ptr(env, args[0]);
    env->copy_string_contents(env, args[1], NULL, &len);
    name = malloc(sizeof(char) * len);
    env->copy_string_contents(env, args[1], name, &len);

    struct wlr_seat *seat = wlr_seat_create(display, name);
    // TODO: free the seat, put in finalizer ?
    return env->make_user_ptr(env, NULL, seat);
}

emacs_value Fwlr_cursor_create(emacs_env *env, ptrdiff_t nargs,
                               emacs_value args[], void *data)
{
    struct wlr_cursor *cursor = wlr_cursor_create();
    return env->make_user_ptr(env, NULL, cursor);
}

emacs_value Fwlr_xcursor_manager_create(emacs_env *env, ptrdiff_t nargs,
                                        emacs_value args[], void *data)
{
    /* don't bother with cursor theme - so name not assignable. */
    char *name = NULL;
    int size = env->extract_integer(env, args[0]);
    struct wlr_xcursor_manager *cursor_mgr = wlr_xcursor_manager_create(name, size);
    return env->make_user_ptr(env, NULL, cursor_mgr);
}

emacs_value Fwlr_output_layout_create(emacs_env *env, ptrdiff_t nargs,
                                      emacs_value args[], void *data)
{
    struct wlr_output_layout *output_layout = wlr_output_layout_create();
    return env->make_user_ptr(env, NULL, output_layout);
}

emacs_value Fwlr_xdg_shell_create(emacs_env *env, ptrdiff_t nargs,
                                  emacs_value args[], void *data)
{
    struct wl_display *display = env->get_user_ptr(env, args[0]);
    struct wlr_xdg_shell *xdg_shell = wlr_xdg_shell_create(display);
    return env->make_user_ptr(env, NULL, xdg_shell);
}

emacs_value Fwlr_xwayland_create(emacs_env *env, ptrdiff_t nargs,
                                 emacs_value args[], void *data)
{
    struct wl_display *display = env->get_user_ptr(env, args[0]);
    struct wlr_compositor *compositor = env->get_user_ptr(env, args[1]);
    struct wlr_xwayland *xwayland = wlr_xwayland_create(display, compositor, true);
    return env->make_user_ptr(env, NULL, xwayland);
}

emacs_value Fwlr_xdg_decoration_manager_v1_create(emacs_env *env, ptrdiff_t nargs,
                                                  emacs_value args[], void *data)
{
    struct wl_display *display = env->get_user_ptr(env, args[0]);
    struct wlr_xdg_decoration_manager_v1 *xdg_deco_mgr =
        wlr_xdg_decoration_manager_v1_create(display);
    return env->make_user_ptr(env, NULL, xdg_deco_mgr);
}

emacs_value Fwlr_export_dmabuf_manager_v1_create(emacs_env *env, ptrdiff_t nargs,
                                                 emacs_value args[], void *data)
{
    struct wl_display *display = env->get_user_ptr(env, args[0]);
    struct wlr_export_dmabuf_manager_v1 *mgr = wlr_export_dmabuf_manager_v1_create(display);
    return env->make_user_ptr(env, NULL, mgr);
}

emacs_value Fwlr_screencopy_v1_create(emacs_env *env, ptrdiff_t nargs,
                                      emacs_value args[], void *data)
{
    struct wl_display *display = env->get_user_ptr(env, args[0]);
    struct wlr_screencopy_manager_v1 *mgr = wlr_screencopy_manager_v1_create(display);
    return env->make_user_ptr(env, NULL, mgr);
}

emacs_value Fwlr_data_device_manager_create(emacs_env *env, ptrdiff_t nargs,
                                            emacs_value args[], void *data)
{
    struct wl_display *display = env->get_user_ptr(env, args[0]);
    struct wlr_data_device_manager *mgr = wlr_data_device_manager_create(display);
    return env->make_user_ptr(env, NULL, mgr);
}

emacs_value Fwlr_gamma_control_manager_v1_create(emacs_env *env, ptrdiff_t nargs,
                                                 emacs_value args[], void *data)
{
    struct wl_display *display = env->get_user_ptr(env, args[0]);
    struct wlr_gamma_control_manager_v1 *mgr = wlr_gamma_control_manager_v1_create(display);
    return env->make_user_ptr(env, NULL, mgr);
}

emacs_value Fwlr_primary_selection_v1_device_manager_create(emacs_env *env, ptrdiff_t nargs,
                                                            emacs_value args[], void *data)
{
    struct wl_display *display = env->get_user_ptr(env, args[0]);
    struct wlr_primary_selection_v1_device_manager *mgr =
        wlr_primary_selection_v1_device_manager_create(display);
    return env->make_user_ptr(env, NULL, mgr);
}

emacs_value Fwlr_viewporter_create(emacs_env *env, ptrdiff_t nargs,
                                   emacs_value args[], void *data)
{
    struct wl_display *display = env->get_user_ptr(env, args[0]);
    struct wlr_viewporter *viewporter = wlr_viewporter_create(display);
    return env->make_user_ptr(env, NULL, viewporter);
}

emacs_value Fwlr_xdg_output_manage_v1_create(emacs_env *env, ptrdiff_t nargs,
                                             emacs_value args[], void *data)
{
    struct wl_display *display = env->get_user_ptr(env, args[0]);
    struct wlr_output_layout *output_layout = env->get_user_ptr(env, args[1]);
    struct wlr_xdg_output_manager_v1 *mgr =
        wlr_xdg_output_manager_v1_create(display, output_layout);
    return env->make_user_ptr(env, NULL, mgr);
}

emacs_value Fwlr_cursor_attach_output_layout(emacs_env *env, ptrdiff_t nargs,
                                             emacs_value args[], void *data)
{
    struct wlr_cursor *cursor = env->get_user_ptr(env, args[0]);
    struct wlr_output_layout *output_layout = env->get_user_ptr(env, args[1]);
    wlr_cursor_attach_output_layout(cursor, output_layout);
    return Qt;
}

emacs_value Fewlc_backend_set_listeners(emacs_env *env, ptrdiff_t nargs,
                                        emacs_value args[], void *data)
{
    struct ewlc_server *s = env->get_user_ptr(env, args[0]);
    struct wlr_backend *backend = env->get_user_ptr(env, args[1]);

    s->backend_new_output_listener.notify = backend_new_output_notify;
    s->backend_new_input_listener.notify = backend_new_input_notify;

    wl_signal_add(backend->events.new_output, &s->backend_new_output_listener);
    wl_signal_add(&backend->events.new_input, &s->backend_new_input_listener);
}

emacs_value Fewlc_xdg_shell_set_listeners(emacs_env *env, ptrdiff_t nargs,
                                          emacs_value args[], void *data)
{
    struct ewlc_server *s = env->get_user_ptr(env, args[0]);
    struct wlr_xdg_shell *xdg_shell = env->get_user_ptr(env, args[1]);

    s->xdg_shell_new_surface_listener.notify = xdg_shell_new_surface_notify;
    wl_signal_add(&xdg_shell->events.new_surface, &s->xdg_shell_new_surface_listener);
}

emacs_value Fewlc_xdg_deco_set_listeners(emacs_env *env, ptrdiff_t nargs,
                                         emacs_value args[], void *data)
{
    struct ewlc_server *s = env->get_user_ptr(env, args[0]);
    struct wlr_xdg_decoration_manager_v1 *xdg_deco = env->get_user_ptr(env, args[1]);

    s->xdeco_mgr_new_top_level_decoration_listener.notify =
        xdeco_mgr_new_toplevel_decoration_notify;
    wl_signal_add(&xdg_deco->events.new_toplevel_decoration,
                  &s->xdeco_mgr_new_top_level_decoration_listener);
}

emacs_value Fewlc_cursor_set_listeners(emacs_env *env, ptrdiff_t nargs,
                                       emacs_value args[], void *data)

{
    struct ewlc_server *s = env->get_user_ptr(env, args[0]);
    struct wlr_cursor *cursor = env->get_user_ptr(env, args[1]);

    s->cursor_axis_listener.notify = cursor_axis_notify;
    s->cursor_button_listener.notify = cursor_button_notify;
    s->cursor_frame_listener.notify = cursor_frame_notify;
    s->cursor_motion_listener.notify = cursor_motion_notify;
    s->cursor_motion_absolute_listener.notify = cursor_motion_absolute_notify;

    wl_signal_add(&cursor->events.axis, &s->cursor_axis_listener);
    wl_signal_add(&cursor->events.button, &s->cursor_button_listener);
    wl_signal_add(&cursor->events.frame, &s->cursor_frame_listener);
    wl_signal_add(&cursor->events.motion, &s->cursor_motion_listener);
    wl_signal_add(&cursor->events.motion_absolute, &->cursor_motion_absolute_listener);
}

emacs_value Fewlc_seat_set_listeners(emacs_env *env, ptrdiff_t nargs,
                                     emacs_value args[], void *data)

{
    struct ewlc_server *s = env->get_user_ptr(env, args[0]);
    struct wlr_seat *seat = env->get_user_ptr(env, args[1]);

    s->seat_request_set_cursor_listener.notify = seat_request_set_cursor_notify;
    s->seat_request_set_selection_listener.notify = seat_request_set_selection_notify;
    s->seat_request_set_primary_selection_listener.notify =
        seat_request_set_primary_selection_notify;

    wl_signal_add(&seat->events.request_set_cursor, &s->seat_request_set_cursor_listener);
    wl_signal_add(&seat->events.request_set_selection,
                  &s->seat_request_set_selection_listener);
    wl_signal_add(&seat->events.request_set_primary_selection,
                  &s->seat_request_set_primary_selection_listener);
}

emacs_value Fewlc_xwayland_set_listeners(emacs_env *env, ptrdiff_t nargs,
                                         emacs_value args[], void *data)

{
    struct ewlc_server *s = env->get_user_ptr(env, args[0]);
    struct wlr_xwayland *xwayland = env->get_user_ptr(env, args[1]);

    s->xwayland_ready_listener.notify = xwayland_ready_notify;
    s->new_xwayland_surface_listener.notify = new_xwayland_surface_notify;

    wl_signal_add(&xwayland->events.ready, &s->xwayland_ready_listener);
    wl_signal_add(&xwayland->events.new_surface, &s->new_xwayland_surface_listener);
}

emacs_value Fewlc_make_server_ptr(emacs_env *env, ptrdiff_t nargs,
                                  emacs_value args[], void *data)
{
    struct ewlc_server *s  = calloc(1, sizeof(*s));
    return env->make_user_ptr(env, NULL, s);
}

struct ewlc_server *ewlc_start(emacs_env *env)
{
    const char *socket;
    char startup_cmd[] = "alacritty";
    struct ewlc_server *srv = calloc(1, sizeof(*srv));

    INFO("into");
    srv->e_env = env;

    e_message(srv->e_env, "ewlc_start: into");

    // Wayland requires XDG_RUNTIME_DIR for creating its communications
    // socket
    if (!getenv("XDG_RUNTIME_DIR"))
        ERROR("XDG_RUNTIME_DIR must be set");

    ewlc_setup(srv);

    srv->startup_pid = -1;
    srv->key_list = NULL;
    srv->event_list = NULL;
    srv->root_color[0] = 0.3;
    srv->root_color[1] = 0.3;
    srv->root_color[2] = 0.3;
    srv->root_color[3] = 1.0;
    srv->border_color[0] = 0.5;
    srv->border_color[1] = 0.5;
    srv->border_color[2] = 0.5;
    srv->border_color[3] = 1.0;
    srv->focus_color[0] = 1.0;
    srv->focus_color[1] = 0.0;
    srv->focus_color[2] = 0.0;
    srv->focus_color[3] = 1.0;

    /* Add a Unix socket to the Wayland display. */
    // const char *socket = wl_display_add_socket_auto(server.display);
    socket = wl_display_add_socket_auto(srv->display);

    if (!socket)
        ERROR("startup: display_add_socket_auto");

    /* Start the backend. This will enumerate outputs and inputs, become the DRM
     * master, etc */
    if (!wlr_backend_start(srv->backend))
        ERROR("startup: backend_start");

    // loop over backend events to make sure all input and outputs are created.
    handle_events(env, srv);

    /* Now that outputs are initialized, choose initial active_output based on
     * cursor position, and set default cursor image */
    srv->active_output = get_output_at_point(srv, srv->cursor->x, srv->cursor->y);

    /* XXX hack to get cursor to display in its initial location (100, 100)
     * instead of (0, 0) and then jumping.  still may not be fully
     * initialized, as the image/coordinates are not transformed for the
     * output when displayed here */
    wlr_cursor_warp_closest(srv->cursor, NULL, srv->cursor->x, srv->cursor->y);
    wlr_xcursor_manager_set_cursor_image(srv->cursor_mgr, "left_ptr", srv->cursor);

    /* Set the WAYLAND_DISPLAY environment variable to our socket and run the
     * startup command if requested. */
    setenv("WAYLAND_DISPLAY", socket, 1);
    srv->startup_pid = fork();
    if (srv->startup_pid < 0)
        EERROR("startup: fork");
    if (srv->startup_pid == 0) {
        execl("/bin/sh", "/bin/sh", "-c", startup_cmd, (void *)NULL);
        EERROR("startup: execl");
    }

    e_message(srv->e_env, "ewlc_start: leaving");

    INFO("leaving");
    return srv;
}

emacs_value Fc_kill(emacs_env *env, ptrdiff_t nargs,
                    emacs_value args[], void *data)
{
    int pid = env->extract_integer(env, args[0]);
    if (pid != -1) {
        kill(pid, SIGTERM);
        waitpid(pid, NULL, 0);
    }
    return Qt;
}

emacs_value Fwlr_xwayland_destroy(emacs_env *env, ptrdiff_t nargs,
                                  emacs_value args[], void *data)
{
    struct wlr_xwayland *xwayland = env->get_user_ptr(env, args[0]);
    wlr_xwayland_destroy(xwayland);
    return Qt;
}

emacs_value Fwl_display_destroy_clients(emacs_env *env, ptrdiff_t nargs,
                                        emacs_value args[], void *data)
{
    struct wl_display *display = env->get_user_ptr(env, args[0]);
    wl_display_destroy_clients(display);
    return Qt;
}

emacs_value Fwl_display_destroy(emacs_env *env, ptrdiff_t nargs,
                                emacs_value args[], void *data)
{
    struct wl_display *display = env->get_user_ptr(env, args[0]);
    wl_display_destroy(display);
    return Qt;
}

emacs_value Fwlr_xcursor_manager_destroy(emacs_env *env, ptrdiff_t nargs,
                                         emacs_value args[], void *data)
{
    struct wlr_xcursor_manager *manager = env->get_user_ptr(env, args[0]);
    wlr_xcursor_manager_destroy(manager);
    return Qt;
}

emacs_value Fwlr_cursor_destroy(emacs_env *env, ptrdiff_t nargs,
                                emacs_value args[], void *data)
{
    struct wlr_cursor *cursor = env->get_user_ptr(env, args[0]);
    wlr_cursor_destroy(cursor);
    return Qt;
}

emacs_value Fwlr_output_layout_destroy(emacs_env *env, ptrdiff_t nargs,
                                       emacs_value args[], void *data)
{
    struct wlr_output_layout *output_layout = env->get_user_ptr(env, args[0]);
    wlr_output_layout_destroy(output_layout);
    return Qt;
}

emacs_value Fwl_display_get_event_loop(emacs_env *env, ptrdiff_t nargs,
                                       emacs_value args[], void *data)
{
    struct wl_display *display = env->get_user_ptr(env, args[0]);
    struct wl_event_loop *loop = wl_display_get_event_loop(display);
    return env->make_user_ptr(env, NULL, loop);
}

emacs_value Fwl_display_flush_clients(emacs_env *env, ptrdiff_t nargs,
                                      emacs_value args[], void *data)
{
    struct wl_display *display = env->get_user_ptr(env, args[0]);
    wl_display_flush_clients(display);
    return Qt;
}

emacs_value Fwl_event_loop_dispatch(emacs_env *env, ptrdiff_t nargs,
                                    emacs_value args[], void *data)
{
    struct wl_event_loop *loop = env->get_user_ptr(env, args[0]);
    int timeout = env->extract_integer(env, args[1]);
    if (wl_event_loop_dispatch(loop, timeout) == 0)
        return Qt;
    return Qnil;
}

int handle_events(emacs_env *env, struct ewlc_server *srv)
{
    struct wl_listener *listener;
    struct wlr_input_device *device;
    struct ewlc_keyboard *kb;
    void *data;
    int type;
    int handled;
    emacs_value e_listener, e_data, e_kb, e_device;

    while (srv->event_list != NULL) {
        handled = 0;
        listener = srv->event_list->listener;
        data = srv->event_list->data;
        type = srv->event_list->type;

        switch (type) {
        case EWLC_CURSOR_AXIS:
            INFO("EWLC_CURSOR_AXIS");

            e_data = env->make_user_ptr(env, NULL, data);

            env->funcall(env, env->intern(env, "ewlc-pointer-axis-handler"),
                         1, (emacs_value[]){e_data});

            // cursor_axis_handler(listener, data);
            handled = 1;
            break;
        case EWLC_CURSOR_BUTTON:
            INFO("EWLC_CURSOR_BUTTON");

            e_data = env->make_user_ptr(env, NULL, data);

            env->funcall(env, env->intern(env, "ewlc-pointer-button-handler"),
                         1, (emacs_value[]){e_data});

            // cursor_button_handler(listener, data);
            handled = 1;
            break;
        case EWLC_CURSOR_MOTION:
            INFO("EWLC_CURSOR_MOTION");
            cursor_motion_handler(listener, data);
            handled = 1;
            break;
        case EWLC_CURSOR_FRAME:
            INFO("EWLC_CURSOR_FRAME");
            cursor_frame_handler(listener, data);
            handled = 1;
            break;
        case EWLC_CURSOR_MOTION_ABSOLUTE:
            INFO("EWLC_CURSOR_MOTION_ABSOLUTE");
            cursor_motion_absolute_handler(listener, data);
            handled = 1;
            break;
        case EWLC_SEAT_REQUEST_SET_CURSOR:
            INFO("EWLC_SEAT_REQUEST_SET_CURSOR");
            seat_request_set_cursor_handler(listener, data);
            handled = 1;
            break;
        case EWLC_SEAT_REQUEST_SET_PRIMARY_SELECTION:
            INFO("EWLC_SEAT_REQUEST_SET_PRIMARY_SELECTION");
            seat_request_set_primary_selection_handler(listener, data);
            handled = 1;
            break;
        case EWLC_SEAT_REQUEST_SET_SELECTION:
            INFO("EWLC_SEAT_REQUEST_SET_SELECTION");
            seat_request_set_selection_handler(listener, data);
            handled = 1;
            break;

        case EWLC_KEYBOARD_DESTROY:
            INFO("EWLC_KEYBOARD_DESTROY");

            kb = wl_container_of(listener, kb, keyboard_destroy_listener);
            e_kb = env->make_user_ptr(env, NULL, kb);

            env->funcall(env, env->intern(env, "ewlc-keyboard-destroy-handler"),
                         1, (emacs_value[]){e_kb});

            // keyboard_destroy_handler(listener, data);
            handled = 1;
            break;
        case EWLC_BACKEND_NEW_INPUT:
            INFO("EWLC_BACKEND_NEW_INPUT");

            device = data;
            e_device = env->make_user_ptr(env, NULL, device);

            env->funcall(env, env->intern(env, "ewlc-new-input-handler"),
                         1, (emacs_value[]){e_device});

            // backend_new_input_handler(listener, data);
            handled = 1;
            break;
        case EWLC_KEYBOARD_KEY:
            INFO("EWLC_KEYBOARD_KEY");
            keyboard_key_handler(listener, data);
            handled = 1;
            break;
        case EWLC_KEYBOARD_MODIFIERS:
            INFO("EWLC_KEYBOARD_MODIFIERS");

            kb = wl_container_of(listener, kb, keyboard_modifiers_listener);
            e_kb = env->make_user_ptr(env, NULL, kb);

            env->funcall(env, env->intern(env, "ewlc-keyboard-modifiers-handler"),
                         1, (emacs_value[]){e_kb});

            // keyboard_modifiers_handler(listener, data);
            handled = 1;
            break;


        case EWLC_XWAYLAND_READY:
            xwayland_ready_handler(listener, data);
            handled = 1;
            break;
        case EWLC_DECO_REQUEST_MODE:
            deco_request_mode_handler(listener, data);
            handled = 1;
            break;
        case EWLC_DECO_DESTROY:
            deco_destroy_handler(listener, data);
            handled = 1;
            break;
        case EWLC_NEW_TOPLEVEL_DECORATION:
            xdeco_mgr_new_toplevel_decoration_handler(listener, data);
            handled = 1;
            break;


        case EWLC_OUTPUT_DESTROY:
            INFO("EWLC_OUTPUT_DESTROY");
            output_destroy_handler(listener, data);
            handled = 1;
            break;
        case EWLC_OUTPUT_FRAME:
            // INFO("EWLC_OUTPUT_FRAME");
            output_frame_handler(listener, data);
            handled = 1;
            break;
        case EWLC_BACKEND_NEW_OUTPUT:
            INFO("EWLC_BACKEND_NEW_OUTPUT");
            backend_new_output_handler(listener, data);
            handled = 1;
            break;


        case EWLC_XDG_SURFACE_COMMIT:
            INFO("EWLC_XDG_SURFACE_COMMIT");
            xdg_surface_commit_handler(listener, data);
            handled = 1;
            break;
        case EWLC_XDG_SHELL_NEW_SURFACE:
            INFO("EWLC_XDG_SHELL_NEW_SURFACE");
            xdg_shell_new_surface_handler(listener, data);
            handled = 1;
            break;
        case EWLC_SURFACE_DESTROY:
            INFO("EWLC_SURFACE_DESTROY");
            surface_destroy_handler(listener, data);
            handled = 1;
            break;
        case EWLC_SURFACE_MAP:
            INFO("EWLC_SURFACE_MAP");
            surface_map_handler(listener, data);
            handled = 1;
            break;
        case EWLC_SURFACE_UNMAP:
            INFO("EWLC_SURFACE_UNMAP");
            surface_unmap_handler(listener, data);
            handled = 1;
            break;
        case EWLC_X_SURFACE_REQUEST_ACTIVATE:
            INFO("EWLC_X_SURFACE_REQUEST_ACTIVATE");
            xwayland_surface_request_activate_handler(listener, data);
            handled = 1;
            break;
        case EWLC_NEW_X_SURFACE:
            INFO("EWLC_NEW_X_SURFACE");
            new_xwayland_surface_handler(listener, data);
            handled = 1;
            break;
        }

        if (handled == 0) {
            DEBUG("Error: event not handled! type: %d", handled);
            return handled;
        }

        srv->event_list = remove_event(srv->event_list);
    }
    return handled;
}

// ----------------------------------------------------------------------

void xwayland_ready_notify(struct wl_listener *listener, void *data)
{
    struct ewlc_server *s = wl_container_of(listener, s, xwayland_ready_listener);
    struct event_node *e_node;

    e_node = create_event(listener, data, EWLC_XWAYLAND_READY);
    s->event_list = add_event(s->event_list, e_node);
}

void deco_request_mode_notify(struct wl_listener *listener, void *data)
{
    struct ewlc_decoration *d;
    struct ewlc_server *s;
    struct event_node *e_node;

    d = wl_container_of(listener, d, deco_request_mode_listener);
    s = d->server;
    e_node = create_event(listener, data, EWLC_DECO_REQUEST_MODE);
    s->event_list = add_event(s->event_list, e_node);
}

void deco_destroy_notify(struct wl_listener *listener, void *data)
{
    struct ewlc_decoration *d;
    struct ewlc_server *s;
    struct event_node *e_node;

    d = wl_container_of(listener, d, deco_destroy_listener);
    s = d->server;
    e_node = create_event(listener, data, EWLC_DECO_DESTROY);
    s->event_list = add_event(s->event_list, e_node);
}

void xdeco_mgr_new_toplevel_decoration_notify(struct wl_listener *listener, void *data)
{
    struct ewlc_server *s;
    struct event_node *e_node;

    s = wl_container_of(listener, s, xdeco_mgr_new_top_level_decoration_listener);
    e_node = create_event(listener, data, EWLC_NEW_TOPLEVEL_DECORATION);
    s->event_list = add_event(s->event_list, e_node);
}
