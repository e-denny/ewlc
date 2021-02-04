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

emacs_value Fwl_display_terminate(emacs_env *env, ptrdiff_t nargs,
                                  emacs_value args[], void *data)
{
    struct wl_display *display = env->get_user_ptr(env, args[0]);
    wl_display_terminate(display);
    return Qt;
}

// TODO: need a finalizer to free pointer.
emacs_value Fewlc_make_deco_ptr(emacs_env *env, ptrdiff_t nargs,
                                emacs_value args[], void *data)
{
    struct wlr_xdg_toplevel_decoration_v1 *wlr_deco = env->get_user_ptr(env, args[0]);
    struct ewlc_server *s = env->get_user_ptr(env, args[1]);
    struct ewlc_decoration *d = wlr_deco->data = calloc(1, sizeof(*d));
    d->server = s;
    return env->make_user_ptr(env, NULL, d);
}

emacs_value Fewlc_set_xdg_deco_mgr_toplevel_listeners(emacs_env *env, ptrdiff_t nargs,
                                                      emacs_value args[], void *data)
{
    struct ewlc_decoration *d = env->get_user_ptr(env, args[0]);
    struct wlr_xdg_toplevel_decoration_v1 *wlr_deco = env->get_user_ptr(env, args[1]);

    d->deco_request_mode_listener.notify = deco_request_mode_notify;
    wl_signal_add(&wlr_deco->events.request_mode, &d->deco_request_mode_listener);

    d->deco_destroy_listener.notify = deco_destroy_notify;
    wl_signal_add(&wlr_deco->events.destroy, &d->deco_destroy_listener);

    return Qt;
}

void deco_destroy_handler(struct wl_listener *listener, void *data)
{
    struct wlr_xdg_toplevel_decoration_v1 *wlr_deco = data;
    struct ewlc_decoration *d = wlr_deco->data;

    wl_list_remove(&d->deco_destroy_listener.link);
    wl_list_remove(&d->deco_request_mode_listener.link);
    free(d);
}

emacs_value Fwlr_xdg_toplevel_decoration_v1_set_mode(emacs_env *env, ptrdiff_t nargs,
                                                     emacs_value args[], void *data)
{
    struct wlr_xdg_toplevel_decoration_v1 *wlr_deco = env->get_user_ptr(env, args[0]);
    wlr_xdg_toplevel_decoration_v1_set_mode(wlr_deco,
                                            WLR_XDG_TOPLEVEL_DECORATION_V1_MODE_SERVER_SIDE);
    return Qt;
}

void sigchld(int unused)
{
    if (signal(SIGCHLD, sigchld) == SIG_ERR)
        EERROR("can't install SIGCHLD handler");
    while (0 < waitpid(-1, NULL, WNOHANG));
}

emacs_value Fewlc_floating_type_p(emacs_env *env, ptrdiff_t nargs,
                                  emacs_value args[], void *data)
{
    struct wlr_xwayland_surface *surface = env->get_user_ptr(env, args[0]);
    Atom *netatom = env->get_user_ptr(env, args[1]);
    for (size_t i = 0; i < surface->window_type_len; i++)
        if (surface->window_type[i] == netatom[NetWMWindowTypeDialog] ||
            surface->window_type[i] == netatom[NetWMWindowTypeSplash] ||
            surface->window_type[i] == netatom[NetWMWindowTypeToolbar] ||
            surface->window_type[i] == netatom[NetWMWindowTypeUtility])
            return Qt;
    return Qnil;
}

emacs_value Fewlc_xcb_connect(emacs_env *env, ptrdiff_t nargs,
                              emacs_value args[], void *data)
{
    struct wlr_xwayland *xwayland = env->get_user_ptr(env, args[0]);
    xcb_connection_t *xc = xcb_connect(xwayland->display_name, NULL);
    return env->make_user_ptr(env, NULL, xc);
}

emacs_value Fewlc_xcb_disconnect(emacs_env *env, ptrdiff_t nargs,
                                 emacs_value args[], void *data)
{

    xcb_connection_t *xc = env->get_user_ptr(env, args[0]);
    xcb_disconnect(xc);
    return Qt;
}

emacs_value Fewlc_xcb_connect_has_error(emacs_env *env, ptrdiff_t nargs,
                                        emacs_value args[], void *data)
{
    xcb_connection_t *xc = env->get_user_ptr(env, args[0]);
    int err = xcb_connection_has_error(xc);
    return env->make_integer(env, err);
}

Atom get_atom(xcb_connection_t *xc, const char *name)
{
    Atom atom = 0;
    xcb_intern_atom_cookie_t cookie;
    xcb_intern_atom_reply_t *reply;

    cookie = xcb_intern_atom(xc, 0, strlen(name), name);
    if ((reply = xcb_intern_atom_reply(xc, cookie, NULL)))
        atom = reply->atom;
    free(reply);

    return atom;
}

emacs_value Fewlc_set_atoms(emacs_env *env, ptrdiff_t nargs,
                            emacs_value args[], void *data)
{
    xcb_connection_t *xc = env->get_user_ptr(env, args[0]);
    Atom *netatom = calloc(4, sizeof(Atom));

    // FIXME: where is the function get_atom()
    netatom[NetWMWindowTypeDialog] = get_atom(xc, "_NET_WM_WINDOW_TYPE_DIALOG");
    netatom[NetWMWindowTypeSplash] = get_atom(xc, "_NET_WM_WINDOW_TYPE_SPLASH");
    netatom[NetWMWindowTypeUtility] = get_atom(xc, "_NET_WM_WINDOW_TYPE_TOOLBAR");
    netatom[NetWMWindowTypeToolbar] = get_atom(xc, "_NET_WM_WINDOW_TYPE_UTILITY");

    return env->make_user_ptr(env, NULL, netatom);
}

emacs_value Fwlr_xwayland_set_seat(emacs_env *env, ptrdiff_t nargs,
                                   emacs_value args[], void *data)
{
    struct wlr_xwayland *xwayland = env->get_user_ptr(env, args[0]);
    struct wlr_seat *seat = env->get_user_ptr(env, args[1]);
    wlr_xwayland_set_seat(xwayland, seat);
    return Qt;
}

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

emacs_value Fwl_display_add_socket_auto(emacs_env *env, ptrdiff_t nargs,
                                        emacs_value args[], void *data)
{
    struct wl_display *display = env->get_user_ptr(env, args[0]);
    char *socket = wl_display_add_socket_auto(srv->display);
    return env->make_string(env, socket, strlen(socket));
}

emacs_value Fwlr_backend_start(emacs_env *env, ptrdiff_t nargs,
                               emacs_value args[], void *data)
{
    struct wlr_backend *backend = env->get_user_ptr(env, args[0]);
    if (wlr_backend_start(backend))
        return Qt;
    return Qnil;
}

emacs_value Fwlr_xwayland_display_name(emacs_env *env, ptrdiff_t nargs,
                                       emacs_value args[], void *data)
{
    struct wlr_xwayland *xwayland = env->get_user_ptr(env, args[0]);
    const char *name = xwayland->display_name;
    return env->make_string(env, name, strlen(name));
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

emacs_value Fewlc_pending_events_p(emacs_env *env, ptrdiff_t nargs,
                                   emacs_value args[], void *data)
{
    struct ewlc_server *s = env->get_user_ptr(env, args[0]);
    if (s->event_list != NULL)
        return Qt;
    return Qnil;
}

emacs_value Fewlc_pending_events_p(emacs_env *env, ptrdiff_t nargs,
                                   emacs_value args[], void *data)
{
    struct ewlc_server *s = env->get_user_ptr(env, args[0]);
    struct wl_listener *listener = s->event_list->listener;
    void *data = s->event_list->data;
    char *type = s->event_list->type;
    emacs_value ret[2];
    ret[0] = env->make_user_ptr(env, NULL, data);
    ret[1] = env->intern(env, type);
    return list(env, ret, 2);
}

emacs_value Fewlc_remove_event(emacs_env *env, ptrdiff_t nargs,
                               emacs_value args[], void *data)
{
    struct ewlc_server *s = env->get_user_ptr(env, args[0]);

    // FIXME: free the event here rather in lisp code
    srv->event_list = remove_event(srv->event_list);
    return Qt;
}

// ----------------------------------------------------------------------

void xwayland_ready_notify(struct wl_listener *listener, void *data)
{
    struct ewlc_server *s = wl_container_of(listener, s, xwayland_ready_listener);
    struct event_node *e_node;

    e_node = create_event(listener, data, "ewlc-xwayland-ready");
    s->event_list = add_event(s->event_list, e_node);
}

void deco_request_mode_notify(struct wl_listener *listener, void *data)
{
    struct ewlc_decoration *d;
    struct ewlc_server *s;
    struct event_node *e_node;

    d = wl_container_of(listener, d, deco_request_mode_listener);
    s = d->server;
    e_node = create_event(listener, data, "ewlc-deco-request-mode");
    s->event_list = add_event(s->event_list, e_node);
}

void deco_destroy_notify(struct wl_listener *listener, void *data)
{
    struct ewlc_decoration *d;
    struct ewlc_server *s;
    struct event_node *e_node;

    d = wl_container_of(listener, d, deco_destroy_listener);
    s = d->server;
    e_node = create_event(listener, data, "ewlc_deco_destroy");
    s->event_list = add_event(s->event_list, e_node);
}

void xdeco_mgr_new_toplevel_decoration_notify(struct wl_listener *listener, void *data)
{
    struct ewlc_server *s;
    struct event_node *e_node;

    s = wl_container_of(listener, s, xdeco_mgr_new_top_level_decoration_listener);
    e_node = create_event(listener, data, "ewlc_new_toplevel_decoration");
    s->event_list = add_event(s->event_list, e_node);
}
