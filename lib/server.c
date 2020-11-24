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

void xdeco_mgr_new_toplevel_decoration_notify(struct wl_listener *listener, void *data)
{
    struct wlr_xdg_toplevel_decoration_v1 *wlr_deco = data;
    struct ewlc_decoration *d = wlr_deco->data = calloc(1, sizeof(*d));

    d->deco_request_mode_listener.notify = deco_request_mode_notify;
    wl_signal_add(&wlr_deco->events.request_mode, &d->deco_request_mode_listener);

    d->deco_destroy_listener.notify = deco_destroy_notify;
    wl_signal_add(&wlr_deco->events.destroy, &d->deco_destroy_listener);

    deco_request_mode_notify(&d->deco_request_mode_listener, wlr_deco);
}

void deco_destroy_notify(struct wl_listener *listener, void *data)
{
    struct wlr_xdg_toplevel_decoration_v1 *wlr_deco = data;
    struct ewlc_decoration *d = wlr_deco->data;

    wl_list_remove(&d->deco_destroy_listener.link);
    wl_list_remove(&d->deco_request_mode_listener.link);
    free(d);
}

void deco_request_mode_notify(struct wl_listener *listener, void *data)
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

void xwayland_ready_notify(struct wl_listener *listener, void *data)
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

void ewlc_setup(struct ewlc_server *srv)
{
    /* The Wayland display is managed by libwayland. It handles accepting
     * clients from the Unix socket, manging Wayland globals, and so on. */
    INFO("into");
    srv->display = wl_display_create();

    /* clean up child processes immediately */
    sigchld(0);

    /* The backend is a wlroots feature which abstracts the underlying input and
     * output hardware. The autocreate option will choose the most suitable
     * backend based on the current environment, such as opening an X11 window
     * if an X11 server is running. The NULL argument here optionally allows you
     * to pass in a custom renderer if wlr_renderer doesn't meet your needs. The
     * backend uses the renderer, for example, to fall back to software cursors
     * if the backend does not support hardware cursors (some older GPUs
     * don't). */
    if (!(srv->backend = wlr_backend_autocreate(srv->display, NULL)))
        ERROR("couldn't create backend");

    /* If we don't provide a renderer, autocreate makes a GLES2 renderer for us.
     * The renderer is responsible for defining the various pixel formats it
     * supports for shared memory, this configures that for clients. */
    srv->renderer = wlr_backend_get_renderer(srv->backend);
    wlr_renderer_init_wl_display(srv->renderer, srv->display);

    /* This creates some hands-off wlroots interfaces. The compositor is
     * necessary for clients to allocate surfaces and the data device manager
     * handles the clipboard. Each of these wlroots interfaces has room for you
     * to dig your fingers in and play with their behavior if you want. Note
     * that the clients cannot set the selection directly without compositor
     * approval, */
    srv->compositor = wlr_compositor_create(srv->display, srv->renderer);
    wlr_export_dmabuf_manager_v1_create(srv->display);
    wlr_screencopy_manager_v1_create(srv->display);
    wlr_data_device_manager_create(srv->display);
    wlr_gamma_control_manager_v1_create(srv->display);
    wlr_primary_selection_v1_device_manager_create(srv->display);
    wlr_viewporter_create(srv->display);

    /* Creates an output layout, which is a wlroots utility for working with an
     * arrangement of screens in a physical layout. */
    srv->output_layout = wlr_output_layout_create();
    wlr_xdg_output_manager_v1_create(srv->display, srv->output_layout);

    /* Configure a listener to be notified when new outputs are available on the
     * backend. */
    wl_list_init(&srv->output_list);

    srv->backend_new_output_listener.notify = backend_new_output_notify;
    wl_signal_add(&srv->backend->events.new_output,
                  &srv->backend_new_output_listener);

    /* Set up our client lists and the xdg-shell. The xdg-shell is a
     * Wayland protocol which is used for application windows. For more
     * detail on shells, refer to the article:
     *
     * https://drewdevault.com/2018/07/29/Wayland-shells.html
     */
    wl_list_init(&srv->client_list);
    wl_list_init(&srv->client_focus_list);
    wl_list_init(&srv->client_stack_list);
    wl_list_init(&srv->independent_list);

    srv->xdg_shell = wlr_xdg_shell_create(srv->display);
    srv->xdg_shell_new_surface_listener.notify = xdg_shell_new_surface_notify;
    wl_signal_add(&srv->xdg_shell->events.new_surface,
                  &srv->xdg_shell_new_surface_listener);

    /* Use xdg_decoration protocol to negotiate swever side decorations */
    srv->xdeco_mgr = wlr_xdg_decoration_manager_v1_create(srv->display);

    srv->xdeco_mgr_new_top_level_decoration_listener.notify =
        xdeco_mgr_new_toplevel_decoration_notify;
    wl_signal_add(&srv->xdeco_mgr->events.new_toplevel_decoration,
                  &srv->xdeco_mgr_new_top_level_decoration_listener);

    /*
     * Creates a cursor, which is a wlroots utility for tracking the cursor
     * image shown on screen.
     */
    srv->cursor = wlr_cursor_create();
    wlr_cursor_attach_output_layout(srv->cursor, srv->output_layout);

    /* Creates an xcursor manager, another wlroots utility which loads up
     * Xcursor themes to source cursor images from and makes sure that cursor
     * images are available at all scale factors on the screen (necessary for
     * HiDPI support). Scaled cursors will be loaded with each output. */
    srv->cursor_mgr = wlr_xcursor_manager_create(NULL, 24);

    /*
     * wlr_cursor *only* displays an image on screen. It does not move around
     * when the pointer moves. However, we can attach input devices to it, and
     * it will generate aggregate events for all of them. In these events, we
     * can choose how we want to process them, forwarding them to clients and
     * moving the cursor around. More detail on this process is described in my
     * input handling blog post:
     *
     * https://drewdevault.com/2018/07/17/Input-handling-in-wlroots.html
     *
     * And more comments are sprinkled throughout the notify functions above.
     */

    srv->cursor_axis_listener.notify = cursor_axis_notify;
    srv->cursor_button_listener.notify = cursor_button_notify;
    srv->cursor_frame_listener.notify = cursor_frame_notify;
    srv->cursor_motion_listener.notify = cursor_motion_notify;
    srv->cursor_motion_absolute_listener.notify = cursor_motion_absolute_notify;

    wl_signal_add(&srv->cursor->events.axis, &srv->cursor_axis_listener);
    wl_signal_add(&srv->cursor->events.button, &srv->cursor_button_listener);
    wl_signal_add(&srv->cursor->events.frame, &srv->cursor_frame_listener);
    wl_signal_add(&srv->cursor->events.motion, &srv->cursor_motion_listener);
    wl_signal_add(&srv->cursor->events.motion_absolute,
                  &srv->cursor_motion_absolute_listener);

    /*
     * Configures a seat, which is a single "seat" at which a user sits and
     * operates the computer. This conceptually includes up to one keyboard,
     * pointer, touch, and drawing tablet device. We also rig up a listener to
     * let us know when new input devices are available on the backend.
     */
    wl_list_init(&srv->keyboard_list);

    srv->backend_new_input_listener.notify = backend_new_input_notify;
    wl_signal_add(&srv->backend->events.new_input, &srv->backend_new_input_listener);

    srv->seat = wlr_seat_create(srv->display, "seat0");
    DEBUG("srv->seat = '%p'", srv->seat);

    srv->seat_request_set_cursor_listener.notify = seat_request_set_cursor_notify;
    wl_signal_add(&srv->seat->events.request_set_cursor,
                  &srv->seat_request_set_cursor_listener);

    srv->seat_request_set_selection_listener.notify =
        seat_request_set_selection_notify;
    wl_signal_add(&srv->seat->events.request_set_selection,
                  &srv->seat_request_set_selection_listener);

    srv->seat_request_set_primary_selection_listener.notify =
        seat_request_set_primary_selection_notify;
    wl_signal_add(&srv->seat->events.request_set_primary_selection,
                  &srv->seat_request_set_primary_selection_listener);

#ifdef XWAYLAND
    /*
     * Initialise the XWayland X server.
     * It will be started when the first X client is started.
     */
    srv->xwayland =
        wlr_xwayland_create(srv->display, srv->compositor, true);
    if (srv->xwayland) {
        srv->xwayland_ready_listener.notify = xwayland_ready_notify;
        wl_signal_add(&srv->xwayland->events.ready, &srv->xwayland_ready_listener);

        srv->new_xwayland_surface_listener.notify = new_xwayland_surface_notify;
        wl_signal_add(&srv->xwayland->events.new_surface,
                      &srv->new_xwayland_surface_listener);

        setenv("DISPLAY", srv->xwayland->display_name, true);
    } else {
        fprintf(stderr,
                "Failed to setup XWayland X server, continuing without it.\n");
    }
#endif
    INFO("leaving");
}

struct ewlc_server *ewlc_start(emacs_env *env)
{
    const char *socket;
    char startup_cmd[] = "alacritty";
    struct ewlc_server *srv = calloc(1, sizeof(*srv));

    INFO("into");
    srv->e_env = env;

    // Wayland requires XDG_RUNTIME_DIR for creating its communications
    // socket
    if (!getenv("XDG_RUNTIME_DIR"))
        ERROR("XDG_RUNTIME_DIR must be set");

    ewlc_setup(srv);

    srv->startup_pid = -1;
    srv->key_list = NULL;
    srv->sloppyfocus = 1;
    srv->border_px = 1;
    srv->repeat_rate = 25;
    srv->repeat_delay = 600;
    strncpy(srv->broken, "broken", 7);
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

int ewlc_cleanup(struct ewlc_server *srv)
{
    e_message(srv->e_env, "ewlc_cleanup: enter");

    if (srv->startup_pid != -1) {
        kill(srv->startup_pid, SIGTERM);
        waitpid(srv->startup_pid, NULL, 0);
    }

#ifdef XWAYLAND
    wlr_xwayland_destroy(srv->xwayland);
#endif
    wl_display_destroy_clients(srv->display);
    wl_display_destroy(srv->display);

    wlr_xcursor_manager_destroy(srv->cursor_mgr);
    wlr_cursor_destroy(srv->cursor);

    wlr_output_layout_destroy(srv->output_layout);
    free(srv);

    return EXIT_SUCCESS;
}

int ewlc_display_dispatch(struct ewlc_server *srv)
{
    struct wl_event_loop *loop = wl_display_get_event_loop(srv->display);
    // INFO("into");

    wl_display_flush_clients(srv->display);
    wl_event_loop_dispatch(loop, -1);
    // INFO("leaving");
    return 0;
}
