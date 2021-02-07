/*
 See LICENSE file for copyright and license details.
 */
#define _POSIX_C_SOURCE 200809L
#include "server.h"
#include "util.h"
#include "client.h"
#include "output.h"
// #include <linux/input-event-codes.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <time.h>
//#include <unistd.h>
#include <wayland-client.h>
#include <wayland-server-core.h>
#include <wlr/backend.h> */
#include <wlr/render/wlr_renderer.h> */
#include <wlr/types/wlr_compositor.h> */
#include <wlr/types/wlr_cursor.h> */
#include <wlr/types/wlr_data_device.h> */
#include <wlr/types/wlr_export_dmabuf_v1.h> */
#include <wlr/types/wlr_gamma_control_v1.h> */
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

#include <X11/Xlib.h>
#include <wlr/xwayland.h>

emacs_value Fwl_display_terminate(emacs_env *env, ptrdiff_t nargs,
                                  emacs_value args[], void *data)
{
    struct wl_display *display = env->get_user_ptr(env, args[0]);
    wl_display_terminate(display);
    return Qt;
}

emacs_value Fwl_display_create(emacs_env *env, ptrdiff_t nargs,
                               emacs_value args[], void *data)
{
    struct wl_display *d = wl_display_create();
    return env->make_user_ptr(env, NULL, d);
}

emacs_value Fwl_display_add_socket_auto(emacs_env *env, ptrdiff_t nargs,
                                        emacs_value args[], void *data)
{
    struct wl_display *display = env->get_user_ptr(env, args[0]);
    char *socket = wl_display_add_socket_auto(srv->display);
    return env->make_string(env, socket, strlen(socket));
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
