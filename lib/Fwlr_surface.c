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


emacs_value Fwlr_surface_send_frame_done(emacs_env *env, ptrdiff_t nargs,
                                         emacs_value args[], void *data)
{
    struct wlr_surface *surface = env->get_user_ptr(env, args[0]);
    // extract_time requires emacs 27
    struct timespec now = env->extract_time(env, args[1]);

    wlr_surface_send_frame_done(surface, &now);
    return Qt;
}

emacs_value Fwlr_surface_surface_at(emacs_env *env, ptrdiff_t nargs,
                                    emacs_value args[], void *data)
{
    struct wlr_surface *surface = env->get_user_ptr(env, args[0]);
    double sx = env->extract_float(env, args[1]);
    double sy = env->extract_float(env, args[2]);
    double sub_x = 0;
    double sub_y = 0;
    struct wlr_surface *s = wlr_surface_surface_at(surface, sx, sy, &sub_x, &sub_y);
    emacs_value ret[3];
    ret[0] = env->make_user_ptr(env, NULL, s);
    ret[1] = env->make_float(env, sub_x);
    ret[2] = env->make_float(env, sub_y);
    return list(env, ret, 3);
}

emacs_value Fwlr_surface_current_width(emacs_env *env, ptrdiff_t nargs,
                                       emacs_value args[], void *data)
{
    struct wlr_surface *surface = env->get_user_ptr(env, args[0]);
    return env->make_user_ptr(env, NULL, surface->current.width);
}

emacs_value Fwlr_surface_current_height(emacs_env *env, ptrdiff_t nargs,
                                        emacs_value args[], void *data)
{
    struct wlr_surface *surface = env->get_user_ptr(env, args[0]);
    return env->make_user_ptr(env, NULL, surface->current.height);
}

emacs_value Fwlr_surface_for_each_surface_render(emacs_env *env, ptrdiff_t nargs,
                                                 emacs_value args[], void *data)
{
    struct wlr_surface *surface = env->get_user_ptr(env, args[0]);
    struct render_data *r_data = env->get_user_ptr(env, args[1]);
    wlr_surface_for_each_surface(surface, render_surface, r_data);
    return Qt;
}
