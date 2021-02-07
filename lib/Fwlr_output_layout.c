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
/* #include <wlr/backend.h> */
/* #include <wlr/render/wlr_renderer.h> */
/* #include <wlr/types/wlr_compositor.h> */
/* #include <wlr/types/wlr_cursor.h> */
/* #include <wlr/types/wlr_data_device.h> */
/* #include <wlr/types/wlr_export_dmabuf_v1.h> */
/* #include <wlr/types/wlr_gamma_control_v1.h> */
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
// #include <wlr/types/wlr_xdg_decoration_v1.h>
#include <wlr/types/wlr_xdg_output_v1.h>
#include <wlr/types/wlr_xdg_shell.h>
#include <wlr/util/log.h>
#include <xkbcommon/xkbcommon.h>

#include <X11/Xlib.h>
#include <wlr/xwayland.h>

emacs_value Fwlr_output_layout_get_box(emacs_env *env, ptrdiff_t nargs,
                                       emacs_value args[], void *data)
{
    struct wlr_output_layout *output_layout = env->get_user_ptr(env, args[0]);
    struct wlr_box *box = wlr_output_layout_get_box(output_layout, NULL);
    return env->make_user_ptr(env, NULL, box);
}

emacs_value Fwlr_output_layout_add_auto(emacs_env *env, ptrdiff_t nargs,
                                        emacs_value args[], void *data)
{
    struct wlr_output_layout *output_layout = env->get_user_ptr(env, args[0]);
    struct wlr_output *output = env->get_user_ptr(env, args[1]);
    wlr_output_layout_add_auto(output_layout, output);
    return Qt;
}

emacs_value Fwlr_output_layout_output_at(emacs_env *env, ptrdiff_t nargs,
                                         emacs_value args[], void *data)
{
    struct wlr_output_layout *output_layout = env->get_user_ptr(env, args[0]);
    struct wlr_cursor *cursor = env->get_user_ptr(env, args[1]);
    struct wlr_output *o = wlr_output_layout_output_at(output_layout, cursor->x, cursor->y);
    if (o) {
        return env->make_user_ptr(env, NULL, o->data);
    }
    return Qnil;
}

emacs_value Fwlr_output_layout_intersects(emacs_env *env, ptrdiff_t nargs,
                                          emacs_value args[], void *data)
{
    struct wlr_output_layout *output_layout = env->get_user_ptr(env, args[0]);
    struct wlr_output *output = env->get_user_ptr(env, args[1]);
    struct wlr_box *box = env->get_user_ptr(env, args[2]);
    if (wlr_output_layout_intersects(output_layout, output, box))
        return Qt;
    return Qnil;
}

emacs_value Fwlr_output_layout_output_coords(emacs_env *env, ptrdiff_t nargs,
                                             emacs_value args[], void *data)
{
    struct wlr_output_layout *output_layout = env->get_user_ptr(env, args[0]);
    struct wlr_output *output = env->get_user_ptr(env, args[1]);
    double ox = env->extract_float(env, args[2]);
    double oy = env->extract_float(env, args[3]);
    emacs_value coords[2];
    wlr_output_layout_output_coords(output_layout, output, &ox, &oy);
    coords[0] = env->make_float(env, ox);
    coords[1] = env->make_float(env, oy);
    return list(env, coords, 2);
}

emacs_value Fwlr_output_layout_get_box(emacs_env *env, ptrdiff_t nargs,
                                       emacs_value args[], void *data)
{
    struct wlr_output *output = env->get_user_ptr(env, args[0]);
    struct wlr_output_layout *output_layout = env->get_user_ptr(env, args[1]);
    struct wlr_box *output_box;

    output_box = wlr_output_layout_get_box(output_layout, output);
    return env->make_user_ptr(env, NULL, output_box);
}

emacs_value Fwlr_output_layout_create(emacs_env *env, ptrdiff_t nargs,
                                      emacs_value args[], void *data)
{
    struct wlr_output_layout *output_layout = wlr_output_layout_create();
    return env->make_user_ptr(env, NULL, output_layout);
}

emacs_value Fwlr_output_layout_destroy(emacs_env *env, ptrdiff_t nargs,
                                       emacs_value args[], void *data)
{
    struct wlr_output_layout *output_layout = env->get_user_ptr(env, args[0]);
    wlr_output_layout_destroy(output_layout);
    return Qt;
}
