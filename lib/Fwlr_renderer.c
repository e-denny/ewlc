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

emacs_value Fwlr_render_begin(emacs_env *env, ptrdiff_t nargs,
                              emacs_value args[], void *data)
{
    struct wlr_renderer *renderer = env->get_user_ptr(env, args[0]);
    struct wlr_output *output = env->get_user_ptr(env, args[1]);
    wlr_renderer_begin(renderer, output->width, output->height))
    return Qt;
}

emacs_value Fwlr_render_clear(emacs_env *env, ptrdiff_t nargs,
                              emacs_value args[], void *data)
{
    struct wlr_renderer *renderer = env->get_user_ptr(env, args[0]);
    float *root_color = env->get_user_ptr(env, args[1]);
    wlr_renderer_clear(renderer, root_color);
    return Qt;
}

emacs_value Fwlr_render_end(emacs_env *env, ptrdiff_t nargs,
                            emacs_value args[], void *data)
{
    struct wlr_renderer *renderer = env->get_user_ptr(env, args[0]);
    wlr_renderer_end(renderer);
    return Qt;
}

emacs_value Fwlr_render_rect(emacs_env *env, ptrdiff_t nargs,
                             emacs_value args[], void *data)
{
    struct wlr_renderer *renderer = env->get_user_ptr(env, args[0]);
    struct wlr_box *box = env->get_user_ptr(env, args[1]);
    float *color = env->get_user_ptr(env, args[2]);
    struct wlr_output *output = env->get_user_ptr(env, args[3]);
    wlr_render_rect(renderer, box, color, output->transform_matrix);
    return Qt;
}
