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

#ifdef XWAYLAND
#include <X11/Xlib.h>
#include <wlr/xwayland.h>
#endif

emacs_value Fewlc_compare_outputs(emacs_env *env, ptrdiff_t nargs,
                                  emacs_value args[], void *data)
{
    struct ewlc_output *o_a = env->get_user_ptr(env, args[0]);
    struct ewlc_output *o_b = env->get_user_ptr(env, args[1]);
    if (o_a == o_b)
        return Qt;
    return Qnil;
}

emacs_value Fewlc_output_layout_get_box(emacs_env *env, ptrdiff_t nargs,
                                        emacs_value args[], void *data)
{
    struct wlr_output *output = env->get_user_ptr(env, args[0]);
    struct wlr_output_layout *output_layout = env->get_user_ptr(env, args[1]);
    struct wlr_box *output_box;

    output_box = wlr_output_layout_get_box(output_layout, output);
    return env->make_user_ptr(env, NULL, output_box);
}

emacs_value Fewlc_output_set_event_listeners(emacs_env *env, ptrdiff_t nargs,
                                             emacs_value args[], void *data)
{
    struct ewlc_output *e_output = env->get_user_ptr(env, args[0]);
    struct wlr_output *w_output = env->get_user_ptr(env, args[1]);

    /* Set up event listeners */
    e_output->output_frame_listener.notify = output_frame_notify;
    wl_signal_add(&w_output->events.frame, &e_output->output_frame_listener);
    e_output->output_destroy_listener.notify = output_destroy_notify;
    wl_signal_add(&w_output->events.destroy, &e_output->output_destroy_listener);
}

emacs_value Fwlr_output_set_mode(emacs_env *env, ptrdiff_t nargs,
                                 emacs_value args[], void *data)
{
    struct wlr_output *output = env->get_user_ptr(env, args[0]);
    struct wlr_output_mode *output_mode = env->get_user_ptr(env, args[1]);
    wlr_output_set_mode(output, output_mode);
    return Qt;
}

emacs_value Fwlr_output_preferred_mode(emacs_env *env, ptrdiff_t nargs,
                                       emacs_value args[], void *data)
{
    struct wlr_output *output = env->get_user_ptr(env, args[0]);
    struct wlr_output_mode *output_mode = wlr_output_preferred_mode(output);
    return env->make_user_ptr(env, NULL, output_mode);
}

emacs_value Fewlc_make_output_ptr(emacs_env *env, ptrdiff_t nargs,
                                  emacs_value args[], void *data)
{
    struct ewlc_output *o;
    struct wlr_output *wlr_output = env->get_user_ptr(env, args[0]);
    o = wlr_output->data = calloc(1, sizeof(*o));
    o->wlr_output = wlr_output;
    return env->make_user_ptr(env, NULL, o);
}

emacs_value Fwlr_output_set_scale(emacs_env *env, ptrdiff_t nargs,
                                  emacs_value args[], void *data)
{
    struct wlr_output *wlr_output = env->get_user_ptr(env, args[0]);
    float scale = env->extract_float(env, args[1]);
    wlr_output_set_scale(wlr_output, scale);
    return Qt;
}

emacs_value Fwlr_xcursor_manager_load(emacs_env *env, ptrdiff_t nargs,
                                      emacs_value args[], void *data)
{
    struct wlr_xcursor_manager *cursor_mgr = env->get_user_ptr(env, args[0]);
    float scale = env->extract_float(env, args[1]);
    if (wlr_xcursor_manager_load(cursor_mgr, scale))
        return Qt;
    return Qnil;
}

emacs_value Fwlr_output_set_transform(emacs_env *env, ptrdiff_t nargs,
                                      emacs_value args[], void *data)
{
    struct wlr_output *wlr_output = env->get_user_ptr(env, args[0]);
    // FIXME: add option of other transform enums.
    wlr_output_set_transform(wlr_output, WL_OUTPUT_TRANSFORM_NORMAL);
    return Qt;
}

emacs_value Fwlr_output_enable(emacs_env *env, ptrdiff_t nargs,
                               emacs_value args[], void *data)
{
    struct wlr_output *wlr_output = env->get_user_ptr(env, args[0]);
    wlr_output_enable(wlr_output, 1);
    return Qt;
}

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

emacs_value Fwlr_surface_send_frame_done(emacs_env *env, ptrdiff_t nargs,
                                         emacs_value args[], void *data)
{
    struct wlr_surface *surface = env->get_user_ptr(env, args[0]);
    // extract_time requires emacs 27
    struct timespec now = env->extract_time(env, args[1]);

    wlr_surface_send_frame_done(surface, &now);
    return Qt;
}

emacs_value Fwlr_output_attach_render(emacs_env *env, ptrdiff_t nargs,
                                      emacs_value args[], void *data)
{
    struct wlr_output *output = env->get_user_ptr(env, args[0]);
    if (wlr_output_attach_render(output, NULL))
        return Qt;
    return Qnil;
}

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

emacs_value Fwlr_output_commit(emacs_env *env, ptrdiff_t nargs,
                               emacs_value args[], void *data)
{
    struct wlr_output *output = env->get_user_ptr(env, args[0]);
    if (wlr_output_commit(output))
        return Qt;
    return Qnil;
}

emacs_value Fwlr_output_render_software_cursors(emacs_env *env, ptrdiff_t nargs,
                                                emacs_value args[], void *data)
{
    struct wlr_output *output = env->get_user_ptr(env, args[0]);
    wlr_output_render_software_cursors(output, NULL);
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

// ----------------------------------------------------------------------

void backend_new_output_notify(struct wl_listener *listener, void *data)
{
    /* This event is raised by the backend when a new output (aka a display or
     * output) becomes available. */

    struct ewlc_server *s;
    struct event_node *e;

    s = wl_container_of(listener, s, backend_new_output_listener);
    e = create_event(listener, data, EWLC_BACKEND_NEW_OUTPUT);
    s->event_list = add_event(s->event_list, e);
}

void output_destroy_notify(struct wl_listener *listener, void *data)
{
    struct ewlc_server *s;
    struct ewlc_output *o;
    struct event_node *e;

    o = wl_container_of(listener, o, output_destroy_listener);
    s = o->server;
    e = create_event(listener, data, EWLC_OUTPUT_DESTROY);
    s->event_list = add_event(s->event_list, e);
}

void output_frame_notify(struct wl_listener *listener, void *data)
{
    /* This function is called every time an output is ready to display a frame,
     * generally at the output's refresh rate (e.g. 60Hz). */

    struct ewlc_output *o;
    struct ewlc_server *s;
    struct event_node *e;

    o = wl_container_of(listener, o, output_frame_listener);
    s = o->server;
    e = create_event(listener, data, EWLC_OUTPUT_FRAME);
    s->event_list = add_event(s->event_list, e);
}
