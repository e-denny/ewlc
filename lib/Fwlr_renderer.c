/*
 See LICENSE file for copyright and license details.
 */
#define _POSIX_C_SOURCE 200809L
#include <emacs-module.h>
#include "module.h"
#include "notify.h"
#include "Fwlr.h"
#include <wayland-client.h>
#include <wayland-server-core.h>
#include <wlr/render/wlr_renderer.h>
#include <wlr/types/wlr_output.h>
#include <wlr/types/wlr_box.h>

emacs_value Fwlr_renderer_begin(emacs_env *env, ptrdiff_t nargs,
                              emacs_value args[], void *data)
{
    struct wlr_renderer *renderer = env->get_user_ptr(env, args[0]);
    struct wlr_output *output = env->get_user_ptr(env, args[1]);
    wlr_renderer_begin(renderer, output->width, output->height);
    return Qt;
}

emacs_value Fwlr_renderer_clear(emacs_env *env, ptrdiff_t nargs,
                              emacs_value args[], void *data)
{
    struct wlr_renderer *renderer = env->get_user_ptr(env, args[0]);
    emacs_value color_list = args[1];
    float root_color[4];
    emacs_value c;
    c = nth(env, 0, color_list);
    root_color[0] = env->extract_float(env, c);
    c = nth(env, 1, color_list);
    root_color[1] = env->extract_float(env, c);
    c = nth(env, 2, color_list);
    root_color[2] = env->extract_float(env, c);
    c = nth(env, 3, color_list);
    root_color[3] = env->extract_float(env, c);
    wlr_renderer_clear(renderer, root_color);
    return Qt;
}

emacs_value Fwlr_renderer_end(emacs_env *env, ptrdiff_t nargs,
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
    emacs_value color_list = args[2];
    struct wlr_output *output = env->get_user_ptr(env, args[3]);
    float color[4];
    emacs_value c;
    INFO(">>>>>>>>");
    c = nth(env, 0, color_list);
    color[0] = env->extract_float(env, c);
    c = nth(env, 1, color_list);
    color[1] = env->extract_float(env, c);
    c = nth(env, 2, color_list);
    color[2] = env->extract_float(env, c);
    c = nth(env, 3, color_list);
    color[3] = env->extract_float(env, c);
    wlr_render_rect(renderer, box, color, output->transform_matrix);
    INFO("<<<<<<<<<<<<");
    return Qt;
}

emacs_value Fwlr_renderer_init_wl_display(emacs_env *env, ptrdiff_t nargs,
                                          emacs_value args[], void *data)
{
    struct wlr_renderer *r = env->get_user_ptr(env, args[0]);
    struct wl_display *d = env->get_user_ptr(env, args[1]);
    if (wlr_renderer_init_wl_display(r, d))
        return Qt;
    return Qnil;
}

void init_wlr_renderer(emacs_env *env)
{
    emacs_value func;
    func = env->make_function(env, 2, 2, Fwlr_renderer_begin, "", NULL);
    bind_function(env, "wlr-renderer-begin", func);

    func = env->make_function(env, 2, 2, Fwlr_renderer_clear, "", NULL);
    bind_function(env, "wlr-renderer-clear", func);

    func = env->make_function(env, 1, 1, Fwlr_renderer_end, "", NULL);
    bind_function(env, "wlr-renderer-end", func);

    func = env->make_function(env, 4, 4, Fwlr_render_rect, "", NULL);
    bind_function(env, "wlr-render-rect", func);

    func = env->make_function(env, 2, 2, Fwlr_renderer_init_wl_display, "", NULL);
    bind_function(env, "wlr-renderer-init-wl-display", func);
}
