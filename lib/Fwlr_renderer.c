/*
 See LICENSE file for copyright and license details.
 */
#define _POSIX_C_SOURCE 200809L
#include <emacs-module.h>
#include "module.h"
#include "Fwlr.h"
#include <wayland-client.h>
#include <wayland-server-core.h>
#include <wlr/render/wlr_renderer.h>
#include <wlr/types/wlr_output.h>
#include <wlr/types/wlr_box.h>

emacs_value Fwlr_render_begin(emacs_env *env, ptrdiff_t nargs,
                              emacs_value args[], void *data)
{
    struct wlr_renderer *renderer = env->get_user_ptr(env, args[0]);
    struct wlr_output *output = env->get_user_ptr(env, args[1]);
    wlr_renderer_begin(renderer, output->width, output->height);
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

void init_wlr_renderer(emacs_env *env)
{
    emacs_value func;
    func = env->make_function(env, 2, 2, Fwlr_render_begin, "", NULL);
    bind_function(env, "wlr-render-begin", func);

    func = env->make_function(env, 2, 2, Fwlr_render_clear, "", NULL);
    bind_function(env, "wlr-render-clear", func);

    func = env->make_function(env, 1, 1, Fwlr_render_end, "", NULL);
    bind_function(env, "wlr-render-end", func);

    func = env->make_function(env, 4, 4, Fwlr_render_rect, "", NULL);
    bind_function(env, "wlr-render-rect", func);
}
