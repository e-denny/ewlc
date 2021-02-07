/*
 See LICENSE file for copyright and license details.
 */
#define _POSIX_C_SOURCE 200809L
#include <emacs-module.h>
#include "Fwlr.h
#include <wayland-client.h>
#include <wayland-server-core.h>
#include <wlr/types/wlr_output.h>

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

emacs_value Fwlr_output_set_scale(emacs_env *env, ptrdiff_t nargs,
                                  emacs_value args[], void *data)
{
    struct wlr_output *wlr_output = env->get_user_ptr(env, args[0]);
    float scale = env->extract_float(env, args[1]);
    wlr_output_set_scale(wlr_output, scale);
    return Qt;
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

emacs_value Fwlr_output_attach_render(emacs_env *env, ptrdiff_t nargs,
                                      emacs_value args[], void *data)
{
    struct wlr_output *output = env->get_user_ptr(env, args[0]);
    if (wlr_output_attach_render(output, NULL))
        return Qt;
    return Qnil;
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

void init_wlr_output(emacs_env *env)
{
    emacs_value func;
    func = env->make_function(env, 2, 2, Fwlr_output_set_mode, "", NULL);
    bind_function(env, "wlr-output-set-mode", func);

    func = env->make_function(env, 1, 1, Fwlr_output_preferred_mode, "", NULL);
    bind_function(env, "wlr-output-preferred-mode", func);

    func = env->make_function(env, 2, 2, Fwlr_output_set_scale"", NULL);
    bind_function(env, "wlr-output-set-scale", func);

    func = env->make_function(env, 1, 1, Fwlr_output_set_transform"", NULL);
    bind_function(env, "wlr-output-set-transform", func);

    func = env->make_function(env, 1, 1, Fwlr_output_enable"", NULL);
    bind_function(env, "wlr-output-enable", func);

    func = env->make_function(env, 1, 1, Fwlr_output_attach_render"", NULL);
    bind_function(env, "wlr-output-attach-render", func);

    func = env->make_function(env, 1, 1, Fwlr_output_commit"", NULL);
    bind_function(env, "wlr-output-commit", func);

    func = env->make_function(env, 1, 1, Fwlr_output_render_software_cursors"", NULL);
    bind_function(env, "wlr-output-render-software-cursors", func);
}
