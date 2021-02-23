/*
 See LICENSE file for copyright and license details.
 */
#define _POSIX_C_SOURCE 200809L
#include <stdlib.h>
#include <emacs-module.h>
#include "module.h"
#include "Fwlr.h"
#include <wayland-client.h>
#include <wayland-server-core.h>
#include <wlr/types/wlr_cursor.h>
#include <wlr/types/wlr_box.h>

// FIXME: need finalizer to free
emacs_value Fwlr_box_create(emacs_env *env, ptrdiff_t nargs,
                            emacs_value args[], void *data)
{
    struct wlr_box *box = calloc(1, sizeof(*box));
    box->x = env->extract_integer(env, args[0]);
    box->y = env->extract_integer(env, args[1]);
    box->width = env->extract_integer(env, args[2]);
    box->height = env->extract_integer(env, args[3]);
    return env->make_user_ptr(env, NULL, box);
}

emacs_value Fwlr_box_contains_point(emacs_env *env, ptrdiff_t nargs,
                                    emacs_value args[], void *data)
{
    struct wlr_box *geom = env->get_user_ptr(env, args[0]);
    struct wlr_cursor *cursor = env->get_user_ptr(env, args[1]);
    if (wlr_box_contains_point(geom, cursor->x, cursor->y))
        return Qt;
    return Qnil;
}

emacs_value Fwlr_scale_box(emacs_env *env, ptrdiff_t nargs,
                           emacs_value args[], void *data)
{
    struct wlr_box *box = env->get_user_ptr(env, args[0]);
    float scale = env->extract_float(env, args[1]);
    box->x *= scale;
    box->y *= scale;
    box->width *= scale;
    box->height *= scale;
    return Qt;
}

emacs_value Fwlr_box_x(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    struct wlr_box *box = env->get_user_ptr(env, args[0]);
    return env->make_integer(env, box->x);
}

emacs_value Fwlr_set_box_x(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    struct wlr_box *box = env->get_user_ptr(env, args[0]);
    int val = env->extract_integer(env, args[1]);
    box->x = val;
    return Qt;
}

emacs_value Fwlr_box_y(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    struct wlr_box *box = env->get_user_ptr(env, args[0]);
    return env->make_integer(env, box->y);
}

emacs_value Fwlr_set_box_y(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    struct wlr_box *box = env->get_user_ptr(env, args[0]);
    int val = env->extract_integer(env, args[1]);
    box->y = val;
    return Qt;
}

emacs_value Fwlr_box_width(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    struct wlr_box *box = env->get_user_ptr(env, args[0]);
    return env->make_integer(env, box->width);
}

emacs_value Fwlr_set_box_width(emacs_env *env, ptrdiff_t nargs,
                               emacs_value args[], void *data)
{
    struct wlr_box *box = env->get_user_ptr(env, args[0]);
    int val = env->extract_integer(env, args[1]);
    box->width = val;
    return Qt;
}

emacs_value Fwlr_box_height(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    struct wlr_box *box = env->get_user_ptr(env, args[0]);
    return env->make_integer(env, box->height);
}

emacs_value Fwlr_set_box_height(emacs_env *env, ptrdiff_t nargs,
                                emacs_value args[], void *data)
{
    struct wlr_box *box = env->get_user_ptr(env, args[0]);
    int val = env->extract_integer(env, args[1]);
    box->height = val;
    return Qt;
}

void init_wlr_box(emacs_env *env)
{
    emacs_value func;
    func = env->make_function(env, 4, 4, Fwlr_box_create, "", NULL);
    bind_function(env, "wlr-box-create", func);

    func = env->make_function(env, 2, 2, Fwlr_box_contains_point, "", NULL);
    bind_function(env, "wlr-box-contains-point", func);

    func = env->make_function(env, 2, 2, Fwlr_scale_box, "", NULL);
    bind_function(env, "wlr-scale-box", func);

    func = env->make_function(env, 1, 1, Fwlr_box_x, "", NULL);
    bind_function(env, "wlr-box-x", func);

    func = env->make_function(env, 2, 2, Fwlr_set_box_x, "", NULL);
    bind_function(env, "wlr-set-box-x", func);

    func = env->make_function(env, 1, 1, Fwlr_box_y, "", NULL);
    bind_function(env, "wlr-box-y", func);

    func = env->make_function(env, 2, 2, Fwlr_set_box_y, "", NULL);
    bind_function(env, "wlr-set-box-y", func);

    func = env->make_function(env, 1, 1, Fwlr_box_width, "", NULL);
    bind_function(env, "wlr-box-width", func);

    func = env->make_function(env, 2, 2, Fwlr_set_box_width, "", NULL);
    bind_function(env, "wlr-set-box-width", func);

    func = env->make_function(env, 1, 1, Fwlr_box_height, "", NULL);
    bind_function(env, "wlr-box-height", func);

    func = env->make_function(env, 2, 2, Fwlr_set_box_height, "", NULL);
    bind_function(env, "wlr-set-box-height", func);
}
