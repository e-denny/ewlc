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
#include <wlr/types/wlr_output.h>
#include <wlr/types/wlr_cursor.h>
#include <wlr/types/wlr_box.h>
#include <wlr/types/wlr_output_layout.h>

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

emacs_value Fwlr_output_layout_get_box(emacs_env *env, ptrdiff_t nargs,
                                       emacs_value args[], void *data)
{
    struct wlr_output_layout *output_layout = env->get_user_ptr(env, args[0]);
    struct wlr_output *output = env->get_user_ptr(env, args[1]);
    struct wlr_box *box = wlr_output_layout_get_box(output_layout, output);
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
    DEBUG("wlr_output o: '%p'", o);
    if (o) {
        return env->make_user_ptr(env, NULL, o);
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
    struct wlr_output_layout *output_layout;
    struct wlr_output *output;
    double x, y;
    emacs_value coords[2];
    output_layout = env->get_user_ptr(env, args[0]);
    output = env->get_user_ptr(env, args[1]);
    x = (double)env->extract_integer(env, args[2]);
    y = (double)env->extract_integer(env, args[3]);
    wlr_output_layout_output_coords(output_layout, output, &x, &y);
    coords[0] = env->make_integer(env, (int)x);
    coords[1] = env->make_integer(env, (int)y);
    return list(env, coords, 2);
}

void init_wlr_output_layout(emacs_env *env)
{
    emacs_value func;
    func = env->make_function(env, 0, 0, Fwlr_output_layout_create, "", NULL);
    bind_function(env, "wlr-output-layout-create", func);

    func = env->make_function(env, 1, 1, Fwlr_output_layout_destroy, "", NULL);
    bind_function(env, "wlr-output-layout-destroy", func);

    func = env->make_function(env, 2, 2, Fwlr_output_layout_get_box, "", NULL);
    bind_function(env, "wlr-output-layout-get-box", func);

    func = env->make_function(env, 2, 2, Fwlr_output_layout_add_auto, "", NULL);
    bind_function(env, "wlr-output-layout-add-auto", func);

    func = env->make_function(env, 2, 2, Fwlr_output_layout_output_at, "", NULL);
    bind_function(env, "wlr-output-layout-output-at", func);

    func = env->make_function(env, 3, 3, Fwlr_output_layout_intersects, "", NULL);
    bind_function(env, "wlr-output-layout-intersects", func);

    func = env->make_function(env, 4, 4, Fwlr_output_layout_output_coords, "", NULL);
    bind_function(env, "wlr-output-layout-output-coords", func);
}
