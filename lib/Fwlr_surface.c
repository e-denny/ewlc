#define _POSIX_C_SOURCE 200809L
#include <emacs-module.h>
#include "module.h"
#include "Fwlr.h"
#include "server.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <time.h>
#include <wayland-client.h>
#include <wayland-server-core.h>
#include <wlr/types/wlr_surface.h>
#include <wlr/types/wlr_xdg_shell.h>


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

emacs_value Fwlr_surface_send_leave(emacs_env *env, ptrdiff_t nargs,
                                    emacs_value args[], void *data)
{
    struct wlr_surface *surface = env->get_user_ptr(env, args[0]);
    struct wlr_output *output = env->get_user_ptr(env, args[1]);
    wlr_surface_send_leave(surface, output);
    return Qt;
}

emacs_value Fwlr_surface_send_enter(emacs_env *env, ptrdiff_t nargs,
                                    emacs_value args[], void *data)
{
    struct wlr_surface *surface = env->get_user_ptr(env, args[0]);
    struct wlr_output *output = env->get_user_ptr(env, args[1]);
    wlr_surface_send_enter(surface, output);
    return Qt;
}

emacs_value Fwlr_surface_for_each_surface_render(emacs_env *env, ptrdiff_t nargs,
                                                 emacs_value args[], void *data)
{
    struct wlr_surface *surface = env->get_user_ptr(env, args[0]);
    struct render_data *r_data = env->get_user_ptr(env, args[1]);
    wlr_surface_for_each_surface(surface, render_surface, r_data);
    return Qt;
}

void init_wlr_surface(emacs_env *env)
{
    emacs_value func;
    func = env->make_function(env, 2, 2, Fwlr_surface_send_frame_done, "", NULL);
    bind_function(env, "wlr-surface-send-frame-done", func);

    func = env->make_function(env, 3, 3, Fwlr_surface_surface_at, "", NULL);
    bind_function(env, "wlr-surface-surface-at", func);

    func = env->make_function(env, 1, 1, Fwlr_surface_current_width, "", NULL);
    bind_function(env, "wlr-surface-current-width", func);

    func = env->make_function(env, 1, 1, Fwlr_surface_current_height, "", NULL);
    bind_function(env, "wlr-surface-current-height", func);

    func = env->make_function(env, 2, 2, Fwlr_surface_send_leave, "", NULL);
    bind_function(env, "wlr-surface-send-leave", func);

    func = env->make_function(env, 2, 2, Fwlr_surface_send_enter, "", NULL);
    bind_function(env, "wlr-surface-send-enter", func);

    func = env->make_function(env, 2, 2, Fwlr_surface_for_each_surface_render, "", NULL);
    bind_function(env, "wlr-surface-for-each-surface-render", func);
}
