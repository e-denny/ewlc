/*
 See LICENSE file for copyright and license details.
 */
#define _POSIX_C_SOURCE 200809L
#include "server.h"
#include "util.h"
#include "module.h"
#include "Fwlr.h"
#include <wayland-server-core.h>
#include <wlr/render/wlr_renderer.h>
#include <wlr/types/wlr_xdg_shell.h>

emacs_value Fwlr_xdg_shell_create(emacs_env *env, ptrdiff_t nargs,
                                  emacs_value args[], void *data)
{
    struct wl_display *display = env->get_user_ptr(env, args[0]);
    struct wlr_xdg_shell *xdg_shell = wlr_xdg_shell_create(display);
    return env->make_user_ptr(env, NULL, xdg_shell);
}

emacs_value Fwlr_xdg_toplevel_set_size(emacs_env *env, ptrdiff_t nargs,
                                       emacs_value args[], void *data)
{
    struct wlr_xdg_surface *surface = env->get_user_ptr(env, args[0]);
    int width = env->extract_integer(env, args[1]);
    int height = env->extract_integer(env, args[2]);
    wlr_xdg_toplevel_set_size(surface, width, height);
    return Qt;
}

emacs_value Fwlr_xdg_toplevel_set_activated(emacs_env *env, ptrdiff_t nargs,
                                            emacs_value args[], void *data)
{
    struct wlr_xdg_surface *surface = env->get_user_ptr(env, args[0]);
    int activated = env->extract_integer(env, args[1]);
    wlr_xdg_toplevel_set_activated(surface, activated);
    return Qt;
}

emacs_value Fwlr_xdg_surface_for_each_surface_render(emacs_env *env, ptrdiff_t nargs,
                                                     emacs_value args[], void *data)
{
    struct wlr_xdg_surface *surface = env->get_user_ptr(env, args[0]);
    struct render_data *r_data = env->get_user_ptr(env, args[1]);
    wlr_xdg_surface_for_each_surface(surface, render_surface, r_data);
    return Qt;
}

emacs_value Fwlr_xdg_toplevel_send_close(emacs_env *env, ptrdiff_t nargs,
                                         emacs_value args[], void *data)
{
    struct wlr_xdg_surface *surface = env->get_user_ptr(env, args[0]);
    wlr_xdg_toplevel_send_close(surface);
    return Qt;
}

emacs_value Fwlr_xdg_surface_surface_at(emacs_env *env, ptrdiff_t nargs,
                                        emacs_value args[], void *data)
{
    struct wlr_xdg_surface *surface = env->get_user_ptr(env, args[0]);
    double sx = env->extract_float(env, args[1]);
    double sy = env->extract_float(env, args[2]);
    double sub_x = 0;
    double sub_y = 0;
    struct wlr_surface *s = wlr_xdg_surface_surface_at(surface, sx, sy, &sub_x, &sub_y);
    emacs_value ret[3];
    ret[0] = env->make_user_ptr(env, NULL, s);
    ret[1] = env->make_float(env, sub_x);
    ret[2] = env->make_float(env, sub_y);
    return list(env, ret, 3);
}

emacs_value Fwlr_xdg_surface_get_app_id(emacs_env *env, ptrdiff_t nargs,
                                        emacs_value args[], void *data)
{
    struct wlr_xdg_surface *xdg_surface = env->get_user_ptr(env, args[0]);
    char *str = xdg_surface->toplevel->app_id;
    return env->make_string(env, str, strlen(str));
}

emacs_value Fwlr_xdg_surface_get_title(emacs_env *env, ptrdiff_t nargs,
                                       emacs_value args[], void *data)
{
    struct wlr_xdg_surface *xdg_surface = env->get_user_ptr(env, args[0]);
    char *str = xdg_surface->toplevel->title;
    return env->make_string(env, str, strlen(str));
}
emacs_value Fwlr_xdg_surface_get_configure_serial(emacs_env *env, ptrdiff_t nargs,
                                              emacs_value args[], void *data)
{
    struct wlr_xdg_surface *xdg_surface = env->get_user_ptr(env, args[0]);
    return env->make_integer(env, xdg_surface->configure_serial);
}

emacs_value Fwlr_xdg_surface_role_toplevel(emacs_env *env, ptrdiff_t nargs,
                                           emacs_value args[], void *data)
{
    struct wlr_xdg_surface *xdg_surface = env->get_user_ptr(env, args[0]);
    if (xdg_surface->role == WLR_XDG_SURFACE_ROLE_TOPLEVEL)
        return Qt;
    return Qnil;
}

emacs_value Fwlr_xdg_toplevel_set_tiled(emacs_env *env, ptrdiff_t nargs,
                                         emacs_value args[], void *data)
{
    struct wlr_xdg_surface *xdg_surface = env->get_user_ptr(env, args[0]);
    wlr_xdg_toplevel_set_tiled(xdg_surface,
                               WLR_EDGE_TOP | WLR_EDGE_BOTTOM |
                               WLR_EDGE_LEFT | WLR_EDGE_RIGHT);
    return Qt;
}

emacs_value Fwlr_xdg_surface_get_geometry(emacs_env *env, ptrdiff_t nargs,
                                          emacs_value args[], void *data)
{
    struct wlr_xdg_surface *surface = env->get_user_ptr(env, args[0]);
    struct wlr_box *geom = env->get_user_ptr(env, args[1]);
    wlr_xdg_surface_get_geometry(surface, geom);
    return env->make_user_ptr(env, NULL, geom);
}

emacs_value Fwlr_xdg_surface_get_wlr_surface(emacs_env *env, ptrdiff_t nargs,
                                             emacs_value args[], void *data)
{
    struct wlr_xdg_surface *xdg_surface = env->get_user_ptr(env, args[0]);
    struct wlr_surface *wlr_surface = xdg_surface->surface;
    return env->make_user_ptr(env, NULL, wlr_surface);
}

void init_wlr_xdg_shell(emacs_env *env)
{
    emacs_value func;
    func = env->make_function(env, 1, 1, Fwlr_xdg_shell_create, "", NULL);
    bind_function(env, "wlr-xdg-shell-create", func);

    func = env->make_function(env, 3, 3, Fwlr_xdg_toplevel_set_size, "", NULL);
    bind_function(env, "wlr-xdg-toplevel-set-size", func);

    func = env->make_function(env, 2, 2, Fwlr_xdg_toplevel_set_activated, "", NULL);
    bind_function(env, "wlr-xdg-toplevel-set-activated", func);

    func = env->make_function(env, 2, 2, Fwlr_xdg_surface_for_each_surface_render, "", NULL);
    bind_function(env, "wlr-xdg-surface-for-each-surface-render", func);

    func = env->make_function(env, 1, 1, Fwlr_xdg_toplevel_send_close, "", NULL);
    bind_function(env, "wlr-xdg-toplevel-send-close", func);

    func = env->make_function(env, 3, 3, Fwlr_xdg_surface_surface_at, "", NULL);
    bind_function(env, "wlr-xdg-surface-surface-at", func);

    func = env->make_function(env, 1, 1, Fwlr_xdg_surface_get_app_id, "", NULL);
    bind_function(env, "wlr-xdg-surface-get-app-id", func);

    func = env->make_function(env, 1, 1, Fwlr_xdg_surface_get_title, "", NULL);
    bind_function(env, "wlr-xdg-surface-get-title", func);

    func = env->make_function(env, 1, 1, Fwlr_xdg_surface_get_configure_serial, "", NULL);
    bind_function(env, "wlr-xdg-surface-get-configure-serial", func);

    func = env->make_function(env, 1, 1, Fwlr_xdg_surface_role_toplevel, "", NULL);
    bind_function(env, "wlr-xdg-surface-role-toplevel", func);

    func = env->make_function(env, 1, 1, Fwlr_xdg_toplevel_set_tiled, "", NULL);
    bind_function(env, "wlr-xdg-toplevel-set-tiled", func);

    func = env->make_function(env, 2, 2, Fwlr_xdg_surface_get_geometry, "", NULL);
    bind_function(env, "wlr-xdg-surface-get-geometry", func);

    func = env->make_function(env, 1, 1, Fwlr_xdg_surface_get_wlr_surface, "", NULL);
    bind_function(env, "wlr-xdg-surface-get-wlr-surface", func);
}
