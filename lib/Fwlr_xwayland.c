#define _POSIX_C_SOURCE 200809L
#include "server.h"
#include "util.h"
#include "module.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <wayland-client.h>
#include <wayland-server-core.h>
#include <wlr/types/wlr_compositor.h>
#include <wlr/types/wlr_seat.h>
#include <xkbcommon/xkbcommon.h>

#include <X11/Xlib.h>
#include <wlr/xwayland.h>

emacs_value Fwlr_xwayland_set_seat(emacs_env *env, ptrdiff_t nargs,
                                   emacs_value args[], void *data)
{
    struct wlr_xwayland *xwayland = env->get_user_ptr(env, args[0]);
    struct wlr_seat *seat = env->get_user_ptr(env, args[1]);
    wlr_xwayland_set_seat(xwayland, seat);
    return Qt;
}

emacs_value Fwlr_xwayland_display_name(emacs_env *env, ptrdiff_t nargs,
                                       emacs_value args[], void *data)
{
    struct wlr_xwayland *xwayland = env->get_user_ptr(env, args[0]);
    const char *name = xwayland->display_name;
    return env->make_string(env, name, strlen(name));
}

emacs_value Fwlr_xwayland_destroy(emacs_env *env, ptrdiff_t nargs,
                                  emacs_value args[], void *data)
{
    struct wlr_xwayland *xwayland = env->get_user_ptr(env, args[0]);
    wlr_xwayland_destroy(xwayland);
    return Qt;
}

emacs_value Fwlr_xwayland_create(emacs_env *env, ptrdiff_t nargs,
                                 emacs_value args[], void *data)
{
    struct wl_display *display = env->get_user_ptr(env, args[0]);
    struct wlr_compositor *compositor = env->get_user_ptr(env, args[1]);
    struct wlr_xwayland *xwayland = wlr_xwayland_create(display, compositor, true);
    return env->make_user_ptr(env, NULL, xwayland);
}

emacs_value Fwlr_xwayland_surface_close(emacs_env *env, ptrdiff_t nargs,
                                        emacs_value args[], void *data)
{
    struct wlr_xwayland_surface *surface = env->get_user_ptr(env, args[0]);
    wlr_xwayland_surface_close(surface);
    return Qt;
}

emacs_value Fwlr_xwayland_surface_get_wlr_surface(emacs_env *env, ptrdiff_t nargs,
                                                  emacs_value args[], void *data)
{
    struct wlr_xwayland_surface *xwayland_surface = env->get_user_ptr(env, args[0]);
    return env->make_user_ptr(env, NULL, xwayland_surface->surface);
}

emacs_value Fwlr_xwayland_surface_get_title(emacs_env *env, ptrdiff_t nargs,
                                            emacs_value args[], void *data)
{
    struct wlr_xwayland_surface *xwayland_surface = env->get_user_ptr(env, args[0]);
    char *str = xwayland_surface->title;
    return env->make_string(env, str, strlen(str));
}

emacs_value Fwlr_xwayland_surface_get_class(emacs_env *env, ptrdiff_t nargs,
                                            emacs_value args[], void *data)
{
    struct wlr_xwayland_surface *xwayland_surface = env->get_user_ptr(env, args[0]);
    char *str = xwayland_surface->class;
    return env->make_string(env, str, strlen(str));
}

emacs_value Fwlr_xwayland_surface_configure(emacs_env *env, ptrdiff_t nargs,
                                            emacs_value args[], void *data)
{
    struct wlr_xwayland_surface *surface = env->get_user_ptr(env, args[0]);
    int x = env->extract_integer(env, args[1]);
    int y = env->extract_integer(env, args[2]);
    int width = env->extract_integer(env, args[3]);
    int height = env->extract_integer(env, args[4]);
    wlr_xwayland_surface_configure(surface, x, y, width, height);
    return Qt;
}

emacs_value Fwlr_xwayland_surface_activate(emacs_env *env, ptrdiff_t nargs,
                                           emacs_value args[], void *data)
{
    struct wlr_xwayland_surface *surface = env->get_user_ptr(env, args[0]);
    int activated = env->extract_integer(env, args[1]);
    wlr_xwayland_surface_activate(surface, activated);
    return Qt;
}

emacs_value Fwlr_xwayland_surface_override_redirect(emacs_env *env, ptrdiff_t nargs,
                                                    emacs_value args[], void *data)
{
    struct wlr_xwayland_surface *xwayland_surface = env->get_user_ptr(env, args[0]);
    if (xwayland_surface->override_redirect)
        return Qt;
    return Qnil;
}

// TODO: add a finalizer to free
emacs_value Fwlr_xwayland_surface_get_wlr_box(emacs_env *env, ptrdiff_t nargs,
                                              emacs_value args[], void *data)
{
    struct wlr_xwayland_surface *surface = env->get_user_ptr(env, args[0]);
    struct wlr_box *geom = calloc(1, sizeof(*geom));
    geom->x = surface->x;
    geom->y = surface->y;
    geom->width = surface->width;
    geom->height = surface->height;
    return env->make_user_ptr(env, NULL, geom);
}

void init_wlr_xwayland(emacs_env *env)
{
    emacs_value func;

    func = env->make_function(env, 2, 2, Fwlr_xwayland_set_seat, "", NULL);
    bind_function(env, "wlr-xwayland-set-seat", func);

    func = env->make_function(env, 1, 1, Fwlr_xwayland_display_name, "", NULL);
    bind_function(env, "wlr-xwayland-set-seat", func);

    func = env->make_function(env, 1, 1, Fwlr_xwayland_destroy, "", NULL);
    bind_function(env, "wlr-xwayland-destroy", func);

    func = env->make_function(env, 2, 2, Fwlr_xwayland_create, "", NULL);
    bind_function(env, "wlr-xwayland-create", func);

    func = env->make_function(env, 1, 1, Fwlr_xwayland_surface_close, "", NULL);
    bind_function(env, "wlr-xwayland-surface-close", func);

    func = env->make_function(env, 1, 1, Fwlr_xwayland_surface_get_wlr_surface, "", NULL);
    bind_function(env, "wlr-xwayland-surface-get-wlr-surface", func);

    func = env->make_function(env, 1, 1, Fwlr_xwayland_surface_get_title, "", NULL);
    bind_function(env, "wlr-xwayland-surface-get-title", func);

    func = env->make_function(env, 1, 1, Fwlr_xwayland_surface_get_class, "", NULL);
    bind_function(env, "wlr-xwayland-surface-get-class", func);

    func = env->make_function(env, 5, 5, Fwlr_xwayland_surface_configure, "", NULL);
    bind_function(env, "wlr-xwayland-surface-configure", func);

    func = env->make_function(env, 1, 1, Fwlr_xwayland_surface_activate, "", NULL);
    bind_function(env, "wlr-xwayland-surface-activate", func);

    func = env->make_function(env, 1, 1, Fwlr_xwayland_surface_override_redirect, "", NULL);
    bind_function(env, "wlr-xwayland-surface-override-redirect", func);

    func = env->make_function(env, 1, 1, Fwlr_xwayland_surface_get_wlr_box, "", NULL);
    bind_function(env, "wlr-xwayland-surface-get-wlr-box", func);
}
