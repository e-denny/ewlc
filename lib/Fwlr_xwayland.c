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

emacs_value Fwlr_get_xwayland_surface_wlr_surface(emacs_env *env, ptrdiff_t nargs,
                                                  emacs_value args[], void *data)
{
    struct wlr_xwayland_surface *xwayland_surface = env->get_user_ptr(env, args[0]);
    return env->make_user_ptr(env, NULL, xwayland_surface->surface);
}

emacs_value Fwlr_get_xwayland_surface_title(emacs_env *env, ptrdiff_t nargs,
                                            emacs_value args[], void *data)
{
    struct wlr_xwayland_surface *xwayland_surface = env->get_user_ptr(env, args[0]);
    char *str = xwayland_surface->title;
    return env->make_string(env, str, strlen(str));
}

emacs_value Fwlr_get_xwayland_surface_class(emacs_env *env, ptrdiff_t nargs,
                                            emacs_value args[], void *data)
{
    struct wlr_xwayland_surface *xwayland_surface = env->get_user_ptr(env, args[0]);
    char *str = xwayland_surface->class;
    return env->make_string(env, str, strlen(str));
}

emacs_value Fwlr_xwayland_surface_wlr_surface(emacs_env *env, ptrdiff_t nargs,
                                              emacs_value args[], void *data)
{
    struct wlr_xwayland_surface *xwayland_surface = env->get_user_ptr(env, args[0]);
    return env->make_user_ptr(env, NULL, xwayland_surface->surface);
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
emacs_value Fwlr_surface_xwayland_get_box(emacs_env *env, ptrdiff_t nargs,
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
