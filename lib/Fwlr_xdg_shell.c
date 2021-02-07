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

emacs_value Fwlr_xdg_shell_create(emacs_env *env, ptrdiff_t nargs,
                                  emacs_value args[], void *data)
{
    struct wl_display *display = env->get_user_ptr(env, args[0]);
    struct wlr_xdg_shell *xdg_shell = wlr_xdg_shell_create(display);
    return env->make_user_ptr(env, NULL, xdg_shell);
}

emacs_value Fwlr_xdg_topllevel_set_size(emacs_env *env, ptrdiff_t nargs,
                                        emacs_value args[], void *data)
{
    struct wlr_xdg_surface *surface = env->get_user_ptr(env, args[0]);
    int width = env->extract_integer(env, args[1]);
    int height = env->extract_integer(env, args[2]);
    wlr_xdg_toplevel_set_size(surface, x, y, width, height);
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

emacs_value Fwlr_get_xdg_surface_wlr_surface(emacs_env *env, ptrdiff_t nargs,
                                             emacs_value args[], void *data)
{
    struct wlr_xdg_surface *xdg_surface = env->get_user_ptr(env, args[0]);
    return env->make_user_ptr(env, NULL, xdg_surface->surface);
}

emacs_value Fwlr_get_xdg_surface_app_id(emacs_env *env, ptrdiff_t nargs,
                                        emacs_value args[], void *data)
{
    struct wlr_xdg_surface *xdg_surface = env->get_user_ptr(env, args[0]);
    char *str = xdg_surface->toplevel->app_id;
    return env->make_string(env, str, strlen(str));
}

emacs_value Fwlr_get_xdg_surface_title(emacs_env *env, ptrdiff_t nargs,
                                        emacs_value args[], void *data)
{
    struct wlr_xdg_surface *xdg_surface = env->get_user_ptr(env, args[0]);
    char *str = xdg_surface->toplevel->title;
    return env->make_string(env, str, strlen(str));
}
emacs_value Fwlr_xdg_surface_configure_serial(emacs_env *env, ptrdiff_t nargs,
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
    wlr_xdg_surface_get_geometry(c->surface.xdg, geom);
    return env->make_user_ptr(env, NULL, geom);
}

emacs_value Fwlr_xdg_surface_wlr_surface(emacs_env *env, ptrdiff_t nargs,
                                              emacs_value args[], void *data)
{
    struct wlr_xdg_surface *xdg_surface = env->get_user_ptr(env, args[0]);
    return env->make_user_ptr(env, NULL, xdg_surface->surface);
}
