/*
 p See LICENSE file for copyright and license details.
 */
#define _POSIX_C_SOURCE 200809L
#include "util.h"
#include "server.h"
#include "client.h"
#include "output.h"
#include "module.h"
#include <emacs-module.h>
#include <getopt.h>
#include <linux/input-event-codes.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>
#include <wayland-client.h>
#include <wayland-server-core.h>
#include <wlr/backend.h>
#include <wlr/render/wlr_renderer.h>
#include <wlr/types/wlr_compositor.h>
#include <wlr/types/wlr_cursor.h>
#include <wlr/types/wlr_data_device.h>
#include <wlr/types/wlr_export_dmabuf_v1.h>
#include <wlr/types/wlr_gamma_control_v1.h>
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

#ifdef XWAYLAND
#include <X11/Xlib.h>
#include <wlr/xwayland.h>
#endif

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

emacs_value Fwlr_get_xwayland_surface_wlr_surface(emacs_env *env, ptrdiff_t nargs,
                                                  emacs_value args[], void *data)
{
    struct wlr_xwayland_surface *xwayland_surface = env->get_user_ptr(env, args[0]);
    return env->make_user_ptr(env, NULL, xwayland_surface->surface);
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

// TODO: need a finalizer to free pointer.
emacs_value Fewlc_make_xdg_surface_client_ptr(emacs_env *env, ptrdiff_t nargs,
                                              emacs_value args[], void *data)
{
    struct wlr_xdg_surface *xdg_surface = env->get_user_ptr(env, args[0]);
    struct ewlc_client *c;
    c = xdg_surface->data = calloc(1, sizeof(*c));
    return env->make_user_ptr(env, NULL, c);
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

emacs_value Fewlc_set_xdg_surface_client_listeners(emacs_env *env, ptrdiff_t nargs,
                                                   emacs_value args[], void *data)
{
    struct ewlc_client *c = env->get_user_ptr(env, args[0]);
    struct wlr_xdg_surface *xdg_surface = env->get_user_ptr(env, args[1]);

    /* Listen to the various events it can emit */
    c->surface_commit_listener.notify = xdg_surface_commit_notify;
    wl_signal_add(&xdg_surface->surface->events.commit, &c->surface_commit_listener);

    c->surface_map_listener.notify = surface_map_notify;
    wl_signal_add(&xdg_surface->events.map, &c->surface_map_listener);

    c->surface_unmap_listener.notify = surface_unmap_notify;
    wl_signal_add(&xdg_surface->events.unmap, &c->surface_unmap_listener);

    c->surface_destroy_listener.notify = surface_destroy_notify;
    wl_signal_add(&xdg_surface->events.destroy, &c->surface_destroy_listener);
    return Qt;
}

emacs_value Fewlc_remove_client_listeners(emacs_env *env, ptrdiff_t nargs,
                                          emacs_value args[], void *data)
{
    struct ewlc_client *c = env->get_user_ptr(env, args[0]);
    emacs_value c_type = args[1];
    emacs_value x11_managed = env->intern(env, "x11-managed");
    emacs_value xdg_shell = env->intern(env, "xdg-shell");

    wl_list_remove(&c->surface_map_listener.link);
    wl_list_remove(&c->surface_unmap_listener.link);
    wl_list_remove(&c->surface_destroy_listener.link);
    if (c_type == x11_managed)
        wl_list_remove(&c->xwayland_surface_request_activate_listener.link);
    else if (c_type == xdg_shell)
        wl_list_remove(&c->surface_commit_listener.link);
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

emacs_value Fwlr_seat_pointer_notify_clear_focus(emacs_env *env, ptrdiff_t nargs,
                                                 emacs_value args[], void *data)
{
    struct wlr_seat *seat = env->get_user_ptr(env, args[0]);
    wlr_seat_pointer_notify_clear_focus(seat);
    return Qt;
}

emacs_value Fwlr_seat_pointer_notify_motion(emacs_env *env, ptrdiff_t nargs,
                                            emacs_value args[], void *data)
{
    struct wlr_seat *seat = env->get_user_ptr(env, args[0]);
    int time_msec = env->extract_integer(env, args[1]);
    double x = env->extract_float(env, args[2]);
    double y = env->extract_float(env, args[3]);
    wlr_seat_pointer_notify_motion(seat, time_msec, x, y);
    return Qt;
}

emacs_value Fwlr_seat_pointer_enter(emacs_env *env, ptrdiff_t nargs,
                                    emacs_value args[], void *data)
{
    struct wlr_seat *seat = env->get_user_ptr(env, args[0]);
    struct wlr_surface *surface = env->get_user_ptr(env, args[1]);
    double x = env->extract_float(env, args[2]);
    double y = env->extract_float(env, args[3]);
    wlr_seat_pointer_notify_enter(seat, surface, x, y);
    return Qt;
}

void render_surface(struct wlr_surface *surface, int sx, int sy, void *data)
{
    /* This function is called for every surface that needs to be rendered. */
    struct render_data *rdata = data;
    struct wlr_output *output = rdata->output;
    struct wlr_output_layout *output_layout = rdata->output_layout;
    struct wlr_renderer *renderer = rdata->renderer;
    double ox = 0, oy = 0;
    struct wlr_box obox;
    float matrix[9];
    enum wl_output_transform transform;

    /* We first obtain a wlr_texture, which is a GPU resource. wlroots
     * automatically handles negotiating these with the client. The underlying
     * resource could be an opaque handle passed from the client, or the client
     * could have sent a pixel buffer which we copied to the GPU, or a few other
     * means. You don't have to worry about this, wlroots takes care of it. */
    struct wlr_texture *texture = wlr_surface_get_texture(surface);
    if (!texture)
        return;

    /* The client has a position in layout coordinates. If you have two
     * displays, one next to the other, both 1080p, a client on the rightmost
     * display might have layout coordinates of 2000,100. We need to translate
     * that to output-local coordinates, or (2000 - 1920). */
    wlr_output_layout_output_coords(output_layout, output, &ox, &oy);

    /* We also have to apply the scale factor for HiDPI outputs. This is only
     * part of the puzzle, ewlc does not fully support HiDPI. */
    obox.x = ox + rdata->x + sx;
    obox.y = oy + rdata->y + sy;
    obox.width = surface->current.width;
    obox.height = surface->current.height;
    scale_box(&obox, output->scale);

    /*
     * Those familiar with OpenGL are also familiar with the role of matrices
     * in graphics programming. We need to prepare a matrix to render the
     * client with. wlr_matrix_project_box is a helper which takes a box with
     * a desired x, y coordinates, width and height, and an output geometry,
     * then prepares an orthographic projection and multiplies the necessary
     * transforms to produce a model-view-projection matrix.
     *
     * Naturally you can do this any way you like, for example to make a 3D
     * compositor.
     */
    transform = wlr_output_transform_invert(surface->current.transform);
    wlr_matrix_project_box(matrix, &obox, transform, 0,
                           output->transform_matrix);

    /* This takes our matrix, the texture, and an alpha, and performs the actual
     * rendering on the GPU. */
    wlr_render_texture_with_matrix(renderer, texture, matrix, 1);

    /* This lets the client know that we've displayed that frame and it can
     * prepare another one now if it likes. */
    wlr_surface_send_frame_done(surface, &rdata->when);
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

emacs_value Fwlr_xwayland_surface_wlr_surface(emacs_env *env, ptrdiff_t nargs,
                                              emacs_value args[], void *data)
{
    struct wlr_xwayland_surface *xwayland_surface = env->get_user_ptr(env, args[0]);
    return env->make_user_ptr(env, NULL, xwayland_surface->surface);
}

emacs_value Fwlr_xdg_surface_wlr_surface(emacs_env *env, ptrdiff_t nargs,
                                              emacs_value args[], void *data)
{
    struct wlr_xdg_surface *xdg_surface = env->get_user_ptr(env, args[0]);
    return env->make_user_ptr(env, NULL, xdg_surface->surface);
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

emacs_value Fwlr_output_layout_output_coords(emacs_env *env, ptrdiff_t nargs,
                                             emacs_value args[], void *data)
{
    struct wlr_output_layout *output_layout = env->get_user_ptr(env, args[0]);
    struct wlr_output *output = env->get_user_ptr(env, args[1]);
    double ox = env->extract_float(env, args[2]);
    double oy = env->extract_float(env, args[3]);
    emacs_value coords[2];
    wlr_output_layout_output_coords(output_layout, output, &ox, &oy);
    coords[0] = env->make_float(env, ox);
    coords[1] = env->make_float(env, oy);
    return list(env, coords, 2);
}

emacs_value Fwlr_surface_for_each_surface_render(emacs_env *env, ptrdiff_t nargs,
                                                 emacs_value args[], void *data)
{
    struct wlr_surface *surface = env->get_user_ptr(env, args[0]);
    struct render_data *r_data = env->get_user_ptr(env, args[1]);
    wlr_surface_for_each_surface(surface, render_surface, r_data);
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

// TODO: finalizer to free pointer
emacs_value Fewlc_create_render_data(emacs_env *env, ptrdiff_t nargs,
                                     emacs_value args[], void *data)
{
    struct render_data *r_data = calloc(1, sizeof(*r_data));
    struct wlr_output *output = env->get_user_ptr(env, args[0]);
    struct wlr_output_layout *output_layout = env->get_user_ptr(env, args[1]);
    struct wlr_renderer *renderer = env->get_user_ptr(env, args[2]);
    int x = env->extract_integer(env, args[3]);
    int y = env->extract_integer(env, args[4]);
    struct timespec when = env->extract_time(env, args[5]);

    r_data->output = output;
    r_data->output_layout = output_layout;
    r_data->renderer = renderer;
    r_data->x = x;
    r_data->y = y;
    r_data->when = when;
    return env->make_user_ptr(env, NULL, r_data);
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

emacs_value Fwlr_xdg_topllevel_set_size(emacs_env *env, ptrdiff_t nargs,
                                        emacs_value args[], void *data)
{
    struct wlr_xdg_surface *surface = env->get_user_ptr(env, args[0]);
    int width = env->extract_integer(env, args[1]);
    int height = env->extract_integer(env, args[2]);
    wlr_xdg_toplevel_set_size(surface, x, y, width, height);
    return Qt;
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
}

emacs_value Fwlr_seat_keyboard_notify_enter(emacs_env *env, ptrdiff_t nargs,
                                            emacs_value args[], void *data)
{
    struct wlr_seat *seat = env->get_user_ptr(env, args[0]);
    struct wlr_surface *surface = env->get_user_ptr(env, args[1]);
    struct wlr_keyboard *kb = env->get_user_ptr(env, args[2]);
    wlr_seat_keyboard_notify_enter(seat, surface, kb->keycodes,
                                   kb->num_keycodes, &kb->modifiers);
    return Qt;
}

emacs_value Fwlr_seat_keyboard_notify_clear_focus(emacs_env *env, ptrdiff_t nargs,
                                                  emacs_value args[], void *data)
{
    struct wlr_seat *seat = env->get_user_ptr(env, args[0]);
    wlr_seat_keyboard_notify_clear_focus(seat);
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

emacs_value Fwlr_xdg_toplevel_set_activated(emacs_env *env, ptrdiff_t nargs,
                                            emacs_value args[], void *data)
{
    struct wlr_xdg_surface *surface = env->get_user_ptr(env, args[0]);
    int activated = env->extract_integer(env, args[1]);
    wlr_xdg_toplevel_set_activated(surface, activated);
    return Qt;
}

emacs_value Fewlc_make_xwayland_surface_client_ptr(emacs_env *env, ptrdiff_t nargs,
                                                   emacs_value args[], void *data)
{
    struct wlr_xwayland_surface *xwayland_surface = env->get_user_ptr(env, args[0]);
    struct ewlc_client *c;
    c = xwayland_surface->data = calloc(1, sizeof(*c));
    return env->make_user_ptr(env, NULL, c);
}

emacs_value Fwlr_xwayland_surface_override_redirect(emacs_env *env, ptrdiff_t nargs,
                                                    emacs_value args[], void *data)
{
    struct wlr_xwayland_surface *xwayland_surface = env->get_user_ptr(env, args[0]);
    if (xwayland_surface->override_redirect)
        return Qt;
    return Qnil;
}

emacs_value Fewlc_set_xwayland_surface_client_listeners(emacs_env *env, ptrdiff_t nargs,
                                                        emacs_value args[], void *data)
{
    struct ewlc_client *c = env->get_user_ptr(env, args[0]);
    struct wlr_xwayland_surface *xwayland_surface = env->get_user_ptr(env, args[1]);

    /* Listen to the various events it can emit */
    c->surface_map_listener.notify = surface_map_notify;
    wl_signal_add(&xwayland_surface->events.map, &c->surface_map_listener);

    c->surface_unmap_listener.notify = surface_unmap_notify;
    wl_signal_add(&xwayland_surface->events.unmap, &c->surface_unmap_listener);

    c->xwayland_surface_request_activate_listener.notify =
        xwayland_surface_request_activate_notify;
    wl_signal_add(&xwayland_surface->events.request_activate,
                  &c->xwayland_surface_request_activate_listener);

    c->surface_destroy_listener.notify = surface_destroy_notify;
    wl_signal_add(&xwayland_surface->events.destroy, &c->surface_destroy_listener);
}

Atom get_atom(xcb_connection_t *xc, const char *name)
{
    Atom atom = 0;
    xcb_intern_atom_cookie_t cookie;
    xcb_intern_atom_reply_t *reply;

    cookie = xcb_intern_atom(xc, 0, strlen(name), name);
    if ((reply = xcb_intern_atom_reply(xc, cookie, NULL)))
        atom = reply->atom;
    free(reply);

    return atom;
}

emacs_value Fwlr_box_contains_point(emacs_env *env, ptrdiff_t nargs,
                                    emacs_value args[], void *data)
{
    struct wlr_cursor *cursor = env->get_user_ptr(env, args[0]);
    struct wlr_box *geom = env->get_user_ptr(env, args[0]);
    if (wlr_box_contains_point(geom, cursor->x, cursor->y))
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

// ----------------------------------------------------------------------

void xdg_surface_commit_notify(struct wl_listener *listener, void *data)
{
    struct ewlc_client *c;
    struct ewlc_server *s;
    struct event_node *e;

    c = wl_container_of(listener, c, surface_commit_listener);
    s = c->server;
    e = create_event(listener, data, EWLC_XDG_SURFACE_COMMIT);
    s->event_list = add_event(s->event_list, e);
}

void xdg_shell_new_surface_notify(struct wl_listener *listener, void *data)
{
    /* This event is raised when wlr_xdg_shell receives a new xdg surface from a
     * client, either a toplevel (application window) or popup. */
    struct ewlc_server *s;
    struct event_node *e;

    s = wl_container_of(listener, s, xdg_shell_new_surface_listener);
    e = create_event(listener, data, EWLC_XDG_SHELL_NEW_SURFACE);
    s->event_list = add_event(s->event_list, e);
}

void surface_destroy_notify(struct wl_listener *listener, void *data)
{
    /* Called when the surface is destroyed and should never be shown again. */
    struct ewlc_client *c;
    struct ewlc_server *s;
    struct event_node *e;

    c = wl_container_of(listener, c, surface_destroy_listener);
    s = c->server;
    e = create_event(listener, data, EWLC_SURFACE_DESTROY);
    s->event_list = add_event(s->event_list, e);
}

void surface_map_notify(struct wl_listener *listener, void *data)
{
    /* Called when the surface is mapped, or ready to display on-screen. */
    struct ewlc_client *c;
    struct ewlc_server *s;
    struct event_node *e;

    c = wl_container_of(listener, c, surface_map_listener);
    s = c->server;
    e = create_event(listener, data, EWLC_SURFACE_MAP);
    s->event_list = add_event(s->event_list, e);
}

void surface_unmap_notify(struct wl_listener *listener, void *data)
{
    /* Called when the surface is unmapped, and should no longer be shown. */
    struct ewlc_client *c;
    struct ewlc_server *s;
    struct event_node *e;

    c = wl_container_of(listener, c, surface_unmap_listener);
    s = c->server;
    e = create_event(listener, data, EWLC_SURFACE_UNMAP);
    s->event_list = add_event(s->event_list, e);
}

void xwayland_surface_request_activate_notify(struct wl_listener *listener, void *data)
{
    struct ewlc_client *c;
    struct ewlc_server *s;
    struct event_node *e;

    c = wl_container_of(listener, c, xwayland_surface_request_activate_listener);
    s = c->server;
    e = create_event(listener, data, EWLC_X_SURFACE_REQUEST_ACTIVATE);
    s->event_list = add_event(s->event_list, e);
}

void new_xwayland_surface_notify(struct wl_listener *listener, void *data)
{
    struct ewlc_server *s;
    struct event_node *e;

    s = wl_container_of(listener, s, new_xwayland_surface_listener);
    e = create_event(listener, data, EWLC_NEW_X_SURFACE);
    s->event_list = add_event(s->event_list, e);
}
