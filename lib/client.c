/*
 p See LICENSE file for copyright and license details.
 */
#define _POSIX_C_SOURCE 200809L
#include "util.h"
#include "client.h"

#include "ewlc.h"
#include "ewlc-module.h"
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

struct wlr_surface *get_surface(struct ewlc_client *c)
{
#ifdef XWAYLAND
    if (c->type != XDG_SHELL)
        return c->surface.xwayland->surface;
#endif
    return c->surface.xdg->surface;
}

bool is_visible_on(struct ewlc_client *c, struct ewlc_output *o)
{
    if (c->output == o)
        return true;
    return false;
}

void apply_bounds(struct ewlc_client *c, struct wlr_box *bbox)
{
    /* set minimum possible */
    c->geom.width = MAX(1, c->geom.width);
    c->geom.height = MAX(1, c->geom.height);

    if (c->geom.x >= bbox->x + bbox->width)
        c->geom.x = bbox->x + bbox->width - c->geom.width;

    if (c->geom.y >= bbox->y + bbox->height)
        c->geom.y = bbox->y + bbox->height - c->geom.height;

    if (c->geom.x + c->geom.width + 2 * c->border_width <= bbox->x)
        c->geom.x = bbox->x;

    if (c->geom.y + c->geom.height + 2 * c->border_width <= bbox->y)
        c->geom.y = bbox->y;
}

void apply_title(struct ewlc_client *c)
{
    const char *appid, *title;

    /* rule matching */
#ifdef XWAYLAND
    if (c->type != XDG_SHELL) {
        update_window_type(c);
        appid = c->surface.xwayland->class;
        title = c->surface.xwayland->title;
    } else
#endif
    {
        appid = c->surface.xdg->toplevel->app_id;
        title = c->surface.xdg->toplevel->title;
    }
    if (!appid)
        appid = broken;
    if (!title)
        title = broken;

    set_output(c, active_output);
}

void xdg_surface_commit_notify(struct wl_listener *listener, void *data)
{
    struct ewlc_client *c = wl_container_of(listener, c, surface_commit_listener);

    /* mark a pending resize as completed */
    if (c->resize && c->resize <= c->surface.xdg->configure_serial)
        c->resize = 0;
}

void xdg_shell_new_surface_notify(struct wl_listener *listener, void *data)
{
    /* This event is raised when wlr_xdg_shell receives a new xdg surface from a
     * client, either a toplevel (application window) or popup. */
    struct wlr_xdg_surface *xdg_surface = data;
    struct ewlc_client *c;
    struct ewlc_server *serv;

    if (xdg_surface->role != WLR_XDG_SURFACE_ROLE_TOPLEVEL)
        return;

    serv = wl_container_of(listener, serv, xdg_shell_new_surface_listener);

    /* Allocate a Client for this surface */
    c = xdg_surface->data = calloc(1, sizeof(*c));
    c->surface.xdg = xdg_surface;
    c->server = serv;
    c->border_width = border_px;

    /* Tell the client not to try anything fancy */
    wlr_xdg_toplevel_set_tiled(c->surface.xdg, WLR_EDGE_TOP | WLR_EDGE_BOTTOM |
                                                   WLR_EDGE_LEFT |
                                                   WLR_EDGE_RIGHT);

    /* Listen to the various events it can emit */
    /* FIXME: this this 'notify' funvtion correct? */
    c->surface_commit_listener.notify = xdg_surface_commit_notify;
    wl_signal_add(&xdg_surface->surface->events.commit, &c->surface_commit_listener);

    c->surface_map_listener.notify = surface_map_notify;
    wl_signal_add(&xdg_surface->events.map, &c->surface_map_listener);

    c->surface_unmap_listener.notify = surface_unmap_notify;
    wl_signal_add(&xdg_surface->events.unmap, &c->surface_unmap_listener);

    c->surface_destroy_listener.notify = surface_destroy_notify;
    wl_signal_add(&xdg_surface->events.destroy, &c->surface_destroy_listener);
}

void surface_destroy_notify(struct wl_listener *listener, void *data)
{
    /* Called when the surface is destroyed and should never be shown again. */
    struct ewlc_client *c = wl_container_of(listener, c, surface_destroy_listener);

    wl_list_remove(&c->surface_map_listener.link);
    wl_list_remove(&c->surface_unmap_listener.link);
    wl_list_remove(&c->surface_destroy_listener.link);
#ifdef XWAYLAND
    if (c->type == X11_MANAGED)
        wl_list_remove(&c->xwayland_surface_request_activate_listener.link);
    else if (c->type == XDG_SHELL)
#endif
        wl_list_remove(&c->surface_commit_listener.link);
    free(c);
}

struct ewlc_client *focus_top(struct ewlc_output *o)
{
    struct ewlc_client *c;
    struct ewlc_server *s = o->server;

    wl_list_for_each(c, &s->client_focus_list, client_focus_link)
        if (is_visible_on(c, o))
            return c;
    return NULL;
}

void surface_map_notify(struct wl_listener *listener, void *data)
{
    /* Called when the surface is mapped, or ready to display on-screen. */
    struct ewlc_client *c = wl_container_of(listener, c, surface_map_listener);
    struct ewlc_server *s = c->server;

#ifdef XWAYLAND
    if (c->type == X11_UNMANAGED) {
        /* Insert this independent into independents lists. */
        wl_list_insert(&server.independent_list, &c->client_link);
        return;
    }
#endif

    /* Insert this client into client lists. */
    wl_list_insert(&s->client_list, &c->client_link);
    wl_list_insert(&s->client_focus_list, &c->client_focus_link);
    wl_list_insert(&s->client_stack_list, &c->client_stack_link);

#ifdef XWAYLAND
    if (c->type != XDG_SHELL) {
        c->geom.x = c->surface.xwayland->x;
        c->geom.y = c->surface.xwayland->y;
        c->geom.width = c->surface.xwayland->width + 2 * c->border_width;
        c->geom.height = c->surface.xwayland->height + 2 * c->border_width;
    } else
#endif
    {
        wlr_xdg_surface_get_geometry(c->surface.xdg, &c->geom);
        c->geom.width += 2 * c->border_width;
        c->geom.height += 2 * c->border_width;
    }

    /* Set initial output, floating status, and focus */
    apply_title(c);
}

void move_resize(const Arg *arg)
{
    server.grabbed_client =
        get_client_at_point(server.cursor->x, server.cursor->y);
    if (!server.grabbed_client)
        return;

    /* Float the window and tell motionnotify to grab it */
    set_floating(server.grabbed_client, 1);
    switch (server.cursor_mode = arg->ui) {
    case CUR_MOVE:
        server.grabc_x = server.cursor->x - server.grabbed_client->geom.x;
        server.grabc_y = server.cursor->y - server.grabbed_client->geom.y;
        wlr_xcursor_manager_set_cursor_image(server.cursor_mgr, "fleur",
                                             server.cursor);
        break;
    case CUR_RESIZE:
        /* Doesn't work for X11 output - the next absolute motion event
         * returns the cursor to where it started */
        wlr_cursor_warp_closest(
            server.cursor, NULL,
            server.grabbed_client->geom.x + server.grabbed_client->geom.width,
            server.grabbed_client->geom.y + server.grabbed_client->geom.height);
        wlr_xcursor_manager_set_cursor_image(
            server.cursor_mgr, "bottom_right_corner", server.cursor);
        break;
    }
}

void pointer_focus(struct ewlc_client *c, struct wlr_surface *surface,
                   double sx, double sy, uint32_t time)
{
    ewlc_server *s = c->server;

    /* Use top level surface if nothing more specific given */
    if (c && !surface)
        surface = get_surface(c);

    /* If surface is NULL, clear pointer focus */
    if (!surface) {
        wlr_seat_pointer_notify_clear_focus(s->seat);
        return;
    }

    /* If surface is already focused, only notify of motion */
    if (surface == s->seat->pointer_state.focused_surface) {
        wlr_seat_pointer_notify_motion(s->seat, time, sx, sy);
        return;
    }

    /* Otherwise, let the client know that the mouse cursor has entered one
     * of its surfaces, and make keyboard focus follow if desired. */
    wlr_seat_pointer_notify_enter(s->seat, surface, sx, sy);

#if XWAYLAND
    if (c->type == X11_UNMANAGED)
        return;
#endif

    if (sloppyfocus)
        focus_client(get_active_client(), c, 0);
}

void render_surface(struct wlr_surface *surface, int sx, int sy, void *data)
{
    /* This function is called for every surface that needs to be rendered. */
    struct render_data *rdata = data;
    struct wlr_output *output = rdata->output;
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
    wlr_output_layout_output_coords(server.output_layout, output, &ox, &oy);

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
    wlr_render_texture_with_matrix(server.renderer, texture, matrix, 1);

    /* This lets the client know that we've displayed that frame and it can
     * prepare another one now if it likes. */
    wlr_surface_send_frame_done(surface, rdata->when);
}

void render_clients(struct ewlc_output *o, struct timespec *now)
{
    struct ewlc_client *c, *active_c = get_active_client();
    struct ewlc_server *s = o->server;
    const float *color;
    double ox, oy;
    int i, w, h;
    struct render_data rdata;
    struct wlr_box *borders;
    struct wlr_surface *surface;
    /* Each subsequent window we render is rendered on top of the last. Because
     * our stacking list is ordered front-to-back, we iterate over it backwards.
     */
    wl_list_for_each_reverse(c, &s->client_stack_list, client_stack_link)
    {
        /* Only render visible clients which show on this output */
        if (!is_visible_on(c, c->output) ||
            !wlr_output_layout_intersects(s->output_layout, o->wlr_output,
                                          &c->geom))
            continue;

        surface = get_surface(c);
        ox = c->geom.x;
        oy = c->geom.y;
        wlr_output_layout_output_coords(s->output_layout, o->wlr_output,
                                        &ox, &oy);
        w = surface->current.width;
        h = surface->current.height;
        borders = (struct wlr_box[4]){
            {ox, oy, w + 2 * c->border_width, c->border_width}, /* top */
            {ox, oy + c->border_width, c->border_width, h},     /* left */
            {ox + c->border_width + w, oy + c->border_width, c->border_width,
             h}, /* right */
            {ox, oy + c->border_width + h, w + 2 * c->border_width,
             c->border_width}, /* bottom */
        };

        /* Draw window borders */
        color = (c == active_c) ? focus_color : border_color;
        for (i = 0; i < 4; i++) {
            scale_box(&borders[i], o->wlr_output->scale);
            wlr_render_rect(s->renderer, &borders[i], color,
                            o->wlr_output->transform_matrix);
        }

        /* This calls our render function for each surface among the
         * xdg_surface's toplevel and popups. */
        rdata.output = o->wlr_output;
        rdata.when = now;
        rdata.x = c->geom.x + c->border_width;
        rdata.y = c->geom.y + c->border_width;
#ifdef XWAYLAND
        if (c->type != XDG_SHELL)
            wlr_surface_for_each_surface(c->surface.xwayland->surface, render_surface,
                                         &rdata);
        else
#endif
            wlr_xdg_surface_for_each_surface(c->surface.xdg, render_surface, &rdata);
    }
}

void resize(struct ewlc_client *c, int x, int y, int w, int h, int interact)
{
    /*
     * Note that I took some shortcuts here. In a more fleshed-out
     * compositor, you'd wait for the client to prepare a buffer at
     * the new size, then commit any movement that was prepared.
     */
    struct ewlc_server s = c->server;
    struct wlr_box *bbox = interact ? &s->output_geom : &c->output->w;

    c->geom.x = x;
    c->geom.y = y;
    c->geom.width = w;
    c->geom.height = h;
    apply_bounds(c, bbox);

    /* wlroots makes this a no-op if size hasn't changed */
#ifdef XWAYLAND
    if (c->type != XDG_SHELL)
        wlr_xwayland_surface_configure(c->surface.xwayland, c->geom.x,
                                       c->geom.y,
                                       c->geom.width - 2 * c->border_width,
                                       c->geom.height - 2 * c->border_width);
    else
#endif
        c->resize = wlr_xdg_toplevel_set_size(c->surface.xdg,
                                              c->geom.width - 2 * c->border_width,
                                              c->geom.height - 2 * c->border_width);
}

void scale_box(struct wlr_box *box, float scale)
{
    box->x *= scale;
    box->y *= scale;
    box->width *= scale;
    box->height *= scale;
}

struct ewlc_client *get_active_client(void)
{
    struct ewlc_client *c = wl_container_of(server.client_focus_list.next,
                                            c,
                                            client_focus_link);
    if (wl_list_empty(&server.client_focus_list) || !is_visible_on(c, active_output))
        return NULL;
    return c;
}

void set_floating(struct ewlc_client *c, int floating)
{
    if (c->is_floating == floating)
        return;
    c->is_floating = floating;
    arrange(c->output);
}

void set_output(struct ewlc_client *c, struct ewlc_output *o)
{
    struct ewlc_output *old_output = c->output;
    struct ewlc_client *old_c = get_active_client();

    if (old_output == o)
        return;
    c->output = o;

    /* XXX leave/enter is not optimal but works */
    if (old_output) {
        wlr_surface_send_leave(get_surface(c), old_output->wlr_output);
        arrange(old_output);
    }
    if (o) {
        /* Make sure window actually overlaps with the output */
        apply_bounds(c, &o->m);
        wlr_surface_send_enter(get_surface(c), o->wlr_output);
        arrange(o);
    }
    focus_client(old_c, focus_top(active_output), 1);
}

void surface_unmap_notify(struct wl_listener *listener, void *data)
{
    /* Called when the surface is unmapped, and should no longer be shown. */
    struct ewlc_client *c = wl_container_of(listener, c, surface_unmap_listener);
    wl_list_remove(&c->client_link);
#ifdef XWAYLAND
    if (c->type == X11_UNMANAGED)
        return;
#endif
    set_output(c, NULL);
    wl_list_remove(&c->client_focus_link);
    wl_list_remove(&c->client_stack_link);
}

struct ewlc_client *get_client_at_point(double x, double y)
{
    /* Find the topmost visible client (if any) at point (x, y), including
     * borders. This relies on stack being ordered from top to bottom. */
    struct ewlc_client *c;

    wl_list_for_each(c, &server.client_stack_list, client_stack_link)
        if (is_visible_on(c, c->output) &&
            wlr_box_contains_point(&c->geom, x, y))
            return c;

    return NULL;
}

#ifdef XWAYLAND
void xwayland_surface_request_activate_notify(struct wl_listener *listener, void *data)
{
    struct ewlc_client *c = wl_container_of(listener, c,
                                            xwayland_surface_request_activate_listener);

    /* Only "managed" windows can be activated */
    if (c->type == X11_MANAGED)
        wlr_xwayland_surface_activate(c->surface.xwayland, 1);
}

void new_xwayland_surface_notify(struct wl_listener *listener, void *data)
{
    struct wlr_xwayland_surface *xwayland_surface = data;
    struct ewlc_client *c;

    /* Allocate a struct ewlc_client for this surface */
    c = xwayland_surface->data = calloc(1, sizeof(*c));
    c->surface.xwayland = xwayland_surface;
    c->type = xwayland_surface->override_redirect ? X11_UNMANAGED : X11_MANAGED;
    c->border_width = border_px;

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

void render_independents(struct wlr_output *o, struct timespec *now)
{
    struct ewlc_client *c;
    struct render_data rdata;
    struct wlr_box geom;

    wl_list_for_each_reverse(c, &server.independent_list, client_link)
    {
        geom.x = c->surface.xwayland->x;
        geom.y = c->surface.xwayland->y;
        geom.width = c->surface.xwayland->width;
        geom.height = c->surface.xwayland->height;

        /* Only render visible clients which show on this output */
        if (!wlr_output_layout_intersects(server.output_layout, o, &geom))
            continue;

        rdata.output = o;
        rdata.when = now;
        rdata.x = c->surface.xwayland->x;
        rdata.y = c->surface.xwayland->y;

        wlr_surface_for_each_surface(c->surface.xwayland->surface, render_surface,
                                     &rdata);
    }
}

struct ewlc_client *get_independent_at_point(double x, double y)
{
    struct ewlc_client *c;
    struct wlr_box geom;
    wl_list_for_each_reverse(c, &server.independent_list, client_link)
    {
        geom.x = c->surface.xwayland->x;
        geom.y = c->surface.xwayland->y;
        geom.width = c->surface.xwayland->width;
        geom.height = c->surface.xwayland->height;
        if (wlr_box_contains_point(&geom, x, y))
            return c;
    }
    return NULL;
}
#endif
