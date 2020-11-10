/*
 See LICENSE file for copyright and license details.
 */
#define _POSIX_C_SOURCE 200809L
#include "util.h"
#inclide "output.h"
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

// ok
void arrange(struct ewlc_output *o)
{
    /* Get effective output geometry to use for window area */
    ewlc_server *s = o->server;

    o->m = *wlr_output_layout_get_box(s->output_layout, o->wlr_output);
    o->w = o->m;
    tile(o);
    /* XXX recheck pointer focus here... or in resize()? */
}

// ok
void output_destroy_notify(struct wl_listener *listener, void *data)
{
    // struct wlr_output *wlr_output = data;
    // struct ewlc_output *o = wlr_output->data;
    struct ewlc_output *o = wl_container_of(listener, o, output_destroy_listner);

    wl_list_remove(&o->output_destroy_listener.link);
    free(o);
}


// ok
void backend_new_output_notify(struct wl_listener *listener, void *data)
{
    /* This event is raised by the backend when a new output (aka a display or
     * output) becomes available. */
    struct ewlc_server *s = wl_container_of(listener, s, backend_new_output_listener);
    struct wlr_output *wlr_output = data;
    struct ewlc_output *o;
    const struct ewlc_output_rule *r;

    /* The mode is a tuple of (width, height, refresh rate), and each
     * output supports only a specific set of modes. We just pick the
     * output's preferred mode; a more sophisticated compositor would let
     * the user configure it. */
    wlr_output_set_mode(wlr_output, wlr_output_preferred_mode(wlr_output));

    /* Allocates and configures output state using configured rules */
    o = wlr_output->data = calloc(1, sizeof(*o));
    o->wlr_output = wlr_output;
    o->server = s;

    for (r = output_rules; r < END(output_rules); r++) {
        if (!r->name || strstr(wlr_output->name, r->name)) {
            o->master_ratio = r->master_ratio;
            o->num_master = r->num_master;
            wlr_output_set_scale(wlr_output, r->scale);
            wlr_xcursor_manager_load(serv->cursor_mgr, r->scale);
            wlr_output_set_transform(wlr_output, r->rr);
            break;
        }
    }
    /* Set up event listeners */
    o->output_frame_listener.notify = output_frame_notify;
    wl_signal_add(&wlr_output->events.frame, &o->output_frame_listener);
    o->output_destroy_listener.notify = output_destroy_notify;
    wl_signal_add(&wlr_output->events.destroy, &o->output_destroy_listener);

    wl_list_insert(&s->output_list, &o->output_link);

    wlr_output_enable(wlr_output, 1);
    if (!wlr_output_commit(wlr_output))
        return;

    /* Adds this to the output layout. The add_auto function arranges outputs
     * from left-to-right in the order they appear. A more sophisticated
     * compositor would let the user configure the arrangement of outputs in the
     * layout.
     *
     * The output layout utility automatically adds a wl_output global to the
     * display, which Wayland clients can see to find out information about the
     * output (such as DPI, scale factor, manufacturer, etc).
     */
    wlr_output_layout_add_auto(s->output_layout, wlr_output);
    serv->output_geom = *wlr_output_layout_get_box(s->output_layout, NULL);
}

// ok - FIXME: changed parameters - also need active output
// FIXME: also add active_output to 'ewlc_server'
struct ewlc_output *get_next_output(int direction, ewlc_server *s)
{
    struct ewlc_output *o;

    if (direction > 0) {
        if (s->active_output->output_link.next == &s->output_list)
            return wl_container_of(s->output_list.next, o, output_link);
        return wl_container_of(s->active_output->output_link.next, o, output_link);
    } else {
        if (s->active_output->output_link.prev == &s->output_list)
            return wl_container_of(s->output_list.prev, o, output_link);
        return wl_container_of(s->active_output->output_link.prev, o, output_link);
    }
}

// ok
void output_frame_notify(struct wl_listener *listener, void *data)
{
    struct ewlc_client *c;
    struct ewlc_server *s;
    struct timespec now;
    int render = 1;

    /* This function is called every time an output is ready to display a frame,
     * generally at the output's refresh rate (e.g. 60Hz). */
    struct ewlc_output *o = wl_container_of(listener, o, output_frame_listener);
    s = o->server;

    clock_gettime(CLOCK_MONOTONIC, &now);

    /* Do not render if any XDG clients have an outstanding resize. */
    wl_list_for_each(c, &s->client_stack_list, client_stack_link)
    {
        if (c->resize) {
            wlr_surface_send_frame_done(get_surface(c), &now);
            render = 0;
        }
    }

    /* wlr_output_attach_render makes the OpenGL context current. */
    if (!wlr_output_attach_render(o->wlr_output, NULL))
        return;

    if (render) {
        /* Begin the renderer (calls glViewport and some other GL sanity checks)
         */
        wlr_renderer_begin(s->renderer, o->wlr_output->width,
                           o->wlr_output->height);
        wlr_renderer_clear(s->renderer, root_color);

        render_clients(o, &now);
#ifdef XWAYLAND
        render_independents(o->wlr_output, &now);
#endif

        /* Hardware cursors are rendered by the GPU on a separate plane, and can
         * be moved around without re-rendering what's beneath them - which is
         * more efficient. However, not all hardware supports hardware cursors.
         * For this reason, wlroots provides a software fallback, which we ask
         * it to render here. wlr_cursor handles configuring hardware vs
         * software cursors for
         * you, and this function is a no-op when hardware cursors are in use.
         */
        wlr_output_render_software_cursors(o->wlr_output, NULL);

        /* Conclude rendering and swap the buffers, showing the final frame
         * on-screen. */
        wlr_renderer_end(s->renderer);
    }

    wlr_output_commit(o->wlr_output);
}

// ok
void tile(struct ewlc_output *o)
{
    unsigned int i = 0, n = 0, h, mw, my = 0, ty = 0;
    struct ewlc_server *s = o->server;
    struct ewlc_client *c;

    wl_list_for_each(c, &s->client_list, client_link)
        if (is_visible_on(c, o) && !c->is_floating)
            n++;

    if (n == 0)
        return;

    if (n > o->num_master)
        mw = o->num_master ? o->w.width * o->master_ratio : 0;
    else
        mw = o->w.width;

    wl_list_for_each(c, &s->client_list, client_link)
    {
        if (!is_visible_on(c, o) || c->is_floating)
            continue;
        if (i < o->num_master) {
            h = (o->w.height - my) / (MIN(n, o->num_master) - i);
            resize(c, o->w.x, o->w.y + my, mw, h, 0);
            my += c->geom.height;
        } else {
            h = (o->w.height - ty) / (n - i);
            resize(c, o->w.x + mw, o->w.y + ty, o->w.width - mw, h, 0);
            ty += c->geom.height;
        }
        i++;
    }
}

// ok
struct ewlc_output *get_output_at_point(struct ewlc_server *s, double x, double y)
{
    struct wlr_output *o =
        wlr_output_layout_output_at(s->output_layout, x, y);
    return o ? o->data : NULL;
}
