/*
 p See LICENSE file for copyright and license details.
 */
#define _POSIX_C_SOURCE 200809L
#include "util.h"
#include "server.h"
#include "client.h"
#include "output.h"
#include "pointer.h"
#include "keyboard.h"
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

// ----------------------------------------------------------------------

void xwayland_ready_notify(struct wl_listener *listener, void *data)
{
    struct ewlc_server *s = wl_container_of(listener, s, xwayland_ready_listener);
    struct event_node *e_node;

    e_node = create_event(listener, data, "ewlc-xwayland-ready");
    s->event_list = add_event(s->event_list, e_node);
}

// ----------------------------------------------------------------------

void deco_request_mode_notify(struct wl_listener *listener, void *data)
{
    struct ewlc_decoration *d;
    struct ewlc_server *s;
    struct event_node *e_node;

    d = wl_container_of(listener, d, deco_request_mode_listener);
    s = d->server;
    e_node = create_event(listener, data, "ewlc-deco-request-mode");
    s->event_list = add_event(s->event_list, e_node);
}

void deco_destroy_notify(struct wl_listener *listener, void *data)
{
    struct ewlc_decoration *d;
    struct ewlc_server *s;
    struct event_node *e_node;

    d = wl_container_of(listener, d, deco_destroy_listener);
    s = d->server;
    e_node = create_event(listener, data, "ewlc_deco_destroy");
    s->event_list = add_event(s->event_list, e_node);
}

void xdeco_mgr_new_toplevel_decoration_notify(struct wl_listener *listener, void *data)
{
    struct ewlc_server *s;
    struct event_node *e_node;

    s = wl_container_of(listener, s, xdeco_mgr_new_top_level_decoration_listener);
    e_node = create_event(listener, data, "ewlc_new_toplevel_decoration");
    s->event_list = add_event(s->event_list, e_node);
}

// ----------------------------------------------------------------------

void seat_request_set_primary_selection_notify(struct wl_listener *listener,
                                               void *data)
{
    /* This event is raised by the seat when a client wants to set the
     * selection, usually when the user copies something. */

    struct ewlc_server *s;
    struct event_node *e;
    struct wlr_seat_request_set_primary_selection_event *event = data;
    struct wlr_seat_request_set_primary_selection_event *event_cpy =
        calloc(1, sizeof(*event_cpy));

    s = wl_container_of(listener, s, seat_request_set_primary_selection_listener);

    event_cpy->serial = event->serial;
    event_cpy->source = event->source;

    e = create_event(listener, (void *)event_cpy, EWLC_SEAT_REQUEST_SET_PRIMARY_SELECTION);
    s->event_list = add_event(s->event_list, e);
}

void seat_request_set_selection_notify(struct wl_listener *listener, void *data)
{
    /* This event is raised by the seat when a client wants to set the
     * selection, usually when the user copies something. */

    struct ewlc_server *s;
    struct event_node *e_node;
    struct wlr_seat_request_set_selection_event *event = data;
    struct wlr_seat_request_set_selection_event *event_cpy = calloc(1, sizeof(*event_cpy));

    event_cpy->serial = event->serial;
    event_cpy->source = event->source;

    s = wl_container_of(listener, s, seat_request_set_selection_listener);
    e_node = create_event(listener, (void *)event_cpy, EWLC_SEAT_REQUEST_SET_SELECTION);
    s->event_list = add_event(s->event_list, e_node);
}

// ----------------------------------------------------------------------

void cursor_axis_notify(struct wl_listener *listener, void *data)
{
    /* This event is forwarded by the cursor when a pointer emits an axis event,
     * for example when you move the scroll wheel. */
    struct ewlc_server *s;
    struct event_node *e;
    struct wlr_event_pointer_axis *event = data;
    struct wlr_event_pointer_axis *event_cpy = calloc(1, sizeof(*event_cpy));;

    event_cpy->time_msec = event->time_msec;
    event_cpy->orientation = event->orientation;
    event_cpy->delta = event->delta;
    event_cpy->delta_discrete = event->delta_discrete;
    event_cpy->source = event->source;

    s = wl_container_of(listener, s, cursor_axis_listener);
    e = create_event(listener, (void *)event_cpy, EWLC_CURSOR_AXIS);
    s->event_list = add_event(s->event_list, e);
}

void cursor_button_notify(struct wl_listener *listener, void *data)
{
    struct ewlc_server *s;
    struct event_node *e;
    struct wlr_event_pointer_button *event = data;
    struct wlr_event_pointer_button *event_cpy = calloc(1, sizeof(*event_cpy));

    event_cpy->time_msec = event->time_msec;
    event_cpy->button = event->button;
    event_cpy->state = event->state;

    s = wl_container_of(listener, s, cursor_button_listener);
    e = create_event(listener, (void *)event_cpy, EWLC_CURSOR_BUTTON);
    s->event_list = add_event(s->event_list, e);
}

void cursor_frame_notify(struct wl_listener *listener, void *data)
{
    struct ewlc_server *s;
    struct event_node *e;

    s = wl_container_of(listener, s, cursor_frame_listener);
    e = create_event(listener, data, EWLC_CURSOR_FRAME);
    s->event_list = add_event(s->event_list, e);
}

void cursor_motion_absolute_notify(struct wl_listener *listener, void *data)
{
    /* This event is forwarded by the cursor when a pointer emits an _absolute_
     * motion event, from 0..1 on each axis. This happens, for example, when
     * wlroots is running under a Wayland window rather than KMS+DRM, and you
     * move the mouse over the window. You could enter the window from any edge,
     * so we have to warp the mouse there. There is also some hardware which
     * emits these events. */

    struct ewlc_server *s;
    struct event_node *e;
    struct wlr_event_pointer_motion_absolute *event = data;
    struct wlr_event_pointer_motion_absolute *event_cpy = calloc(1, sizeof(*event_cpy));

    event_cpy->time_msec = event->time_msec;
    event_cpy->device = event->device;
    event_cpy->x = event->x;
    event_cpy->y = event->y;

    s = wl_container_of(listener, s, cursor_motion_absolute_listener);
    e = create_event(listener, (void *)event_cpy, EWLC_CURSOR_MOTION_ABSOLUTE);
    s->event_list = add_event(s->event_list, e);
}

void cursor_motion_notify(struct wl_listener *listener, void *data)
{
    /* This event is forwarded by the cursor when a pointer emits a _relative_
     * pointer motion event (i.e. a delta) */

    struct ewlc_server *s;
    struct event_node *e;
    struct wlr_event_pointer_motion *event = data;
    struct wlr_event_pointer_motion *event_cpy = calloc(1, sizeof(*event_cpy));

    event_cpy->time_msec = event->time_msec;
    event_cpy->device = event->device;
    event_cpy->delta_x = event->delta_x;
    event_cpy->delta_y = event->delta_y;

    s = wl_container_of(listener, s, cursor_motion_listener);
    e = create_event(listener, (void *)event_cpy, EWLC_CURSOR_MOTION);
    s->event_list = add_event(s->event_list, e);
}

void seat_request_set_cursor_notify(struct wl_listener *listener, void *data)
{
    /* This event is raised by the seat when a client provides a cursor image */

    struct ewlc_server *s;
    struct event_node *e;
    struct wlr_seat_pointer_request_set_cursor_event *event = data;
    struct wlr_seat_pointer_request_set_cursor_event *event_cpy
        = calloc(1, sizeof(*event_cpy));

    event_cpy->surface = event->surface;
    event_cpy->hotspot_x = event->hotspot_x;
    event_cpy->hotspot_y = event->hotspot_y;
    event_cpy->seat_client = event->seat_client;

    s = wl_container_of(listener, s, seat_request_set_cursor_listener);
    e = create_event(listener, (void *)event_cpy, EWLC_SEAT_REQUEST_SET_CURSOR);
    s->event_list = add_event(s->event_list, e);
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

// ----------------------------------------------------------------------

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

// ----------------------------------------------------------------------

void keyboard_destroy_notify(struct wl_listener *listener, void *data)
{
    /* struct wlr_input_device *device = data; */
    struct ewlc_keyboard *kb;
    struct ewlc_server *s;
    struct event_node *e;

    kb = wl_container_of(listener, kb, keyboard_destroy_listener);
    s = kb->server;
    e = create_event(listener, data, EWLC_KEYBOARD_DESTROY);
    s->event_list = add_event(s->event_list, e);
}

void backend_new_input_notify(struct wl_listener *listener, void *data)
{
    struct ewlc_server *s;
    struct event_node *e;

    s = wl_container_of(listener, s, backend_new_input_listener);
    e = create_event(listener, data, EWLC_BACKEND_NEW_INPUT);
    s->event_list = add_event(s->event_list, e);
}

/* void keyboard_key_notify(struct wl_listener *listener, void *data) */
/* { */
/*     struct ewlc_keyboard *kb; */
/*     struct ewlc_server *s; */
/*     struct event_node *e_node; */
/*     struct wlr_event_keyboard_key *event = data; */
/*     struct wlr_event_keyboard_key *event_cpy = calloc(1, sizeof(*event_cpy)); */

/*     INFO(">>>"); */
/*     event_cpy->keycode = event->keycode; */
/*     event_cpy->state = event->state; */
/*     event_cpy->time_msec = event->time_msec; */

/*     DEBUG("event: %p", event_cpy); */
/*     DEBUG("event keycode: %d", event_cpy->keycode); */
/*     DEBUG("event state: %d", event_cpy->state); */
/*     DEBUG("event time: %d", event_cpy->time_msec); */

/*     kb = wl_container_of(listener, kb, keyboard_key_listener); */
/*     s = kb->server; */
/*     e_node = create_event(listener, (void *)event_cpy, EWLC_KEYBOARD_KEY); */
/*     s->event_list = add_event(s->event_list, e_node); */
/*     INFO("<<<"); */
/* } */

void keyboard_modifiers_notify(struct wl_listener *listener, void *data)
{
    /* This event is raised when a modifier key, such as shift or alt, is
     * pressed. We simply communicate this to the client. */
    struct ewlc_keyboard *kb;
    struct ewlc_server *s;
    struct event_node *e;

    kb = wl_container_of(listener, kb, keyboard_modifiers_listener);
    s = kb->server;
    e = create_event(listener, data, EWLC_KEYBOARD_MODIFIERS);
    s->event_list = add_event(s->event_list, e);
}

// ----------------------------------------------------------------------

void backend_new_output_notify(struct wl_listener *listener, void *data)
{
    /* This event is raised by the backend when a new output (aka a display or
     * output) becomes available. */

    struct ewlc_server *s;
    struct event_node *e;

    s = wl_container_of(listener, s, backend_new_output_listener);
    e = create_event(listener, data, "ewlc-backend-new-output");
    s->event_list = add_event(s->event_list, e);
}

void output_destroy_notify(struct wl_listener *listener, void *data)
{
    struct ewlc_server *s;
    struct ewlc_output *o;
    struct event_node *e;

    o = wl_container_of(listener, o, output_destroy_listener);
    s = o->server;
    e = create_event(listener, data, "ewlc-output-destroy");
    s->event_list = add_event(s->event_list, e);
}

void output_frame_notify(struct wl_listener *listener, void *data)
{
    /* This function is called every time an output is ready to display a frame,
     * generally at the output's refresh rate (e.g. 60Hz). */

    struct ewlc_output *o;
    struct ewlc_server *s;
    struct event_node *e;

    o = wl_container_of(listener, o, output_frame_listener);
    s = o->server;
    e = create_event(listener, data, "ewlc-output-frame");
    s->event_list = add_event(s->event_list, e);
}
