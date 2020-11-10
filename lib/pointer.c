/*
 p See LICENSE file for copyright and license details.
 */
#define _POSIX_C_SOURCE 200809L
#include "util.h"
#include "pointer.h"
#include "server.h"

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

// ok
void cursor_axis_notify(struct wl_listener *listener, void *data)
{
    /* This event is forwarded by the cursor when a pointer emits an axis event,
     * for example when you move the scroll wheel. */
    struct wlr_event_pointer_axis *event = data;
    struct ewlc_server *s = wl_container_of(listener, s, cursor_axis_listener);
    /* Notify the client with pointer focus of the axis event. */
    wlr_seat_pointer_notify_axis(s->seat, event->time_msec,
                                 event->orientation, event->delta,
                                 event->delta_discrete, event->source);
}

// ok
void cursor_button_notify(struct wl_listener *listener, void *data)
{
    struct wlr_event_pointer_button *event = data;
    struct ewlc_server *s = wl_container_of(listener, s, cursor_button_listener);
    struct wlr_keyboard *keyboard;
    uint32_t mods;
    struct ewlc_client *c;
    const Button *b;

    switch (event->state) {
    case WLR_BUTTON_PRESSED:;
        /* Change focus if the button was _pressed_ over a client */
        if ((c = get_client_at_point(s->cursor->x, s->cursor->y)))
            focus_client(get_active_client(), c, 1);

        keyboard = wlr_seat_get_keyboard(s->seat);
        mods = wlr_keyboard_get_modifiers(keyboard);
        for (b = buttons; b < END(buttons); b++) {
            if (CLEANMASK(mods) == CLEANMASK(b->mod) &&
                event->button == b->button && b->func) {
                b->func(&b->arg);
                return;
            }
        }
        break;
    case WLR_BUTTON_RELEASED:
        /* If you released any buttons, we exit interactive move/resize mode. */
        /* XXX should reset to the pointer focus's current setcursor */
        if (s->cursor_mode != CUR_NORMAL) {
            wlr_xcursor_manager_set_cursor_image(s->cursor_mgr, "left_ptr",
                                                 s->cursor);
            server.cursor_mode = CUR_NORMAL;
            /* Drop the window off on its new output */
            active_output =
                get_output_at_point(s, s->cursor->x, s->cursor->y);
            set_output(s->grabbed_client, active_output);
            return;
        }
        break;
    }
    /* If the event wasn't handled by the compositor, notify the client with
     * pointer focus that a button press has occurred */
    wlr_seat_pointer_notify_button(s->seat, event->time_msec, event->button,
                                   event->state);
}

// ok
void create_pointer(struct ewlc_server *s, struct wlr_input_device *device)
{
    /* We don't do anything special with pointers. All of our pointer handling
     * is proxied through wlr_cursor. On another compositor, you might take this
     * opportunity to do libinput configuration on the device to set
     * acceleration, etc. */
    wlr_cursor_attach_input_device(server.cursor, device);
}

// ok
void cursor_frame_notify(struct wl_listener *listener, void *data)
{
    /* This event is forwarded by the cursor when a pointer emits an frame
     * event. Frame events are sent after regular pointer events to group
     * multiple events together. For instance, two axis events may happen at the
     * same time, in which case a frame event won't be sent in between. */
    /* Notify the client with pointer focus of the frame event. */
    struct ewlc_server *s = wl_container_of(listener, s, cursor_frame_listener);

    wlr_seat_pointer_notify_frame(s->seat);
}

// ok
void cursor_motion_absolute_notify(struct wl_listener *listener, void *data)
{
    /* This event is forwarded by the cursor when a pointer emits an _absolute_
     * motion event, from 0..1 on each axis. This happens, for example, when
     * wlroots is running under a Wayland window rather than KMS+DRM, and you
     * move the mouse over the window. You could enter the window from any edge,
     * so we have to warp the mouse there. There is also some hardware which
     * emits these events. */
    struct wlr_event_pointer_motion_absolute *event = data;
    struct ewlc_server *s = wl_container_of(listener, s, cursor_motion_absolute_listener);

    wlr_cursor_warp_absolute(s->cursor, event->device, event->x, event->y);
    motion_notify(s, event->time_msec);
}

// ok
void motion_notify(ewlc_server *s, uint32_t time)
{
    double sx = 0, sy = 0;
    struct wlr_surface *surface = NULL;
    struct ewlc_client *c;

    /* Update active_output (even while dragging a window) */
    if (sloppyfocus)
        active_output = get_output_at_point(s, s->cursor->x, s->cursor->y);

    /* If we are currently grabbing the mouse, handle and return */
    if (s->cursor_mode == CUR_MOVE) {
        /* Move the grabbed client to the new position. */
        resize(s->grabbed_client, s->cursor->x - s->grabc_x,
               s->cursor->y - s->grabc_y,
               s->grabbed_client->geom.width,
               s->grabbed_client->geom.height, 1);
        return;
    } else if (s->cursor_mode == CUR_RESIZE) {
        resize(s->grabbed_client, s->grabbed_client->geom.x,
               s->grabbed_client->geom.y,
               s->cursor->x - s->grabbed_client->geom.x,
               s->cursor->y - s->grabbed_client->geom.y, 1);
        return;
    }

#ifdef XWAYLAND
    /* Find an independent under the pointer and send the event along. */
    if ((c = get_independent_at_point(s->cursor->x, s->cursor->y))) {
        surface = wlr_surface_surface_at(
            c->surface.xwayland->surface,
            s->cursor->x - c->surface.xwayland->x - c->border_width,
            s->cursor->y - c->surface.xwayland->y - c->border_width, &sx,
            &sy);

        /* Otherwise, find the client under the pointer and send the event
         * along. */
    } else
#endif
        if ((c = get_client_at_point(s->cursor->x, s->cursor->y))) {
#ifdef XWAYLAND
        if (c->type != XDG_SHELL)
            surface = wlr_surface_surface_at(
                c->surface.xwayland->surface,
                s->cursor->x - c->geom.x - c->border_width,
                s->cursor->y - c->geom.y - c->border_width, &sx, &sy);
        else
#endif
            surface = wlr_xdg_surface_surface_at(
                c->surface.xdg, s->cursor->x - c->geom.x - c->border_width,
                s->cursor->y - c->geom.y - c->border_width, &sx, &sy);
    }
    /* If there's no client surface under the cursor, set the cursor image to a
     * default. This is what makes the cursor image appear when you move it
     * off of a client or over its border. */
    if (!surface)
        wlr_xcursor_manager_set_cursor_image(s->cursor_mgr, "left_ptr",
                                             s->cursor);

    pointer_focus(c, surface, sx, sy, time);
}

// ok
void cursor_motion_notify(struct wl_listener *listener, void *data)
{
    /* This event is forwarded by the cursor when a pointer emits a _relative_
     * pointer motion event (i.e. a delta) */
    struct wlr_event_pointer_motion *event = data;
    struct ewlc_server *s = wl_container_of(listener, s, cursor_motion_listener);
    /* The cursor doesn't move unless we tell it to. The cursor automatically
     * handles constraining the motion to the output layout, as well as any
     * special configuration applied for the specific input device which
     * generated the event. You can pass NULL for the device if you want to move
     * the cursor around without any input. */
    wlr_cursor_move(s->cursor, event->device, event->delta_x,
                    event->delta_y);
    motion_notify(s, event->time_msec);
}

// ok
void seat_request_set_cursor_notify(struct wl_listener *listener, void *data)
{
    /* This event is raised by the seat when a client provides a cursor image */
    struct wlr_seat_pointer_request_set_cursor_event *event = data;
    struct ewlc_server *s = wl_container_of(listener, s, seat_request_set_cursor_listener);
    /* If we're "grabbing" the cursor, don't use the client's image */
    /* XXX still need to save the provided surface to restore later */
    if (s->cursor_mode != CUR_NORMAL)
        return;
    /* This can be sent by any client, so we check to make sure this one is
     * actually has pointer focus first. If so, we can tell the cursor to
     * use the provided surface as the cursor image. It will set the
     * hardware cursor on the output that it's currently on and continue to
     * do so as the cursor moves between outputs. */
    if (event->seat_client == s->seat->pointer_state.focused_client)
        wlr_cursor_set_surface(s->cursor, event->surface, event->hotspot_x,
                               event->hotspot_y);
}

// ok
void seat_request_set_primary_selection_notify(struct wl_listener *listener,
                                               void *data)
{
    /* This event is raised by the seat when a client wants to set the
     * selection, usually when the user copies something. wlroots allows
     * compositors to ignore such requests if they so choose, but we always
     * honor
     */
    struct wlr_seat_request_set_primary_selection_event *event = data;
    struct ewlc_server *s = wl_container_of(listener, s,
                                             seat_request_set_primary_selection_listener);
    wlr_seat_set_primary_selection(s->seat, event->source, event->serial);
}

//ok
void seat_request_set_selection_notify(struct wl_listener *listener, void *data)
{
    /* This event is raised by the seat when a client wants to set the
     * selection, usually when the user copies something. wlroots allows
     * compositors to ignore such requests if they so choose, but we always
     * honor
     */
    struct wlr_seat_request_set_selection_event *event = data;
    struct ewlc_server *s = wl_container_of(listener, s,
                                             seat_request_set_selection_listener);
    wlr_seat_set_selection(s->seat, event->source, event->serial);
}
