/*
 p See LICENSE file for copyright and license details.
 */
#define _POSIX_C_SOURCE 200809L
#include "util.h"
#include "server.h"
#include "pointer.h"
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

emacs_value Fwlr_seat_pointer_notify_axis(emacs_env *env, ptrdiff_t nargs,
                                          emacs_value args[], void *data)
{
    struct wlr_seat *seat = env->get_user_ptr(env, args[0]);
    struct wlr_event_pointer_axis *event = env->get_user_ptr(env, args[1]);
    wlr_seat_pointer_notify_axis(seat, event->time_msec,
                                 event->orientation, event->delta,
                                 event->delta_discrete, event->source);
    return Qt;
}

emacs_value Fwlr_get_button_press_state(emacs_env *env, ptrdiff_t nargs,
                                         emacs_value args[], void *data)
{
    struct wlr_event_pointer_button *event = env->get_user_ptr(env, args[0]);

    if (event->state == WLR_BUTTON_PRESSED) {
        return env->intern(env, "pressed");
    } else if (event->state == WLR_BUTTON_RELEASED) {
        return env->intern(env, "released");
    }
    return Qnil;
}


emacs_value Fwlr_seat_pointer_notify_button(emacs_env *env, ptrdiff_t nargs,
                                             emacs_value args[], void *data)
{
    struct wlr_seat *seat = env->get_user_ptr(env, args[0]);
    struct wlr_event_pointer_button *event = env->get_user_ptr(env, args[1]);

    wlr_seat_pointer_notify_button(seat, event->time_msec, event->button,
                                   event->state);
    return Qt;
}

emacs_value Fwlr_xcursor_manager_set_cursor_image(emacs_env *env, ptrdiff_t nargs,
                                                  emacs_value args[], void *data)
{
    char* image_text;
    ptrdiff_t len = 0;
    struct wlr_xcursor_manager *cursor_mgr = env->get_user_ptr(env, args[0]);
    struct wlr_cursor *cursor = env->get_user_ptr(env, args[2]);

    env->copy_string_contents(env, args[1], NULL, &len);
    image_text = malloc(sizeof(char) * len);
    env->copy_string_contents(env, args[1], image_text, &len);

    wlr_xcursor_manager_set_cursor_image(cursor_mgr, image_text, cursor);
    // FIXME: free image_text?
}

emacs_value Fewlc_apply_button_action(emacs_env *env, ptrdiff_t nargs,
                                      emacs_value args[], void *data)
{
    struct wlr_seat *seat = env->get_user_ptr(env, args[0]);
    struct wlr_event_pointer_button *event = env->get_user_ptr(env, args[1]);

    struct wlr_keyboard *keyboard;
    uint32_t mods;
    const Button *b;

    keyboard = wlr_seat_get_keyboard(seat);
    mods = wlr_keyboard_get_modifiers(keyboard);

    for (b = s->buttons; b < END(s->buttons); b++) {
        if (CLEANMASK(mods) == CLEANMASK(b->mod) &&
            event->button == b->button && b->func) {
            b->func(s, &b->arg);
            return Qt;
        }
    }
    return Qnil;
}

emacs_value Fewlc_create_pointer(emacs_env *env, ptrdiff_t nargs,
                                 emacs_value args[], void *data)
{
    struct wlr_input_device *device;
    struct wlr_cursor *cursor;

    device = env->get_user_ptr(env, args[0]);
    cursor = env->get_user_ptr(env, args[1]);

    /* TODO: libinput configuration on the device to set acceleration, etc. */

    /* TODO: Handle buttons somewhere, somehow */
    /* srv->buttons[0] = {MODKEY, BTN_LEFT, move_resize}; */
    /* srv->buttons[1] = {MODKEY, BTN_MIDDLE, ewlc_toggle_floating}; */
    /* srv->buttons[2] = {MODKEY, BTN_RIGHT, move_resize}; */

    wlr_cursor_attach_input_device(cursor, device);
    return Qt;
}

emacs_value Fwlr_seat_pointer_notify_frame(emacs_env *env, ptrdiff_t nargs,
                                            emacs_value args[], void *data)
{
    struct wlr_seat *seat = env->get_user_ptr(env, args[0]);
    /* Notify the client with pointer focus of the frame event. */
    wlr_seat_pointer_notify_frame(seat);
    return Qt;
}

emacs_value Fwlr_cursor_warp_absolute(emacs_env *env, ptrdiff_t nargs,
                                      emacs_value args[], void *data)
{
    struct wlr_cursor *cursor = env->get_user_ptr(env, args[0]);
    struct wlr_event_pointer_motion_absolute *event = env->get_user_ptr(env, args[1]);
    wlr_cursor_warp_absolute(cursor, event->device, event->x, event->y);
    return Qt;
}

emacs_value Fwlr_cursor_x(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    struct wlr_cursor *cursor = env->get_user_ptr(env, args[0]);
    // TODO: should this be double or int?
    return env->make_integer(env, cursor->x);
}

emacs_value Fwlr_cursor_y(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    struct wlr_cursor *cursor = env->get_user_ptr(env, args[0]);
    return env->make_integer(env, cursor->y);
}

// FIXME: need finalizer to free
emacs_value Fwlr_box_create(emacs_env *env, ptrdiff_t nargs,
                            emacs_value args[], void *data)
{
    struct wlr_box *box = calloc(1, sizeof(*box));
    box->x = env->extract_integer(env, args[0]);
    box->y = env->extract_integer(env, args[1]);
    box->width = env->extract_integer(env, args[2]);
    box->height = env->extract_integer(env, args[3]);
    return env->make_user_ptr(env, NULL, box);
}

emacs_value Fwlr_box_x(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    struct wlr_box *box = env->get_user_ptr(env, args[0]);
    return env->make_integer(env, box->x);
}

emacs_value Fwlr_set_box_x(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    struct wlr_box *box = env->get_user_ptr(env, args[0]);
    int val = env->extract_integer(env, args[1]);
    box->x = val;
    return Qt;
}

emacs_value Fwlr_box_y(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    struct wlr_box *box = env->get_user_ptr(env, args[0]);
    return env->make_integer(env, box->y);
}

emacs_value Fwlr_set_box_y(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    struct wlr_box *box = env->get_user_ptr(env, args[0]);
    int val = env->extract_integer(env, args[1]);
    box->y = val;
    return Qt;
}

emacs_value Fwlr_box_width(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    struct wlr_box *box = env->get_user_ptr(env, args[0]);
    return env->make_integer(env, box->width);
}

emacs_value Fwlr_set_box_width(emacs_env *env, ptrdiff_t nargs,
                               emacs_value args[], void *data)
{
    struct wlr_box *box = env->get_user_ptr(env, args[0]);
    int val = env->extract_integer(env, args[1]);
    box->width = val;
    return Qt;
}

emacs_value Fwlr_box_height(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    struct wlr_box *box = env->get_user_ptr(env, args[0]);
    return env->make_integer(env, box->height);
}

emacs_value Fwlr_set_box_height(emacs_env *env, ptrdiff_t nargs,
                                emacs_value args[], void *data)
{
    struct wlr_box *box = env->get_user_ptr(env, args[0]);
    int val = env->extract_integer(env, args[1]);
    box->height = val;
    return Qt;
}

emacs_value Fewlc_client_type(emacs_env *env, ptrdiff_t nargs, emacs_value args[],
                              void *data)
{
    struct ewlc_client *client = env->get_user_ptr(env, args[0]);
    if (client->type == XDG_SHELL) {
        return env->intern(env, "xdg_shell");
    }
    return env->intern(env, "xwayland");
}

emacs_value Fwlr_cursor_move(emacs_env *env, ptrdiff_t nargs,
                             emacs_value args[], void *data)
{
    struct wlr_cursor *cursor = env->get_user_ptr(env, args[0]);
    struct wlr_event_pointer_motion *event = env->get_user_ptr(env, args[1]);
    wlr_cursor_move(cursor, event->device, event->delta_x, event->delta_y);
    return Qt;
}

emacs_value Fwlr_cursor_set_surface(emacs_env *env, ptrdiff_t nargs,
                                    emacs_value args[], void *data)
{
    struct wlr_cursor *cursor = env->get_user_ptr(env, args[0]);
    struct wlr_seat *seat = env->get_user_ptr(env, args[1]);
    struct wlr_seat_pointer_request_set_cursor_event *event =
        env->get_user_ptr(env, args[2]);
    if (event->seat_client == seat->pointer_state.focused_client)
        wlr_cursor_set_surface(cursor, event->surface, event->hotspot_x, event->hotspot_y);
    return Qt;
}

emacs_value Fewlc_seat_set_primary_selection(emacs_env *env, ptrdiff_t nargs,
                                             emacs_value args[], void *data)
{
    struct wlr_seat *seat = env->get_user_ptr(env, args[0]);
    struct wlr_seat_request_set_primary_selection_event *event =
        env->get_user_ptr(env, args[1]);
    wlr_seat_set_primary_selection(seat, event->source, event->serial);
    return Qt;
}

emacs_value Fwlr_seat_set_selection(emacs_env *env, ptrdiff_t nargs,
                                     emacs_value args[], void *data)
{
    struct wlr_seat *seat = env->get_user_ptr(env, args[0]);
    struct wlr_seat_request_set_selection_event *event = env->get_user_ptr(env, args[1]);
    wlr_seat_set_selection(seat, event->source, event->serial);
    return Qt;
}

emacs_value Fewlc_cursor_warp_closest(emacs_env *env, ptrdiff_t nargs,
                                      emacs_value args[], void *data)
{
    struct wlr_cursor *cursor = env->get_user_ptr(env, args[0]);
    int x = env->extract_integer(env, args[1]);
    int y = env->extract_integer(env, args[2]);

    wlr_cursor_warp_closest(cursor, NULL, x, y);
    return Qt;
}

emacs_value Fwlr_event_pointer_motion_time_msec(emacs_env *env, ptrdiff_t nargs,
                                                emacs_value args[], void *data)
{
    struct wlr_event_pointer_motion *event = env->get_user_ptr(env, args[0]);
    return env->make_integer(env, event->time_msec);
}

emacs_value Fwlr_event_pointer_motion_absolute_time_msec(emacs_env *env, ptrdiff_t nargs,
                                                         emacs_value args[], void *data)
{
    struct wlr_event_pointer_motion_absolute *event = env->get_user_ptr(env, args[0]);
    return env->make_integer(env, event->time_msec);
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
