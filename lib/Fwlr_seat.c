/*
 See LICENSE file for copyright and license details.
 */
#define _POSIX_C_SOURCE 200809L
#include <emacs-module.h>
#include "module.h"
#include "Fwlr.h"
#include <stdlib.h>
#include <wayland-client.h>
#include <wayland-server-core.h>
#include <wlr/types/wlr_cursor.h>
#include <wlr/types/wlr_input_device.h>
#include <wlr/types/wlr_data_device.h>
#include <wlr/types/wlr_pointer.h>
#include <wlr/types/wlr_keyboard.h>
#include <wlr/types/wlr_surface.h>
#include <wlr/types/wlr_seat.h>
#include <wlr/types/wlr_primary_selection.h>
#include <wlr/types/wlr_primary_selection_v1.h>

emacs_value Fwlr_seat_create(emacs_env *env, ptrdiff_t nargs,
                             emacs_value args[], void *data)
{
    struct wl_display *display = env->get_user_ptr(env, args[0]);
    char* name;
    ptrdiff_t len = 0;
    struct wlr_seat *seat;

    env->copy_string_contents(env, args[1], NULL, &len);
    name = malloc(sizeof(char) * len);
    env->copy_string_contents(env, args[1], name, &len);
    seat = wlr_seat_create(display, name);
    // TODO: free the seat, put in finalizer ?
    return env->make_user_ptr(env, NULL, seat);
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

emacs_value Fwlr_seat_keyboard_notify_enter(emacs_env *env, ptrdiff_t nargs,
                                            emacs_value args[], void *data)
{
    struct wlr_seat *seat = env->get_user_ptr(env, args[0]);
    struct wlr_surface *surface = env->get_user_ptr(env, args[1]);
    struct wlr_keyboard *kb = env->get_user_ptr(env, args[2]);
    wlr_seat_keyboard_notify_enter(seat, surface, kb->keycodes, kb->num_keycodes, &kb->modifiers);
    return Qt;
}

emacs_value Fwlr_seat_keyboard_notify_clear_focus(emacs_env *env, ptrdiff_t nargs,
                                                  emacs_value args[], void *data)
{
    struct wlr_seat *seat = env->get_user_ptr(env, args[0]);
    wlr_seat_keyboard_notify_clear_focus(seat);
    return Qt;
}

emacs_value Fwlr_seat_set_keyboard(emacs_env *env, ptrdiff_t nargs,
                                   emacs_value args[], void *data)
{
    struct wlr_seat *seat = env->get_user_ptr(env, args[0]);
    struct wlr_input_device *device = env->get_user_ptr(env, args[1]);
    wlr_seat_set_keyboard(seat, device);
    return Qt;
}

emacs_value Fwlr_seat_get_keyboard(emacs_env *env, ptrdiff_t nargs,
                                   emacs_value args[], void *data)
{
    struct wlr_seat *seat = env->get_user_ptr(env, args[0]);
    struct wlr_keyboard *keyboard = wlr_seat_get_keyboard(seat);
    return env->make_user_ptr(env, NULL, keyboard);
}

emacs_value Fwlr_seat_set_capabilities(emacs_env *env, ptrdiff_t nargs,
                                       emacs_value args[], void *data)
{
    uint32_t caps;
    struct wlr_seat *seat = env->get_user_ptr(env, args[0]);
    emacs_value keyboard_exists = args[1];

    caps = WL_SEAT_CAPABILITY_POINTER;
    if (keyboard_exists == Qt)
        caps |= WL_SEAT_CAPABILITY_KEYBOARD;
    wlr_seat_set_capabilities(seat, caps);
    return Qt;
}

emacs_value Fwlr_seat_keyboard_notify_modifiers(emacs_env *env, ptrdiff_t nargs,
                                                emacs_value args[], void *data)
{
    struct wlr_seat *seat = env->get_user_ptr(env, args[0]);
    struct wlr_input_device *device = env->get_user_ptr(env, args[1]);
    wlr_seat_keyboard_notify_modifiers(seat, &device->keyboard->modifiers);
    return Qt;
}

emacs_value Fwlr_seat_keyboard_notify_key(emacs_env *env, ptrdiff_t nargs,
                                          emacs_value args[], void *data)
{
    struct wlr_seat *seat = env->get_user_ptr(env, args[0]);
    int time_msec = env->extract_integer(env, args[1]);
    int key = env->extract_integer(env, args[2]);
    int state = env->extract_integer(env, args[3]);
    wlr_seat_keyboard_notify_key(seat, time_msec, key, state);
    return Qt;
}

emacs_value Fwlr_seat_pointer_notify_axis(emacs_env *env, ptrdiff_t nargs,
                                          emacs_value args[], void *data)
{
    struct wlr_seat *seat = env->get_user_ptr(env, args[0]);
    struct wlr_event_pointer_axis *event = env->get_user_ptr(env, args[1]);
    wlr_seat_pointer_notify_axis(seat, event->time_msec, event->orientation, event->delta,
                                 event->delta_discrete, event->source);
    return Qt;
}

emacs_value Fwlr_seat_pointer_notify_button(emacs_env *env, ptrdiff_t nargs,
                                            emacs_value args[], void *data)
{
    struct wlr_seat *seat = env->get_user_ptr(env, args[0]);
    struct wlr_event_pointer_button *event = env->get_user_ptr(env, args[1]);

    wlr_seat_pointer_notify_button(seat, event->time_msec, event->button, event->state);
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

emacs_value Fwlr_seat_set_primary_selection(emacs_env *env, ptrdiff_t nargs,
                                            emacs_value args[], void *data)
{
    struct wlr_seat *seat = env->get_user_ptr(env, args[0]);
    struct wlr_seat_request_set_primary_selection_event *event = env->get_user_ptr(env, args[1]);
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

void init_wlr_seat(emacs_env *env)
{
    emacs_value func;
    func = env->make_function(env, 2, 2, Fwlr_seat_create, "", NULL);
    bind_function(env, "wlr-seat-create", func);

    func = env->make_function(env, 1, 1, Fwlr_seat_pointer_notify_clear_focus, "", NULL);
    bind_function(env, "wlr-seat-pointer-notify-clear-focus", func);

    func = env->make_function(env, 4, 4, Fwlr_seat_pointer_notify_motion, "", NULL);
    bind_function(env, "wlr-seat-pointer-notify-motion", func);

    func = env->make_function(env, 4, 4, Fwlr_seat_pointer_enter, "", NULL);
    bind_function(env, "wlr-seat-pointer-enter", func);

    func = env->make_function(env, 3, 3, Fwlr_seat_keyboard_notify_enter, "", NULL);
    bind_function(env, "wlr-seat-keyboard-notify-enter", func);

    func = env->make_function(env, 1, 1, Fwlr_seat_keyboard_notify_clear_focus, "", NULL);
    bind_function(env, "wlr-seat-keyboard-notify-clear-focus", func);

    func = env->make_function(env, 2, 2, Fwlr_seat_set_keyboard, "", NULL);
    bind_function(env, "wlr-seat-set-keyboard", func);

    func = env->make_function(env, 1, 1, Fwlr_seat_get_keyboard, "", NULL);
    bind_function(env, "wlr-seat-get-keyboard", func);

    func = env->make_function(env, 2, 2, Fwlr_seat_set_capabilities, "", NULL);
    bind_function(env, "wlr-seat-set-capabilites", func);

    func = env->make_function(env, 2, 2, Fwlr_seat_keyboard_notify_modifiers, "", NULL);
    bind_function(env, "wlr-seat-keyboard-notify-modifiers", func);

    func = env->make_function(env, 2, 2, Fwlr_seat_pointer_notify_axis, "", NULL);
    bind_function(env, "wlr-seat-pointer-notify-axis", func);

    func = env->make_function(env, 2, 2, Fwlr_seat_pointer_notify_button, "", NULL);
    bind_function(env, "wlr-seat-pointer-notify-button", func);

    func = env->make_function(env, 1, 1, Fwlr_seat_pointer_notify_frame, "", NULL);
    bind_function(env, "wlr-seat-pointer-notify-frame", func);

    func = env->make_function(env, 2, 2, Fwlr_seat_set_primary_selection, "", NULL);
    bind_function(env, "wlr-seat-set-primary-selection", func);

    func = env->make_function(env, 4, 4, Fwlr_seat_keyboard_notify_key, "", NULL);
    bind_function(env, "wlr-seat-keyboard-notify-key", func);

    func = env->make_function(env, 2, 2, Fwlr_seat_set_selection, "", NULL);
    bind_function(env, "wlr-seat-set-selection", func);
}
