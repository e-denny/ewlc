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

emacs_value Fwlr_seat_create(emacs_env *env, ptrdiff_t nargs,
                             emacs_value args[], void *data)
{
    char* name;
    ptrdiff_t len = 0;
    struct wl_display *display = env->get_user_ptr(env, args[0]);
    env->copy_string_contents(env, args[1], NULL, &len);
    name = malloc(sizeof(char) * len);
    env->copy_string_contents(env, args[1], name, &len);

    struct wlr_seat *seat = wlr_seat_create(display, name);
    // TODO: free the seat, put in finalizer ?
    return env->make_user_ptr(env, NULL, seat);
}

emacs_value Fwlr_seat_set_keyboard(emacs_env *env, ptrdiff_t nargs,
                                   emacs_value args[], void *data)
{
    struct wlr_seat *seat = env->get_user_ptr(env, args[0]);
    struct wlr_input_device *device = env->get_user_ptr(env, args[1]);
    wlr_seat_set_keyboard(seat, device);
    return Qt;
}

emacs_value Fwlr_set_seat_capabilities(emacs_env *env, ptrdiff_t nargs,
                                       emacs_value args[], void *data)
{
    uint32_t caps;
    struct wlr_seat *seat;
    emacs_value keyboard_exists;

    seat = env->get_user_ptr(env, args[0]);
    keyboard_exists = args[1];

    caps = WL_SEAT_CAPABILITY_POINTER;
    if (keyboard_exists == Qt)
        caps |= WL_SEAT_CAPABILITY_KEYBOARD;
    wlr_seat_set_capabilities(srv->seat, caps);
    return Qt;
}

emacs_value Fwlr_seat_keyboard_notify_modifiers(emacs_env *env, ptrdiff_t nargs,
                                                emacs_value args[], void *data)
{
    struct wlr_seat *seat = env->get_user_ptr(env, args[0]);
    struct ewlc_keyboard *kb = env->get_user_ptr(env, args[1]);
    wlr_seat_keyboard_notify_modifiers(seat, &kb->device->keyboard->modifiers);
    return Qt;
}

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

emacs_value Fwlr_seat_pointer_notify_button(emacs_env *env, ptrdiff_t nargs,
                                             emacs_value args[], void *data)
{
    struct wlr_seat *seat = env->get_user_ptr(env, args[0]);
    struct wlr_event_pointer_button *event = env->get_user_ptr(env, args[1]);

    wlr_seat_pointer_notify_button(seat, event->time_msec, event->button,
                                   event->state);
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
