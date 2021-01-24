/*
 p See LICENSE file for copyright and license details.
 */
#define _POSIX_C_SOURCE 200809L
#include "util.h"
#include "server.h"
#include "keyboard.h"
#include "pointer.h"
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

const struct xkb_rule_names xkb_rules = {0};

emacs_value Fewlc_free(emacs_env *env, ptrdiff_t nargs,
                       emacs_value args[], void *data)
{
    free(env->get_user_ptr(env, args[0]));;
    return Qt;
}

emacs_value Fewlc_create_keyboard(emacs_env *env, ptrdiff_t nargs,
                                  emacs_value args[], void *data)
{
    struct wlr_input_device *device;
    struct wlr_seat *seat;
    int repeat_rate;
    int repeat_delay;

    struct xkb_context *context;
    struct xkb_keymap *keymap;
    struct ewlc_keyboard *kb;

    device = env->get_user_ptr(env, args[0]);
    seat = env->get_user_ptr(env, args[1]);
    repeat_rate = env->extract_integer(env, args[2]);
    repeat_delay = env->extract_integer(env, args[3]);

    kb = device->data = calloc(1, sizeof(*kb));

    kb->device = device;

    /* Create an XKB keymap and assign it to the keyboard. */

    context = xkb_context_new(XKB_CONTEXT_NO_FLAGS);
    keymap = xkb_map_new_from_names(context, &xkb_rules, XKB_KEYMAP_COMPILE_NO_FLAGS);
    wlr_keyboard_set_keymap(device->keyboard, keymap);
    xkb_keymap_unref(keymap);
    xkb_context_unref(context);

    wlr_keyboard_set_repeat_info(device->keyboard, repeat_rate, repeat_delay);

    /* Set up listeners for keyboard events. */

    kb->keyboard_modifiers_listener.notify = keyboard_modifiers_notify;
    kb->keyboard_key_listener.notify = keyboard_key_notify;
    kb->keyboard_destroy_listener.notify = keyboard_destroy_notify;

    wl_signal_add(&device->keyboard->events.modifiers, &kb->keyboard_modifiers_listener);
    wl_signal_add(&device->keyboard->events.key, &kb->keyboard_key_listener);
    wl_signal_add(&device->events.destroy, &kb->keyboard_destroy_listener);

    /* Assign the keyboard to the seat. */

    wlr_seat_set_keyboard(seat, device);

    return env->make_user_ptr(env, NULL, kb);
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

emacs_value Fwlr_get_device_type(emacs_env *env, ptrdiff_t nargs,
                                 emacs_value args[], void *data)
{
    struct wlr_input_device *device;

    device = env->get_user_ptr(env, args[0]);

    if (device->type == WLR_INPUT_DEVICE_KEYBOARD) {
        return Qkeyboard;
    } else if (device->type == WLR_INPUT_DEVICE_POINTER) {
        return Qpointer;
    }
    return Qnil;
}

void keyboard_key_notify(struct wl_listener *listener, void *data)
{
    /* This event is raised when a key is pressed or released. */
    struct ewlc_keyboard *kb = wl_container_of(listener, kb, keyboard_key_listener);
    struct wlr_event_keyboard_key *event = data;
    struct ewlc_server *srv = kb->server;
    /* Translate libinput keycode -> xkbcommon */
    uint32_t keycode = event->keycode + 8;
    /* Get a list of keysyms based on the keymap for this keyboard */
    const xkb_keysym_t *syms;
    int nsyms;
    uint32_t mods;
    INFO(">>>");
    DEBUG("srv = '%p'", srv);

    DEBUG("event: %p", event);
    DEBUG("keycode = '%d'", event->keycode);
    DEBUG("state = '%d'", event->state);
    DEBUG("time = '%d'", event->time_msec);

    nsyms = xkb_state_key_get_syms(kb->device->keyboard->xkb_state,
                                       keycode, &syms);
    DEBUG("nsyms = '%d'", nsyms);
    mods = wlr_keyboard_get_modifiers(kb->device->keyboard);
    DEBUG("mods = '%d'", mods);

    /* On _press_ and mod, and to list to check if a compositor keybinding. */
    if (event->state == WLR_KEY_PRESSED &&
        (mods & WLR_MODIFIER_ALT) == WLR_MODIFIER_ALT) {
        DEBUG("mod: %d", WLR_MODIFIER_ALT);
        DEBUG("nsyms: %d", nsyms);
        for (int i = 0; i < nsyms; i++) {
            DEBUG("i: %d", i);
            srv->key_list = add_to_end(srv->key_list, mods, syms[i], kb, event);
        }
        INFO("<<<return");
        return;
    }

    /* Pass non-modifier keycodes straight to the client. */
    DEBUG("seat = '%p'", srv->seat);
    DEBUG("device = '%p'", kb->device);

    wlr_seat_set_keyboard(srv->seat, kb->device);
    wlr_seat_keyboard_notify_key(srv->seat, event->time_msec,
                                 event->keycode, event->state);
    INFO("<<<");
    // free(event);
}

emacs_value Fwlr_seat_set_keyboard(emacs_env *env, ptrdiff_t nargs,
                                   emacs_value args[], void *data)
{
    struct wlr_seat *seat = env->get_user_ptr(env, args[0]);
    struct ewlc_keyboard *kb = env->get_user_ptr(env, args[1]);
    wlr_seat_set_keyboard(seat, kb->device);
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
