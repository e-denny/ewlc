/*
 p See LICENSE file for copyright and license details.
 */
#define "util.h"
#define "keyboard.h"
#define _POSIX_C_SOURCE 200809L
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


void keyboard_destroy_notify(struct wl_listener *listener, void *data)
{
    struct wlr_input_device *device = data;
    struct ewlc_keyboard *kb = wl_container_of(listener, kb, keyboard_destroy_listener);

    wl_list_remove(&kb->keyboard_destroy_listener.link);
    free(kb);
}

void create_keyboard(emacs_env *env, struct wlr_input_device *device)
{
    struct xkb_context *context;
    struct xkb_keymap *keymap;
    struct ewlc_keyboard *kb;

    kb = device->data = calloc(1, sizeof(*kb));
    kb->device = device;
    kb->e_env = env;

    /* Prepare an XKB keymap and assign it to the keyboard. */
    context = xkb_context_new(XKB_CONTEXT_NO_FLAGS);
    keymap = xkb_map_new_from_names(context, &xkb_rules,
                                    XKB_KEYMAP_COMPILE_NO_FLAGS);

    wlr_keyboard_set_keymap(device->keyboard, keymap);
    xkb_keymap_unref(keymap);
    xkb_context_unref(context);
    wlr_keyboard_set_repeat_info(device->keyboard, repeat_rate, repeat_delay);

    /* Here we set up listeners for keyboard events. */
    kb->keyboard_modifiers_listener.notify = keyboard_modifiers_notify;
    wl_signal_add(&device->keyboard->events.modifiers, &kb->keyboard_modifiers_listener);

    kb->keyboard_key_listener.notify = keyboard_key_notify;
    wl_signal_add(&device->keyboard->events.key, &kb->keyboard_key_listener);

    kb->keyboard_destroy_listener.notify = keyboard_destroy_notify;
    wl_signal_add(&device->events.destroy, &kb->keyboard_destroy_listener);

    wlr_seat_set_keyboard(server.seat, device);

    /* And add the keyboard to our list of keyboards */
    wl_list_insert(&server.keyboard_list, &kb->keyboard_link);
}

void backend_new_input_notify(struct wl_listener *listener, void *data)
{
    /* This event is raised by the backend when a new input device becomes
     * available. */
    struct wlr_input_device *device = data;
    uint32_t caps;
    struct ewlc_server *serv = wl_container_of(listener, s, backend_new_input_listener);

    switch (device->type) {
    case WLR_INPUT_DEVICE_KEYBOARD:
        create_keyboard(serv->e_env, device);
        break;
    case WLR_INPUT_DEVICE_POINTER:
        create_pointer(serv, device);
        break;
    default:
        /* XXX handle other input device types */
        break;
    }
    /* We need to let the wlr_seat know what our capabilities are, which is
     * communiciated to the client. We always have a cursor, even if
     * there are no pointer devices, so we always include that capability. */
    /* XXX do we actually require a cursor? */
    caps = WL_SEAT_CAPABILITY_POINTER;
    if (!wl_list_empty(&serv->keyboard_list))
        caps |= WL_SEAT_CAPABILITY_KEYBOARD;
    wlr_seat_set_capabilities(serv->seat, caps);
}

void keyboard_key_notify(struct wl_listener *listener, void *data)
{
    /* This event is raised when a key is pressed or released. */
    struct ewlc_keyboard *kb = wl_container_of(listener, kb, keyboard_key_listener);
    struct wlr_event_keyboard_key *event = data;
    /* Translate libinput keycode -> xkbcommon */
    uint32_t keycode = event->keycode + 8;
    /* Get a list of keysyms based on the keymap for this keyboard */
    const xkb_keysym_t *syms;
    int nsyms;
    uint32_t mods;

    // e_message(kb->e_env, "into: keyboard_key_notify");

    nsyms = xkb_state_key_get_syms(kb->device->keyboard->xkb_state,
                                       keycode, &syms);
    mods = wlr_keyboard_get_modifiers(kb->device->keyboard);

    /* On _press_ and mod, and to list to check if a compositor keybinding. */
    if (event->state == WLR_KEY_PRESSED &&
        (mods & WLR_MODIFIER_ALT) == WLR_MODIFIER_ALT) {
        printf("Alt pressed\n");
        printf("nsyms: %i\n", nsyms);
        for (int i = 0; i < nsyms; i++) {
            printf("i: %i\n", i);
            key_list = add_to_end(key_list, mods, syms[i], kb, event);
        }
        return;
    }

    /* Pass non-modifier keycodes straight to the client. */
    wlr_seat_set_keyboard(server.seat, kb->device);
    wlr_seat_keyboard_notify_key(server.seat, event->time_msec,
                                 event->keycode, event->state);
}

void keyboard_modifiers_notify(struct wl_listener *listener, void *data)
{
    /* This event is raised when a modifier key, such as shift or alt, is
     * pressed. We simply communicate this to the client. */
    struct ewlc_keyboard *kb = wl_container_of(listener, kb, keyboard_modifiers_listener);
    /*
     * A seat can only have one keyboard, but this is a limitation of the
     * Wayland protocol - not wlroots. We assign all connected keyboards to the
     * same seat. You can swap out the underlying wlr_keyboard like this and
     * wlr_seat handles this transparently.
     */
    wlr_seat_set_keyboard(server.seat, kb->device);
    /* Send modifiers to the client. */
    wlr_seat_keyboard_notify_modifiers(server.seat,
                                       &kb->device->keyboard->modifiers);
}
