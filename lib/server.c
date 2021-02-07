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

void deco_destroy_handler(struct wl_listener *listener, void *data)
{
    struct wlr_xdg_toplevel_decoration_v1 *wlr_deco = data;
    struct ewlc_decoration *d = wlr_deco->data;

    wl_list_remove(&d->deco_destroy_listener.link);
    wl_list_remove(&d->deco_request_mode_listener.link);
    free(d);
}

emacs_value Fwlr_xdg_toplevel_decoration_v1_set_mode(emacs_env *env, ptrdiff_t nargs,
                                                     emacs_value args[], void *data)
{
    struct wlr_xdg_toplevel_decoration_v1 *wlr_deco = env->get_user_ptr(env, args[0]);
    wlr_xdg_toplevel_decoration_v1_set_mode(wlr_deco,
                                            WLR_XDG_TOPLEVEL_DECORATION_V1_MODE_SERVER_SIDE);
    return Qt;
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
