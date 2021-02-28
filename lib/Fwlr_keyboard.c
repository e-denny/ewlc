/*
 See LICENSE file for copyright and license details.
 */
#define _POSIX_C_SOURCE 200809L
#include <emacs-module.h>
#include "module.h"
#include "notify.h"
#include "Fwlr.h"
#include "server.h"
#include <string.h>
#include <wayland-client.h>
#include <wayland-server-core.h>
#include <wlr/types/wlr_input_device.h>
#include <wlr/types/wlr_keyboard.h>
#include <xkbcommon/xkbcommon.h>

emacs_value Fwlr_keyboard_set_keymap(emacs_env *env, ptrdiff_t nargs,
                                     emacs_value args[], void *data)
{
    struct wlr_input_device *device = env->get_user_ptr(env, args[0]);
    struct xkb_rule_names xkb_rules = {0};
    struct xkb_context *context = xkb_context_new(XKB_CONTEXT_NO_FLAGS);
    struct xkb_keymap * keymap = xkb_map_new_from_names(context,
                                                        &xkb_rules,
                                                        XKB_KEYMAP_COMPILE_NO_FLAGS);
    wlr_keyboard_set_keymap(device->keyboard, keymap);
    xkb_keymap_unref(keymap);
    xkb_context_unref(context);
    return Qt;
}

emacs_value Fxkb_state_key_get_syms(emacs_env *env, ptrdiff_t nargs,
                                    emacs_value args[], void *data)
{
    struct wlr_keyboard *keyboard = env->get_user_ptr(env, args[0]);
    int keycode = env->extract_integer(env, args[1]);
    const xkb_keysym_t *syms;
    int nsyms;
    emacs_value keys[64];
    char key_str[100];

    nsyms = xkb_state_key_get_syms(keyboard->xkb_state, keycode, &syms);
    for (int i = 0; i < nsyms; i++) {
        if (xkb_keysym_get_name(syms[i], key_str, sizeof(key_str)) == -1)
            ERROR("xkb_keysym_get_name failed.:");
        keys[i] = env->make_string(env, key_str, strlen(key_str));
    }
    return list(env, keys, nsyms);
}

emacs_value Fwlr_keyboard_set_repeat_info(emacs_env *env, ptrdiff_t nargs,
                                          emacs_value args[], void *data)
{
    struct wlr_input_device *device = env->get_user_ptr(env, args[0]);
    int repeat_rate = env->extract_integer(env, args[1]);
    int repeat_delay = env->extract_integer(env, args[2]);
    wlr_keyboard_set_repeat_info(device->keyboard, repeat_rate, repeat_delay);
    return Qt;
}

emacs_value Fwlr_keyboard_get_modifiers(emacs_env *env, ptrdiff_t nargs,
                                        emacs_value args[], void *data)
{
    struct wlr_keyboard *keyboard = env->get_user_ptr(env, args[0]);
    uint32_t mods = wlr_keyboard_get_modifiers(keyboard);
    char mods_str[1];
    // TODO: handle other modifiers and combinations of modifiers
    if ((mods & WLR_MODIFIER_ALT) == WLR_MODIFIER_ALT) {
        mods_str[0] = 'M';
    } else {
        mods_str[0] = '\0';
    }
    return env->make_string(env, mods_str, 1);
}

emacs_value Fwlr_event_keyboard_key_get_state(emacs_env *env, ptrdiff_t nargs,
                                              emacs_value args[], void *data)
{
    struct wlr_event_keyboard_key *event = env->get_user_ptr(env, args[0]);
    if (event->state == WLR_KEY_PRESSED)
        return env->intern(env, "wlr-key-pressed");
    if (event->state == WLR_KEY_RELEASED)
        return env->intern(env, "wlr-key-released");
    return Qnil;
}

emacs_value Fwlr_event_keyboard_key_get_keycode(emacs_env *env, ptrdiff_t nargs,
                                              emacs_value args[], void *data)
{
    struct wlr_event_keyboard_key *event = env->get_user_ptr(env, args[0]);
    return env->make_integer(env, event->keycode);
}

emacs_value Fwlr_event_keyboard_key_get_time_msec(emacs_env *env, ptrdiff_t nargs,
                                                  emacs_value args[], void *data)
{
    struct wlr_event_keyboard_key *event = env->get_user_ptr(env, args[0]);
    return env->make_integer(env, event->time_msec);
}

void init_wlr_keyboard(emacs_env *env)
{
    emacs_value func;
    func = env->make_function(env, 1, 1, Fwlr_keyboard_set_keymap, "", NULL);
    bind_function(env, "wlr-keyboard-set-keymap", func);

    func = env->make_function(env, 1, 1, Fwlr_keyboard_get_modifiers, "", NULL);
    bind_function(env, "wlr-keyboard-get-modifiers", func);

    func = env->make_function(env, 3, 3, Fwlr_keyboard_set_repeat_info, "", NULL);
    bind_function(env, "wlr-keyboard-set-repeat-info", func);

    func = env->make_function(env, 1, 1, Fwlr_event_keyboard_key_get_state, "", NULL);
    bind_function(env, "wlr-event-keyboard-key-get-state", func);

    func = env->make_function(env, 1, 1, Fwlr_event_keyboard_key_get_keycode, "", NULL);
    bind_function(env, "wlr-event-keyboard-key-get-keycode", func);

    func = env->make_function(env, 1, 1, Fwlr_event_keyboard_key_get_time_msec, "", NULL);
    bind_function(env, "wlr-event-keyboard-key-get-time-msec", func);

    func = env->make_function(env, 2, 2, Fxkb_state_key_get_syms, "", NULL);
    bind_function(env, "xkb-state-key-get-syms", func);
}
