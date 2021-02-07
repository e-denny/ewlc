/*
 See LICENSE file for copyright and license details.
 */
#define _POSIX_C_SOURCE 200809L
#include <emacs-module.h>
#include "Fwlr.h
#include "server.h"
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

emacs_value Fwlr_keyboard_set_repeat_info(emacs_env *env, ptrdiff_t nargs,
                                          emacs_value args[], void *data)
{
    struct wlr_input_device *device = env->get_user_ptr(env, args[0]);
    int repeat_rate = env->extract_integer(env, args[1]);
    int repeat_delay = env->extract_integer(env, args[2]);
    wlr_keyboard_set_repeat_info(device->keyboard, repeat_rate, repeat_delay);
    return Qt;
}

void init_wlr_keyboard(emacs_env *env)
{
    emacs_value func;
    func = env->make_function(env, 1, 1, Fwlr_keyboard_set_keymap, "", NULL);
    bind_function(env, "wlr-keyboard-set-keymap", func);

    func = env->make_function(env, 3, 3, Fwlr_keyboard_set_repeat_info, "", NULL);
    bind_function(env, "wlr-keyboard-set-repeat-info", func);
}
