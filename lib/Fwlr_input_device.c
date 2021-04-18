/*
 See LICENSE file for copyright and license details.
 */
#define _POSIX_C_SOURCE 200809L
#include <emacs-module.h>
#include "module.h"
#include "Fwlr.h"
#include <wayland-client.h>
#include <wayland-server-core.h>
#include <wlr/types/wlr_input_device.h>

emacs_value Fwlr_input_device_get_type(emacs_env *env, ptrdiff_t nargs,
                                       emacs_value args[], void *data)
{
    struct wlr_input_device *device = env->get_user_ptr(env, args[0]);

    if (device->type == WLR_INPUT_DEVICE_KEYBOARD) {
        return env->intern(env, "wlr-input-device-keyboard");
    } else if (device->type == WLR_INPUT_DEVICE_POINTER) {
        return env->intern(env, "wlr-input-device-pointer");
    }
    return Qnil;
}

emacs_value Fwlr_input_device_get_keyboard(emacs_env *env, ptrdiff_t nargs,
                                           emacs_value args[], void *data)
{
    struct wlr_input_device *device = env->get_user_ptr(env, args[0]);
    struct wlr_keyboard *keyboard = device->keyboard;
    return env->make_user_ptr(env, NULL, keyboard);
}

// TODO: can I put all these into a single function that returns a defstruct object?
// i.e. does something like this work?
// return env->make-some-defstruct(env, (emacs_value[]){Qkey1, val1, Qkey2, val2}, 4);
// or a property list?
//
void init_wlr_input_device(emacs_env *env)
{
    emacs_value func;
    func = env->make_function(env, 1, 1, Fwlr_input_device_get_type, "", NULL);
    bind_function(env, "wlr-input-device-get-type", func);

    func = env->make_function(env, 1, 1, Fwlr_input_device_get_keyboard, "", NULL);
    bind_function(env, "wlr-input-device-get-keyboard", func);
}
