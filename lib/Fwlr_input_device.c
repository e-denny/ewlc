/*
 See LICENSE file for copyright and license details.
 */
#define _POSIX_C_SOURCE 200809L
#include <emacs-module.h>
#include "Fwlr.h
#include <wayland-client.h>
#include <wayland-server-core.h>
#include <wlr/types/wlr_input_device.h>

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

void init_wlr_input_device(emacs_env *env)
{
    emacs_value func;
    func = env->make_function(env, 1, 1, Fwlr_get_device_type, "", NULL);
    bind_function(env, "wlr-get-device-type", func);
}
