/*
 p See LICENSE file for copyright and license details.
 */
#ifndef __KEYBOARD_H_
#define __KEYBOARD_H_

#define _POSIX_C_SOURCE 200809L
#include "server.h"
#include <emacs-module.h>
#include <wlr/types/wlr_input_device.h>


struct ewlc_keyboard {
    // does this need to be here ?
    struct wlr_input_device *device;
    struct ewlc_server *server;

    struct wl_listener keyboard_modifiers_listener;
    struct wl_listener keyboard_key_listener;
    struct wl_listener keyboard_destroy_listener;
};

void keyboard_destroy_handler(struct wl_listener *listener, void *data);
void keyboard_key_handler(struct wl_listener *listener, void *data);
void keyboard_modifiers_handler(struct wl_listener *listener, void *data);
void backend_new_input_handler(struct wl_listener *listener, void *data);

#endif // __KEYBOARD_H_
