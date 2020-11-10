/*
 p See LICENSE file for copyright and license details.
 */
#ifndef __KEYBOARD_H_
#define __KEYBOARD_H_

#define _POSIX_C_SOURCE 200809L
#include <emacs-module.h>
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

struct ewlc_keyboard {
    struct wl_list keyboard_link;
    struct wlr_input_device *device;

    emacs_env *e_env;
    struct wl_listener keyboard_modifiers_listener;
    struct wl_listener keyboard_key_listener;
    struct wl_listener keyboard_destroy_listener;
};

static void keyboard_destroy_notify(struct wl_listener *listener, void *data);
void create_keyboard(emacs_env *env, struct wlr_input_device *device);
void keyboard_key_notify(struct wl_listener *listener, void *data);
static void keyboard_modifiers_notify(struct wl_listener *listener, void *data);
void backend_new_input_notify(struct wl_listener *listener, void *data);

static const struct xkb_rule_names xkb_rules = {
    /* can specify fields: rules, model, layout, variant, options */
    /* example:
        .options = "ctrl:nocaps",
        */
};

#endif // __KEYBOARD_H_
