/*
 p See LICENSE file for copyright and license details.
 */

#ifndef __SERVER_H_
#define __SERVER_H_

#define _POSIX_C_SOURCE 200809L
#include "server.h"
#include "util.h"
#include <emacs-module.h>
#include <getopt.h>
#include <linux/input-event-codes.h>
#include <wayland-client.h>
#include <wayland-server-core.h>
#include <wlr/backend.h>
#include <wlr/render/wlr_renderer.h>
#include <wlr/types/wlr_compositor.h>
#include <wlr/types/wlr_cursor.h>
#include <wlr/types/wlr_input_device.h>
#include <wlr/types/wlr_keyboard.h>
#include <wlr/types/wlr_output.h>
#include <wlr/types/wlr_output_layout.h>
#include <wlr/types/wlr_pointer.h>
#include <wlr/types/wlr_seat.h>
#include <wlr/types/wlr_xcursor_manager.h>
#include <wlr/types/wlr_xdg_shell.h>
#include <wlr/util/log.h>
#include <xkbcommon/xkbcommon.h>

#include <X11/Xlib.h>
#include <wlr/xwayland.h>

enum {
    NetWMWindowTypeDialog,
    NetWMWindowTypeSplash,
    NetWMWindowTypeToolbar,
    NetWMWindowTypeUtility,
    NetLast
};

struct ewlc_output;
struct ewlc_server;

typedef union {
    int i;
    unsigned int ui;
    float f;
    const void *v;
} Arg;

typedef struct {
    unsigned int mod;
    unsigned int button;
    void (*func)(struct ewlc_server *srv, const Arg *);
    const Arg arg;
} Button;

struct ewlc_server {
    struct key_node *key_list;
    struct event_node *event_list;

    struct wl_listener xdg_shell_new_surface_listener;

    struct wl_listener cursor_axis_listener;
    struct wl_listener cursor_button_listener;
    struct wl_listener cursor_frame_listener;
    struct wl_listener cursor_motion_listener;
    struct wl_listener cursor_motion_absolute_listener;

    struct wl_listener seat_request_set_cursor_listener;
    struct wl_listener seat_request_set_selection_listener;
    struct wl_listener seat_request_set_primary_selection_listener;

    struct wl_listener backend_new_input_listener;
    struct wl_listener backend_new_output_listener;


    struct wl_listener xdeco_mgr_new_top_level_decoration_listener;

    struct wl_listener new_xwayland_surface_listener;
    struct wl_listener xwayland_ready_listener;

    Button buttons[3];
};

struct ewlc_decoration {
    struct ewlc_server *server;
    struct wl_listener deco_request_mode_listener;
    struct wl_listener deco_destroy_listener;
};


/* function declarations */
void xdeco_mgr_new_toplevel_decoration_notify(struct wl_listener *listener, void *data);
void deco_destroy_notify(struct wl_listener *listener, void *data);
void deco_request_mode_notify(struct wl_listener *listener, void *data);

int handle_events(emacs_env *env, struct ewlc_server *srv);

void xwayland_ready_notify(struct wl_listener *listener, void *data);
void update_window_type(struct ewlc_client *c);

// TODO: make a variable
#define MODKEY WLR_MODIFIER_ALT

#endif // __SERVER_H_
