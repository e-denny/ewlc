#ifndef __SERVER_H_
#define __SERVER_H_

#define _POSIX_C_SOURCE 200809L
#include <emacs-module.h>
#include <wayland-client.h>
#include <wayland-server-core.h>
#include <wlr/render/wlr_renderer.h>
#include <wlr/types/wlr_input_device.h>
#include <wlr/types/wlr_output.h>
#include <wlr/types/wlr_output_layout.h>

enum {
    NetWMWindowTypeDialog,
    NetWMWindowTypeSplash,
    NetWMWindowTypeToolbar,
    NetWMWindowTypeUtility,
    NetLast
};

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
};

struct ewlc_output {
    struct ewlc_server *server;
    struct wl_listener output_frame_listener;
    struct wl_listener output_destroy_listener;
};

struct ewlc_client {
    struct ewlc_server *server;
    // TODO: does this need to be here ?
    struct ewlc_output *output;
    struct wl_listener xwayland_surface_request_activate_listener;
    struct wl_listener surface_commit_listener;
    struct wl_listener surface_map_listener;
    struct wl_listener surface_unmap_listener;
    struct wl_listener surface_destroy_listener;
};

struct ewlc_output_rule {
    const char *name;
    float master_ratio;
    int num_master;
    float scale;
    enum wl_output_transform rr;
};

struct ewlc_decoration {
    struct ewlc_server *server;
    struct wl_listener deco_request_mode_listener;
    struct wl_listener deco_destroy_listener;
};

struct ewlc_keyboard {
    // TODO: does this need to be here ?
    struct wlr_input_device *device;
    struct ewlc_server *server;

    struct wl_listener keyboard_modifiers_listener;
    struct wl_listener keyboard_key_listener;
    struct wl_listener keyboard_destroy_listener;
};

/* Used to move all of the data necessary to render a surface from the top-level
 * frame handler to the per-surface render function. */
struct render_data {
    struct wlr_output *output;
    struct wlr_output_layout *output_layout;
    struct wlr_renderer *renderer;
    struct timespec when;
    int x, y; /* layout-relative */
};

#define MODKEY WLR_MODIFIER_ALT

#endif // __SERVER_H_
