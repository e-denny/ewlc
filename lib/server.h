/*
 p See LICENSE file for copyright and license details.
 */


#ifndef __SERVER_H_
#define __SERVER_H_

#define _POSIX_C_SOURCE 200809L
#ifndef XWAYLAND
#define XWAYLAND
#endif
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

#ifdef XWAYLAND
#include <X11/Xlib.h>
#include <wlr/xwayland.h>

enum {
    NetWMWindowTypeDialog,
    NetWMWindowTypeSplash,
    NetWMWindowTypeToolbar,
    NetWMWindowTypeUtility,
    NetLast
};

#endif

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
    struct wl_display *display;
    struct wlr_renderer *renderer;
    struct wlr_compositor *compositor;
    emacs_env *e_env;

    struct wlr_xdg_shell *xdg_shell;
    struct wl_listener xdg_shell_new_surface_listener;

    struct wlr_cursor *cursor;
    struct wlr_xcursor_manager *cursor_mgr;
    struct wl_listener cursor_axis_listener;
    struct wl_listener cursor_button_listener;
    struct wl_listener cursor_frame_listener;
    struct wl_listener cursor_motion_listener;
    struct wl_listener cursor_motion_absolute_listener;

    struct wlr_seat *seat;
    struct wl_listener seat_request_set_cursor_listener;
    struct wl_listener seat_request_set_selection_listener;
    struct wl_listener seat_request_set_primary_selection_listener;

    struct wlr_backend *backend;
    struct wl_listener backend_new_input_listener;

    struct wl_list keyboard_list;
    struct key_node *key_list;

    struct event_node *event_list;

    struct wl_list output_list;
    struct wlr_output_layout *output_layout;
    struct ewlc_output *active_output;
    struct wl_listener backend_new_output_listener;
    struct wlr_box output_geom;

    unsigned int cursor_mode;

    struct ewlc_client *grabbed_client;
    int grabc_x, grabc_y; /* client-relative */

    struct wlr_xdg_decoration_manager_v1 *xdeco_mgr;
    struct wl_listener xdeco_mgr_new_top_level_decoration_listener;

#ifdef XWAYLAND
    struct wlr_xwayland *xwayland;
    struct wl_listener new_xwayland_surface_listener;
    struct wl_listener xwayland_ready_listener;
    Atom netatom[NetLast];
#endif

    struct wl_list client_list;     /* tiling order */
    struct wl_list client_focus_list; /* focus order */
    struct wl_list independent_list;
    struct wl_list client_stack_list; /* stacking z-order */

    pid_t startup_pid;

    Button buttons[3];
    char broken[7];
    int sloppyfocus;        /* focus follows mouse */
    unsigned int border_px; /* border pixel of windows */
    float root_color[4];
    float border_color[4];
    float focus_color[4];
    int repeat_rate;
    int repeat_delay;
};

struct ewlc_decoration {
    struct ewlc_server *server;
    struct wl_listener deco_request_mode_listener;
    struct wl_listener deco_destroy_listener;
};


/* function declarations */
void xdeco_mgr_new_toplevel_decoration_notify(struct wl_listener *listener,
                                    void *data);
void deco_destroy_notify(struct wl_listener *listener, void *data);
void deco_request_mode_notify(struct wl_listener *listener, void *data);
void sigchld(int unused);
void ewlc_setup(struct ewlc_server *srv);
struct ewlc_server *ewlc_start(emacs_env *env);
int ewlc_cleanup(struct ewlc_server *srv);
int ewlc_display_dispatch(struct ewlc_server *srv);
int handle_events(emacs_env *env, struct ewlc_server *srv);

#ifdef XWAYLAND
void xwayland_ready_notify(struct wl_listener *listener, void *data);
void update_window_type(struct ewlc_client *c);
#endif

// TODO: make a variable
#define MODKEY WLR_MODIFIER_ALT

#endif // __SERVER_H_
