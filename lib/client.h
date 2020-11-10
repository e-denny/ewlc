/*
 p See LICENSE file for copyright and license details.
 */
#ifndef __CLIENT_H_
#define __CLIENT_H_

#define _POSIX_C_SOURCE 200809L
#include "ewlc.h"
#include "ewlc-module.h"
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

#ifdef XWAYLAND
enum {
    NetWMWindowTypeDialog,
    NetWMWindowTypeSplash,
    NetWMWindowTypeToolbar,
    NetWMWindowTypeUtility,
    NetLast
}; /* EWMH atoms */

enum { XDG_SHELL, X11_MANAGED, X11_UNMANAGED }; /* client types */
#endif

struct ewlc_output;
struct ewlc_server;

struct ewlc_client {
    struct ewlc_server *server;
    struct ewlc_output *output;
    struct wl_list client_link;
    struct wl_list client_focus_link;
    struct wl_list client_stack_link;
    union {
        struct wlr_xdg_surface *xdg;
#ifdef XWAYLAND
        struct wlr_xwayland_surface *xwayland;
#endif
    } surface;
#ifdef XWAYLAND
    struct wl_listener xwayland_surface_request_activate_listener;
    unsigned int type;
#endif
    struct wl_listener surface_commit_listener;
    struct wl_listener surface_map_listener;
    struct wl_listener surface_unmap_listener;
    struct wl_listener surface_destroy_listener;
    struct wlr_box geom; /* layout-relative, includes border */
    int border_width;
    int is_floating;
    uint32_t resize; /* configure serial of a pending resize */
};

static struct wlr_surface *get_surface(struct ewlc_client *c);
static bool is_visible_on(struct ewlc_client *c, struct ewlc_output *o);
static void apply_bounds(struct ewlc_client *c, struct wlr_box *bbox);
static void xdg_surface_commit_notify(struct wl_listener *listener, void *data);
static void xdg_shell_new_surface_notify(struct wl_listener *listener, void *data);
static void surface_destroy_notify(struct wl_listener *listener, void *data);
static void focus_client(struct ewlc_client *old, struct ewlc_client *c,
                         int lift);
static struct ewlc_client *focus_top(struct ewlc_output *o);
static void surface_map_notify(struct wl_listener *listener, void *data);
static void render_surface(struct wlr_surface *surface, int sx, int sy, void *data);
static void render_clients(struct ewlc_output *o, struct timespec *now);
static void resize(struct ewlc_client *c, int x, int y, int w, int h,
                   int interact);
static struct ewlc_client *get_active_client(void);
static void set_floating(struct ewlc_client *c, int floating);
static void set_output(struct ewlc_client *c, struct ewlc_output *o);
static void surface_unmap_notify(struct wl_listener *listener, void *data);
static struct ewlc_client *get_client_at_point(double x, double y);
static void apply_title(struct ewlc_client *c);

#ifdef XWAYLAND
static void xwayland_surface_request_activate_notify(struct wl_listener *listener,
                                                     void *data);
static void new_xwayland_surface_notify(struct wl_listener *listener,
                                        void *data);
static void render_independents(struct wlr_output *output,
                                struct timespec *now);
static void xwayland_ready_notify(struct wl_listener *listener, void *data);
static struct ewlc_client *get_independent_at_point(double x, double y);
#endif

#endif // __CLIENT_H_
