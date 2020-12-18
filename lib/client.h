/*
 p See LICENSE file for copyright and license details.
 */
#ifndef __CLIENT_H_
#define __CLIENT_H_

#define _POSIX_C_SOURCE 200809L
#include "server.h"
#include "client.h"
#include "module.h"
#include <emacs-module.h>
#include <time.h>
#include <wayland-client.h>
#include <wayland-server-core.h>
#include <wlr/types/wlr_output.h>
#include <wlr/types/wlr_output_layout.h>
#include <xkbcommon/xkbcommon.h>
#include <wlr/types/wlr_surface.h>

#ifdef XWAYLAND
#include <X11/Xlib.h>
#include <wlr/xwayland.h>
#endif

#ifdef XWAYLAND
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

/* Used to move all of the data necessary to render a surface from the top-level
 * frame handler to the per-surface render function. */
struct render_data {
    struct wlr_output *output;
    struct ewlc_client *client;
    struct timespec *when;
    int x, y; /* layout-relative */
};

struct wlr_surface *get_surface(struct ewlc_client *c);
bool is_visible_on(struct ewlc_client *c, struct ewlc_output *o);
void apply_bounds(struct ewlc_client *c, struct wlr_box *bbox);
void xdg_surface_commit_notify(struct wl_listener *listener, void *data);
void xdg_shell_new_surface_notify(struct wl_listener *listener, void *data);
void surface_destroy_notify(struct wl_listener *listener, void *data);
void focus_client(struct ewlc_client *old, struct ewlc_client *c, int lift);
void e_focus_client(struct ewlc_client *old, struct ewlc_client *c);
struct ewlc_client *focus_top(struct ewlc_output *o);
void surface_map_notify(struct wl_listener *listener, void *data);
void render_surface(struct wlr_surface *surface, int sx, int sy, void *data);
void render_clients(struct ewlc_output *o, struct timespec *now);
void resize(struct ewlc_client *c, int x, int y, int w, int h, int interact);
struct ewlc_client *get_active_client(struct ewlc_server *s);
void set_floating(struct ewlc_client *c, int floating);
void set_output(struct ewlc_client *c, struct ewlc_output *o);
void surface_unmap_notify(struct wl_listener *listener, void *data);
struct ewlc_client *get_client_at_point(struct ewlc_server *s, double x, double y);
void apply_title(struct ewlc_client *c, struct ewlc_output *active_output);
void scale_box(struct wlr_box *box, float scale);

#ifdef XWAYLAND
void xwayland_surface_request_activate_notify(struct wl_listener *listener,
                                              void *data);
void new_xwayland_surface_notify(struct wl_listener *listener, void *data);
void render_independents(struct ewlc_server *s, struct wlr_output *output,
                         struct timespec *now);
struct ewlc_client *get_independent_at_point(struct ewlc_server *s,double x, double y);
Atom get_atom(xcb_connection_t *xc, const char *name);
#endif

#endif // __CLIENT_H_
