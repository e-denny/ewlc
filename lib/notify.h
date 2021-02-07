#ifndef __NOTIFY_H_
#define __NOTIFY_H_

#define _POSIX_C_SOURCE 200809L
#include <emacs-module.h>
#include <wayland-client.h>
#include <wayland-server-core.h>

void cursor_axis_notify(struct wl_listener *listener, void *data);
void cursor_button_notify(struct wl_listener *listener, void *data);
void cursor_frame_notify(struct wl_listener *listener, void *data);
void cursor_motion_notify(struct wl_listener *listener, void *data);
void cursor_motion_absolute_notify(struct wl_listener *listener, void *data);

void seat_request_set_cursor_notify(struct wl_listener *listener, void *data);
void seat_request_set_selection_notify(struct wl_listener *listener, void *data);
void seat_request_set_primary_selection_notify(struct wl_listener *listener, void *data);

void output_destroy_notify(struct wl_listener *listener, void *data);
void output_frame_notify(struct wl_listener *listener, void *data);
void backend_new_output_notify(struct wl_listener *listener, void *data);

void xdeco_mgr_new_toplevel_decoration_notify(struct wl_listener *listener, void *data);
void deco_destroy_notify(struct wl_listener *listener, void *data);
void deco_request_mode_notify(struct wl_listener *listener, void *data);

void xwayland_ready_notify(struct wl_listener *listener, void *data);

void xdg_surface_commit_notify(struct wl_listener *listener, void *data);
void xdg_shell_new_surface_notify(struct wl_listener *listener, void *data);
void surface_destroy_notify(struct wl_listener *listener, void *data);
void surface_map_notify(struct wl_listener *listener, void *data);
void surface_unmap_notify(struct wl_listener *listener, void *data);

void xwayland_surface_request_activate_notify(struct wl_listener *listener, void *data);
void new_xwayland_surface_notify(struct wl_listener *listener, void *data);

void keyboard_destroy_notify(struct wl_listener *listener, void *data);
void keyboard_key_notify(struct wl_listener *listener, void *data);
void keyboard_modifiers_notify(struct wl_listener *listener, void *data);
void backend_new_input_notify(struct wl_listener *listener, void *data);

#endif // __NOTIFY_H_
