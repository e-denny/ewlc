/*
 p See LICENSE file for copyright and license details.
 */
#ifndef __POINTER_H_
#define __POINTER_H_

#define _POSIX_C_SOURCE 200809L
#include "server.h"
#include "client.h"
#include "commands.h"
#include <wayland-client.h>
#include <wayland-server-core.h>
#include <wlr/types/wlr_input_device.h>
#include <wlr/types/wlr_surface.h>

/* enums */
enum { CUR_NORMAL, CUR_MOVE, CUR_RESIZE }; /* cursor */

/* function declarations */
void create_pointer(struct ewlc_server *s, struct wlr_input_device *device);
void motion_notify(struct ewlc_server *s, uint32_t time);
void move_resize(struct ewlc_server *s, const Arg *arg);
void pointer_focus(struct ewlc_client *c, struct wlr_surface *surface,
                   double sx, double sy, uint32_t time);
void resize(struct ewlc_client *c, int x, int y, int w, int h, int interact);

void cursor_axis_notify(struct wl_listener *listener, void *data);
void cursor_button_notify(struct wl_listener *listener, void *data);
void cursor_frame_notify(struct wl_listener *listener, void *data);
void cursor_motion_notify(struct wl_listener *listener, void *data);
void cursor_motion_absolute_notify(struct wl_listener *listener, void *data);
void seat_request_set_cursor_notify(struct wl_listener *listener, void *data);
void seat_request_set_selection_notify(struct wl_listener *listener, void *data);
void seat_request_set_primary_selection_notify(struct wl_listener *listener, void *data);


void cursor_axis_handler(struct wl_listener *listener, void *data);
void cursor_button_handler(struct wl_listener *listener, void *data);
void cursor_frame_handler(struct wl_listener *listener, void *data);
void cursor_motion_handler(struct wl_listener *listener, void *data);
void cursor_motion_absolute_handler(struct wl_listener *listener, void *data);
void seat_request_set_cursor_handler(struct wl_listener *listener, void *data);
void seat_request_set_selection_handler(struct wl_listener *listener, void *data);
void seat_request_set_primary_selection_handler(struct wl_listener *listener, void *data);

#endif // __POINTER_H_
