/*
 p See LICENSE file for copyright and license details.
 */
#ifndef __POINTER_H_
#define __POINTER_H_

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


/* enums */
enum { CUR_NORMAL, CUR_MOVE, CUR_RESIZE }; /* cursor */

typedef union {
    int i;
    unsigned int ui;
    float f;
    const void *v;
} Arg;

typedef struct {
    unsigned int mod;
    unsigned int button;
    void (*func)(const Arg *);
    const Arg arg;
} Button;


static const Button buttons[] = {
    {MODKEY, BTN_LEFT, move_resize, {.ui = CUR_MOVE}},
    /* TODO: Fix this */
    {MODKEY, BTN_MIDDLE, ewlc_toggle_floating, {0}},
    {MODKEY, BTN_RIGHT, move_resize, {.ui = CUR_RESIZE}},
};

/* function declarations */
static void create_pointer(struct ewlc_server *s, struct wlr_input_device *device);
static void motion_notify(struct ewlc_server *s, uint32_t time);
static void move_resize(const Arg *arg);
static void pointer_focus(struct ewlc_client *c, struct wlr_surface *surface,
                          double sx, double sy, uint32_t time);
static void resize(struct ewlc_client *c, int x, int y, int w, int h,
                   int interact);
static void scale_box(struct wlr_box *box, float scale);

static void cursor_axis_notify(struct wl_listener *listener, void *data);
static void cursor_button_notify(struct wl_listener *listener, void *data);
static void cursor_frame_notify(struct wl_listener *listener, void *data);
static void cursor_motion_notify(struct wl_listener *listener, void *data);
static void cursor_motion_absolute_notify(struct wl_listener *listener,
                                          void *data);

static void seat_request_set_cursor_notify(struct wl_listener *listener,
                                       void *data);
static void seat_request_set_selection_notify(struct wl_listener *listener,
                                              void *data);
static void
seat_request_set_primary_selection_notify(struct wl_listener *listener,
                                          void *data);

#endif // __POINTER_H_
