/*
 See LICENSE file for copyright and license details.
 */

#ifnef __OUTPUT_H_
#define __OUTPUT_H_

/* #include <linux/input-event-codes.h> */
/* #include <signal.h> */
/* #include <stdbool.h> */
/* #include <stdio.h> */
/* #include <stdlib.h> */
/* #include <string.h> */
/* #include <sys/wait.h> */
/* #include <time.h> */
/* #include <unistd.h> */
#include <wayland-client.h>
#include <wayland-server-core.h>
/* #include <wlr/backend.h> */
/* #include <wlr/render/wlr_renderer.h> */
/* #include <wlr/types/wlr_compositor.h> */
/* #include <wlr/types/wlr_cursor.h> */
/* #include <wlr/types/wlr_data_device.h> */
/* #include <wlr/types/wlr_export_dmabuf_v1.h> */
/* #include <wlr/types/wlr_gamma_control_v1.h> */
/* #include <wlr/types/wlr_input_device.h> */
/* #include <wlr/types/wlr_keyboard.h> */
/* #include <wlr/types/wlr_matrix.h> */
#include <wlr/types/wlr_output.h>
#include <wlr/types/wlr_output_layout.h>
/* #include <wlr/types/wlr_pointer.h> */
/* #include <wlr/types/wlr_primary_selection.h> */
/* #include <wlr/types/wlr_primary_selection_v1.h> */
/* #include <wlr/types/wlr_screencopy_v1.h> */
/* #include <wlr/types/wlr_seat.h> */
/* #include <wlr/types/wlr_viewporter.h> */
/* #include <wlr/types/wlr_xcursor_manager.h> */
/* #include <wlr/types/wlr_xdg_decoration_v1.h> */
/* #include <wlr/types/wlr_xdg_output_v1.h> */
/* #include <wlr/types/wlr_xdg_shell.h> */
/* #include <wlr/util/log.h> */

struct ewlc_server;

/*  structs  */
struct ewlc_output {
    struct wlr_output *wlr_output;
    struct ewlc_server *server;
    struct wl_list output_link;
    struct wl_listener output_frame_listener;
    struct wl_listener output_destroy_listener;
    struct wlr_box m; /* output area */
    struct wlr_box w; /* window area */
    double master_ratio;
    int num_master;
};

struct ewlc_output_rule {
    const char *name;
    float master_ratio;
    int num_master;
    float scale;
    enum wl_output_transform rr;
};

/* function declarations */
static void arrange(struct ewlc_output *o);
static void output_destroy_notify(struct wl_listener *listener, void *data);
static struct ewlc_output *get_next_output(int direction, ewlc_server *s);
static void output_frame_notify(struct wl_listener *listener, void *data);
static void tile(struct ewlc_output *o);
struct ewlc_output *get_output_at_point(struct ewlc_server *s, double x, double y);

/* GLOBAL VARIABLES */
struct ewlc_output *active_output;

const struct ewlc_output_rule output_rules[] = {
    /* name       master_ratio num_master scale  rotate/reflect */
    /* example of a HiDPI laptop monitor:
        { "eDP-1",    0.5,  1,      2,    WL_OUTPUT_TRANSFORM_NORMAL },
        */
    /* defaults */
    {NULL, 0.5, 1, 1, WL_OUTPUT_TRANSFORM_NORMAL},
};

#endif // __OUTPUT_H_
