/*
 See LICENSE file for copyright and license details.
 */

#ifndef __OUTPUT_H_
#define __OUTPUT_H_

#include <wayland-client.h>
#include <wayland-server-core.h>
#include <wlr/types/wlr_output.h>
#include <wlr/types/wlr_output_layout.h>

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
void arrange(struct ewlc_output *o);
void output_destroy_notify(struct wl_listener *listener, void *data);
struct ewlc_output *set_next_output(int direction, struct ewlc_server *s);
void output_frame_notify(struct wl_listener *listener, void *data);
void backend_new_output_notify(struct wl_listener *listener, void *data);
void tile(struct ewlc_output *o);
struct ewlc_output *get_output_at_point(struct ewlc_server *s, double x, double y);

#endif // __OUTPUT_H_
