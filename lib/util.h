#ifndef __UTIL_H_
#define __UTIL_H_

/* #include "ewlc.h" */
/* #include "ewlc-module.h" */
/* #include <emacs-module.h> */
/* #include <getopt.h> */
/* #include <linux/input-event-codes.h> */
/* #include <signal.h> */
/* #include <stdbool.h> */
#include <stdio.h>
#include <stdlib.h>
/* #include <string.h> */
/* #include <sys/wait.h> */
/* #include <time.h> */
/* #include <unistd.h> */
/* #include <wayland-client.h> */
/* #include <wayland-server-core.h> */
/* #include <wlr/backend.h> */
/* #include <wlr/render/wlr_renderer.h> */
/* #include <wlr/types/wlr_compositor.h> */
/* #include <wlr/types/wlr_cursor.h> */
/* #include <wlr/types/wlr_data_device.h> */
/* #include <wlr/types/wlr_export_dmabuf_v1.h> */
/* #include <wlr/types/wlr_gamma_control_v1.h> */
/* #include <wlr/types/wlr_input_device.h> */
#include <wlr/types/wlr_keyboard.h>
/* #include <wlr/types/wlr_matrix.h> */
/* #include <wlr/types/wlr_output.h> */
/* #include <wlr/types/wlr_output_layout.h> */
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
#include <xkbcommon/xkbcommon.h>

/* macros */
#define ERROR(fmt, ...)                                                        \
    do {                                                                       \
        fprintf(stderr, fmt "\n", ##__VA_ARGS__);                              \
        exit(EXIT_FAILURE);                                                    \
    } while (0)
#define EERROR(fmt, ...) ERROR(fmt ": %s", ##__VA_ARGS__, strerror(errno))
#define MAX(A, B) ((A) > (B) ? (A) : (B))
#define MIN(A, B) ((A) < (B) ? (A) : (B))
#define CLEANMASK(mask) (mask & ~WLR_MODIFIER_CAPS)
#define LENGTH(X) (sizeof X / sizeof X[0])
#define END(A) ((A) + LENGTH(A))

struct ewlc_keyboard;

struct key_node {
    unit32_t mods;
    xkb_keysym_t sym;
    ewlc_keyboard *kb;
    wlr_event_keyboard_key *event;
    key_node *next;
};

struct key_node *create_node(uint32_t mods,
                             xkb_keysym_t sym,
                             struct ewlc_keyboard *kb,
                             struct wlr_event_keyboard_key *event);
struct key_node *add_to_end(struct key_node *list,
                            uint32_t mods,
                            xkb_keysym_t sym,
                            struct ewlc_keyboard *kb,
                            struct wlr_event_keyboard_key *event);
struct key_node *remove_from_start(struct key_node *list);

// Global value
struct key_node *key_list = NULL;

#endif // __UTIL_H_
