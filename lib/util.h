#ifndef __UTIL_H_
#define __UTIL_H_

#include <stdio.h>
#include <stdlib.h>
#include <wlr/types/wlr_keyboard.h>
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

#define INFO(msg) \
    fprintf(stderr, "  info:  %s:%d : %s : ", __FILE__, __LINE__, __func__); \
    fprintf(stderr, "%s\n", msg);

#define DEBUG(fmt, ...) \
    fprintf(stderr, "  debug: %s:%d: %s : ", __FILE__, __LINE__, __func__); \
    fprintf(stderr, fmt , __VA_ARGS__); \
    fprintf(stderr, "\n");

enum ewlc_events {
    EWLC_CURSOR_AXIS,
    EWLC_CURSOR_BUTTON,
    EWLC_CURSOR_MOTION,
    EWLC_CURSOR_FRAME,
    EWLC_CURSOR_MOTION_ABSOLUTE,
    EWLC_SEAT_REQUEST_SET_CURSOR,
    EWLC_SEAT_REQUEST_SET_PRIMARY_SELECTION,
    EWLC_SEAT_REQUEST_SET_SELECTION,
    EWLC_KEYBOARD_DESTROY,
    EWLC_BACKEND_NEW_INPUT,
    EWLC_KEYBOARD_KEY,
    EWLC_KEYBOARD_MODIFIERS,
    EWLC_XWAYLAND_READY,
    EWLC_DECO_REQUEST_MODE,
    EWLC_DECO_DESTROY,
    EWLC_NEW_TOPLEVEL_DECORATION,
    EWLC_OUTPUT_DESTROY,
    EWLC_OUTPUT_FRAME,
    EWLC_BACKEND_NEW_OUTPUT,
    EWLC_XDG_SURFACE_COMMIT,
    EWLC_XDG_SHELL_NEW_SURFACE,
    EWLC_SURFACE_DESTROY,
    EWLC_SURFACE_MAP,
    EWLC_SURFACE_UNMAP,
    EWLC_X_SURFACE_REQUEST_ACTIVATE,
    EWLC_NEW_X_SURFACE
};

struct ewlc_keyboard;

struct key_node {
    uint32_t mods;
    xkb_keysym_t sym;
    struct ewlc_keyboard *kb;
    struct wlr_event_keyboard_key *event;
    struct key_node *next;
};

struct event_node {
    struct wl_listener *listener;
    void *data;
    int type;
    struct event_node *next;
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

struct event_node *create_event(struct wl_listener *listener, void *data, int type);
struct event_node *add_event(struct event_node *list, struct event_node *new_node);
struct event_node *remove_event(struct event_node *list);

#endif // __UTIL_H_
