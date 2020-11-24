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

struct ewlc_keyboard;

struct key_node {
    uint32_t mods;
    xkb_keysym_t sym;
    struct ewlc_keyboard *kb;
    struct wlr_event_keyboard_key *event;
    struct key_node *next;
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

#endif // __UTIL_H_
