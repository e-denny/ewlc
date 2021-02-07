/*
 See LICENSE file for copyright and license details.
 */
#define _POSIX_C_SOURCE 200809L
#include <emacs-module.h>
#include "Fwlc.h"
#include <wayland-client.h>
#include <wayland-server-core.h>
#include <wlr/types/wlr_cursor.h>
#include <wlr/types/wlr_output_layout.h>
#include <wlr/types/wlr_pointer.h>
#include <wlr/types/wlr_seat.h>

emacs_value Fwlr_cursor_move(emacs_env *env, ptrdiff_t nargs,
                             emacs_value args[], void *data)
{
    struct wlr_cursor *cursor = env->get_user_ptr(env, args[0]);
    struct wlr_event_pointer_motion *event = env->get_user_ptr(env, args[1]);
    wlr_cursor_move(cursor, event->device, event->delta_x, event->delta_y);
    return Qt;
}

emacs_value Fwlr_cursor_set_surface(emacs_env *env, ptrdiff_t nargs,
                                    emacs_value args[], void *data)
{
    struct wlr_cursor *cursor = env->get_user_ptr(env, args[0]);
    struct wlr_seat *seat = env->get_user_ptr(env, args[1]);
    struct wlr_seat_pointer_request_set_cursor_event *event = env->get_user_ptr(env, args[2]);
    if (event->seat_client == seat->pointer_state.focused_client)
        wlr_cursor_set_surface(cursor, event->surface, event->hotspot_x, event->hotspot_y);
    return Qt;
}
emacs_value Fwlr_cursor_warp_closest(emacs_env *env, ptrdiff_t nargs,
                                      emacs_value args[], void *data)
{
    struct wlr_cursor *cursor = env->get_user_ptr(env, args[0]);
    int x = env->extract_integer(env, args[1]);
    int y = env->extract_integer(env, args[2]);

    wlr_cursor_warp_closest(cursor, NULL, x, y);
    return Qt;
}

emacs_value Fwlr_cursor_warp_absolute(emacs_env *env, ptrdiff_t nargs,
                                      emacs_value args[], void *data)
{
    struct wlr_cursor *cursor = env->get_user_ptr(env, args[0]);
    struct wlr_event_pointer_motion_absolute *event = env->get_user_ptr(env, args[1]);
    wlr_cursor_warp_absolute(cursor, event->device, event->x, event->y);
    return Qt;
}

emacs_value Fwlr_cursor_x(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    struct wlr_cursor *cursor = env->get_user_ptr(env, args[0]);
    // TODO: should this be double or int?
    return env->make_integer(env, cursor->x);
}

emacs_value Fwlr_cursor_y(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    struct wlr_cursor *cursor = env->get_user_ptr(env, args[0]);
    return env->make_integer(env, cursor->y);
}

emacs_value Fwlr_cursor_create(emacs_env *env, ptrdiff_t nargs,
                               emacs_value args[], void *data)
{
    struct wlr_cursor *cursor = wlr_cursor_create();
    return env->make_user_ptr(env, NULL, cursor);
}

emacs_value Fwlr_cursor_attach_output_layout(emacs_env *env, ptrdiff_t nargs,
                                             emacs_value args[], void *data)
{
    struct wlr_cursor *cursor = env->get_user_ptr(env, args[0]);
    struct wlr_output_layout *output_layout = env->get_user_ptr(env, args[1]);
    wlr_cursor_attach_output_layout(cursor, output_layout);
    return Qt;
}

emacs_value Fwlr_cursor_destroy(emacs_env *env, ptrdiff_t nargs,
                                emacs_value args[], void *data)
{
    struct wlr_cursor *cursor = env->get_user_ptr(env, args[0]);
    wlr_cursor_destroy(cursor);
    return Qt;
}


void init_wlr_cursor(emacs_env *env)
{
    emacs_value func;
    func = env->make_function(env, 2, 2, Fwlr_cursor_move, "", NULL);
    bind_function(env, "wlr-cursor-move", func);

    func = env->make_function(env, 3, 3, Fwlr_cursor_set_surface, "", NULL);
    bind_function(env, "wlr-cursor-set-surface", func);

    func = env->make_function(env, 3, 3, Fwlr_cursor_warp_closest, "", NULL);
    bind_function(env, "wlr-cursor-warp-closest, func);

    func = env->make_function(env, 2, 2, Fwlr_cursor_warp_absolute, "", NULL);
    bind_function(env, "wlr-cursor-warp-absolute, func);

    func = env->make_function(env, 1, 1, Fwlr_cursor_x, "", NULL);
    bind_function(env, "wlr-cursor-x, func);

    func = env->make_function(env, 1, 1, Fwlr_cursor_y, "", NULL);
    bind_function(env, "wlr-cursor-y, func);

    func = env->make_function(env, 1, 1, Fwlr_cursor_y, "", NULL);
    bind_function(env, "wlr-cursor-y, func);

    func = env->make_function(env, 0, 0, Fwlr_cursor_create, "", NULL);
    bind_function(env, "wlr-cursor-create, func);

    func = env->make_function(env, 2, 2, Fwlr_cursor_attach_output_layout, "", NULL);
    bind_function(env, "wlr-cursor-attach-output-layout, func);

    func = env->make_function(env, 1, 1, Fwlr_cursor_destroy, "", NULL);
    bind_function(env, "wlr-cursor-destroy, func);
}
