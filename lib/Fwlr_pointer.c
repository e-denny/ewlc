#define _POSIX_C_SOURCE 200809L
#include <emacs-module.h>
#include "module.h"
#include "Fwlr.h"
#include <wayland-client.h>
#include <wayland-server-core.h>
#include <wlr/types/wlr_pointer.h>

emacs_value Fwlr_get_button_press_state(emacs_env *env, ptrdiff_t nargs,
                                        emacs_value args[], void *data)
{
    struct wlr_event_pointer_button *event = env->get_user_ptr(env, args[0]);

    if (event->state == WLR_BUTTON_PRESSED) {
        return env->intern(env, "wlr-button-pressed");
    } else if (event->state == WLR_BUTTON_RELEASED) {
        return env->intern(env, "wlr-button-released");
    }
    return Qnil;
}

emacs_value Fwlr_event_pointer_motion_time_msec(emacs_env *env, ptrdiff_t nargs,
                                                emacs_value args[], void *data)
{
    struct wlr_event_pointer_motion *event = env->get_user_ptr(env, args[0]);
    return env->make_integer(env, event->time_msec);
}

emacs_value Fwlr_event_pointer_motion_absolute_time_msec(emacs_env *env, ptrdiff_t nargs,
                                                         emacs_value args[], void *data)
{
    struct wlr_event_pointer_motion_absolute *event = env->get_user_ptr(env, args[0]);
    return env->make_integer(env, event->time_msec);
}


emacs_value Fwlr_event_pointer_get_button(emacs_env *env, ptrdiff_t nargs,
                                          emacs_value args[], void *data)
{
    struct wlr_event_pointer_button *event = env->get_user_ptr(env, args[0]);
    // TODO: is this right?
    if (event->button == 0)
        return env->intern(env, "wlr-button-left");
    if (event->button == 1)
        return env->intern(env, "wlr-button-middle");
    if (event->button == 2)
        return env->intern(env, "wlr-button-right");
    return Qnil;
}


void init_wlr_pointer(emacs_env *env)
{
    emacs_value func;
    func = env->make_function(env, 1, 1, Fwlr_get_button_press_state, "", NULL);
    bind_function(env, "wlr-get-button-press-state", func);

    func = env->make_function(env, 1, 1, Fwlr_event_pointer_motion_time_msec, "", NULL);
    bind_function(env, "wlr-event-pointer-motion-time-msec", func);

    func = env->make_function(env, 1, 1, Fwlr_event_pointer_get_button, "", NULL);
    bind_function(env, "wlr-event-pointer-motion-time-msec", func);

    func = env->make_function(env, 1, 1, Fwlr_event_pointer_motion_absolute_time_msec, "", NULL);
    bind_function(env, "wlr-event-pointer-motion-absolute-time-msec", func);
}
