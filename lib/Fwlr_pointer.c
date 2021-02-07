#define _POSIX_C_SOURCE 200809L
#include <emacs-module.h>
#include "Fwlr.h"
#include <wayland-client.h>
#include <wayland-server-core.h>
#include <wlr/types/wlr_pointer.h>

emacs_value Fwlr_get_button_press_state(emacs_env *env, ptrdiff_t nargs,
                                        emacs_value args[], void *data)
{
    struct wlr_event_pointer_button *event = env->get_user_ptr(env, args[0]);

    if (event->state == WLR_BUTTON_PRESSED) {
        return env->intern(env, "pressed");
    } else if (event->state == WLR_BUTTON_RELEASED) {
        return env->intern(env, "released");
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

void init_wlr_pointer(emacs_env *env)
{
    emacs_value func;
    func = env->make_function(env, 1, 1, Fwlr_get_button_press_state, "", NULL);
    bind_function(env, "wlr-get-button-press-state", func);

    func = env->make_function(env, 1, 1, Fwlr_event_pointer_motion_time_msec, "", NULL);
    bind_function(env, "wlr-event-pointer-motion-time-msec", func);

    func = env->make_function(env, 1, 1, Fwlr_event_pointer_motion_absolute_time_msec, "", NULL);
    bind_function(env, "wlr-event-pointer-motion-absolute-time-msec", func);
}
