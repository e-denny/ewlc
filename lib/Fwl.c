#define _POSIX_C_SOURCE 200809L
#include <emacs-module.h>
#include "module.h"
#include "Fwlr.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <wayland-client.h>
#include <wayland-server-core.h>

emacs_value Fwl_display_create(emacs_env *env, ptrdiff_t nargs,
                               emacs_value args[], void *data)
{
    struct wl_display *d = wl_display_create();
    return env->make_user_ptr(env, NULL, d);
}

emacs_value Fwl_display_terminate(emacs_env *env, ptrdiff_t nargs,
                                  emacs_value args[], void *data)
{
    struct wl_display *display = env->get_user_ptr(env, args[0]);
    wl_display_terminate(display);
    return Qt;
}

emacs_value Fwl_display_add_socket_auto(emacs_env *env, ptrdiff_t nargs,
                                        emacs_value args[], void *data)
{
    struct wl_display *display = env->get_user_ptr(env, args[0]);
    const char *socket = wl_display_add_socket_auto(display);
    return env->make_string(env, socket, strlen(socket));
}

emacs_value Fwl_display_destroy_clients(emacs_env *env, ptrdiff_t nargs,
                                        emacs_value args[], void *data)
{
    struct wl_display *display = env->get_user_ptr(env, args[0]);
    wl_display_destroy_clients(display);
    return Qt;
}

emacs_value Fwl_display_destroy(emacs_env *env, ptrdiff_t nargs,
                                emacs_value args[], void *data)
{
    struct wl_display *display = env->get_user_ptr(env, args[0]);
    wl_display_destroy(display);
    return Qt;
}
emacs_value Fwl_display_get_event_loop(emacs_env *env, ptrdiff_t nargs,
                                       emacs_value args[], void *data)
{
    struct wl_display *display = env->get_user_ptr(env, args[0]);
    struct wl_event_loop *loop = wl_display_get_event_loop(display);
    return env->make_user_ptr(env, NULL, loop);
}

emacs_value Fwl_display_flush_clients(emacs_env *env, ptrdiff_t nargs,
                                      emacs_value args[], void *data)
{
    struct wl_display *display = env->get_user_ptr(env, args[0]);
    wl_display_flush_clients(display);
    return Qt;
}

emacs_value Fwl_event_loop_dispatch(emacs_env *env, ptrdiff_t nargs,
                                    emacs_value args[], void *data)
{
    struct wl_event_loop *loop = env->get_user_ptr(env, args[0]);
    int timeout = env->extract_integer(env, args[1]);
    if (wl_event_loop_dispatch(loop, timeout) == 0)
        return Qt;
    return Qnil;
}

void init_wl(emacs_env *env)
{
    emacs_value func;
    func = env->make_function(env, 0, 0, Fwl_display_create, "", NULL);
    bind_function(env, "wl-display-create", func);

    func = env->make_function(env, 1, 1, Fwl_display_terminate, "", NULL);
    bind_function(env, "wl-display-terminate", func);

    func = env->make_function(env, 1, 1, Fwl_display_add_socket_auto, "", NULL);
    bind_function(env, "wl-display-add-socket-auto", func);

    func = env->make_function(env, 1, 1, Fwl_display_destroy_clients, "", NULL);
    bind_function(env, "wl-display-destroy-clients", func);

    func = env->make_function(env, 1, 1, Fwl_display_destroy, "", NULL);
    bind_function(env, "wl-display-destroy", func);

    func = env->make_function(env, 1, 1, Fwl_display_get_event_loop, "", NULL);
    bind_function(env, "wl-display-get-event-loop", func);

    func = env->make_function(env, 1, 1, Fwl_display_flush_clients, "", NULL);
    bind_function(env, "wl-display-flush-clients", func);

    func = env->make_function(env, 2, 2, Fwl_event_loop_dispatch, "", NULL);
    bind_function(env, "wl-event-loop-dispatch", func);
}
