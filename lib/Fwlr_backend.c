#define _POSIX_C_SOURCE 200809L
#include <emacs-module.h>
#include "module.h"
#include "Fwlr.h"
#include <wayland-client.h>
#include <wayland-server-core.h>
#include <wlr/backend.h>
#include <wlr/render/wlr_renderer.h>


emacs_value Fwlr_backend_autocreate(emacs_env *env, ptrdiff_t nargs,
                                    emacs_value args[], void *data)
{
    struct wl_display *display = env->get_user_ptr(env, args[0]);
    struct wlr_backend *backend = wlr_backend_autocreate(display, NULL);
    if (backend)
        return env->make_user_ptr(env, NULL, backend);
    return Qnil;
}

emacs_value Fwlr_backend_get_renderer(emacs_env *env, ptrdiff_t nargs,
                                      emacs_value args[], void *data)
{
    struct wlr_backend *backend = env->get_user_ptr(env, args[0]);
    struct wlr_renderer *renderer = wlr_backend_get_renderer(backend);
    return env->make_user_ptr(env, NULL, renderer);
}

emacs_value Fwlr_backend_start(emacs_env *env, ptrdiff_t nargs,
                               emacs_value args[], void *data)
{
    struct wlr_backend *backend = env->get_user_ptr(env, args[0]);
    if (wlr_backend_start(backend))
        return Qt;
    return Qnil;
}

void init_wlr_backend(emacs_env *env)
{
    emacs_value func;
    func = env->make_function(env, 1, 1, Fwlr_backend_autocreate, "", NULL);
    bind_function(env, "wlr-backend-autocreate", func);

    func = env->make_function(env, 1, 1, Fwlr_backend_get_renderer, "", NULL);
    bind_function(env, "wlr-backend-get-renderer", func);

    func = env->make_function(env, 1, 1, Fwlr_backend_start, "", NULL);
    bind_function(env, "wlr-backend-start", func);
}
