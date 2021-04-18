#define _POSIX_C_SOURCE 200809L
#include <emacs-module.h>
#include "module.h"
#include "Fwlr.h"
#include <string.h>
#include <stdlib.h>
#include <wayland-client.h>
#include <wayland-server-core.h>
#include <wlr/types/wlr_cursor.h>
#include <wlr/types/wlr_xcursor_manager.h>

emacs_value Fwlr_xcursor_manager_set_cursor_image(emacs_env *env, ptrdiff_t nargs,
                                                  emacs_value args[], void *data)
{
    char* image_text;
    ptrdiff_t len = 0;
    struct wlr_xcursor_manager *cursor_mgr = env->get_user_ptr(env, args[0]);
    struct wlr_cursor *cursor = env->get_user_ptr(env, args[2]);

    env->copy_string_contents(env, args[1], NULL, &len);
    image_text = malloc(sizeof(char) * len);
    env->copy_string_contents(env, args[1], image_text, &len);

    wlr_xcursor_manager_set_cursor_image(cursor_mgr, image_text, cursor);
    free(image_text);
    return Qt;
}

emacs_value Fwlr_xcursor_manager_create(emacs_env *env, ptrdiff_t nargs,
                                        emacs_value args[], void *data)
{
    /* don't bother with cursor theme - so 'name' not assignable at the moment. */
    char *name = NULL;
    int size = env->extract_integer(env, args[0]);
    struct wlr_xcursor_manager *cursor_mgr = wlr_xcursor_manager_create(name, size);
    return env->make_user_ptr(env, NULL, cursor_mgr);
}

emacs_value Fwlr_xcursor_manager_destroy(emacs_env *env, ptrdiff_t nargs,
                                         emacs_value args[], void *data)
{
    struct wlr_xcursor_manager *manager = env->get_user_ptr(env, args[0]);
    wlr_xcursor_manager_destroy(manager);
    return Qt;
}

emacs_value Fwlr_xcursor_manager_load(emacs_env *env, ptrdiff_t nargs,
                                      emacs_value args[], void *data)
{
    struct wlr_xcursor_manager *cursor_mgr = env->get_user_ptr(env, args[0]);
    float scale = env->extract_float(env, args[1]);
    if (wlr_xcursor_manager_load(cursor_mgr, scale))
        return Qt;
    return Qnil;
}

void init_wlr_xcursor_manager(emacs_env *env)
{
    emacs_value func;
    func = env->make_function(env, 3, 3, Fwlr_xcursor_manager_set_cursor_image, "", NULL);
    bind_function(env, "wlr-xcursor-manager-set-cursor-image", func);

    func = env->make_function(env, 1, 1, Fwlr_xcursor_manager_create, "", NULL);
    bind_function(env, "wlr-xcursor-manager-create", func);

    func = env->make_function(env, 1, 1, Fwlr_xcursor_manager_destroy, "", NULL);
    bind_function(env, "wlr-xcursor-manager-destroy", func);

    func = env->make_function(env, 2, 2, Fwlr_xcursor_manager_load, "", NULL);
    bind_function(env, "wlr-xcursor-manager-load", func);
}
