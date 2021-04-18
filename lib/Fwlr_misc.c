#define _POSIX_C_SOURCE 200809L
#include <emacs-module.h>
#include "module.h"
#include "Fwlr.h"
#include "server.h"
#include <stdlib.h>
#include <wayland-client.h>
#include <wayland-server-core.h>
#include <wlr/render/wlr_renderer.h>
#include <wlr/types/wlr_compositor.h>
#include <wlr/types/wlr_data_device.h>
#include <wlr/types/wlr_export_dmabuf_v1.h>
#include <wlr/types/wlr_gamma_control_v1.h>
#include <wlr/types/wlr_output_layout.h>
#include <wlr/types/wlr_primary_selection.h>
#include <wlr/types/wlr_primary_selection_v1.h>
#include <wlr/types/wlr_screencopy_v1.h>
#include <wlr/types/wlr_viewporter.h>
#include <wlr/types/wlr_xdg_decoration_v1.h>
#include <wlr/types/wlr_xdg_output_v1.h>
#include <wlr/types/wlr_xdg_shell.h>
#include <wlr/xwayland.h>


emacs_value Fwlr_compositor_create(emacs_env *env, ptrdiff_t nargs,
                                   emacs_value args[], void *data)
{
    struct wl_display *display = env->get_user_ptr(env, args[0]);
    struct wlr_renderer *renderer = env->get_user_ptr(env, args[1]);
    struct wlr_compositor *compositor = wlr_compositor_create(display, renderer);
    return env->make_user_ptr(env, NULL, compositor);
}

emacs_value Fwlr_xdg_decoration_manager_v1_create(emacs_env *env, ptrdiff_t nargs,
                                                  emacs_value args[], void *data)
{
    struct wl_display *display = env->get_user_ptr(env, args[0]);
    struct wlr_xdg_decoration_manager_v1 *xdg_deco_mgr = wlr_xdg_decoration_manager_v1_create(display);
    return env->make_user_ptr(env, NULL, xdg_deco_mgr);
}

emacs_value Fwlr_export_dmabuf_manager_v1_create(emacs_env *env, ptrdiff_t nargs,
                                                 emacs_value args[], void *data)
{
    struct wl_display *display = env->get_user_ptr(env, args[0]);
    struct wlr_export_dmabuf_manager_v1 *mgr = wlr_export_dmabuf_manager_v1_create(display);
    return env->make_user_ptr(env, NULL, mgr);
}

emacs_value Fwlr_screencopy_manager_v1_create(emacs_env *env, ptrdiff_t nargs,
                                              emacs_value args[], void *data)
{
    struct wl_display *display = env->get_user_ptr(env, args[0]);
    struct wlr_screencopy_manager_v1 *mgr = wlr_screencopy_manager_v1_create(display);
    return env->make_user_ptr(env, NULL, mgr);
}

emacs_value Fwlr_data_device_manager_create(emacs_env *env, ptrdiff_t nargs,
                                            emacs_value args[], void *data)
{
    struct wl_display *display = env->get_user_ptr(env, args[0]);
    struct wlr_data_device_manager *mgr = wlr_data_device_manager_create(display);
    return env->make_user_ptr(env, NULL, mgr);
}

emacs_value Fwlr_gamma_control_manager_v1_create(emacs_env *env, ptrdiff_t nargs,
                                                 emacs_value args[], void *data)
{
    struct wl_display *display = env->get_user_ptr(env, args[0]);
    struct wlr_gamma_control_manager_v1 *mgr = wlr_gamma_control_manager_v1_create(display);
    return env->make_user_ptr(env, NULL, mgr);
}

emacs_value Fwlr_primary_selection_v1_device_manager_create(emacs_env *env, ptrdiff_t nargs,
                                                            emacs_value args[], void *data)
{
    struct wl_display *display = env->get_user_ptr(env, args[0]);
    struct wlr_primary_selection_v1_device_manager *mgr =
        wlr_primary_selection_v1_device_manager_create(display);
    return env->make_user_ptr(env, NULL, mgr);
}

emacs_value Fwlr_viewporter_create(emacs_env *env, ptrdiff_t nargs,
                                   emacs_value args[], void *data)
{
    struct wl_display *display = env->get_user_ptr(env, args[0]);
    struct wlr_viewporter *viewporter = wlr_viewporter_create(display);
    return env->make_user_ptr(env, NULL, viewporter);
}

emacs_value Fwlr_xdg_output_manager_v1_create(emacs_env *env, ptrdiff_t nargs,
                                              emacs_value args[], void *data)
{
    struct wl_display *display = env->get_user_ptr(env, args[0]);
    struct wlr_output_layout *output_layout = env->get_user_ptr(env, args[1]);
    struct wlr_xdg_output_manager_v1 *mgr = wlr_xdg_output_manager_v1_create(display, output_layout);
    return env->make_user_ptr(env, NULL, mgr);
}

// FIXME: this is not exposed to emacs
void deco_destroy_handler(struct wl_listener *listener, void *data)
{
    struct wlr_xdg_toplevel_decoration_v1 *wlr_deco = data;
    struct ewlc_decoration *d = wlr_deco->data;

    wl_list_remove(&d->deco_destroy_listener.link);
    wl_list_remove(&d->deco_request_mode_listener.link);
    free(d);
}

void init_wlr_misc(emacs_env *env)
{
    emacs_value func;

    func = env->make_function(env, 2, 2, Fwlr_compositor_create, "", NULL);
    bind_function(env, "wlr-compositor-create", func);

    func = env->make_function(env, 1, 1, Fwlr_xdg_decoration_manager_v1_create, "", NULL);
    bind_function(env, "wlr-xdg-decoration-manager-v1-create", func);

    func = env->make_function(env, 1, 1, Fwlr_export_dmabuf_manager_v1_create, "", NULL);
    bind_function(env, "wlr-export-dmabuf-manager-v1-create", func);

    func = env->make_function(env, 1, 1, Fwlr_screencopy_manager_v1_create, "", NULL);
    bind_function(env, "wlr-screencopy-manager-v1-create", func);

    func = env->make_function(env, 1, 1, Fwlr_data_device_manager_create, "", NULL);
    bind_function(env, "wlr-data-device-manager-create", func);

    func = env->make_function(env, 1, 1, Fwlr_gamma_control_manager_v1_create, "", NULL);
    bind_function(env, "wlr-gamma-control-manager-v1-create", func);

    func = env->make_function(env, 1, 1, Fwlr_primary_selection_v1_device_manager_create, "", NULL);
    bind_function(env, "wlr-primary-selection-v1-device-manager-create", func);

    func = env->make_function(env, 1, 1, Fwlr_viewporter_create, "", NULL);
    bind_function(env, "wlr-viewporter-create", func);

    func = env->make_function(env, 2, 2, Fwlr_xdg_output_manager_v1_create, "", NULL);
    bind_function(env, "wlr-xdg-output-manager-v1-create", func);
}
