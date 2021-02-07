/*
 See LICENSE file for copyright and license details.
 */
#define _POSIX_C_SOURCE 200809L
#include "server.h"
#include "util.h"
#include "client.h"
#include "output.h"
// #include <linux/input-event-codes.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <time.h>
//#include <unistd.h>
#include <wayland-client.h>
#include <wayland-server-core.h>
#include <wlr/backend.h> */
#include <wlr/render/wlr_renderer.h> */
#include <wlr/types/wlr_compositor.h> */
#include <wlr/types/wlr_cursor.h> */
#include <wlr/types/wlr_data_device.h> */
#include <wlr/types/wlr_export_dmabuf_v1.h> */
#include <wlr/types/wlr_gamma_control_v1.h> */
#include <wlr/types/wlr_input_device.h>
#include <wlr/types/wlr_keyboard.h>
#include <wlr/types/wlr_matrix.h>
#include <wlr/types/wlr_output.h>
#include <wlr/types/wlr_output_layout.h>
#include <wlr/types/wlr_pointer.h>
#include <wlr/types/wlr_primary_selection.h>
#include <wlr/types/wlr_primary_selection_v1.h>
#include <wlr/types/wlr_screencopy_v1.h>
#include <wlr/types/wlr_seat.h>
#include <wlr/types/wlr_viewporter.h>
#include <wlr/types/wlr_xcursor_manager.h>
#include <wlr/types/wlr_xdg_decoration_v1.h>
#include <wlr/types/wlr_xdg_output_v1.h>
#include <wlr/types/wlr_xdg_shell.h>
#include <wlr/util/log.h>
#include <xkbcommon/xkbcommon.h>

#include <X11/Xlib.h>
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

emacs_value Fwlr_screencopy_v1_create(emacs_env *env, ptrdiff_t nargs,
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
