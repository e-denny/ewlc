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
    // FIXME: free image_text?
}

emacs_value Fwlr_xcursor_manager_create(emacs_env *env, ptrdiff_t nargs,
                                        emacs_value args[], void *data)
{
    /* don't bother with cursor theme - so name not assignable. */
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
