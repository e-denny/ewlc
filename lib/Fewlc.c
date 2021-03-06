#define _POSIX_C_SOURCE 200809L
#include "server.h"
#include "module.h"
#include "Fwlr.h"
#include "notify.h"
#include <signal.h>
#include <linux/input-event-codes.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>
#include <emacs-module.h>
#include <wayland-client.h>
#include <wayland-server-core.h>
#include <wlr/backend.h>
#include <wlr/types/wlr_surface.h>
#include <wlr/types/wlr_xdg_decoration_v1.h>
#include <wlr/types/wlr_xdg_shell.h>
#include <wlr/types/wlr_cursor.h>
#include <wlr/types/wlr_seat.h>
#include <wlr/render/wlr_renderer.h>
#include <wlr/types/wlr_matrix.h>
#include <wlr/types/wlr_output.h>
#include <wlr/types/wlr_output_layout.h>
#include <wlr/types/wlr_input_device.h>
#include <wlr/types/wlr_keyboard.h>
#include <xkbcommon/xkbcommon.h>
#include <X11/Xlib.h>
#include <wlr/xwayland.h>


emacs_value Fewlc_output_ptr_equal(emacs_env *env, ptrdiff_t nargs,
                                   emacs_value args[], void *data)
{
    struct ewlc_output *o1 = env->get_user_ptr(env, args[0]);
    struct ewlc_output *o2 = env->get_user_ptr(env, args[1]);
    if ( o1 == o2)
        return Qt;
    return Qnil;
}

emacs_value Fewlc_client_ptr_equal(emacs_env *env, ptrdiff_t nargs,
                                   emacs_value args[], void *data)
{
    struct ewlc_client *c1 = env->get_user_ptr(env, args[0]);
    struct ewlc_client *c2 = env->get_user_ptr(env, args[1]);
    if ( c1 == c2)
        return Qt;
    return Qnil;
}

emacs_value Fewlc_keyboard_ptr_equal(emacs_env *env, ptrdiff_t nargs,
                                   emacs_value args[], void *data)
{
    struct ewlc_keyboard *kb1 = env->get_user_ptr(env, args[0]);
    struct ewlc_keyboard *kb2 = env->get_user_ptr(env, args[1]);
    if ( kb1 == kb2)
        return Qt;
    return Qnil;
}

// TODO: need a finalizer to free pointer
emacs_value Fewlc_make_deco_ptr(emacs_env *env, ptrdiff_t nargs,
                                emacs_value args[], void *data)
{
    struct ewlc_server *s = env->get_user_ptr(env, args[0]);
    struct ewlc_decoration *d = calloc(1, sizeof(*d));
    d->server = s;
    return env->make_user_ptr(env, NULL, d);
}

emacs_value Fewlc_set_xdg_deco_mgr_toplevel_listeners(emacs_env *env, ptrdiff_t nargs,
                                                      emacs_value args[], void *data)
{
    struct ewlc_decoration *d = env->get_user_ptr(env, args[0]);
    struct wlr_xdg_toplevel_decoration_v1 *wlr_deco = env->get_user_ptr(env, args[1]);

    d->deco_request_mode_listener.notify = deco_request_mode_notify;
    wl_signal_add(&wlr_deco->events.request_mode, &d->deco_request_mode_listener);

    d->deco_destroy_listener.notify = deco_destroy_notify;
    wl_signal_add(&wlr_deco->events.destroy, &d->deco_destroy_listener);

    return Qt;
}

// FIXME: choose a better name - just for xwindows
emacs_value Fewlc_floating_type_p(emacs_env *env, ptrdiff_t nargs,
                                  emacs_value args[], void *data)
{
    struct wlr_xwayland_surface *surface = env->get_user_ptr(env, args[0]);
    Atom *netatom = env->get_user_ptr(env, args[1]);
    for (size_t i = 0; i < surface->window_type_len; i++)
        if (surface->window_type[i] == netatom[NetWMWindowTypeDialog] ||
            surface->window_type[i] == netatom[NetWMWindowTypeSplash] ||
            surface->window_type[i] == netatom[NetWMWindowTypeToolbar] ||
            surface->window_type[i] == netatom[NetWMWindowTypeUtility])
            return Qt;
    return Qnil;
}

emacs_value Fewlc_xcb_connect(emacs_env *env, ptrdiff_t nargs,
                              emacs_value args[], void *data)
{
    struct wlr_xwayland *xwayland = env->get_user_ptr(env, args[0]);
    xcb_connection_t *xc = xcb_connect(xwayland->display_name, NULL);
    return env->make_user_ptr(env, NULL, xc);
}

emacs_value Fewlc_xcb_disconnect(emacs_env *env, ptrdiff_t nargs,
                                 emacs_value args[], void *data)
{
    xcb_connection_t *xc = env->get_user_ptr(env, args[0]);
    xcb_disconnect(xc);
    return Qt;
}

emacs_value Fewlc_xcb_connect_has_error(emacs_env *env, ptrdiff_t nargs,
                                        emacs_value args[], void *data)
{
    xcb_connection_t *xc = env->get_user_ptr(env, args[0]);
    int err = xcb_connection_has_error(xc);
    return env->make_integer(env, err);
}

Atom get_atom(xcb_connection_t *xc, const char *name)
{
    Atom atom = 0;
    xcb_intern_atom_cookie_t cookie;
    xcb_intern_atom_reply_t *reply;

    cookie = xcb_intern_atom(xc, 0, strlen(name), name);
    if ((reply = xcb_intern_atom_reply(xc, cookie, NULL)))
        atom = reply->atom;
    free(reply);

    return atom;
}

emacs_value Fewlc_set_atoms(emacs_env *env, ptrdiff_t nargs,
                            emacs_value args[], void *data)
{
    xcb_connection_t *xc = env->get_user_ptr(env, args[0]);
    Atom *netatom = calloc(4, sizeof(Atom));

    // FIXME: where is the function get_atom()
    netatom[NetWMWindowTypeDialog] = get_atom(xc, "_NET_WM_WINDOW_TYPE_DIALOG");
    netatom[NetWMWindowTypeSplash] = get_atom(xc, "_NET_WM_WINDOW_TYPE_SPLASH");
    netatom[NetWMWindowTypeUtility] = get_atom(xc, "_NET_WM_WINDOW_TYPE_TOOLBAR");
    netatom[NetWMWindowTypeToolbar] = get_atom(xc, "_NET_WM_WINDOW_TYPE_UTILITY");

    return env->make_user_ptr(env, NULL, netatom);
}

void sigchld(int unused)
{
    if (signal(SIGCHLD, sigchld) == SIG_ERR)
        ERROR("can't install SIGCHLD handler");
    while (0 < waitpid(-1, NULL, WNOHANG));
}

emacs_value Fewlc_set_sigchld(emacs_env *env, ptrdiff_t nargs,
                              emacs_value args[], void *data)
{
    int v = env->extract_integer(env, args[0]);
    sigchld(v);
    return Qt;
}

emacs_value Fewlc_backend_set_listeners(emacs_env *env, ptrdiff_t nargs,
                                        emacs_value args[], void *data)
{
    struct ewlc_server *s = env->get_user_ptr(env, args[0]);
    struct wlr_backend *backend = env->get_user_ptr(env, args[1]);

    s->backend_new_output_listener.notify = backend_new_output_notify;
    s->backend_new_input_listener.notify = backend_new_input_notify;

    wl_signal_add(&backend->events.new_output, &s->backend_new_output_listener);
    wl_signal_add(&backend->events.new_input, &s->backend_new_input_listener);
    return Qt;
}

emacs_value Fewlc_xdg_shell_set_listeners(emacs_env *env, ptrdiff_t nargs,
                                          emacs_value args[], void *data)
{
    struct ewlc_server *s = env->get_user_ptr(env, args[0]);
    struct wlr_xdg_shell *xdg_shell = env->get_user_ptr(env, args[1]);

    s->xdg_shell_new_surface_listener.notify = xdg_shell_new_surface_notify;
    wl_signal_add(&xdg_shell->events.new_surface, &s->xdg_shell_new_surface_listener);
    return Qt;
}

emacs_value Fewlc_xdg_deco_set_listeners(emacs_env *env, ptrdiff_t nargs,
                                         emacs_value args[], void *data)
{
    struct ewlc_server *s = env->get_user_ptr(env, args[0]);
    struct wlr_xdg_decoration_manager_v1 *xdg_deco = env->get_user_ptr(env, args[1]);

    s->xdeco_mgr_new_top_level_decoration_listener.notify =
        xdeco_mgr_new_toplevel_decoration_notify;
    wl_signal_add(&xdg_deco->events.new_toplevel_decoration,
                  &s->xdeco_mgr_new_top_level_decoration_listener);
    return Qt;
}

emacs_value Fewlc_cursor_set_listeners(emacs_env *env, ptrdiff_t nargs,
                                       emacs_value args[], void *data)

{
    struct ewlc_server *s = env->get_user_ptr(env, args[0]);
    struct wlr_cursor *cursor = env->get_user_ptr(env, args[1]);

    s->cursor_axis_listener.notify = cursor_axis_notify;
    s->cursor_button_listener.notify = cursor_button_notify;
    s->cursor_frame_listener.notify = cursor_frame_notify;
    s->cursor_motion_listener.notify = cursor_motion_notify;
    s->cursor_motion_absolute_listener.notify = cursor_motion_absolute_notify;

    wl_signal_add(&cursor->events.axis, &s->cursor_axis_listener);
    wl_signal_add(&cursor->events.button, &s->cursor_button_listener);
    wl_signal_add(&cursor->events.frame, &s->cursor_frame_listener);
    wl_signal_add(&cursor->events.motion, &s->cursor_motion_listener);
    wl_signal_add(&cursor->events.motion_absolute, &s->cursor_motion_absolute_listener);
    return Qt;
}

emacs_value Fewlc_seat_set_listeners(emacs_env *env, ptrdiff_t nargs,
                                     emacs_value args[], void *data)

{
    struct ewlc_server *s = env->get_user_ptr(env, args[0]);
    struct wlr_seat *seat = env->get_user_ptr(env, args[1]);

    s->seat_request_set_cursor_listener.notify = seat_request_set_cursor_notify;
    s->seat_request_set_selection_listener.notify = seat_request_set_selection_notify;
    s->seat_request_set_primary_selection_listener.notify =
        seat_request_set_primary_selection_notify;

    wl_signal_add(&seat->events.request_set_cursor, &s->seat_request_set_cursor_listener);
    wl_signal_add(&seat->events.request_set_selection,
                  &s->seat_request_set_selection_listener);
    wl_signal_add(&seat->events.request_set_primary_selection,
                  &s->seat_request_set_primary_selection_listener);
    return Qt;
}

emacs_value Fewlc_xwayland_set_listeners(emacs_env *env, ptrdiff_t nargs,
                                         emacs_value args[], void *data)

{
    struct ewlc_server *s = env->get_user_ptr(env, args[0]);
    struct wlr_xwayland *xwayland = env->get_user_ptr(env, args[1]);

    s->xwayland_ready_listener.notify = xwayland_ready_notify;
    s->new_xwayland_surface_listener.notify = new_xwayland_surface_notify;

    wl_signal_add(&xwayland->events.ready, &s->xwayland_ready_listener);
    wl_signal_add(&xwayland->events.new_surface, &s->new_xwayland_surface_listener);
    return Qt;
}

emacs_value Fewlc_make_server_ptr(emacs_env *env, ptrdiff_t nargs,
                                  emacs_value args[], void *data)
{
    struct ewlc_server *s  = calloc(1, sizeof(*s));
    return env->make_user_ptr(env, NULL, s);
}

emacs_value Fewlc_kill(emacs_env *env, ptrdiff_t nargs,
                       emacs_value args[], void *data)
{
    int pid = env->extract_integer(env, args[0]);
    if (pid != -1) {
        kill(pid, SIGTERM);
        waitpid(pid, NULL, 0);
    }
    return Qt;
}
emacs_value Fewlc_pending_events_p(emacs_env *env, ptrdiff_t nargs,
                                   emacs_value args[], void *data)
{
    struct ewlc_server *s = env->get_user_ptr(env, args[0]);
    if (s->event_list != NULL)
        return Qt;
    return Qnil;
}

emacs_value Fewlc_get_event(emacs_env *env, ptrdiff_t nargs,
                            emacs_value args[], void *data)
{
    struct ewlc_server *s = env->get_user_ptr(env, args[0]);
    void *event_container = s->event_list->event_container;
    void *event_data = s->event_list->event_data;
    char *event_type = s->event_list->event_type;
    // FIXME: can I return an array of emacs_value, or do I need a malloc?
    emacs_value ret[3];
    DEBUG("s->event_list->event_type: '%s'", s->event_list->event_type);
    DEBUG("event_type: '%s'", event_type);
    ret[0] = env->make_user_ptr(env, NULL, event_container);
    ret[1] = env->make_user_ptr(env, NULL, event_data);
    ret[2] = env->intern(env, event_type);
    return list(env, ret, 3);
}

emacs_value Fewlc_remove_event(emacs_env *env, ptrdiff_t nargs,
                               emacs_value args[], void *data)
{
    struct ewlc_server *s = env->get_user_ptr(env, args[0]);

    // TODO: free the event here rather in lisp code - is this correct now?
    s->event_list = remove_event(s->event_list);
    return Qt;
}

// TODO: need a finalizer to free pointer.
emacs_value Fewlc_make_xdg_surface_client_ptr(emacs_env *env, ptrdiff_t nargs,
                                              emacs_value args[], void *data)
{
    struct ewlc_server *s = env->get_user_ptr(env, args[0]);
    struct ewlc_client *c;
    c = calloc(1, sizeof(*c));
    c->server = s;
    return env->make_user_ptr(env, NULL, c);
}

emacs_value Fewlc_make_xwayland_surface_client_ptr(emacs_env *env, ptrdiff_t nargs,
                                                   emacs_value args[], void *data)
{
    struct ewlc_server *s = env->get_user_ptr(env, args[0]);
    struct ewlc_client *c;
    c = calloc(1, sizeof(*c));
    c->server = s;
    return env->make_user_ptr(env, NULL, c);
}

emacs_value Fewlc_set_xdg_surface_client_listeners(emacs_env *env, ptrdiff_t nargs,
                                                   emacs_value args[], void *data)
{
    struct ewlc_client *c = env->get_user_ptr(env, args[0]);
    struct wlr_xdg_surface *xdg_surface = env->get_user_ptr(env, args[1]);

    /* Listen to the various events it can emit */
    c->surface_commit_listener.notify = xdg_surface_commit_notify;
    wl_signal_add(&xdg_surface->surface->events.commit, &c->surface_commit_listener);

    c->surface_map_listener.notify = surface_map_notify;
    wl_signal_add(&xdg_surface->events.map, &c->surface_map_listener);

    c->surface_unmap_listener.notify = surface_unmap_notify;
    wl_signal_add(&xdg_surface->events.unmap, &c->surface_unmap_listener);

    c->surface_destroy_listener.notify = surface_destroy_notify;
    wl_signal_add(&xdg_surface->events.destroy, &c->surface_destroy_listener);
    return Qt;
}

emacs_value Fewlc_remove_client_listeners(emacs_env *env, ptrdiff_t nargs,
                                          emacs_value args[], void *data)
{
    struct ewlc_client *c = env->get_user_ptr(env, args[0]);
    emacs_value c_type = args[1];
    emacs_value x11_managed = env->intern(env, "x11-managed");
    emacs_value xdg_shell = env->intern(env, "xdg-shell");

    wl_list_remove(&c->surface_map_listener.link);
    wl_list_remove(&c->surface_unmap_listener.link);
    wl_list_remove(&c->surface_destroy_listener.link);
    if (c_type == x11_managed)
        wl_list_remove(&c->xwayland_surface_request_activate_listener.link);
    else if (c_type == xdg_shell)
        wl_list_remove(&c->surface_commit_listener.link);
    return Qt;
}

void render_surface(struct wlr_surface *surface, int sx, int sy, void *data)
{
    /* This function is called for every surface that needs to be rendered. */
    struct render_data *rdata = data;
    struct wlr_output *output = rdata->output;
    struct wlr_output_layout *output_layout = rdata->output_layout;
    struct wlr_renderer *renderer = rdata->renderer;
    struct wlr_surface_state state = surface->current;
    double ox = 0, oy = 0;
    struct wlr_box obox;
    float matrix[9];
    enum wl_output_transform transform;

    /* We first obtain a wlr_texture, which is a GPU resource. wlroots
     * automatically handles negotiating these with the client. The underlying
     * resource could be an opaque handle passed from the client, or the client
     * could have sent a pixel buffer which we copied to the GPU, or a few other
     * means. You don't have to worry about this, wlroots takes care of it. */
    struct wlr_texture *texture = wlr_surface_get_texture(surface);
    if (!texture)
        return;

    /* The client has a position in layout coordinates. If you have two
     * displays, one next to the other, both 1080p, a client on the rightmost
     * display might have layout coordinates of 2000,100. We need to translate
     * that to output-local coordinates, or (2000 - 1920). */
    wlr_output_layout_output_coords(output_layout, output, &ox, &oy);

    /* We also have to apply the scale factor for HiDPI outputs. This is only
     * part of the puzzle, ewlc does not fully support HiDPI. */

    obox.x = (ox + rdata->x + sx) * output->scale;
    obox.y = (oy + rdata->y + sy) * output->scale;
    obox.width = state.width * output->scale;
    obox.height = state.height * output->scale;

    /*
     * Those familiar with OpenGL are also familiar with the role of matrices
     * in graphics programming. We need to prepare a matrix to render the
     * client with. wlr_matrix_project_box is a helper which takes a box with
     * a desired x, y coordinates, width and height, and an output geometry,
     * then prepares an orthographic projection and multiplies the necessary
     * transforms to produce a model-view-projection matrix.
     *
     * Naturally you can do this any way you like, for example to make a 3D
     * compositor.
     */
    transform = wlr_output_transform_invert(surface->current.transform);
    wlr_matrix_project_box(matrix, &obox, transform, 0,
                           output->transform_matrix);

    /* This takes our matrix, the texture, and an alpha, and performs the actual
     * rendering on the GPU. */
    wlr_render_texture_with_matrix(renderer, texture, matrix, 1);

    /* This lets the client know that we've displayed that frame and it can
     * prepare another one now if it likes. */
    wlr_surface_send_frame_done(surface, &rdata->when);
}

// TODO: finalizer to free pointer
emacs_value Fewlc_create_render_data(emacs_env *env, ptrdiff_t nargs,
                                     emacs_value args[], void *data)
{
    struct render_data *r_data;
    struct wlr_output *output;
    struct wlr_output_layout *output_layout;
    struct wlr_renderer *renderer;
    int x;
    int y;
    struct timespec when;
    INFO(">>>>>>>>");

    output = env->get_user_ptr(env, args[0]);
    DEBUG("output: '%p'", output);
    output_layout = env->get_user_ptr(env, args[1]);
    DEBUG("output_layout: '%p'", output_layout);
    renderer = env->get_user_ptr(env, args[2]);
    DEBUG("renderer: '%p'", renderer);
    x = env->extract_integer(env, args[3]);
    y = env->extract_integer(env, args[4]);
    DEBUG("x: '%d'", x);
    DEBUG("y: '%d'", y);
    when = env->extract_time(env, args[5]);
    INFO("check1");

    r_data = calloc(1, sizeof(*r_data));
    r_data->output = output;
    r_data->output_layout = output_layout;
    r_data->renderer = renderer;
    r_data->x = x;
    r_data->y = y;
    r_data->when = when;
    INFO("<<<<<<<<<<<<");
    return env->make_user_ptr(env, NULL, r_data);
    //    return Qt;
}

emacs_value Fewlc_set_xwayland_surface_client_listeners(emacs_env *env, ptrdiff_t nargs,
                                                        emacs_value args[], void *data)
{
    struct ewlc_client *c = env->get_user_ptr(env, args[0]);
    struct wlr_xwayland_surface *xwayland_surface = env->get_user_ptr(env, args[1]);

    /* Listen to the various events it can emit */
    c->surface_map_listener.notify = surface_map_notify;
    wl_signal_add(&xwayland_surface->events.map, &c->surface_map_listener);

    c->surface_unmap_listener.notify = surface_unmap_notify;
    wl_signal_add(&xwayland_surface->events.unmap, &c->surface_unmap_listener);

    c->xwayland_surface_request_activate_listener.notify =
        xwayland_surface_request_activate_notify;
    wl_signal_add(&xwayland_surface->events.request_activate,
                  &c->xwayland_surface_request_activate_listener);

    c->surface_destroy_listener.notify = surface_destroy_notify;
    wl_signal_add(&xwayland_surface->events.destroy, &c->surface_destroy_listener);
    return Qt;
}

emacs_value Fewlc_free(emacs_env *env, ptrdiff_t nargs,
                       emacs_value args[], void *data)
{
    free(env->get_user_ptr(env, args[0]));
    return Qt;
}

emacs_value Fewlc_make_keyboard_ptr(emacs_env *env, ptrdiff_t nargs,
                                    emacs_value args[], void *data)
{
    struct ewlc_keyboard *kb;
    struct ewlc_server *s = env->get_user_ptr(env, args[0]);
    kb = calloc(1, sizeof(*kb));
    kb->server = s;
    return env->make_user_ptr(env, NULL, kb);
}

emacs_value Fewlc_keyboard_set_listeners(emacs_env *env, ptrdiff_t nargs,
                                         emacs_value args[], void *data)
{
    struct ewlc_keyboard *kb = env->get_user_ptr(env, args[0]);
    struct wlr_input_device *device = env->get_user_ptr(env, args[1]);

    /* Set up event listeners */
    kb->keyboard_modifiers_listener.notify = keyboard_modifiers_notify;
    kb->keyboard_key_listener.notify = keyboard_key_notify;
    kb->keyboard_destroy_listener.notify = keyboard_destroy_notify;

    wl_signal_add(&device->keyboard->events.modifiers, &kb->keyboard_modifiers_listener);
    wl_signal_add(&device->keyboard->events.key, &kb->keyboard_key_listener);
    wl_signal_add(&device->events.destroy, &kb->keyboard_destroy_listener);

    return Qt;
}

emacs_value Fewlc_compare_outputs(emacs_env *env, ptrdiff_t nargs,
                                  emacs_value args[], void *data)
{
    struct ewlc_output *o_a = env->get_user_ptr(env, args[0]);
    struct ewlc_output *o_b = env->get_user_ptr(env, args[1]);
    if (o_a == o_b)
        return Qt;
    return Qnil;
}

emacs_value Fewlc_output_set_listeners(emacs_env *env, ptrdiff_t nargs,
                                       emacs_value args[], void *data)
{
    struct ewlc_output *o = env->get_user_ptr(env, args[0]);
    struct wlr_output *wlr_output = env->get_user_ptr(env, args[1]);

    /* Set up event listeners */
    o->output_frame_listener.notify = output_frame_notify;
    wl_signal_add(&wlr_output->events.frame, &o->output_frame_listener);

    o->output_destroy_listener.notify = output_destroy_notify;
    wl_signal_add(&wlr_output->events.destroy, &o->output_destroy_listener);
    return Qt;
}

emacs_value Fewlc_make_output_ptr(emacs_env *env, ptrdiff_t nargs,
                                  emacs_value args[], void *data)
{
    struct ewlc_output *o;
    struct ewlc_server *s = env->get_user_ptr(env, args[0]);
    o = calloc(1, sizeof(*o));
    o->server = s;
    return env->make_user_ptr(env, NULL, o);
}

static emacs_value Fewlc_chvt(emacs_env *env, ptrdiff_t nargs,
                              emacs_value args[], void *data)
{
    struct wlr_backend *backend = env->get_user_ptr(env, args[0]);
    int nbr = env->extract_integer(env, args[1]);
    wlr_session_change_vt(wlr_backend_get_session(backend), nbr);
    return Qt;
}

static emacs_value Fewlc_spawn(emacs_env *env, ptrdiff_t nargs,
                               emacs_value args[], void *data)
{
    ptrdiff_t len;
    char *cmd;
    // TODO: extend to more arguments
    char *cmd_args[3];

    len = string_bytes(env, args[0]);
    cmd = cmd_args[0] = malloc(sizeof(char) * len);
    env->copy_string_contents(env, args[0], cmd, &len);
    env->copy_string_contents(env, args[0], cmd_args[0], &len);

    if (nargs > 1) {
        len = string_bytes(env, args[1]);
        cmd_args[1] = malloc(sizeof(char) * len);
        env->copy_string_contents(env, args[1], cmd_args[1], &len);
    }
    cmd_args[2] = NULL;

    if (fork() == 0) {
        setsid();
        execvp(cmd, cmd_args);
        ERROR("ewlc: execvp %s failed", cmd);
    }
    free(cmd);
    free(cmd_args[0]);
    if (nargs > 1) free(cmd_args[1]);
    return Qt;
}

emacs_value Fewlc_compare_clients(emacs_env *env, ptrdiff_t nargs,
                                  emacs_value args[], void *data)
{
    struct ewlc_client *c_a = env->get_user_ptr(env, args[0]);
    struct ewlc_client *c_b = env->get_user_ptr(env, args[1]);
    if (c_a == c_b)
        return Qt;
    return Qnil;
}

void init_ewlc(emacs_env *env)
{
    emacs_value func;

    func = env->make_function(env, 0, 0, Fewlc_make_server_ptr, "", NULL);
    bind_function(env, "ewlc-make-server-ptr", func);

    func = env->make_function(env, 1, 1, Fewlc_make_output_ptr, "", NULL);
    bind_function(env, "ewlc-make-output-ptr", func);

    func = env->make_function(env, 1, 1, Fewlc_make_deco_ptr, "", NULL);
    bind_function(env, "ewlc-make-deco-ptr", func);

    func = env->make_function(env, 1, 1, Fewlc_make_xdg_surface_client_ptr, "", NULL);
    bind_function(env, "ewlc-make-xdg-surface-client-ptr", func);

    func = env->make_function(env, 1, 1, Fewlc_make_xwayland_surface_client_ptr, "", NULL);
    bind_function(env, "ewlc-make-xwayland-surface-client-ptr", func);

    func = env->make_function(env, 1, 1, Fewlc_make_keyboard_ptr, "", NULL);
    bind_function(env, "ewlc-make-keyboard-ptr", func);

    func = env->make_function(env, 1, 1, Fewlc_free, "", NULL);
    bind_function(env, "ewlc-free", func);

    // --------------------------------

    func = env->make_function(env, 2, 2, Fewlc_set_xdg_deco_mgr_toplevel_listeners, "", NULL);
    bind_function(env, "ewlc-set-xdg-deco-mgr-toplevel-listeners", func);

    func = env->make_function(env, 2, 2, Fewlc_backend_set_listeners, "", NULL);
    bind_function(env, "ewlc-backend-set-listeners", func);

    func = env->make_function(env, 2, 2, Fewlc_output_set_listeners, "", NULL);
    bind_function(env, "ewlc-output-set-listeners", func);

    func = env->make_function(env, 2, 2, Fewlc_xdg_shell_set_listeners, "", NULL);
    bind_function(env, "ewlc-xdg-shell-set-listeners", func);

    func = env->make_function(env, 2, 2, Fewlc_xdg_deco_set_listeners, "", NULL);
    bind_function(env, "ewlc-xdg-deco-set-listeners", func);

    func = env->make_function(env, 2, 2, Fewlc_cursor_set_listeners, "", NULL);
    bind_function(env, "ewlc-cursor-set-listeners", func);

    func = env->make_function(env, 2, 2, Fewlc_seat_set_listeners, "", NULL);
    bind_function(env, "ewlc-seat-set-listeners", func);

    func = env->make_function(env, 2, 2, Fewlc_xwayland_set_listeners, "", NULL);
    bind_function(env, "ewlc-xwayland-set-listeners", func);

    func = env->make_function(env, 2, 2, Fewlc_set_xdg_surface_client_listeners, "", NULL);
    bind_function(env, "ewlc-set-xdg-surface-client-listeners", func);

    func = env->make_function(env, 2, 2, Fewlc_set_xwayland_surface_client_listeners, "", NULL);
    bind_function(env, "ewlc-set-xwayland-surface-client-listeners", func);

    func = env->make_function(env, 2, 2, Fewlc_keyboard_set_listeners, "", NULL);
    bind_function(env, "ewlc-keyboard-set-listeners", func);

    func = env->make_function(env, 2, 2, Fewlc_remove_client_listeners, "", NULL);
    bind_function(env, "ewlc-remove-client-listeners", func);

    // --------------------------------

    func = env->make_function(env, 1, 1, Fewlc_pending_events_p, "", NULL);
    bind_function(env, "ewlc-pending-events-p", func);

    func = env->make_function(env, 1, 1, Fewlc_get_event, "", NULL);
    bind_function(env, "ewlc-get-event", func);

    func = env->make_function(env, 1, 1, Fewlc_remove_event, "", NULL);
    bind_function(env, "ewlc-remove-event", func);

    // --------------------------------

    func = env->make_function(env, 2, 2, Fewlc_compare_outputs, "", NULL);
    bind_function(env, "ewlc-compare-outputs", func);

    func = env->make_function(env, 2, 2, Fewlc_compare_clients, "", NULL);
    bind_function(env, "ewlc-compare-clients", func);

    // --------------------------------

    func = env->make_function(env, 1, 1, Fewlc_xcb_connect, "", NULL);
    bind_function(env, "ewlc-xcb-connect", func);

    func = env->make_function(env, 1, 1, Fewlc_xcb_disconnect, "", NULL);
    bind_function(env, "ewlc-xcb-disconnect", func);

    func = env->make_function(env, 1, 1, Fewlc_xcb_connect_has_error, "", NULL);
    bind_function(env, "ewlc-xcb-connect-has-error", func);

    func = env->make_function(env, 1, 1, Fewlc_set_atoms, "", NULL);
    bind_function(env, "ewlc-set-atoms", func);

    func = env->make_function(env, 2, 2, Fewlc_floating_type_p, "", NULL);
    bind_function(env, "ewlc-floating-type-p", func);

    // --------------------------------

    func = env->make_function(env, 1, 1, Fewlc_set_sigchld, "", NULL);
    bind_function(env, "ewlc-set-sigchld", func);

    func = env->make_function(env, 6, 6, Fewlc_create_render_data, "", NULL);
    bind_function(env, "ewlc-create-render-data", func);

    func = env->make_function(env, 2, 2, Fewlc_chvt, "", NULL);
    bind_function(env, "ewlc-chvt", func);

    func = env->make_function(env, 1, 2, Fewlc_spawn, "", NULL);
    bind_function(env, "ewlc-spawn", func);

    // --------------------------------

    func = env->make_function(env, 2, 2, Fewlc_output_ptr_equal, "", NULL);
    bind_function(env, "ewlc-output-ptr=", func);

    func = env->make_function(env, 2, 2, Fewlc_client_ptr_equal, "", NULL);
    bind_function(env, "ewlc-client-ptr=", func);

    func = env->make_function(env, 2, 2, Fewlc_keyboard_ptr_equal, "", NULL);
    bind_function(env, "ewlc-keyboard-ptr=", func);
}
