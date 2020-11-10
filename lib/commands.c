/*
 p See LICENSE file for copyright and license details.
 */
#define _POSIX_C_SOURCE 200809L
#include "ewlc.h"
#include "ewlc-module.h"
#include <emacs-module.h>
#include <getopt.h>
#include <linux/input-event-codes.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>
#include <wayland-client.h>
#include <wayland-server-core.h>
#include <wlr/backend.h>
#include <wlr/render/wlr_renderer.h>
#include <wlr/types/wlr_compositor.h>
#include <wlr/types/wlr_cursor.h>
#include <wlr/types/wlr_data_device.h>
#include <wlr/types/wlr_export_dmabuf_v1.h>
#include <wlr/types/wlr_gamma_control_v1.h>
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

#ifdef XWAYLAND
#include <X11/Xlib.h>
#include <wlr/xwayland.h>
#endif

void e_message(emacs_env *env, char *msg_str)
{
    emacs_value e_msg;
    e_msg = env->make_string(env, msg_str, strlen(msg_str));
    env->funcall(env, env->intern(env, "message"), 1, (emacs_value[]){e_msg});
}

void ewlc_chvt(int nbr)
{
    wlr_session_change_vt(wlr_backend_get_session(server.backend), nbr);
}

void focus_client(struct ewlc_client *old, struct ewlc_client *c, int lift)
{
    struct ewlc_server *s = old->server;
    struct wlr_keyboard *kb = wlr_seat_get_keyboard(s->seat);

    /* Raise client in stacking order if requested */
    if (c && lift) {
        wl_list_remove(&c->client_stack_link);
        wl_list_insert(&s->client_stack_list, &c->client_stack_link);
    }

    /* Nothing else to do? */
    if (c == old)
        return;

    /* Deactivate old client if focus is changing */
    if (c != old && old) {
#ifdef XWAYLAND
        if (old->type != XDG_SHELL)
            wlr_xwayland_surface_activate(old->surface.xwayland, 0);
        else
#endif
            wlr_xdg_toplevel_set_activated(old->surface.xdg, 0);
    }

    /* Update wlroots' keyboard focus */
    if (!c) {
        /* With no client, all we have left is to clear focus */
        wlr_seat_keyboard_notify_clear_focus(s->seat);
        return;
    }

    /* Have a client, so focus its top-level wlr_surface */
    wlr_seat_keyboard_notify_enter(s->seat, get_surface(c), kb->keycodes,
                                   kb->num_keycodes, &kb->modifiers);

    /* Put the new client atop the focus stack and select its output */
    wl_list_remove(&c->client_focus_link);
    wl_list_insert(&s->client_focus_list, &c->client_focus_link);
    active_output = c->output;

    /* Activate the new client */
#ifdef XWAYLAND
    if (c->type != XDG_SHELL)
        wlr_xwayland_surface_activate(c->surface.xwayland, 1);
    else
#endif
        wlr_xdg_toplevel_set_activated(c->surface.xdg, 1);
}

void ewlc_focus_output(int direction)
{
    struct ewlc_client *c = get_active_client();

    active_output = get_next_output(direction);
    focus_client(c, focus_top(active_output), 1);
}

void ewlc_focus_next_client(int direction)
{
    /* Focus the next or previous client (in tiling order) on active_output */
    struct ewlc_client *c_next, *c_active = get_active_client();
    struct ewlc_server *serv = c_active->server;

    if (!c_active)
        return;
    if (direction > 0) {
        wl_list_for_each(c_next, &c_active->client_link, client_link)
        {
            if (&c_next->client_link == &serv->client_list)
                continue; /* wrap past the sentinel node */
            if (is_visible_on(c_next, active_output))
                break; /* found it */
        }
    } else {
        wl_list_for_each_reverse(c_next, &c_active->client_link, client_link)
        {
            if (&c_next->client_link == &serv->client_list)
                continue; /* wrap past the sentinel node */
            if (is_visible_on(c_next, active_output))
                break; /* found it */
        }
    }
    /* If only one client is visible on active_output, then c_next == c_active
     */
    focus_client(c_active, c_next, 1);
}

void ewlc_next_master(int direction)
{
    active_output->num_master = MAX(active_output->num_master + direction, 0);
    arrange(active_output);
}

void ewlc_set_master_ratio(float inc)
{
    float f;

    f = inc < 1.0 ? inc + active_output->master_ratio : inc - 1.0;
    if (f < 0.1 || f > 0.9)
        return;
    active_output->master_ratio = f;
    arrange(active_output);
}

emacs_value Fewlc_handle_keybindings(emacs_env *env, ptrdiff_t nargs,
                                     emacs_value args[], void *data)
{
    /* Get the next pending key bindingd, pass it to emacs, or pass
       it to the client. This called within the emacs ewlc event loop. */
    int handled;
    uint32_t mods;
    xkb_keysym_t sym;
    char key_str[100];
    char mods_str[100];
    emacs_value e_key, e_mods, e_handled;
    struct ewlc_keyboard *kb;
    struct wlr_event_keyboard_key *event;

    if (key_list == NULL)
        return Qnil;

    handled = 0;
    mods = key_list->mods;
    sym = key_list->sym;
    event = key_list->event;
    kb = key_list->kb;

    if ((mods & WLR_MODIFIER_ALT) == WLR_MODIFIER_ALT) {
        strcpy(mods_str, "M-");
        e_mods = env->make_string(env, mods_str, strlen(mods_str));

        if (xkb_keysym_get_name(sym, key_str, sizeof(key_str)) == -1)
            ERROR("xkb_keysym_get_name failed.:");
        e_key = env->make_string(env, key_str, strlen(key_str));
        e_handled = env->funcall(env, env->intern(env, "ewlc-apply-keybinding"), 2,
                                 (emacs_value[]){e_mods, e_key});
        handled = env->extract_integer(env, e_handled);
    }

    key_list = remove_from_start(key_list);

    if (!handled) {
        /* Pass non-handled keycodes straight to client. */
        wlr_seat_set_keyboard(server.seat, kb->device);
        wlr_seat_keyboard_notify_key(server.seat, event->time_msec,
                                     event->keycode, event->state);
    }
    return Qt;
}

void ewlc_kill_client()
{
    struct ewlc_client *c = get_active_client();
    if (!c)
        return;

#ifdef XWAYLAND
    if (c->type != XDG_SHELL)
        wlr_xwayland_surface_close(c->surface.xwayland);
    else
#endif
        wlr_xdg_toplevel_send_close(c->surface.xdg);
}

void ewlc_quit()
{
    wl_display_terminate(server.display);
}

void ewlc_spawn(char *cmd, char *args[])
{
    if (fork() == 0) {
        setsid();
        execvp(cmd, args);
        EERROR("ewlc: execvp %s failed", cmd);
    }
}

void ewlc_toggle_floating()
{
    struct ewlc_client *c = get_active_client();
    if (!c)
        return;
    /* return if fullscreen */
    set_floating(c, !c->is_floating);
}

void ewlc_view()
{
    struct ewlc_client *c = get_active_client();
    focus_client(c, focus_top(active_output), 1);
    arrange(active_output);
}

void ewlc_zoom()
{
    struct ewlc_client *c, *c_active = get_active_client(), *old_c_active;
    struct ewlc_server *s = c_active->server;

    old_c_active = c_active;

    if (!c_active || c_active->is_floating)
        return;

    /* Search for the first tiled window that is not c_active, marking c_active
     * as NULL if we pass it along the way */
    wl_list_for_each(c, &s->client_list, c_active->client_link)
    {
        if (is_visible_on(c, active_output) && !c->is_floating) {
            if (c != c_active)
                break;
            c_active = NULL;
        }
    }

    /* Return if no other tiled window was found */
    if (&c->client_link == &s->client_list)
        return;

    /* If we passed c_active, move c to the front; otherwise, move c_active to
     * the front */
    if (!c_active)
        c_active = c;
    wl_list_remove(&c_active->client_link);
    wl_list_insert(&s->client_list, &c_active->client_link);

    focus_client(old_c_active, c_active, 1);
    arrange(active_output);
}
