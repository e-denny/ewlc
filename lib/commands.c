/*
 p See LICENSE file for copyright and license details.
 */
#define _POSIX_C_SOURCE 200809L
#include "server.h"
#include "output.h"
#include "client.h"
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

void ewlc_chvt(struct ewlc_server *s, int nbr)
{
    wlr_session_change_vt(wlr_backend_get_session(s->backend), nbr);
}

void ewlc_focus_output(int direction, struct ewlc_server *s)
{
    struct ewlc_client *c = get_active_client(s);
    struct ewlc_output *o;
    o = set_next_output(direction, s);
    focus_client(c, focus_top(o), 1);
}

void ewlc_add_master(struct ewlc_server *s, int delta)
{
    s->active_output->num_master = MAX(s->active_output->num_master + delta, 0);
    arrange(s->active_output);
}

void ewlc_set_master_ratio(float inc, struct ewlc_server *s)
{
    float f;
    INFO(">>> entering");

    f = inc < 1.0 ? inc + s->active_output->master_ratio : inc - 1.0;
    if (f < 0.1 || f > 0.9)
        return;
    s->active_output->master_ratio = f;
    arrange(s->active_output);
    INFO("<<< leaving");
}

void ewlc_kill_client(struct ewlc_server *s)
{
    struct ewlc_client *c = get_active_client(s);
    if (!c)
        return;

#ifdef XWAYLAND
    if (c->type != XDG_SHELL)
        wlr_xwayland_surface_close(c->surface.xwayland);
    else
#endif
        wlr_xdg_toplevel_send_close(c->surface.xdg);
}

void ewlc_quit(struct ewlc_server *s)
{
    wl_display_terminate(s->display);
}

void ewlc_spawn(char *cmd, char *args[])
{
    if (fork() == 0) {
        setsid();
        execvp(cmd, args);
        EERROR("ewlc: execvp %s failed", cmd);
    }
}

void ewlc_toggle_floating(struct ewlc_server *s)
{
    struct ewlc_client *c;
    INFO(">>> ewlc_toggle_floating");
    c = get_active_client(s);
    DEBUG("c = '%p'", c);
    if (!c)
        return;
    /* return if fullscreen */
    DEBUG("c->is_floating = '%d'", c->is_floating);
    set_floating(c, !c->is_floating);
    INFO("<<< ewlc_toggle_floating");
}

void ewlc_view(struct ewlc_server *s)
{
    struct ewlc_client *c = get_active_client(s);
    focus_client(c, focus_top(s->active_output), 1);
    arrange(s->active_output);
}

void ewlc_zoom(struct ewlc_server *s)
{
    struct ewlc_client *c, *c_active = get_active_client(s), *old_c_active;

    old_c_active = c_active;

    if (!c_active || c_active->is_floating)
        return;

    /* Search for the first tiled window that is not c_active, marking c_active
     * as NULL if we pass it along the way */
    wl_list_for_each(c, &s->client_list, client_link)
    {
        if (is_visible_on(c, s->active_output) && !c->is_floating) {
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
    arrange(s->active_output);
}
