/*
 p See LICENSE file for copyright and license details.
 */

#ifndef __COMMANDS_H
#define __COMMANDS_H

#define _POSIX_C_SOURCE 200809L
#include "server.h"
#include "module.h"
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

void ewlc_chvt(struct ewlc_server *s, int nbr);
void ewlc_focus_output(int direction, struct ewlc_server *s);
void ewlc_focus_next_client(int direction, struct ewlc_server *s);
void ewlc_add_master(struct ewlc_server *s, int delta);
void ewlc_set_master_ratio(float inc, struct ewlc_server *s);
void ewlc_kill_client(struct ewlc_server *s);
void ewlc_quit(struct ewlc_server *s);
void ewlc_spawn(char *cmd, char *args[]);
void ewlc_toggle_floating(struct ewlc_server *s);
void ewlc_view(struct ewlc_server *s);
void ewlc_zoom(struct ewlc_server *s);

#endif // __COMMANDS_H
