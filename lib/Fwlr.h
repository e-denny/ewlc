#ifndef __FWLR_H_
#define __FWLR_H_

#define _POSIX_C_SOURCE 200809L
#include <emacs-module.h>
#include <wlr/types/wlr_surface.h>

void render_surface(struct wlr_surface *surface, int sx, int sy, void *data);

void init_wlr_pointer(emacs_env *env);
void init_wlr_backend(emacs_env *env);
void init_wlr_cursor(emacs_env *env);
void init_wlr_input_device(emacs_env *env);
void init_wlr_keyboard(emacs_env *env);
void init_wlr_output(emacs_env *env);
void init_wlr_output_layout(emacs_env *env);
void init_wlr_box(emacs_env *env);
void init_wlr_renderer(emacs_env *env);
void init_wlr_seat(emacs_env *env);
void init_wlr_surface(emacs_env *env);
void init_wlr_xcursor_manager(emacs_env *env);
void init_wl(emacs_env *env);
void init_ewlc(emacs_env *env);
void init_wlr_misc(emacs_env *env);
void init_wlr_xwayland(emacs_env *env);
void init_wlr_xdg_shell(emacs_env *env);

#endif // __FWLR_H_
