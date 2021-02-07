#ifndef __FWLR_H_
#define __FWLR_H_

#define _POSIX_C_SOURCE 200809L
#include <emacs-module.h>

void init_wlr_pointer(emacs_env *env);
void init_wlr_backend(emacs_env *env);
void init_wlr_cursor(emacs_env *env);
void init_wlr_input_device(emacs_env *env);
void init_wlr_keymap(emacs_env *env);
void init_wlr_output(emacs_env *env);
void init_wlr_box(emacs_env *env);

#endif // __FWLR_H_
