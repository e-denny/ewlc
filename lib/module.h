#ifndef __MODULE_H
#define __MODULE_H

#include <emacs-module.h>

// Emacs symbols
extern emacs_value Qt;
extern emacs_value Qnil;

extern emacs_value Fewlc_apply_keybinding;
void e_message(emacs_env *env, char *msg_str);

#endif /* __EWLC_MODULE_H */
