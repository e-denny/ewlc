#ifndef __EWLC_MODULE_H
#define __EWLC_MODULE_H

#include <emacs-module.h>

// Emacs symbols
extern emacs_value Qt;
extern emacs_value Qnil;

extern emacs_value Fewlc_apply_keybinding;

emacs_value Fewlc_handle_keybindings(emacs_env *env, ptrdiff_t nargs,
                                     emacs_value args[], void *data);
#endif /* __EWLC_MODULE_H */
