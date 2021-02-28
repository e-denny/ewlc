#ifndef __MODULE_H
#define __MODULE_H

#include <emacs-module.h>

// Emacs symbols
extern emacs_value Qt;
extern emacs_value Qnil;

void e_message(emacs_env *env, char *msg_str);
void bind_function(emacs_env *env, const char *name, emacs_value Sfun);

int string_bytes(emacs_env *env, emacs_value string);

emacs_value list(emacs_env *env, emacs_value elements[], ptrdiff_t len);
int length(emacs_env *env, emacs_value lst);
emacs_value nth(emacs_env *env, int idx, emacs_value lst);

#endif /* __EWLC_MODULE_H */
