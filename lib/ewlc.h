#ifndef __EWLC_H_
#define __EWLC_H_

#include <emacs-module.h>

void ewlc_start(emacs_env *env);
int ewlc_display_dispatch();
void ewlc_focus_next_client(int direction);
void ewlc_set_master_ratio(float inc);
void ewlc_kill_client();
void ewlc_quit();
void ewlc_zoom();
void ewlc_view();
void ewlc_toggle_floating();
void ewlc_focus_output(int direction);
void ewlc_next_master(int direction);
int ewlc_cleanup();

#endif // __EWLC_H_
