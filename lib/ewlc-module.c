#include "ewlc-module.h"
#include "ewlc.h"
#include <emacs-module.h>
#include <stdio.h>

emacs_value Qt;
emacs_value Qnil;
emacs_env *env;
emacs_value Fewlc_apply_keybinding;

/* Declare mandatory GPL symbol.  */
int plugin_is_GPL_compatible;

static emacs_value Fewlc_start(emacs_env *env, ptrdiff_t nargs,
                               emacs_value args[], void *data) {
  ewlc_start();
  return env->make_integer(env, 0);
}

static emacs_value Fewlc_display_dispatch(emacs_env *env, ptrdiff_t nargs,
                                          emacs_value args[], void *data) {
  int r;
  r = ewlc_display_dispatch();
  return env->make_integer(env, r);
}

static emacs_value Fewlc_cleanup(emacs_env *env, ptrdiff_t nargs,
                                 emacs_value args[], void *data) {
  int r = ewlc_cleanup();
  return env->make_integer(env, r);
}

static emacs_value Fewlc_focus_next_client(emacs_env *env, ptrdiff_t nargs,
                                           emacs_value args[], void *data) {
  int direction = env->extract_integer(env, args[0]);

  ewlc_focus_next_client(direction);
  return Qt;
}

/*------------------------------------------------------------------------*/

/* Bind NAME to FUN.  */
static void bind_function(emacs_env *env, const char *name, emacs_value Sfun) {
  emacs_value Qfset = env->intern(env, "fset");
  emacs_value Qsym = env->intern(env, name);

  emacs_value args[] = {Qsym, Sfun};

  env->funcall(env, Qfset, 2, args);
}

/* Provide FEATURE to Emacs.  */
static void provide(emacs_env *env, const char *feature) {
  emacs_value Qfeat = env->intern(env, feature);
  emacs_value Qprovide = env->intern(env, "provide");
  emacs_value args[] = {Qfeat};

  env->funcall(env, Qprovide, 1, args);
}

emacs_env *get_emacs_env() { return env; }

int emacs_module_init(struct emacs_runtime *ert) {
  emacs_value func;

  env = ert->get_environment(ert);
  printf("init of env: %p\n", env);

  /* symbols */
  Qt = env->make_global_ref(env, env->intern(env, "t"));
  Qnil = env->make_global_ref(env, env->intern(env, "nil"));

  func =
      env->make_function(env, 0, 0, Fewlc_start, "Start the compositor.", NULL);
  bind_function(env, "ewlc-start", func);

  func = env->make_function(env, 0, 0, Fewlc_display_dispatch,
                            "Flush the compositor events.", NULL);
  bind_function(env, "ewlc-display-dispatch", func);

  func =
      env->make_function(env, 0, 0, Fewlc_cleanup, "Cleanup after exit.", NULL);
  bind_function(env, "ewlc-cleanup", func);

  func = env->make_function(env, 1, 1, Fewlc_focus_next_client,
                            "Focus the next client.", NULL);
  bind_function(env, "ewlc-focus-next-client", func);

  func = env->make_function(env, 0, 0, Fewlc_handle_keybindings,
                            "Handle the keybindings.", NULL);
  bind_function(env, "ewlc-handle-keybindings", func);

  //  Fewlc_apply_keybinding = env->intern(env, "ewlc-apply-keybinding");

  /* func = env->make_function(env, 2, 2, Fewlc_apply_keybinding, */
  /*                           "Apply an emacs keybinding.", NULL); */
  /* bind_function(env, "ewlc-apply-keybinding", func); */

  provide(env, "ewlc");

  /* loaded successfully */
  return 0;
}
