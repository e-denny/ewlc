#include <emacs-module.h>
#include "ewlc.h"

/* Declare mandatory GPL symbol.  */
int plugin_is_GPL_compatible;

static emacs_value Fewlc_start(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  ewlc_start();
  return env->make_integer(env, 0);
}

static emacs_value Fewlc_display_dispatch(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  int r;
  r = ewlc_display_dispatch();
  return env->make_integer(env, r);
}

static emacs_value Fewlc_cleanup(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  int r = ewlc_cleanup();
  return env->make_integer(env, r);
}

/* Bind NAME to FUN.  */
static void bind_function(emacs_env *env, const char *name, emacs_value Sfun)
{
  emacs_value Qfset = env->intern(env, "fset");
  emacs_value Qsym = env->intern(env, name);

  emacs_value args[] = { Qsym, Sfun };

  env->funcall(env, Qfset, 2, args);
}

/* Provide FEATURE to Emacs.  */
static void provide(emacs_env *env, const char *feature)
{
  emacs_value Qfeat = env->intern(env, feature);
  emacs_value Qprovide = env->intern(env, "provide");
  emacs_value args[] = { Qfeat };

  env->funcall(env, Qprovide, 1, args);
}

int emacs_module_init(struct emacs_runtime *ert)
{
  emacs_env *env = ert->get_environment(ert);
  emacs_value func;

  func = env->make_function(env, 0, 0, Fewlc_start, "Start the compositor.", NULL);
  bind_function(env, "ewlc-start", func);

  func = env->make_function(env, 0, 0, Fewlc_display_dispatch, "Flush the compositor events.", NULL);
  bind_function(env, "ewlc-display-dispatch", func);

  func = env->make_function(env, 0, 0, Fewlc_cleanup, "Cleanup after exit.", NULL);
  bind_function(env, "ewlc-cleanup", func);

  provide(env, "ewlc");

  /* loaded successfully */
  return 0;
}
