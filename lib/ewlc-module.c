#include "ewlc-module.h"
#include "ewlc.h"
#include <emacs-module.h>
#include <stdio.h>
#include <stdlib.h>

emacs_value Qt;
emacs_value Qnil;
emacs_value Fewlc_apply_keybinding;

/* Declare mandatory GPL symbol.  */
int plugin_is_GPL_compatible;

int string_bytes(emacs_env *env, emacs_value string) {
  ptrdiff_t size = 0;
  env->copy_string_contents(env, string, NULL, &size);
  return size;
}
static emacs_value Fewlc_start(emacs_env *env, ptrdiff_t nargs,
                               emacs_value args[], void *data)
{
    ewlc_start(env);
    return env->make_integer(env, 0);
}

static emacs_value Fewlc_display_dispatch(emacs_env *env, ptrdiff_t nargs,
                                          emacs_value args[], void *data)
{
    int r;
    r = ewlc_display_dispatch();
    return env->make_integer(env, r);
}

static emacs_value Fewlc_cleanup(emacs_env *env, ptrdiff_t nargs,
                                 emacs_value args[], void *data)
{
    int r = ewlc_cleanup();
    return env->make_integer(env, r);
}

static emacs_value Fewlc_focus_next_client(emacs_env *env, ptrdiff_t nargs,
                                           emacs_value args[], void *data)
{
    int direction = env->extract_integer(env, args[0]);
    ewlc_focus_next_client(direction);
    return Qt;
}

static emacs_value Fewlc_set_master_ratio(emacs_env *env, ptrdiff_t nargs,
                                          emacs_value args[], void *data)
{
    float inc = env->extract_float(env, args[0]);
    ewlc_set_master_ratio(inc);
    return Qt;
}

static emacs_value Fewlc_focus_output(emacs_env *env, ptrdiff_t nargs,
                                      emacs_value args[], void *data)
{
    int direction = env->extract_integer(env, args[0]);
    ewlc_focus_output(direction);
    return Qt;
}

static emacs_value Fewlc_chvt(emacs_env *env, ptrdiff_t nargs,
                              emacs_value args[], void *data)
{
    int nbr = env->extract_integer(env, args[0]);
    ewlc_chvt(nbr);
    return Qt;
}

static emacs_value Fewlc_zoom(emacs_env *env, ptrdiff_t nargs,
                              emacs_value args[], void *data)
{
    ewlc_zoom();
    return Qt;
}

static emacs_value Fewlc_next_master(emacs_env *env, ptrdiff_t nargs,
                                     emacs_value args[], void *data)
{
    int direction = env->extract_integer(env, args[0]);
    ewlc_next_master(direction);
    return Qt;
}

static emacs_value Fewlc_spawn(emacs_env *env, ptrdiff_t nargs,
                               emacs_value args[], void *data)
{
    ptrdiff_t len;
    char *cmd, **cmd_args;

    len = string_bytes(env, args[0]);
    cmd = malloc(sizeof(char) * len);
    env->copy_string_contents(env, args[0], cmd, &len);
    if (nargs > 1) {
        len = string_bytes(env, args[1]);
        cmd_args = malloc(sizeof(char*));
        cmd_args[0] = malloc(sizeof(char) * len);
        env->copy_string_contents(env, args[1], cmd_args[0], &len);
    }
    ewlc_spawn(cmd, cmd_args);
    free(cmd);
    // FIXME: this is a mess, is wrong, and leaks.
    if (nargs > 1) free(cmd_args);
    return Qt;
}

static emacs_value Fewlc_kill_client(emacs_env *env, ptrdiff_t nargs,
                                     emacs_value args[], void *data)
{
    ewlc_kill_client();
    return Qt;
}

static emacs_value Fewlc_toggle_floating(emacs_env *env, ptrdiff_t nargs,
                                         emacs_value args[], void *data)
{
    ewlc_toggle_floating();
    return Qt;
}

static emacs_value Fewlc_view(emacs_env *env, ptrdiff_t nargs,
                              emacs_value args[], void *data)
{
    ewlc_view();
    return Qt;
}

static emacs_value Fewlc_quit(emacs_env *env, ptrdiff_t nargs,
                              emacs_value args[], void *data)
{
    ewlc_quit();
    return Qt;
}
/*------------------------------------------------------------------------*/

/* Bind NAME to FUN.  */
static void bind_function(emacs_env *env, const char *name, emacs_value Sfun)
{
    emacs_value Qfset = env->intern(env, "fset");
    emacs_value Qsym = env->intern(env, name);
    emacs_value args[] = {Qsym, Sfun};
    env->funcall(env, Qfset, 2, args);
}

/* Provide FEATURE to Emacs.  */
static void provide(emacs_env *env, const char *feature)
{
    emacs_value Qfeat = env->intern(env, feature);
    emacs_value Qprovide = env->intern(env, "provide");
    emacs_value args[] = {Qfeat};
    env->funcall(env, Qprovide, 1, args);
}

int emacs_module_init(struct emacs_runtime *ert)
{
    emacs_value func;
    emacs_env *env;

    env = ert->get_environment(ert);
    printf("init of env: %p\n", env);

    /* symbols */
    Qt = env->make_global_ref(env, env->intern(env, "t"));
    Qnil = env->make_global_ref(env, env->intern(env, "nil"));

    func = env->make_function(env, 0, 0, Fewlc_start, "Start the compositor.",
                              NULL);
    bind_function(env, "ewlc-start", func);

    func = env->make_function(env, 0, 0, Fewlc_display_dispatch,
                              "Flush the compositor events.", NULL);
    bind_function(env, "ewlc-display-dispatch", func);

    func = env->make_function(env, 0, 0, Fewlc_cleanup, "Cleanup after exit.",
                              NULL);
    bind_function(env, "ewlc-cleanup", func);

    func = env->make_function(env, 1, 1, Fewlc_focus_next_client,
                              "Focus the next client.", NULL);
    bind_function(env, "ewlc-focus-next-client", func);

    func = env->make_function(env, 1, 1, Fewlc_set_master_ratio,
                              "Adjust master ratio.", NULL);
    bind_function(env, "ewlc-set-master-ratio", func);

    func = env->make_function(env, 1, 1, Fewlc_next_master,
                              "Set next master.", NULL);
    bind_function(env, "ewlc-next-master", func);

    func = env->make_function(env, 0, 0, Fewlc_kill_client,
                              "Kill the focused client.", NULL);
    bind_function(env, "ewlc-kill-client", func);

    func = env->make_function(env, 1, 1, Fewlc_quit,
                              "Quit the window manager.", NULL);
    bind_function(env, "ewlc-quit", func);

    func = env->make_function(env, 0, 0, Fewlc_handle_keybindings,
                              "Handle the keybindings.", NULL);
    bind_function(env, "ewlc-handle-keybindings", func);

    func = env->make_function(env, 0, 0, Fewlc_zoom,
                              "Zoom.", NULL);
    bind_function(env, "ewlc-zoom", func);

    func = env->make_function(env, 0, 0, Fewlc_toggle_floating,
                              "Toggle Floating.", NULL);
    bind_function(env, "ewlc-toggle_floating", func);

    func = env->make_function(env, 1, 1, Fewlc_focus_output,
                              "focus output.", NULL);
    bind_function(env, "ewlc-focus-output", func);

    func = env->make_function(env, 0, 0, Fewlc_view,
                              "view.", NULL);
    bind_function(env, "ewlc-view", func);

    func = env->make_function(env, 1, 2, Fewlc_spawn,
                              "Spawn a command.", NULL);
    bind_function(env, "ewlc-spawn", func);

    func = env->make_function(env, 1, 1, Fewlc_chvt,
                              "Change VT.", NULL);
    bind_function(env, "ewlc-chvt", func);
    //  Fewlc_apply_keybinding = env->intern(env, "ewlc-apply-keybinding");

    /* func = env->make_function(env, 2, 2, Fewlc_apply_keybinding, */
    /*                           "Apply an emacs keybinding.", NULL); */
    /* bind_function(env, "ewlc-apply-keybinding", func); */

    provide(env, "ewlc");

    /* loaded successfully */
    return 0;
}
