#include "server.h"
#include "commands.h"
#include "keyboard.h"
#include "module.h"
#include "util.h"
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
    struct ewlc_server *srv;

    srv = ewlc_start(env);
    return env->make_user_ptr(env, NULL, srv);
}

static emacs_value Fewlc_display_dispatch(emacs_env *env, ptrdiff_t nargs,
                                          emacs_value args[], void *data)
{
    int r;
    struct ewlc_server *srv = env->get_user_ptr(env, args[0]);
    r = ewlc_display_dispatch(srv);
    return env->make_integer(env, r);
}

static emacs_value Fewlc_cleanup(emacs_env *env, ptrdiff_t nargs,
                                 emacs_value args[], void *data)
{
    struct ewlc_server *srv = env->get_user_ptr(env, args[0]);
    int r = ewlc_cleanup(srv);
    return env->make_integer(env, r);
}

static emacs_value Fewlc_focus_next_client(emacs_env *env, ptrdiff_t nargs,
                                           emacs_value args[], void *data)
{
    struct ewlc_server *server = env->get_user_ptr(env, args[0]);
    int direction = env->extract_integer(env, args[1]);
    DEBUG("server = '%p'", server);
    DEBUG("direction = '%d'", direction);
    ewlc_focus_next_client(direction, server);
    return Qt;
}

static emacs_value Fewlc_set_master_ratio(emacs_env *env, ptrdiff_t nargs,
                                          emacs_value args[], void *data)
{
    struct ewlc_server *server = env->get_user_ptr(env, args[0]);
    float inc = env->extract_float(env, args[1]);

    ewlc_set_master_ratio(inc, server);
    return Qt;
}

static emacs_value Fewlc_focus_output(emacs_env *env, ptrdiff_t nargs,
                                      emacs_value args[], void *data)
{
    struct ewlc_server *server = env->get_user_ptr(env, args[0]);
    int direction = env->extract_integer(env, args[1]);

    ewlc_focus_output(direction, server);
    return Qt;
}

static emacs_value Fewlc_chvt(emacs_env *env, ptrdiff_t nargs,
                              emacs_value args[], void *data)
{
    struct ewlc_server *server = env->get_user_ptr(env, args[0]);
    int nbr = env->extract_integer(env, args[1]);

    ewlc_chvt(server, nbr);
    return Qt;
}

static emacs_value Fewlc_zoom(emacs_env *env, ptrdiff_t nargs,
                              emacs_value args[], void *data)
{
    struct ewlc_server *server = env->get_user_ptr(env, args[0]);

    ewlc_zoom(server);
    return Qt;
}

static emacs_value Fewlc_next_master(emacs_env *env, ptrdiff_t nargs,
                                     emacs_value args[], void *data)
{
    struct ewlc_server *server = env->get_user_ptr(env, args[0]);
    int direction = env->extract_integer(env, args[1]);

    ewlc_next_master(direction, server);
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
    struct ewlc_server *server = env->get_user_ptr(env, args[0]);
    ewlc_kill_client(server);
    return Qt;
}

static emacs_value Fewlc_toggle_floating(emacs_env *env, ptrdiff_t nargs,
                                         emacs_value args[], void *data)
{
    struct ewlc_server *server = env->get_user_ptr(env, args[0]);
    ewlc_toggle_floating(server);
    return Qt;
}

static emacs_value Fewlc_view(emacs_env *env, ptrdiff_t nargs,
                              emacs_value args[], void *data)
{
    struct ewlc_server *server = env->get_user_ptr(env, args[0]);
    ewlc_view(server);
    return Qt;
}

static emacs_value Fewlc_quit(emacs_env *env, ptrdiff_t nargs,
                              emacs_value args[], void *data)
{
    struct ewlc_server *server = env->get_user_ptr(env, args[0]);
    ewlc_quit(server);
    return Qt;
}

emacs_value Fewlc_handle_keybindings(emacs_env *env, ptrdiff_t nargs,
                                     emacs_value args[], void *data)
{
    /* Get the next pending key bindingd, pass it to emacs, or pass
       it to the client. This called within the emacs ewlc event loop. */
    int handled;
    uint32_t mods;
    xkb_keysym_t sym;
    char key_str[100];
    char mods_str[100];
    emacs_value e_key, e_mods, e_handled;
    struct ewlc_keyboard *kb;
    struct ewlc_server *srv;
    struct wlr_event_keyboard_key *event;

    srv= env->get_user_ptr(env, args[0]);
    if (srv->key_list == NULL)
        return Qnil;

    DEBUG("srv: %p", srv);
    handled = 0;
    mods = srv->key_list->mods;
    sym = srv->key_list->sym;
    event = srv->key_list->event;
    kb = srv->key_list->kb;

    if ((mods & WLR_MODIFIER_ALT) == WLR_MODIFIER_ALT) {
        strcpy(mods_str, "M-");
        DEBUG("mods_str: %s", mods_str);
        e_mods = env->make_string(env, mods_str, strlen(mods_str));

        if (xkb_keysym_get_name(sym, key_str, sizeof(key_str)) == -1)
            ERROR("xkb_keysym_get_name failed.:");
        DEBUG("key_str: %s", key_str);
        e_key = env->make_string(env, key_str, strlen(key_str));
        e_handled = env->funcall(env, env->intern(env, "ewlc-apply-keybinding"), 2,
                                 (emacs_value[]){e_mods, e_key});
        handled = env->extract_integer(env, e_handled);
    }

    srv->key_list = remove_from_start(srv->key_list);

    if (!handled) {
        /* Pass non-handled keycodes straight to client. */
        wlr_seat_set_keyboard(srv->seat, kb->device);
        wlr_seat_keyboard_notify_key(srv->seat, event->time_msec,
                                     event->keycode, event->state);
    }
    INFO("leaving");
    return Qt;
}

void e_message(emacs_env *env, char *msg_str)
{
    emacs_value e_msg;
    e_msg = env->make_string(env, msg_str, strlen(msg_str));
    env->funcall(env, env->intern(env, "message"), 1, (emacs_value[]){e_msg});
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

    func = env->make_function(env, 1, 1, Fewlc_display_dispatch,
                              "Flush the compositor events.", NULL);
    bind_function(env, "ewlc-display-dispatch", func);

    func = env->make_function(env, 1, 1, Fewlc_cleanup, "Cleanup after exit.",
                              NULL);
    bind_function(env, "ewlc-cleanup", func);

    func = env->make_function(env, 2, 2, Fewlc_focus_next_client,
                              "Focus the next client.", NULL);
    bind_function(env, "ewlc-focus-next-client", func);

    func = env->make_function(env, 1, 1, Fewlc_set_master_ratio,
                              "Adjust master ratio.", NULL);
    bind_function(env, "ewlc-set-master-ratio", func);

    func = env->make_function(env, 2, 2, Fewlc_next_master,
                              "Set next master.", NULL);
    bind_function(env, "ewlc-next-master", func);

    func = env->make_function(env, 1, 1, Fewlc_kill_client,
                              "Kill the focused client.", NULL);
    bind_function(env, "ewlc-kill-client", func);

    func = env->make_function(env, 1, 1, Fewlc_quit,
                              "Quit the window manager.", NULL);
    bind_function(env, "ewlc-quit", func);

    func = env->make_function(env, 1, 1, Fewlc_handle_keybindings,
                              "Handle the keybindings.", NULL);
    bind_function(env, "ewlc-handle-keybindings", func);

    func = env->make_function(env, 1, 1, Fewlc_zoom,
                              "Zoom.", NULL);
    bind_function(env, "ewlc-zoom", func);

    func = env->make_function(env, 1, 1, Fewlc_toggle_floating,
                              "Toggle Floating.", NULL);
    bind_function(env, "ewlc-toggle_floating", func);

    func = env->make_function(env, 2, 2, Fewlc_focus_output,
                              "focus output.", NULL);
    bind_function(env, "ewlc-focus-output", func);

    func = env->make_function(env, 1, 1, Fewlc_view,
                              "view.", NULL);
    bind_function(env, "ewlc-view", func);

    func = env->make_function(env, 1, 2, Fewlc_spawn,
                              "Spawn a command.", NULL);
    bind_function(env, "ewlc-spawn", func);

    func = env->make_function(env, 2, 2, Fewlc_chvt,
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
