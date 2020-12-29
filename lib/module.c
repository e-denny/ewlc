#include "server.h"
#include "commands.h"
#include "keyboard.h"
#include "module.h"
#include "client.h"
#include "util.h"
#include <emacs-module.h>
#include <wayland-client.h>
#include <wayland-server-core.h>
#include <stdio.h>
#include <stdlib.h>

emacs_value Qt;
emacs_value Qnil;
emacs_value Flist;
emacs_value Flength;
emacs_value Fnth;
emacs_value Fewlc_apply_keybinding;

/* Declare mandatory GPL symbol.  */
int plugin_is_GPL_compatible;

int string_bytes(emacs_env *env, emacs_value string) {
  ptrdiff_t size = 0;
  env->copy_string_contents(env, string, NULL, &size);
  return size;
}

static emacs_value Fewlc_compare_clients(emacs_env *env, ptrdiff_t nargs,
                                         emacs_value args[], void *data)
{
    struct ewlc_client *c_a = env->get_user_ptr(env, args[0]);
    struct ewlc_client *c_b = env->get_user_ptr(env, args[1]);
    if (c_a == c_b)
        return Qt;
    return Qnil;
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

static emacs_value Fewlc_add_master(emacs_env *env, ptrdiff_t nargs,
                                    emacs_value args[], void *data)
{
    struct ewlc_server *server = env->get_user_ptr(env, args[0]);
    int delta = env->extract_integer(env, args[1]);

    ewlc_add_master(server, delta);
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
        cmd_args[1] = malloc(sizeof(char) * len);
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

    srv = env->get_user_ptr(env, args[0]);
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

emacs_value Fewlc_handle_events(emacs_env *env, ptrdiff_t nargs,
                                emacs_value args[], void *e_data)
{
    /* Get the next pending event and pass it to it's handler.
     * This called within the emacs ewlc event loop. */
    struct ewlc_server *srv;
    int handled;

    srv = env->get_user_ptr(env, args[0]);
    handled = handle_events(env, srv);
    if (handled == 1)
        return Qt;
    return Qnil;
}

void e_message(emacs_env *env, char *msg_str)
{
    emacs_value e_msg;
    e_msg = env->make_string(env, msg_str, strlen(msg_str));
    env->funcall(env, env->intern(env, "message"), 1, (emacs_value[]){e_msg});
}

//------------------------------------

emacs_value Fewlc_get_active_client(emacs_env *env, ptrdiff_t nargs,
                                    emacs_value args[], void *data)
{
    struct ewlc_server *srv;
    struct ewlc_client *c;

    srv = env->get_user_ptr(env, args[0]);
    c = get_active_client(srv);
    return env->make_user_ptr(env, NULL, c);
}

emacs_value Fewlc_get_active_output(emacs_env *env, ptrdiff_t nargs,
                                    emacs_value args[], void *data)
{
    struct ewlc_server *srv;
    struct ewlc_output *o;

    srv = env->get_user_ptr(env, args[0]);
    o = srv->active_output;
    return env->make_user_ptr(env, NULL, o);
}

emacs_value Fewlc_get_client_list(emacs_env *env, ptrdiff_t nargs,
                                  emacs_value args[], void *data)
{
    struct ewlc_server *srv;
    struct ewlc_client *c;
    emacs_value elements[128];
    int len = 0;

    srv = env->get_user_ptr(env, args[0]);

    wl_list_for_each(c, &srv->client_list, client_link)
    {
        elements[len++] = env->make_user_ptr(env, NULL, c);
    }
    return list(env, elements, len);
}

emacs_value Fewlc_set_focus_list(emacs_env *env, ptrdiff_t nargs,
                                 emacs_value args[], void *data)
{
    emacs_value focus_lst, val;
    int len;
    struct ewlc_server *srv;
    struct ewlc_client *c;

    srv = env->get_user_ptr(env, args[0]);
    focus_lst = args[1];
    len = length(env, focus_lst);
    val = nth(env, 0, focus_lst);

    if (len > 0) {
        wl_list_init(&srv->client_focus_list);
        for (int i = len - 1; i >= 0; i--) {
            val = nth(env, i, focus_lst);
            c = env->get_user_ptr(env, val);
            wl_list_insert(&srv->client_focus_list, &c->client_focus_link);
        }
    }
    return Qt;
}


emacs_value Fewlc_get_focus_list(emacs_env *env, ptrdiff_t nargs,
                                 emacs_value args[], void *data)
{
    struct ewlc_server *srv;
    struct ewlc_client *c;
    emacs_value elements[128];
    int len = 0;

    srv = env->get_user_ptr(env, args[0]);

    wl_list_for_each(c, &srv->client_focus_list, client_focus_link)
    {
        elements[len++] = env->make_user_ptr(env, NULL, c);
    }
    return list(env, elements, len);
}

emacs_value Fewlc_set_stack_list(emacs_env *env, ptrdiff_t nargs,
                                 emacs_value args[], void *data)
{
    emacs_value stack_lst, val;
    int len;
    struct ewlc_server *srv;
    struct ewlc_client *c;

    srv = env->get_user_ptr(env, args[0]);
    stack_lst = args[1];
    len = length(env, stack_lst);
    val = nth(env, 0, stack_lst);

    if (len > 0) {
        wl_list_init(&srv->client_stack_list);
        for (int i = len - 1; i >= 0; i--) {
            val = nth(env, i, stack_lst);
            c = env->get_user_ptr(env, val);
            wl_list_insert(&srv->client_stack_list, &c->client_stack_link);
        }
    }
    return Qt;
}

emacs_value Fewlc_get_stack_list(emacs_env *env, ptrdiff_t nargs,
                                 emacs_value args[], void *data)
{
    struct ewlc_server *srv;
    struct ewlc_client *c;
    emacs_value elements[128];
    int len = 0;

    srv = env->get_user_ptr(env, args[0]);

    wl_list_for_each(c, &srv->client_stack_list, client_stack_link)
    {
        elements[len++] = env->make_user_ptr(env, NULL, c);
    }
    return list(env, elements, len);
}

emacs_value list(emacs_env *env, emacs_value elements[], ptrdiff_t len)
{
    return env->funcall(env, Flist, len, elements);
}

int length(emacs_env *env, emacs_value lst)
{
    emacs_value len = env->funcall(env, Flength, 1, (emacs_value[]){lst});
    return env->extract_integer(env, len);
}

emacs_value nth(emacs_env *env, int idx, emacs_value lst)
{
    emacs_value e_idx = env->make_integer(env, idx);
    return env->funcall(env, Fnth, 2, (emacs_value[]){e_idx, lst});
}

emacs_value Fewlc_is_visible_on(emacs_env *env, ptrdiff_t nargs,
                                emacs_value args[], void *data)
{
    struct ewlc_client *c;
    struct ewlc_output *o;

    c = env->get_user_ptr(env, args[0]);
    o = env->get_user_ptr(env, args[1]);

    if (is_visible_on(c, o)) {
        return Qt;
    }
    return Qnil;
}

emacs_value Fewlc_focus_client(emacs_env *env, ptrdiff_t nargs,
                               emacs_value args[], void *data)
{

    struct ewlc_client *c_curr;
    struct ewlc_client *c_next;

    INFO(">>>");
    c_curr = env->get_user_ptr(env, args[0]);
    c_next = env->get_user_ptr(env, args[1]);

    e_focus_client(c_curr, c_next);
    INFO("<<<");
    return Qnil;
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
    Flist = env->make_global_ref(env, env->intern(env, "list"));
    Flength = env->make_global_ref(env, env->intern(env, "length"));
    Fnth = env->make_global_ref(env, env->intern(env, "nth"));

    /* functions */
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

    func = env->make_function(env, 2, 2, Fewlc_set_master_ratio,
                              "Adjust master ratio.", NULL);
    bind_function(env, "ewlc-set-master-ratio", func);

    func = env->make_function(env, 2, 2, Fewlc_add_master,
                              "Set next master.", NULL);
    bind_function(env, "ewlc-add-master", func);

    func = env->make_function(env, 1, 1, Fewlc_kill_client,
                              "Kill the focused client.", NULL);
    bind_function(env, "ewlc-kill-client", func);

    func = env->make_function(env, 1, 1, Fewlc_quit,
                              "Quit the window manager.", NULL);
    bind_function(env, "ewlc-quit", func);

    func = env->make_function(env, 1, 1, Fewlc_handle_keybindings,
                              "Handle the keybindings.", NULL);
    bind_function(env, "ewlc-handle-keybindings", func);

    func = env->make_function(env, 1, 1, Fewlc_handle_events,
                              "Handle the events.", NULL);
    bind_function(env, "ewlc-handle-events", func);

    func = env->make_function(env, 1, 1, Fewlc_zoom,
                              "Zoom.", NULL);
    bind_function(env, "ewlc-zoom", func);

    func = env->make_function(env, 1, 1, Fewlc_toggle_floating,
                              "Toggle Floating.", NULL);
    bind_function(env, "ewlc-toggle-floating", func);

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

    func = env->make_function(env, 1, 1, Fewlc_get_active_client,
                              "Get active client.", NULL);
    bind_function(env, "ewlc/c--get-active-client", func);

    func = env->make_function(env, 1, 1, Fewlc_get_active_output,
                              "Get active output.", NULL);
    bind_function(env, "ewlc/c--get-active-output", func);

    func = env->make_function(env, 1, 1, Fewlc_get_client_list,
                              "Get list of clients.", NULL);
    bind_function(env, "ewlc/c--get-client-list", func);

    func = env->make_function(env, 1, 1, Fewlc_get_stack_list,
                              "Get stack list of clients.", NULL);
    bind_function(env, "ewlc/c--get-stack-list", func);

    func = env->make_function(env, 2, 2, Fewlc_set_stack_list,
                              "Set stack list of clients.", NULL);
    bind_function(env, "ewlc/c--set-stack-list", func);

    func = env->make_function(env, 1, 1, Fewlc_get_focus_list,
                              "Get list of focused clients.", NULL);
    bind_function(env, "ewlc/c--get-focus-list", func);

    func = env->make_function(env, 2, 2, Fewlc_set_focus_list,
                              "Set focus list of clients.", NULL);
    bind_function(env, "ewlc/c--set-focus-list", func);

    func = env->make_function(env, 2, 2, Fewlc_is_visible_on,
                              "Is the client visible on the output.", NULL);
    bind_function(env, "ewlc/c--is-visible-on", func);

    func = env->make_function(env, 2, 2, Fewlc_focus_client,
                              "Focus the client.", NULL);
    bind_function(env, "ewlc/c--focus-client", func);

    func = env->make_function(env, 2, 2, Fewlc_compare_clients,
                              "Compare clients.", NULL);
    bind_function(env, "ewlc/c--client=", func);

    provide(env, "ewlc");

    /* loaded successfully */
    return 0;
}
