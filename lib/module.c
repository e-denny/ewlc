#include "server.h"
#include "commands.h"
#include "keyboard.h"
#include "module.h"
#include "client.h"
#include "util.h"
#include "Fwlr.h"
#include <emacs-module.h>
#include <wayland-client.h>
#include <wayland-server-core.h>
#include <stdio.h>
#include <stdlib.h>

emacs_value Qt;
emacs_value Qnil;
emacs_value Qkeyboard;
emacs_value Qpointer;

emacs_value Flist;
emacs_value Flength;
emacs_value Fnth;
emacs_value Fewlc_apply_keybinding;

/* Declare mandatory GPL symbol.  */
int plugin_is_GPL_compatible;

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

//------------------------------------

int string_bytes(emacs_env *env, emacs_value string) {
    ptrdiff_t size = 0;
    env->copy_string_contents(env, string, NULL, &size);
    return size;
}

void e_message(emacs_env *env, char *msg_str)
{
    emacs_value e_msg;
    e_msg = env->make_string(env, msg_str, strlen(msg_str));
    env->funcall(env, env->intern(env, "message"), 1, (emacs_value[]){e_msg});
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

/*------------------------------------------------------------------------*/

/* Bind NAME to FUN.  */
void bind_function(emacs_env *env, const char *name, emacs_value Sfun)
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
    Qkeyboard = env->make_global_ref(env, env->intern(env, "keyboard"));
    Qpointer = env->make_global_ref(env, env->intern(env, "pointer"));

    Flist = env->make_global_ref(env, env->intern(env, "list"));
    Flength = env->make_global_ref(env, env->intern(env, "length"));
    Fnth = env->make_global_ref(env, env->intern(env, "nth"));

    /* functions */

    init_wlr_pointer(env);
    init_wlr_backend(env);
    init_wlr_cursor(env);
    init_wlr_input_device(env);
    init_wlr_keymap(env);
    init_wlr_output(env);
    init_wlr_box(env);

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

    /* ------------------------------------------------------------ */

    func = env->make_function(env, x, x,
                              Fewlc_create_pointer,
                              "Create a pointer.", NULL);
    bind_function(env, "ewlc--c/create-pointer", func);

    func = env->make_function(env, x, x,
                              Fewlc_create_keyboard,
                              "Create a keyboard.", NULL);
    bind_function(env, "ewlc--c/create-keyboard", func);

    func = env->make_function(env, x, x,
                              Fewlc_set_seat_capabilities,
                              "Set the seat's capabilites.", NULL);
    bind_function(env, "ewlc--c/create-set-seat-capabilites", func);

    func = env->make_function(env, x, x,
                              Fewlc_get_device_type,
                              "Get the input device type.", NULL);
    bind_function(env, "ewlc--c/get-device-type", func);

    /* ------------------------------------------------------------ */

    provide(env, "ewlc");

    /* loaded successfully */
    return 0;
}
