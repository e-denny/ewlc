#include "module.h"
#include "Fwlr.h"
#include <emacs-module.h>
//#include <wayland-client.h>
//#include <wayland-server-core.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

emacs_value Qt;
emacs_value Qnil;

/* Declare mandatory GPL symbol.  */
int plugin_is_GPL_compatible;

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
    emacs_value Flist = env->intern(env, "list");
    return env->funcall(env, Flist, len, elements);
}

int length(emacs_env *env, emacs_value lst)
{
    emacs_value Flength = env->intern(env, "length");
    emacs_value len = env->funcall(env, Flength, 1, (emacs_value[]){lst});
    return env->extract_integer(env, len);
}

emacs_value nth(emacs_env *env, int idx, emacs_value lst)
{
    emacs_value Fnth = env->intern(env, "nth");
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

/*------------------------------------------------------------------------*/

int emacs_module_init(struct emacs_runtime *ert)
{
    emacs_env *env;

    env = ert->get_environment(ert);

    /* symbols */
    Qt = env->make_global_ref(env, env->intern(env, "t"));
    Qnil = env->make_global_ref(env, env->intern(env, "nil"));


    /* functions */
    init_wlr_pointer(env);
    init_wlr_backend(env);
    init_wlr_cursor(env);
    init_wlr_input_device(env);
    init_wlr_keyboard(env);
    init_wlr_output(env);
    init_wlr_output_layout(env);
    init_wlr_box(env);
    init_wlr_renderer(env);
    init_wlr_seat(env);
    init_wlr_surface(env);
    init_wlr_xcursor_manager(env);
    init_wlr_misc(env);
    init_wlr_xwayland(env);
    init_wlr_xdg_shell(env);
    init_wl(env);
    init_ewlc(env);

    provide(env, "ewlc");

    /* loaded successfully */
    return 0;
}
