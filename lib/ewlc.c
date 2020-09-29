/*
 p See LICENSE file for copyright and license details.
 */
#define _POSIX_C_SOURCE 200809L
#include <getopt.h>
#include <linux/input-event-codes.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>
#include <wayland-server-core.h>
#include <wayland-client.h>
#include <wlr/backend.h>
#include <wlr/render/wlr_renderer.h>
#include <wlr/types/wlr_compositor.h>
#include <wlr/types/wlr_cursor.h>
#include <wlr/types/wlr_data_device.h>
#include <wlr/types/wlr_export_dmabuf_v1.h>
#include <wlr/types/wlr_gamma_control_v1.h>
#include <wlr/types/wlr_input_device.h>
#include <wlr/types/wlr_keyboard.h>
#include <wlr/types/wlr_matrix.h>
#include <wlr/types/wlr_output.h>
#include <wlr/types/wlr_output_layout.h>
#include <wlr/types/wlr_pointer.h>
#include <wlr/types/wlr_primary_selection.h>
#include <wlr/types/wlr_primary_selection_v1.h>
#include <wlr/types/wlr_screencopy_v1.h>
#include <wlr/types/wlr_seat.h>
#include <wlr/types/wlr_viewporter.h>
#include <wlr/types/wlr_xcursor_manager.h>
#include <wlr/types/wlr_xdg_decoration_v1.h>
#include <wlr/types/wlr_xdg_output_v1.h>
#include <wlr/types/wlr_xdg_shell.h>
#include <wlr/util/log.h>
#include <xkbcommon/xkbcommon.h>
#ifdef XWAYLAND
#include <X11/Xlib.h>
#include <wlr/xwayland.h>
#endif
#include "ewlc.h"

/* macros */
#define ERROR(fmt, ...)         do { fprintf(stderr, fmt "\n", ##__VA_ARGS__); exit(EXIT_FAILURE); } while (0)
#define EERROR(fmt, ...)        ERROR(fmt ": %s", ##__VA_ARGS__, strerror(errno))
#define MAX(A, B)               ((A) > (B) ? (A) : (B))
#define MIN(A, B)               ((A) < (B) ? (A) : (B))
#define CLEANMASK(mask)         (mask & ~WLR_MODIFIER_CAPS)
#define LENGTH(X)               (sizeof X / sizeof X[0])
#define END(A)                  ((A) + LENGTH(A))

/* enums */
enum { CUR_NORMAL, CUR_MOVE, CUR_RESIZE }; /* cursor */

#ifdef XWAYLAND
enum { NetWMWindowTypeDialog, NetWMWindowTypeSplash, NetWMWindowTypeToolbar,
        NetWMWindowTypeUtility, NetLast }; /* EWMH atoms */

enum { XDG_SHELL, X11_MANAGED, X11_UNMANAGED }; /* client types */
#endif

struct ewlc_output;

/*  structs  */
struct ewlc_output {
        struct wl_list link;
        struct wlr_output *wlr_output;
        struct wl_listener frame;
        struct wl_listener destroy;
        struct wlr_box m;      /* output area */
        struct wlr_box w;      /* window area */
        double master_ratio;
        int num_master;
};

struct ewlc_client {
        struct wl_list link;
        struct wl_list focus_link;
        struct wl_list stack_link;
        union {
                struct wlr_xdg_surface *xdg;
#ifdef XWAYLAND
                struct wlr_xwayland_surface *xwayland;
#endif
        } surface;
#ifdef XWAYLAND
        struct wl_listener activate;
        unsigned int type;
#endif
        struct wl_listener commit;
        struct wl_listener map;
        struct wl_listener unmap;
        struct wl_listener destroy;
        struct wlr_box geom;  /* layout-relative, includes border */
        struct ewlc_output *output;
        int border_width;
        int is_floating;
        uint32_t resize; /* configure serial of a pending resize */
};

struct ewlc_server {
        struct wl_display *display;
        struct wlr_renderer *renderer;
        struct wlr_compositor *compositor;

        struct wlr_xdg_shell *xdg_shell;
        struct wl_listener new_xdg_surface;

        struct wlr_cursor *cursor;
        struct wlr_xcursor_manager *cursor_mgr;
        struct wl_listener cursor_axis;
        struct wl_listener cursor_button;
        struct wl_listener cursor_frame;
        struct wl_listener cursor_motion;
        struct wl_listener cursor_motion_absolute;

        struct wlr_seat *seat;
        struct wl_listener seat_request_cursor;
        struct wl_listener seat_request_set_selection;
        struct wl_listener seat_request_set_primary_selection;


        struct wlr_backend *backend;
        struct wl_listener backend_new_input;

        struct wl_list keyboards;

        struct wlr_output_layout *output_layout;
        struct wl_list outputs;
        struct wl_listener backend_new_output;

        unsigned int cursor_mode;

        struct ewlc_client *grabbed_client;
        int grabc_x, grabc_y; /* client-relative */
        struct wlr_box output_geom;

        struct wlr_xdg_decoration_manager_v1 *xdeco_mgr;
        struct wl_listener new_xdeco;

#ifdef XWAYLAND
        struct wlr_xwayland *xwayland;
        struct wl_listener new_xwayland_surface;
        struct wl_listener xwayland_ready;
#endif

        struct wl_list clients; /* tiling order */
        struct wl_list focus_stack;  /* focus order */
        struct wl_list independents;
        struct wl_list stack;   /* stacking z-order */

        pid_t startup_pid;
};

typedef union {
        int i;
        unsigned int ui;
        float f;
        const void *v;
} Arg;

typedef struct {
        unsigned int mod;
        unsigned int button;
        void (*func)(const Arg *);
        const Arg arg;
} Button;

struct ewlc_decoration {
        struct wl_listener request_mode;
        struct wl_listener destroy;
};

typedef struct {
        uint32_t mod;
        xkb_keysym_t keysym;
        void (*func)(const Arg *);
        const Arg arg;
} Key;

struct ewlc_keyboard {
        struct wl_list link;
        struct wlr_input_device *device;

        struct wl_listener modifiers;
        struct wl_listener key;
        struct wl_listener destroy;
};

struct ewlc_output_rule {
        const char *name;
        float master_ratio;
        int num_master;
        float scale;
        enum wl_output_transform rr;
};

/* Used to move all of the data necessary to render a surface from the top-level
 * frame handler to the per-surface render function. */
struct render_data {
        struct wlr_output *output;
        struct timespec *when;
        int x, y; /* layout-relative */
};

/* function declarations */
static void apply_bounds(struct ewlc_client *c, struct wlr_box *bbox);
static void arrange(struct ewlc_output *o);
static void chvt(const Arg *arg);
static void cleanup_keyboard(struct wl_listener *listener, void *data);
static void cleanup_output(struct wl_listener *listener, void *data);
static void commit_notify(struct wl_listener *listener, void *data);
static void create_keyboard(struct wlr_input_device *device);
static void new_xdg_surface_notify(struct wl_listener *listener, void *data);
static void create_pointer(struct wlr_input_device *device);
static void new_toplevel_decoration_notify(struct wl_listener *listener, void *data);
static void destroy_notify(struct wl_listener *listener, void *data);
static void destroy_xdeco(struct wl_listener *listener, void *data);
static struct ewlc_output *get_next_output(int dir);
static void focus_client(struct ewlc_client *old, struct ewlc_client *c, int lift);
static void focus_output(const Arg *arg);
static void focus_next_client(const Arg *arg);
static struct ewlc_client *focus_top(struct ewlc_output *o);
static void get_xdeco_mode(struct wl_listener *listener, void *data);
static void incn_master(const Arg *arg);
static int keybinding(uint32_t mods, xkb_keysym_t sym);
static void keypress(struct wl_listener *listener, void *data);
static void keypress_mod(struct wl_listener *listener, void *data);
static void kill_client(const Arg *arg);
static void map_request(struct wl_listener *listener, void *data);
static void motion_notify(uint32_t time);
static void move_resize(const Arg *arg);
static void pointer_focus(struct ewlc_client *c, struct wlr_surface *surface,
                          double sx, double sy, uint32_t time);
static void quit(const Arg *arg);
static void render(struct wlr_surface *surface, int sx, int sy, void *data);
static void render_clients(struct ewlc_output *o, struct timespec *now);
static void render_output(struct wl_listener *listener, void *data);
static void resize(struct ewlc_client *c, int x, int y, int w, int h, int interact);
static void scale_box(struct wlr_box *box, float scale);
static struct ewlc_client *get_active_client(void);
static void set_floating(struct ewlc_client *c, int floating);
static void set_master_ratio(const Arg *arg);
static void set_output(struct ewlc_client *c, struct ewlc_output *o);
static void sigchld(int unused);
static void spawn(const Arg *arg);
static void tile(struct ewlc_output *o);
static void toggle_floating(const Arg *arg);
static void unmap_notify(struct wl_listener *listener, void *data);
static void view(const Arg *arg);
static struct ewlc_client *get_client_at_point(double x, double y);
static struct ewlc_output *get_output_at_point(double x, double y);
static void zoom(const Arg *arg);
static bool is_visible_on(struct ewlc_client *c, struct ewlc_output *o);
static void apply_title(struct ewlc_client *c);

static void cursor_axis_notify(struct wl_listener *listener, void *data);
static void cursor_button_notify(struct wl_listener *listener, void *data);
static void cursor_frame_notify(struct wl_listener *listener, void *data);
static void cursor_motion_notify(struct wl_listener *listener, void *data);
static void cursor_motion_absolute_notify(struct wl_listener *listener, void *data);

static void seat_request_cursor_notify(struct wl_listener *listener, void *data);
static void seat_request_set_selection_notify(struct wl_listener *listener, void *data);
static void seat_request_set_primary_selection_notify(struct wl_listener *listener, void *data);

static void backend_new_input_notify(struct wl_listener *listener, void *data);
static void backend_new_output_notify(struct wl_listener *listener, void *data);

static struct wlr_surface *get_surface(struct ewlc_client *c);

#ifdef XWAYLAND
static void activate_x11(struct wl_listener *listener, void *data);
static void new_xwayland_surface_notify(struct wl_listener *listener, void *data);
static Atom get_atom(xcb_connection_t *xc, const char *name);
static void render_independents(struct wlr_output *output, struct timespec *now);
static void update_window_type(struct ewlc_client *c);
static void xwayland_ready_notify(struct wl_listener *listener, void *data);
static struct ewlc_client *get_independent_at_point(double x, double y);
#endif

/* global variables */
static const char broken[] = "broken";

struct ewlc_server server;

static struct ewlc_output *active_output;

#ifdef XWAYLAND
static Atom netatom[NetLast];
#endif

/* appearance */
static const int sloppyfocus        = 1;  /* focus follows mouse */
static const unsigned int border_px  = 1;  /* border pixel of windows */
static const float root_color[]      = {0.3, 0.3, 0.3, 1.0};
static const float border_color[]    = {0.5, 0.5, 0.5, 1.0};
static const float focus_color[]     = {1.0, 0.0, 0.0, 1.0};

/* outputs */
static const struct ewlc_output_rule output_rules[] = {
        /* name       master_ratio num_master scale  rotate/reflect */
        /* example of a HiDPI laptop monitor:
        { "eDP-1",    0.5,  1,      2,    WL_OUTPUT_TRANSFORM_NORMAL },
        */
        /* defaults */
        { NULL,       0.5, 1,      1,    WL_OUTPUT_TRANSFORM_NORMAL },
};

/* keyboard */
static const struct xkb_rule_names xkb_rules = {
        /* can specify fields: rules, model, layout, variant, options */
        /* example:
        .options = "ctrl:nocaps",
        */
};
static const int repeat_rate = 25;
static const int repeat_delay = 600;

#define MODKEY WLR_MODIFIER_ALT

/* helper for spawning shell commands in the pre dwm-5.0 fashion */
#define SHCMD(cmd) { .v = (const char*[]){ "/bin/sh", "-c", cmd, NULL } }

/* commands */
static const char *termcmd[]  = { "alacritty", NULL };

static const Key keys[] = {
        /* Note that Shift changes certain key codes: c -> C, 2 -> at, etc. */
        /* modifier                  key                 function        argument */
        { MODKEY|WLR_MODIFIER_SHIFT, XKB_KEY_Return,     spawn,          {.v = termcmd} },
        { MODKEY,                    XKB_KEY_j,          focus_next_client,     {.i = +1} },
        { MODKEY,                    XKB_KEY_k,          focus_next_client,     {.i = -1} },
        { MODKEY,                    XKB_KEY_i,          incn_master,     {.i = +1} },
        { MODKEY,                    XKB_KEY_d,          incn_master,     {.i = -1} },
        { MODKEY,                    XKB_KEY_h,          set_master_ratio,       {.f = -0.05} },
        { MODKEY,                    XKB_KEY_l,          set_master_ratio,       {.f = +0.05} },
        { MODKEY,                    XKB_KEY_Return,     zoom,           {0} },
        { MODKEY,                    XKB_KEY_Tab,        view,           {0} },
        { MODKEY|WLR_MODIFIER_SHIFT, XKB_KEY_C,          kill_client,     {0} },
        { MODKEY|WLR_MODIFIER_SHIFT, XKB_KEY_space,      toggle_floating, {0} },
        { MODKEY,                    XKB_KEY_0,          view,           {.ui = ~0} },
        { MODKEY,                    XKB_KEY_comma,      focus_output,       {.i = -1} },
        { MODKEY,                    XKB_KEY_period,     focus_output,       {.i = +1} },
        { MODKEY|WLR_MODIFIER_SHIFT, XKB_KEY_Q,          quit,           {0} },
        { WLR_MODIFIER_CTRL|WLR_MODIFIER_ALT,XKB_KEY_Terminate_Server, quit, {0} },
#define CHVT(n) { WLR_MODIFIER_CTRL|WLR_MODIFIER_ALT,XKB_KEY_XF86Switch_VT_##n, chvt, {.ui = (n)} }
        CHVT(1), CHVT(2), CHVT(3), CHVT(4), CHVT(5), CHVT(6),
        CHVT(7), CHVT(8), CHVT(9), CHVT(10), CHVT(11), CHVT(12),
};

static const Button buttons[] = {
        { MODKEY, BTN_LEFT,   move_resize,     {.ui = CUR_MOVE} },
        { MODKEY, BTN_MIDDLE, toggle_floating, {0} },
        { MODKEY, BTN_RIGHT,  move_resize,     {.ui = CUR_RESIZE} },
};

/* function implementations */

struct wlr_surface *get_surface(struct ewlc_client *c)
{
#ifdef XWAYLAND
        if (c->type != XDG_SHELL)
                return c->surface.xwayland->surface;
#endif
        return c->surface.xdg->surface;
}


bool is_visible_on(struct ewlc_client *c, struct ewlc_output *o)
{
        if (c->output == o)
                return true;
        return false;
}

void apply_bounds(struct ewlc_client *c, struct wlr_box *bbox)
{
        /* set minimum possible */
        c->geom.width = MAX(1, c->geom.width);
        c->geom.height = MAX(1, c->geom.height);

        if (c->geom.x >= bbox->x + bbox->width)
                c->geom.x = bbox->x + bbox->width - c->geom.width;
        if (c->geom.y >= bbox->y + bbox->height)
                c->geom.y = bbox->y + bbox->height - c->geom.height;
        if (c->geom.x + c->geom.width + 2 * c->border_width <= bbox->x)
                c->geom.x = bbox->x;
        if (c->geom.y + c->geom.height + 2 * c->border_width <= bbox->y)
                c->geom.y = bbox->y;
}

void apply_title(struct ewlc_client *c)
{
        const char *appid, *title;

        /* rule matching */
#ifdef XWAYLAND
        if (c->type != XDG_SHELL) {
                update_window_type(c);
                appid = c->surface.xwayland->class;
                title = c->surface.xwayland->title;
        } else
#endif
        {
                appid = c->surface.xdg->toplevel->app_id;
                title = c->surface.xdg->toplevel->title;
        }
        if (!appid)
                appid = broken;
        if (!title)
                title = broken;

        set_output(c, active_output);
}

void arrange(struct ewlc_output *o)
{
        /* Get effective output geometry to use for window area */
        o->m = *wlr_output_layout_get_box(server.output_layout, o->wlr_output);
        o->w = o->m;
        tile(o);
        /* XXX recheck pointer focus here... or in resize()? */
}

void cursor_axis_notify(struct wl_listener *listener, void *data)
{
        /* This event is forwarded by the cursor when a pointer emits an axis event,
         * for example when you move the scroll wheel. */
        struct wlr_event_pointer_axis *event = data;
        /* Notify the client with pointer focus of the axis event. */
        wlr_seat_pointer_notify_axis(server.seat,
                                     event->time_msec, event->orientation, event->delta,
                                     event->delta_discrete, event->source);
}

void cursor_button_notify(struct wl_listener *listener, void *data)
{
        struct wlr_event_pointer_button *event = data;
        struct wlr_keyboard *keyboard;
        uint32_t mods;
        struct ewlc_client *c;
        const Button *b;

        switch (event->state) {
        case WLR_BUTTON_PRESSED:;
                /* Change focus if the button was _pressed_ over a client */
                if ((c = get_client_at_point(server.cursor->x, server.cursor->y)))
                        focus_client(get_active_client(), c, 1);

                keyboard = wlr_seat_get_keyboard(server.seat);
                mods = wlr_keyboard_get_modifiers(keyboard);
                for (b = buttons; b < END(buttons); b++) {
                        if (CLEANMASK(mods) == CLEANMASK(b->mod) &&
                                        event->button == b->button && b->func) {
                                b->func(&b->arg);
                                return;
                        }
                }
                break;
        case WLR_BUTTON_RELEASED:
                /* If you released any buttons, we exit interactive move/resize mode. */
                /* XXX should reset to the pointer focus's current setcursor */
                if (server.cursor_mode != CUR_NORMAL) {
                        wlr_xcursor_manager_set_cursor_image(server.cursor_mgr,
                                                             "left_ptr", server.cursor);
                        server.cursor_mode = CUR_NORMAL;
                        /* Drop the window off on its new output */
                        active_output = get_output_at_point(server.cursor->x, server.cursor->y);
                        set_output(server.grabbed_client, active_output);
                        return;
                }
                break;
        }
        /* If the event wasn't handled by the compositor, notify the client with
         * pointer focus that a button press has occurred */
        wlr_seat_pointer_notify_button(server.seat,
                                       event->time_msec, event->button, event->state);
}

void chvt(const Arg *arg)
{
        wlr_session_change_vt(wlr_backend_get_session(server.backend), arg->ui);
}


void cleanup_keyboard(struct wl_listener *listener, void *data)
{
        struct wlr_input_device *device = data;
        struct ewlc_keyboard *kb = device->data;

        wl_list_remove(&kb->destroy.link);
        free(kb);
}

void cleanup_output(struct wl_listener *listener, void *data)
{
        struct wlr_output *wlr_output = data;
        struct ewlc_output *o = wlr_output->data;

        wl_list_remove(&o->destroy.link);
        free(o);
}

void commit_notify(struct wl_listener *listener, void *data)
{
        struct ewlc_client *c = wl_container_of(listener, c, commit);

        /* mark a pending resize as completed */
        if (c->resize && c->resize <= c->surface.xdg->configure_serial)
                c->resize = 0;
}

void create_keyboard(struct wlr_input_device *device)
{
        struct xkb_context *context;
        struct xkb_keymap *keymap;
        struct ewlc_keyboard *kb;

        kb = device->data = calloc(1, sizeof(*kb));
        kb->device = device;

        /* Prepare an XKB keymap and assign it to the keyboard. */
        context = xkb_context_new(XKB_CONTEXT_NO_FLAGS);
        keymap = xkb_map_new_from_names(context, &xkb_rules,
                XKB_KEYMAP_COMPILE_NO_FLAGS);

        wlr_keyboard_set_keymap(device->keyboard, keymap);
        xkb_keymap_unref(keymap);
        xkb_context_unref(context);
        wlr_keyboard_set_repeat_info(device->keyboard, repeat_rate, repeat_delay);

        /* Here we set up listeners for keyboard events. */
        kb->modifiers.notify = keypress_mod;
        wl_signal_add(&device->keyboard->events.modifiers, &kb->modifiers);
        kb->key.notify = keypress;
        wl_signal_add(&device->keyboard->events.key, &kb->key);
        kb->destroy.notify = cleanup_keyboard;
        wl_signal_add(&device->events.destroy, &kb->destroy);

        wlr_seat_set_keyboard(server.seat, device);

        /* And add the keyboard to our list of keyboards */
        wl_list_insert(&server.keyboards, &kb->link);
}

void backend_new_output_notify(struct wl_listener *listener, void *data)
{
        /* This event is raised by the backend when a new output (aka a display or
         * output) becomes available. */
        struct wlr_output *wlr_output = data;
        struct ewlc_output *o;
        const struct ewlc_output_rule *r;

        /* The mode is a tuple of (width, height, refresh rate), and each
         * output supports only a specific set of modes. We just pick the
         * output's preferred mode; a more sophisticated compositor would let
         * the user configure it. */
        wlr_output_set_mode(wlr_output, wlr_output_preferred_mode(wlr_output));

        /* Allocates and configures output state using configured rules */
        o = wlr_output->data = calloc(1, sizeof(*o));
        o->wlr_output = wlr_output;
        for (r = output_rules; r < END(output_rules); r++) {
                if (!r->name || strstr(wlr_output->name, r->name)) {
                        o->master_ratio = r->master_ratio;
                        o->num_master = r->num_master;
                        wlr_output_set_scale(wlr_output, r->scale);
                        wlr_xcursor_manager_load(server.cursor_mgr, r->scale);
                        wlr_output_set_transform(wlr_output, r->rr);
                        break;
                }
        }
        /* Set up event listeners */
        o->frame.notify = render_output;
        wl_signal_add(&wlr_output->events.frame, &o->frame);
        o->destroy.notify = cleanup_output;
        wl_signal_add(&wlr_output->events.destroy, &o->destroy);

        wl_list_insert(&server.outputs, &o->link);

        wlr_output_enable(wlr_output, 1);
        if (!wlr_output_commit(wlr_output))
                return;

        /* Adds this to the output layout. The add_auto function arranges outputs
         * from left-to-right in the order they appear. A more sophisticated
         * compositor would let the user configure the arrangement of outputs in the
         * layout.
         *
         * The output layout utility automatically adds a wl_output global to the
         * display, which Wayland clients can see to find out information about the
         * output (such as DPI, scale factor, manufacturer, etc).
         */
        wlr_output_layout_add_auto(server.output_layout, wlr_output);
        server.output_geom = *wlr_output_layout_get_box(server.output_layout, NULL);
}

void new_xdg_surface_notify(struct wl_listener *listener, void *data)
{
        /* This event is raised when wlr_xdg_shell receives a new xdg surface from a
         * client, either a toplevel (application window) or popup. */
        struct wlr_xdg_surface *xdg_surface = data;
        struct ewlc_client *c;

        if (xdg_surface->role != WLR_XDG_SURFACE_ROLE_TOPLEVEL)
                return;

        /* Allocate a Client for this surface */
        c = xdg_surface->data = calloc(1, sizeof(*c));
        c->surface.xdg = xdg_surface;
        c->border_width = border_px;

        /* Tell the client not to try anything fancy */
        wlr_xdg_toplevel_set_tiled(c->surface.xdg, WLR_EDGE_TOP |
                        WLR_EDGE_BOTTOM | WLR_EDGE_LEFT | WLR_EDGE_RIGHT);

        /* Listen to the various events it can emit */
        c->commit.notify = commit_notify;
        wl_signal_add(&xdg_surface->surface->events.commit, &c->commit);
        c->map.notify = map_request;
        wl_signal_add(&xdg_surface->events.map, &c->map);
        c->unmap.notify = unmap_notify;
        wl_signal_add(&xdg_surface->events.unmap, &c->unmap);
        c->destroy.notify = destroy_notify;
        wl_signal_add(&xdg_surface->events.destroy, &c->destroy);
}

void create_pointer(struct wlr_input_device *device)
{
        /* We don't do anything special with pointers. All of our pointer handling
         * is proxied through wlr_cursor. On another compositor, you might take this
         * opportunity to do libinput configuration on the device to set
         * acceleration, etc. */
        wlr_cursor_attach_input_device(server.cursor, device);
}

void new_toplevel_decoration_notify(struct wl_listener *listener, void *data)
{
        struct wlr_xdg_toplevel_decoration_v1 *wlr_deco = data;
        struct ewlc_decoration *d = wlr_deco->data = calloc(1, sizeof(*d));

        wl_signal_add(&wlr_deco->events.request_mode, &d->request_mode);
        d->request_mode.notify = get_xdeco_mode;
        wl_signal_add(&wlr_deco->events.destroy, &d->destroy);
        d->destroy.notify = destroy_xdeco;

        get_xdeco_mode(&d->request_mode, wlr_deco);
}


void cursor_frame_notify(struct wl_listener *listener, void *data)
{
        /* This event is forwarded by the cursor when a pointer emits an frame
         * event. Frame events are sent after regular pointer events to group
         * multiple events together. For instance, two axis events may happen at the
         * same time, in which case a frame event won't be sent in between. */
        /* Notify the client with pointer focus of the frame event. */
        wlr_seat_pointer_notify_frame(server.seat);
}

void destroy_notify(struct wl_listener *listener, void *data)
{
        /* Called when the surface is destroyed and should never be shown again. */
        struct ewlc_client *c = wl_container_of(listener, c, destroy);
        wl_list_remove(&c->map.link);
        wl_list_remove(&c->unmap.link);
        wl_list_remove(&c->destroy.link);
#ifdef XWAYLAND
        if (c->type == X11_MANAGED)
                wl_list_remove(&c->activate.link);
        else if (c->type == XDG_SHELL)
#endif
                wl_list_remove(&c->commit.link);
        free(c);
}

void destroy_xdeco(struct wl_listener *listener, void *data)
{
        struct wlr_xdg_toplevel_decoration_v1 *wlr_deco = data;
        struct ewlc_decoration *d = wlr_deco->data;

        wl_list_remove(&d->destroy.link);
        wl_list_remove(&d->request_mode.link);
        free(d);
}

struct ewlc_output *get_next_output(int direction)
{
        struct ewlc_output *o;

        if (direction > 0) {
                if (active_output->link.next == &server.outputs)
                        return wl_container_of(server.outputs.next, o, link);
                return wl_container_of(active_output->link.next, o, link);
        } else {
                if (active_output->link.prev == &server.outputs)
                        return wl_container_of(server.outputs.prev, o, link);
                return wl_container_of(active_output->link.prev, o, link);
        }
}

void focus_client(struct ewlc_client *old, struct ewlc_client *c, int lift)
{
        struct wlr_keyboard *kb = wlr_seat_get_keyboard(server.seat);

        /* Raise client in stacking order if requested */
        if (c && lift) {
                wl_list_remove(&c->stack_link);
                wl_list_insert(&server.stack, &c->stack_link);
        }

        /* Nothing else to do? */
        if (c == old)
                return;

        /* Deactivate old client if focus is changing */
        if (c != old && old) {
#ifdef XWAYLAND
                if (old->type != XDG_SHELL)
                        wlr_xwayland_surface_activate(old->surface.xwayland, 0);
                else
#endif
                        wlr_xdg_toplevel_set_activated(old->surface.xdg, 0);
        }

        /* Update wlroots' keyboard focus */
        if (!c) {
                /* With no client, all we have left is to clear focus */
                wlr_seat_keyboard_notify_clear_focus(server.seat);
                return;
        }

        /* Have a client, so focus its top-level wlr_surface */
        wlr_seat_keyboard_notify_enter(server.seat, get_surface(c),
                        kb->keycodes, kb->num_keycodes, &kb->modifiers);

        /* Put the new client atop the focus stack and select its output */
        wl_list_remove(&c->focus_link);
        wl_list_insert(&server.focus_stack, &c->focus_link);
        active_output = c->output;

        /* Activate the new client */
#ifdef XWAYLAND
        if (c->type != XDG_SHELL)
                wlr_xwayland_surface_activate(c->surface.xwayland, 1);
        else
#endif
                wlr_xdg_toplevel_set_activated(c->surface.xdg, 1);
}

void focus_output(const Arg *arg)
{
        struct ewlc_client *c = get_active_client();

        active_output = get_next_output(arg->i);
        focus_client(c, focus_top(active_output), 1);
}

void focus_next_client(const Arg *arg)
{
        /* Focus the next or previous client (in tiling order) on active_output */
        struct ewlc_client *c, *c_active = get_active_client();
        if (!c_active)
                return;
        if (arg->i > 0) {
                wl_list_for_each(c, &c_active->link, link) {
                        if (&c->link == &server.clients)
                                continue;  /* wrap past the sentinel node */
                        if (is_visible_on(c, active_output))
                                break;  /* found it */
                }
        } else {
                wl_list_for_each_reverse(c, &c_active->link, link) {
                        if (&c->link == &server.clients)
                                continue;  /* wrap past the sentinel node */
                        if (is_visible_on(c, active_output))
                                break;  /* found it */
                }
        }
        /* If only one client is visible on active_output, then c == c_active */
        focus_client(c_active, c, 1);
}

struct ewlc_client *focus_top(struct ewlc_output *o)
{
        struct ewlc_client *c;
        wl_list_for_each(c, &server.focus_stack, focus_link)
                if (is_visible_on(c, o))
                        return c;
        return NULL;
}

void get_xdeco_mode(struct wl_listener *listener, void *data)
{
        struct wlr_xdg_toplevel_decoration_v1 *wlr_deco = data;
        wlr_xdg_toplevel_decoration_v1_set_mode(wlr_deco,
                        WLR_XDG_TOPLEVEL_DECORATION_V1_MODE_SERVER_SIDE);
}

void incn_master(const Arg *arg)
{
        active_output->num_master = MAX(active_output->num_master + arg->i, 0);
        arrange(active_output);
}

void backend_new_input_notify(struct wl_listener *listener, void *data)
{
        /* This event is raised by the backend when a new input device becomes
         * available. */
        struct wlr_input_device *device = data;
        uint32_t caps;
        switch (device->type) {
        case WLR_INPUT_DEVICE_KEYBOARD:
                create_keyboard(device);
                break;
        case WLR_INPUT_DEVICE_POINTER:
                create_pointer(device);
                break;
        default:
                /* XXX handle other input device types */
                break;
        }
        /* We need to let the wlr_seat know what our capabilities are, which is
         * communiciated to the client. We always have a cursor, even if
         * there are no pointer devices, so we always include that capability. */
        /* XXX do we actually require a cursor? */
        caps = WL_SEAT_CAPABILITY_POINTER;
        if (!wl_list_empty(&server.keyboards))
                caps |= WL_SEAT_CAPABILITY_KEYBOARD;
        wlr_seat_set_capabilities(server.seat, caps);
}

int keybinding(uint32_t mods, xkb_keysym_t sym)
{
        /*
         * Here we handle compositor keybindings. This is when the compositor is
         * processing keys, rather than passing them on to the client for its own
         * processing.
         */
        int handled = 0;
        const Key *k;
        for (k = keys; k < END(keys); k++) {
                if (CLEANMASK(mods) == CLEANMASK(k->mod) &&
                                sym == k->keysym && k->func) {
                        k->func(&k->arg);
                        handled = 1;
                }
        }
        return handled;
}

void keypress(struct wl_listener *listener, void *data)
{
        /* This event is raised when a key is pressed or released. */
        struct ewlc_keyboard *kb = wl_container_of(listener, kb, key);
        struct wlr_event_keyboard_key *event = data;
        int i;

        /* Translate libinput keycode -> xkbcommon */
        uint32_t keycode = event->keycode + 8;
        /* Get a list of keysyms based on the keymap for this keyboard */
        const xkb_keysym_t *syms;
        int nsyms = xkb_state_key_get_syms(
                kb->device->keyboard->xkb_state, keycode, &syms);

        int handled = 0;
        uint32_t mods = wlr_keyboard_get_modifiers(kb->device->keyboard);
        /* On _press_, attempt to process a compositor keybinding. */
        if (event->state == WLR_KEY_PRESSED)
                for (i = 0; i < nsyms; i++)
                        handled = keybinding(mods, syms[i]) || handled;

        if (!handled) {
                /* Pass unhandled keycodes along to the client. */
                wlr_seat_set_keyboard(server.seat, kb->device);
                wlr_seat_keyboard_notify_key(server.seat, event->time_msec,
                        event->keycode, event->state);
        }
}

void keypress_mod(struct wl_listener *listener, void *data)
{
        /* This event is raised when a modifier key, such as shift or alt, is
         * pressed. We simply communicate this to the client. */
        struct ewlc_keyboard *kb = wl_container_of(listener, kb, modifiers);
        /*
         * A seat can only have one keyboard, but this is a limitation of the
         * Wayland protocol - not wlroots. We assign all connected keyboards to the
         * same seat. You can swap out the underlying wlr_keyboard like this and
         * wlr_seat handles this transparently.
         */
        wlr_seat_set_keyboard(server.seat, kb->device);
        /* Send modifiers to the client. */
        wlr_seat_keyboard_notify_modifiers(server.seat,
                &kb->device->keyboard->modifiers);
}

void kill_client(const Arg *arg)
{
        struct ewlc_client *c = get_active_client();
        if (!c)
                return;

#ifdef XWAYLAND
        if (c->type != XDG_SHELL)
                wlr_xwayland_surface_close(c->surface.xwayland);
        else
#endif
                wlr_xdg_toplevel_send_close(c->surface.xdg);
}

void map_request(struct wl_listener *listener, void *data)
{
        /* Called when the surface is mapped, or ready to display on-screen. */
        struct ewlc_client *c = wl_container_of(listener, c, map);

#ifdef XWAYLAND
        if (c->type == X11_UNMANAGED) {
                /* Insert this independent into independents lists. */
                wl_list_insert(&server.independents, &c->link);
                return;
        }
#endif

        /* Insert this client into client lists. */
        wl_list_insert(&server.clients, &c->link);
        wl_list_insert(&server.focus_stack, &c->focus_link);
        wl_list_insert(&server.stack, &c->stack_link);

#ifdef XWAYLAND
        if (c->type != XDG_SHELL) {
                c->geom.x = c->surface.xwayland->x;
                c->geom.y = c->surface.xwayland->y;
                c->geom.width = c->surface.xwayland->width + 2 * c->border_width;
                c->geom.height = c->surface.xwayland->height + 2 * c->border_width;
        } else
#endif
        {
                wlr_xdg_surface_get_geometry(c->surface.xdg, &c->geom);
                c->geom.width += 2 * c->border_width;
                c->geom.height += 2 * c->border_width;
        }

        /* Set initial output, floating status, and focus */
        apply_title(c);
}

void cursor_motion_absolute_notify(struct wl_listener *listener, void *data)
{
        /* This event is forwarded by the cursor when a pointer emits an _absolute_
         * motion event, from 0..1 on each axis. This happens, for example, when
         * wlroots is running under a Wayland window rather than KMS+DRM, and you
         * move the mouse over the window. You could enter the window from any edge,
         * so we have to warp the mouse there. There is also some hardware which
         * emits these events. */
        struct wlr_event_pointer_motion_absolute *event = data;
        wlr_cursor_warp_absolute(server.cursor, event->device, event->x, event->y);
        motion_notify(event->time_msec);
}

void motion_notify(uint32_t time)
{
        double sx = 0, sy = 0;
        struct wlr_surface *surface = NULL;
        struct ewlc_client *c;

        /* Update active_output (even while dragging a window) */
        if (sloppyfocus)
                active_output = get_output_at_point(server.cursor->x, server.cursor->y);

        /* If we are currently grabbing the mouse, handle and return */
        if (server.cursor_mode == CUR_MOVE) {
                /* Move the grabbed client to the new position. */
                resize(server.grabbed_client,
                       server.cursor->x - server.grabc_x,
                       server.cursor->y - server.grabc_y,
                       server.grabbed_client->geom.width,
                       server.grabbed_client->geom.height, 1);
                return;
        } else if (server.cursor_mode == CUR_RESIZE) {
                resize(server.grabbed_client,
                       server.grabbed_client->geom.x,
                       server.grabbed_client->geom.y,
                       server.cursor->x - server.grabbed_client->geom.x,
                       server.cursor->y - server.grabbed_client->geom.y, 1);
                return;
        }

#ifdef XWAYLAND
        /* Find an independent under the pointer and send the event along. */
        if ((c = get_independent_at_point(server.cursor->x, server.cursor->y))) {
                surface = wlr_surface_surface_at(
                        c->surface.xwayland->surface,
                        server.cursor->x - c->surface.xwayland->x - c->border_width,
                        server.cursor->y - c->surface.xwayland->y - c->border_width, &sx, &sy);

        /* Otherwise, find the client under the pointer and send the event along. */
        } else
#endif
        if ((c = get_client_at_point(server.cursor->x, server.cursor->y))) {
#ifdef XWAYLAND
                if (c->type != XDG_SHELL)
                        surface = wlr_surface_surface_at(
                                c->surface.xwayland->surface,
                                server.cursor->x - c->geom.x - c->border_width,
                                server.cursor->y - c->geom.y - c->border_width, &sx, &sy);
                else
#endif
                        surface = wlr_xdg_surface_surface_at(
                                c->surface.xdg,
                                server.cursor->x - c->geom.x - c->border_width,
                                server.cursor->y - c->geom.y - c->border_width, &sx, &sy);
        }
        /* If there's no client surface under the cursor, set the cursor image to a
         * default. This is what makes the cursor image appear when you move it
         * off of a client or over its border. */
        if (!surface)
                wlr_xcursor_manager_set_cursor_image(server.cursor_mgr,
                                                     "left_ptr", server.cursor);

        pointer_focus(c, surface, sx, sy, time);
}

void cursor_motion_notify(struct wl_listener *listener, void *data)
{
        /* This event is forwarded by the cursor when a pointer emits a _relative_
         * pointer motion event (i.e. a delta) */
        struct wlr_event_pointer_motion *event = data;
        /* The cursor doesn't move unless we tell it to. The cursor automatically
         * handles constraining the motion to the output layout, as well as any
         * special configuration applied for the specific input device which
         * generated the event. You can pass NULL for the device if you want to move
         * the cursor around without any input. */
        wlr_cursor_move(server.cursor, event->device,
                        event->delta_x, event->delta_y);
        motion_notify(event->time_msec);
}

void move_resize(const Arg *arg)
{
        server.grabbed_client = get_client_at_point(server.cursor->x, server.cursor->y);
        if (!server.grabbed_client)
                return;

        /* Float the window and tell motionnotify to grab it */
        set_floating(server.grabbed_client, 1);
        switch (server.cursor_mode = arg->ui) {
        case CUR_MOVE:
                server.grabc_x = server.cursor->x - server.grabbed_client->geom.x;
                server.grabc_y = server.cursor->y - server.grabbed_client->geom.y;
                wlr_xcursor_manager_set_cursor_image(server.cursor_mgr, "fleur", server.cursor);
                break;
        case CUR_RESIZE:
                /* Doesn't work for X11 output - the next absolute motion event
                 * returns the cursor to where it started */
                wlr_cursor_warp_closest(server.cursor, NULL,
                                server.grabbed_client->geom.x + server.grabbed_client->geom.width,
                                server.grabbed_client->geom.y + server.grabbed_client->geom.height);
                wlr_xcursor_manager_set_cursor_image(server.cursor_mgr,
                                "bottom_right_corner", server.cursor);
                break;
        }
}

void pointer_focus(struct ewlc_client *c, struct wlr_surface *surface, double sx, double sy,
                uint32_t time)
{
        /* Use top level surface if nothing more specific given */
        if (c && !surface)
                surface = get_surface(c);

        /* If surface is NULL, clear pointer focus */
        if (!surface) {
                wlr_seat_pointer_notify_clear_focus(server.seat);
                return;
        }

        /* If surface is already focused, only notify of motion */
        if (surface == server.seat->pointer_state.focused_surface) {
                wlr_seat_pointer_notify_motion(server.seat, time, sx, sy);
                return;
        }

        /* Otherwise, let the client know that the mouse cursor has entered one
         * of its surfaces, and make keyboard focus follow if desired. */
        wlr_seat_pointer_notify_enter(server.seat, surface, sx, sy);

#if XWAYLAND
        if (c->type == X11_UNMANAGED)
                return;
#endif

        if (sloppyfocus)
                focus_client(get_active_client(), c, 0);
}

void quit(const Arg *arg)
{
        wl_display_terminate(server.display);
}

void render(struct wlr_surface *surface, int sx, int sy, void *data)
{
        /* This function is called for every surface that needs to be rendered. */
        struct render_data *rdata = data;
        struct wlr_output *output = rdata->output;
        double ox = 0, oy = 0;
        struct wlr_box obox;
        float matrix[9];
        enum wl_output_transform transform;

        /* We first obtain a wlr_texture, which is a GPU resource. wlroots
         * automatically handles negotiating these with the client. The underlying
         * resource could be an opaque handle passed from the client, or the client
         * could have sent a pixel buffer which we copied to the GPU, or a few other
         * means. You don't have to worry about this, wlroots takes care of it. */
        struct wlr_texture *texture = wlr_surface_get_texture(surface);
        if (!texture)
                return;

        /* The client has a position in layout coordinates. If you have two displays,
         * one next to the other, both 1080p, a client on the rightmost display might
         * have layout coordinates of 2000,100. We need to translate that to
         * output-local coordinates, or (2000 - 1920). */
        wlr_output_layout_output_coords(server.output_layout, output, &ox, &oy);

        /* We also have to apply the scale factor for HiDPI outputs. This is only
         * part of the puzzle, ewlc does not fully support HiDPI. */
        obox.x = ox + rdata->x + sx;
        obox.y = oy + rdata->y + sy;
        obox.width = surface->current.width;
        obox.height = surface->current.height;
        scale_box(&obox, output->scale);

        /*
         * Those familiar with OpenGL are also familiar with the role of matrices
         * in graphics programming. We need to prepare a matrix to render the
         * client with. wlr_matrix_project_box is a helper which takes a box with
         * a desired x, y coordinates, width and height, and an output geometry,
         * then prepares an orthographic projection and multiplies the necessary
         * transforms to produce a model-view-projection matrix.
         *
         * Naturally you can do this any way you like, for example to make a 3D
         * compositor.
         */
        transform = wlr_output_transform_invert(surface->current.transform);
        wlr_matrix_project_box(matrix, &obox, transform, 0,
                output->transform_matrix);

        /* This takes our matrix, the texture, and an alpha, and performs the actual
         * rendering on the GPU. */
        wlr_render_texture_with_matrix(server.renderer, texture, matrix, 1);

        /* This lets the client know that we've displayed that frame and it can
         * prepare another one now if it likes. */
        wlr_surface_send_frame_done(surface, rdata->when);
}

void render_clients(struct ewlc_output *o, struct timespec *now)
{
        struct ewlc_client *c, *active_c = get_active_client();
        const float *color;
        double ox, oy;
        int i, w, h;
        struct render_data rdata;
        struct wlr_box *borders;
        struct wlr_surface *surface;
        /* Each subsequent window we render is rendered on top of the last. Because
         * our stacking list is ordered front-to-back, we iterate over it backwards. */
        wl_list_for_each_reverse(c, &server.stack, stack_link) {
                /* Only render visible clients which show on this output */
                if (!is_visible_on(c, c->output) || !wlr_output_layout_intersects(
                            server.output_layout, o->wlr_output, &c->geom))
                        continue;

                surface = get_surface(c);
                ox = c->geom.x;
                oy = c->geom.y;
                wlr_output_layout_output_coords(server.output_layout, o->wlr_output,
                                                &ox, &oy);
                w = surface->current.width;
                h = surface->current.height;
                borders = (struct wlr_box[4]) {
                        {ox, oy, w + 2 * c->border_width, c->border_width},             /* top */
                        {ox, oy + c->border_width, c->border_width, h},                 /* left */
                        {ox + c->border_width + w, oy + c->border_width, c->border_width, h},     /* right */
                        {ox, oy + c->border_width + h, w + 2 * c->border_width, c->border_width}, /* bottom */
                };

                /* Draw window borders */
                color = (c == active_c) ? focus_color : border_color;
                for (i = 0; i < 4; i++) {
                        scale_box(&borders[i], o->wlr_output->scale);
                        wlr_render_rect(server.renderer, &borders[i], color,
                                        o->wlr_output->transform_matrix);
                }

                /* This calls our render function for each surface among the
                 * xdg_surface's toplevel and popups. */
                rdata.output = o->wlr_output;
                rdata.when = now;
                rdata.x = c->geom.x + c->border_width;
                rdata.y = c->geom.y + c->border_width;
#ifdef XWAYLAND
                if (c->type != XDG_SHELL)
                        wlr_surface_for_each_surface(c->surface.xwayland->surface, render, &rdata);
                else
#endif
                        wlr_xdg_surface_for_each_surface(c->surface.xdg, render, &rdata);
        }
}

void render_output(struct wl_listener *listener, void *data)
{
        struct ewlc_client *c;
        int render = 1;

        /* This function is called every time an output is ready to display a frame,
         * generally at the output's refresh rate (e.g. 60Hz). */
        struct ewlc_output *o = wl_container_of(listener, o, frame);

        struct timespec now;
        clock_gettime(CLOCK_MONOTONIC, &now);

        /* Do not render if any XDG clients have an outstanding resize. */
        wl_list_for_each(c, &server.stack, stack_link) {
                if (c->resize) {
                        wlr_surface_send_frame_done(get_surface(c), &now);
                        render = 0;
                }
        }

        /* wlr_output_attach_render makes the OpenGL context current. */
        if (!wlr_output_attach_render(o->wlr_output, NULL))
                return;

        if (render) {
                /* Begin the renderer (calls glViewport and some other GL sanity checks) */
                wlr_renderer_begin(server.renderer, o->wlr_output->width, o->wlr_output->height);
                wlr_renderer_clear(server.renderer, root_color);

                render_clients(o, &now);
#ifdef XWAYLAND
                render_independents(o->wlr_output, &now);
#endif

                /* Hardware cursors are rendered by the GPU on a separate plane, and can be
                 * moved around without re-rendering what's beneath them - which is more
                 * efficient. However, not all hardware supports hardware cursors. For this
                 * reason, wlroots provides a software fallback, which we ask it to render
                 * here. wlr_cursor handles configuring hardware vs software cursors for you,
                 * and this function is a no-op when hardware cursors are in use. */
                wlr_output_render_software_cursors(o->wlr_output, NULL);

                /* Conclude rendering and swap the buffers, showing the final frame
                 * on-screen. */
                wlr_renderer_end(server.renderer);
        }

        wlr_output_commit(o->wlr_output);
}

void resize(struct ewlc_client *c, int x, int y, int w, int h, int interact)
{
        /*
         * Note that I took some shortcuts here. In a more fleshed-out
         * compositor, you'd wait for the client to prepare a buffer at
         * the new size, then commit any movement that was prepared.
         */
        struct wlr_box *bbox = interact ? &server.output_geom : &c->output->w;
        c->geom.x = x;
        c->geom.y = y;
        c->geom.width = w;
        c->geom.height = h;
        apply_bounds(c, bbox);
        /* wlroots makes this a no-op if size hasn't changed */
#ifdef XWAYLAND
        if (c->type != XDG_SHELL)
                wlr_xwayland_surface_configure(
                        c->surface.xwayland,
                        c->geom.x,
                        c->geom.y,
                        c->geom.width - 2 * c->border_width,
                        c->geom.height - 2 * c->border_width);
        else
#endif
                c->resize = wlr_xdg_toplevel_set_size(
                        c->surface.xdg,
                        c->geom.width - 2 * c->border_width,
                        c->geom.height - 2 * c->border_width);
}


void scale_box(struct wlr_box *box, float scale)
{
        box->x *= scale;
        box->y *= scale;
        box->width *= scale;
        box->height *= scale;
}

struct ewlc_client *get_active_client(void)
{
        struct ewlc_client *c = wl_container_of(server.focus_stack.next, c, focus_link);
        if (wl_list_empty(&server.focus_stack) || !is_visible_on(c, active_output))
                return NULL;
        return c;
}

void seat_request_cursor_notify(struct wl_listener *listener, void *data)
{
        /* This event is raised by the seat when a client provides a cursor image */
        struct wlr_seat_pointer_request_set_cursor_event *event = data;
        /* If we're "grabbing" the cursor, don't use the client's image */
        /* XXX still need to save the provided surface to restore later */
        if (server.cursor_mode != CUR_NORMAL)
                return;
        /* This can be sent by any client, so we check to make sure this one is
         * actually has pointer focus first. If so, we can tell the cursor to
         * use the provided surface as the cursor image. It will set the
         * hardware cursor on the output that it's currently on and continue to
         * do so as the cursor moves between outputs. */
        if (event->seat_client == server.seat->pointer_state.focused_client)
                wlr_cursor_set_surface(server.cursor, event->surface,
                                       event->hotspot_x, event->hotspot_y);
}

void set_floating(struct ewlc_client *c, int floating)
{
        if (c->is_floating == floating)
                return;
        c->is_floating = floating;
        arrange(c->output);
}

/* arg > 1.0 will set master_ratio absolutely */
void set_master_ratio(const Arg *arg)
{
        float f;

        if (!arg)
                return;
        f = arg->f < 1.0 ? arg->f + active_output->master_ratio : arg->f - 1.0;
        if (f < 0.1 || f > 0.9)
                return;
        active_output->master_ratio = f;
        arrange(active_output);
}

void set_output(struct ewlc_client *c, struct ewlc_output *o)
{
        struct ewlc_output *old_output = c->output;
        struct ewlc_client *old_c = get_active_client();

        if (old_output == o)
                return;
        c->output = o;

        /* XXX leave/enter is not optimal but works */
        if (old_output) {
                wlr_surface_send_leave(get_surface(c), old_output->wlr_output);
                arrange(old_output);
        }
        if (o) {
                /* Make sure window actually overlaps with the output */
                apply_bounds(c, &o->m);
                wlr_surface_send_enter(get_surface(c), o->wlr_output);
                arrange(o);
        }
        focus_client(old_c, focus_top(active_output), 1);
}

void seat_request_set_primary_selection_notify(struct wl_listener *listener, void *data)
{
        /* This event is raised by the seat when a client wants to set the selection,
         * usually when the user copies something. wlroots allows compositors to
         * ignore such requests if they so choose, but we always honor
         */
        struct wlr_seat_request_set_primary_selection_event *event = data;
        wlr_seat_set_primary_selection(server.seat, event->source, event->serial);
}

void seat_request_set_selection_notify(struct wl_listener *listener, void *data)
{
        /* This event is raised by the seat when a client wants to set the selection,
         * usually when the user copies something. wlroots allows compositors to
         * ignore such requests if they so choose, but we always honor
         */
        struct wlr_seat_request_set_selection_event *event = data;
        wlr_seat_set_selection(server.seat, event->source, event->serial);
}


void sigchld(int unused)
{
        if (signal(SIGCHLD, sigchld) == SIG_ERR)
                EERROR("can't install SIGCHLD handler");
        while (0 < waitpid(-1, NULL, WNOHANG))
                ;
}

void spawn(const Arg *arg)
{
        if (fork() == 0) {
                setsid();
                execvp(((char **)arg->v)[0], (char **)arg->v);
                EERROR("ewlc: execvp %s failed", ((char **)arg->v)[0]);
        }
}

void tile(struct ewlc_output *o)
{
        unsigned int i, n = 0, h, mw, my, ty;
        struct ewlc_client *c;

        wl_list_for_each(c, &server.clients, link)
                if (is_visible_on(c, o) && !c->is_floating)
                        n++;
        if (n == 0)
                return;

        if (n > o->num_master)
                mw = o->num_master ? o->w.width * o->master_ratio : 0;
        else
                mw = o->w.width;
        i = my = ty = 0;
        wl_list_for_each(c, &server.clients, link) {
                if (!is_visible_on(c, o) || c->is_floating)
                        continue;
                if (i < o->num_master) {
                        h = (o->w.height - my) / (MIN(n, o->num_master) - i);
                        resize(c, o->w.x, o->w.y + my, mw, h, 0);
                        my += c->geom.height;
                } else {
                        h = (o->w.height - ty) / (n - i);
                        resize(c, o->w.x + mw, o->w.y + ty, o->w.width - mw, h, 0);
                        ty += c->geom.height;
                }
                i++;
        }
}

void toggle_floating(const Arg *arg)
{
        struct ewlc_client *c = get_active_client();
        if (!c)
                return;
        /* return if fullscreen */
        set_floating(c, !c->is_floating);
}

void unmap_notify(struct wl_listener *listener, void *data)
{
        /* Called when the surface is unmapped, and should no longer be shown. */
        struct ewlc_client *c = wl_container_of(listener, c, unmap);
        wl_list_remove(&c->link);
#ifdef XWAYLAND
        if (c->type == X11_UNMANAGED)
                return;
#endif
        set_output(c, NULL);
        wl_list_remove(&c->focus_link);
        wl_list_remove(&c->stack_link);
}

void view(const Arg *arg)
{
        struct ewlc_client *c = get_active_client();
        focus_client(c, focus_top(active_output), 1);
        arrange(active_output);
}

struct ewlc_client *get_client_at_point(double x, double y)
{
        /* Find the topmost visible client (if any) at point (x, y), including
         * borders. This relies on stack being ordered from top to bottom. */
        struct ewlc_client *c;

        wl_list_for_each(c, &server.stack, stack_link)
                if (is_visible_on(c, c->output) && wlr_box_contains_point(&c->geom, x, y))
                        return c;
        return NULL;
}

struct ewlc_output *get_output_at_point(double x, double y)
{
        struct wlr_output *o = wlr_output_layout_output_at(server.output_layout, x, y);
        return o ? o->data : NULL;
}

void zoom(const Arg *arg)
{
        struct ewlc_client *c, *c_active, *old_c_active;

        c_active = get_active_client();
        old_c_active = c_active;

        if (!c_active || c_active->is_floating)
                return;

        /* Search for the first tiled window that is not c_active, marking c_active as
         * NULL if we pass it along the way */
        wl_list_for_each(c, &server.clients, link) {
                if (is_visible_on(c, active_output) && !c->is_floating) {
                        if (c != c_active)
                                break;
                        c_active = NULL;
                }
        }

        /* Return if no other tiled window was found */
        if (&c->link == &server.clients)
                return;

        /* If we passed c_active, move c to the front; otherwise, move c_active to the
         * front */
        if (!c_active)
                c_active = c;
        wl_list_remove(&c_active->link);
        wl_list_insert(&server.clients, &c_active->link);

        focus_client(old_c_active, c_active, 1);
        arrange(active_output);
}

#ifdef XWAYLAND
void activate_x11(struct wl_listener *listener, void *data)
{
       struct ewlc_client *c = wl_container_of(listener, c, activate);

       /* Only "managed" windows can be activated */
       if (c->type == X11_MANAGED)
               wlr_xwayland_surface_activate(c->surface.xwayland, 1);
}

void new_xwayland_surface_notify(struct wl_listener *listener, void *data)
{
        struct wlr_xwayland_surface *xwayland_surface = data;
        struct ewlc_client *c;

        /* Allocate a struct ewlc_client for this surface */
        c = xwayland_surface->data = calloc(1, sizeof(*c));
        c->surface.xwayland = xwayland_surface;
        c->type = xwayland_surface->override_redirect ? X11_UNMANAGED : X11_MANAGED;
        c->border_width = border_px;

        /* Listen to the various events it can emit */
        c->map.notify = map_request;
        wl_signal_add(&xwayland_surface->events.map, &c->map);
        c->unmap.notify = unmap_notify;
        wl_signal_add(&xwayland_surface->events.unmap, &c->unmap);
        c->activate.notify = activate_x11;
        wl_signal_add(&xwayland_surface->events.request_activate, &c->activate);
        c->destroy.notify = destroy_notify;
        wl_signal_add(&xwayland_surface->events.destroy, &c->destroy);
}

Atom get_atom(xcb_connection_t *xc, const char *name)
{
        Atom atom = 0;
        xcb_intern_atom_cookie_t cookie;
        xcb_intern_atom_reply_t *reply;

        cookie = xcb_intern_atom(xc, 0, strlen(name), name);
        if ((reply = xcb_intern_atom_reply(xc, cookie, NULL)))
                atom = reply->atom;
        free(reply);

        return atom;
}

void render_independents(struct wlr_output *o, struct timespec *now)
{
        struct ewlc_client *c;
        struct render_data rdata;
        struct wlr_box geom;

        wl_list_for_each_reverse(c, &server.independents, link) {
                geom.x = c->surface.xwayland->x;
                geom.y = c->surface.xwayland->y;
                geom.width = c->surface.xwayland->width;
                geom.height = c->surface.xwayland->height;

                /* Only render visible clients which show on this output */
                if (!wlr_output_layout_intersects(server.output_layout, o, &geom))
                        continue;

                rdata.output = o;
                rdata.when = now;
                rdata.x = c->surface.xwayland->x;
                rdata.y = c->surface.xwayland->y;

                wlr_surface_for_each_surface(c->surface.xwayland->surface, render, &rdata);
        }
}

void update_window_type(struct ewlc_client *c)
{
        size_t i;
        for (i = 0; i < c->surface.xwayland->window_type_len; i++)
                if (c->surface.xwayland->window_type[i] == netatom[NetWMWindowTypeDialog] ||
                    c->surface.xwayland->window_type[i] == netatom[NetWMWindowTypeSplash] ||
                    c->surface.xwayland->window_type[i] == netatom[NetWMWindowTypeToolbar] ||
                    c->surface.xwayland->window_type[i] == netatom[NetWMWindowTypeUtility])
                        c->is_floating = 1;
}

void xwayland_ready_notify(struct wl_listener *listener, void *data)
{
        xcb_connection_t *xc = xcb_connect(server.xwayland->display_name, NULL);
        int err = xcb_connection_has_error(xc);
        if (err) {
                fprintf(stderr, "xcb_connect to X server failed with code %d\n. Continuing with degraded functionality.\n", err);
                return;
        }

        /* Collect atoms we are interested in.  If get_atom returns 0, we will
         * not detect that window type. */
        netatom[NetWMWindowTypeDialog] = get_atom(xc, "_NET_WM_WINDOW_TYPE_DIALOG");
        netatom[NetWMWindowTypeSplash] = get_atom(xc, "_NET_WM_WINDOW_TYPE_SPLASH");
        netatom[NetWMWindowTypeUtility] = get_atom(xc, "_NET_WM_WINDOW_TYPE_TOOLBAR");
        netatom[NetWMWindowTypeToolbar] = get_atom(xc, "_NET_WM_WINDOW_TYPE_UTILITY");

        /* assign the one and only seat */
        wlr_xwayland_set_seat(server.xwayland, server.seat);

        xcb_disconnect(xc);
}

struct ewlc_client *get_independent_at_point(double x, double y)
{
        struct ewlc_client *c;
        struct wlr_box geom;
        wl_list_for_each_reverse(c, &server.independents, link) {
                geom.x = c->surface.xwayland->x;
                geom.y = c->surface.xwayland->y;
                geom.width = c->surface.xwayland->width;
                geom.height = c->surface.xwayland->height;
                if (wlr_box_contains_point(&geom, x, y))
                        return c;
        }
        return NULL;
}
#endif


void ewlc_setup(void)
{
        /* The Wayland display is managed by libwayland. It handles accepting
         * clients from the Unix socket, manging Wayland globals, and so on. */
        server.display = wl_display_create();

        /* clean up child processes immediately */
        sigchld(0);

        /* The backend is a wlroots feature which abstracts the underlying input and
         * output hardware. The autocreate option will choose the most suitable
         * backend based on the current environment, such as opening an X11 window
         * if an X11 server is running. The NULL argument here optionally allows you
         * to pass in a custom renderer if wlr_renderer doesn't meet your needs. The
         * backend uses the renderer, for example, to fall back to software cursors
         * if the backend does not support hardware cursors (some older GPUs
         * don't). */
        if (!(server.backend = wlr_backend_autocreate(server.display, NULL)))
                ERROR("couldn't create backend");

        /* If we don't provide a renderer, autocreate makes a GLES2 renderer for us.
         * The renderer is responsible for defining the various pixel formats it
         * supports for shared memory, this configures that for clients. */
        server.renderer = wlr_backend_get_renderer(server.backend);
        wlr_renderer_init_wl_display(server.renderer, server.display);

        /* This creates some hands-off wlroots interfaces. The compositor is
         * necessary for clients to allocate surfaces and the data device manager
         * handles the clipboard. Each of these wlroots interfaces has room for you
         * to dig your fingers in and play with their behavior if you want. Note that
         * the clients cannot set the selection directly without compositor approval,
         * see the setsel() function. */
        server.compositor = wlr_compositor_create(server.display, server.renderer);
        wlr_export_dmabuf_manager_v1_create(server.display);
        wlr_screencopy_manager_v1_create(server.display);
        wlr_data_device_manager_create(server.display);
        wlr_gamma_control_manager_v1_create(server.display);
        wlr_primary_selection_v1_device_manager_create(server.display);
        wlr_viewporter_create(server.display);

        /* Creates an output layout, which a wlroots utility for working with an
         * arrangement of screens in a physical layout. */
        server.output_layout = wlr_output_layout_create();
        wlr_xdg_output_manager_v1_create(server.display, server.output_layout);

        /* Configure a listener to be notified when new outputs are available on the
         * backend. */
        wl_list_init(&server.outputs);
        server.backend_new_output.notify = backend_new_output_notify;
        wl_signal_add(&server.backend->events.new_output, &server.backend_new_output);

        /* Set up our client lists and the xdg-shell. The xdg-shell is a
         * Wayland protocol which is used for application windows. For more
         * detail on shells, refer to the article:
         *
         * https://drewdevault.com/2018/07/29/Wayland-shells.html
         */
        wl_list_init(&server.clients);
        wl_list_init(&server.focus_stack);
        wl_list_init(&server.stack);
        wl_list_init(&server.independents);

        server.xdg_shell = wlr_xdg_shell_create(server.display);
        server.new_xdg_surface.notify = new_xdg_surface_notify;
        wl_signal_add(&server.xdg_shell->events.new_surface, &server.new_xdg_surface);

        /* Use xdg_decoration protocol to negotiate server-side decorations */
        server.xdeco_mgr = wlr_xdg_decoration_manager_v1_create(server.display);

        server.new_xdeco.notify = new_toplevel_decoration_notify;
        wl_signal_add(&server.xdeco_mgr->events.new_toplevel_decoration, &server.new_xdeco);

        /*
         * Creates a cursor, which is a wlroots utility for tracking the cursor
         * image shown on screen.
         */
        server.cursor = wlr_cursor_create();
        wlr_cursor_attach_output_layout(server.cursor, server.output_layout);

        /* Creates an xcursor manager, another wlroots utility which loads up
         * Xcursor themes to source cursor images from and makes sure that cursor
         * images are available at all scale factors on the screen (necessary for
         * HiDPI support). Scaled cursors will be loaded with each output. */
        server.cursor_mgr = wlr_xcursor_manager_create(NULL, 24);

        /*
         * wlr_cursor *only* displays an image on screen. It does not move around
         * when the pointer moves. However, we can attach input devices to it, and
         * it will generate aggregate events for all of them. In these events, we
         * can choose how we want to process them, forwarding them to clients and
         * moving the cursor around. More detail on this process is described in my
         * input handling blog post:
         *
         * https://drewdevault.com/2018/07/17/Input-handling-in-wlroots.html
         *
         * And more comments are sprinkled throughout the notify functions above.
         */

        server.cursor_axis.notify = cursor_axis_notify;
        server.cursor_button.notify = cursor_button_notify;
        server.cursor_frame.notify = cursor_frame_notify;
        server.cursor_motion.notify = cursor_motion_notify;
        server.cursor_motion_absolute.notify = cursor_motion_absolute_notify;

        wl_signal_add(&server.cursor->events.axis, &server.cursor_axis);
        wl_signal_add(&server.cursor->events.button, &server.cursor_button);
        wl_signal_add(&server.cursor->events.frame, &server.cursor_frame);
        wl_signal_add(&server.cursor->events.motion, &server.cursor_motion);
        wl_signal_add(&server.cursor->events.motion_absolute,
                      &server.cursor_motion_absolute);

        /*
         * Configures a seat, which is a single "seat" at which a user sits and
         * operates the computer. This conceptually includes up to one keyboard,
         * pointer, touch, and drawing tablet device. We also rig up a listener to
         * let us know when new input devices are available on the backend.
         */
        wl_list_init(&server.keyboards);

        server.backend_new_input.notify = backend_new_input_notify;
        wl_signal_add(&server.backend->events.new_input, &server.backend_new_input);

        server.seat = wlr_seat_create(server.display, "seat0");

        server.seat_request_cursor.notify = seat_request_cursor_notify;
        wl_signal_add(&server.seat->events.request_set_cursor, &server.seat_request_cursor);

        server.seat_request_set_selection.notify = seat_request_set_selection_notify;
        wl_signal_add(&server.seat->events.request_set_selection,
                      &server.seat_request_set_selection);

        server.seat_request_set_primary_selection.notify = seat_request_set_primary_selection_notify;
        wl_signal_add(&server.seat->events.request_set_primary_selection,
                      &server.seat_request_set_primary_selection);

#ifdef XWAYLAND
        /*
         * Initialise the XWayland X server.
         * It will be started when the first X client is started.
         */
        server.xwayland = wlr_xwayland_create(server.display, server.compositor, true);
        if (server.xwayland) {

                server.xwayland_ready.notify = xwayland_ready_notify;
                wl_signal_add(&server.xwayland->events.ready, &server.xwayland_ready);

                server.new_xwayland_surface.notify = new_xwayland_surface_notify;
                wl_signal_add(&server.xwayland->events.new_surface, &server.new_xwayland_surface);

                setenv("DISPLAY", server.xwayland->display_name, true);
        } else {
                fprintf(stderr, "Failed to setup XWayland X server, continuing without it.\n");
        }
#endif
}

void ewlc_start()
{
        const char *socket;
        char startup_cmd[] = "alacritty";

        // Wayland requires XDG_RUNTIME_DIR for creating its communications
        // socket
        if (!getenv("XDG_RUNTIME_DIR"))
                ERROR("XDG_RUNTIME_DIR must be set");

        ewlc_setup();

        server.startup_pid = -1;

        /* Add a Unix socket to the Wayland display. */
        // const char *socket = wl_display_add_socket_auto(server.display);
        socket = wl_display_add_socket_auto(server.display);

        if (!socket)
                ERROR("startup: display_add_socket_auto");

        /* Start the backend. This will enumerate outputs and inputs, become the DRM
         * master, etc */
        if (!wlr_backend_start(server.backend))
                ERROR("startup: backend_start");

        /* Now that outputs are initialized, choose initial active_output based on
         * cursor position, and set default cursor image */
        active_output = get_output_at_point(server.cursor->x, server.cursor->y);

        /* XXX hack to get cursor to display in its initial location (100, 100)
         * instead of (0, 0) and then jumping.  still may not be fully
         * initialized, as the image/coordinates are not transformed for the
         * output when displayed here */
        wlr_cursor_warp_closest(server.cursor, NULL, server.cursor->x, server.cursor->y);
        wlr_xcursor_manager_set_cursor_image(server.cursor_mgr, "left_ptr", server.cursor);

        /* Set the WAYLAND_DISPLAY environment variable to our socket and run the
         * startup command if requested. */
        setenv("WAYLAND_DISPLAY", socket, 1);
        server.startup_pid = fork();
        if (server.startup_pid < 0)
                EERROR("startup: fork");
        if (server.startup_pid == 0) {
                execl("/bin/sh", "/bin/sh", "-c", startup_cmd, (void *)NULL);
                EERROR("startup: execl");
        }
        // wl_display_run(server.display);
}

int ewlc_cleanup(void)
{
        if (server.startup_pid != -1) {
                kill(server.startup_pid, SIGTERM);
                waitpid(server.startup_pid, NULL, 0);
        }

#ifdef XWAYLAND
        wlr_xwayland_destroy(server.xwayland);
#endif
        wl_display_destroy_clients(server.display);
        wl_display_destroy(server.display);

        wlr_xcursor_manager_destroy(server.cursor_mgr);
        wlr_cursor_destroy(server.cursor);

        wlr_output_layout_destroy(server.output_layout);
        return EXIT_SUCCESS;
}

int ewlc_display_dispatch()
{
        struct wl_event_loop *loop = wl_display_get_event_loop(server.display);

        wl_display_flush_clients(server.display);
        wl_event_loop_dispatch(loop, -1);
        return 0;
}
