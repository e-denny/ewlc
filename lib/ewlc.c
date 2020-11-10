/*
 p See LICENSE file for copyright and license details.
 */
#define _POSIX_C_SOURCE 200809L
#include "ewlc.h"
#include "ewlc-module.h"
#include <emacs-module.h>
#include <getopt.h>
#include <linux/input-event-codes.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>
#include <wayland-client.h>
#include <wayland-server-core.h>
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

/* enums */
enum { CUR_NORMAL, CUR_MOVE, CUR_RESIZE }; /* cursor */

#ifdef XWAYLAND
enum {
    NetWMWindowTypeDialog,
    NetWMWindowTypeSplash,
    NetWMWindowTypeToolbar,
    NetWMWindowTypeUtility,
    NetLast
}; /* EWMH atoms */

enum { XDG_SHELL, X11_MANAGED, X11_UNMANAGED }; /* client types */
#endif

struct ewlc_output;
struct ewlc_server;

struct ewlc_server {
    struct wl_display *display;
    struct wlr_renderer *renderer;
    struct wlr_compositor *compositor;
    emacs_env *e_env;

    struct wlr_xdg_shell *xdg_shell;
    struct wl_listener xdg_shell_new_surface_listener;

    struct wlr_cursor *cursor;
    struct wlr_xcursor_manager *cursor_mgr;
    struct wl_listener cursor_axis_listener;
    struct wl_listener cursor_button_listener;
    struct wl_listener cursor_frame_listener;
    struct wl_listener cursor_motion_listener;
    struct wl_listener cursor_motion_absolute_listener;

    struct wlr_seat *seat;
    struct wl_listener seat_request_set_cursor_listener;
    struct wl_listener seat_request_set_selection_listener;
    struct wl_listener seat_request_set_primary_selection_listener;

    struct wlr_backend *backend;
    struct wl_listener backend_new_input_listener;

    struct wl_list keyboard_list;

    struct wlr_output_layout *output_layout;
    struct wl_list output_list;
    struct wl_listener backend_new_output_listener;

    unsigned int cursor_mode;

    struct ewlc_client *grabbed_client;
    int grabc_x, grabc_y; /* client-relative */
    struct wlr_box output_geom;

    struct wlr_xdg_decoration_manager_v1 *xdeco_mgr;
    struct wl_listener xdeco_mgr_new_top_level_decoration_listener;

#ifdef XWAYLAND
    struct wlr_xwayland *xwayland;
    struct wl_listener new_xwayland_surface_listener;
    struct wl_listener xwayland_ready_listener;
#endif

    struct wl_list client_list;     /* tiling order */
    struct wl_list client_focus_list; /* focus order */
    struct wl_list independent_list;
    struct wl_list client_stack_list; /* stacking z-order */

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
    struct wl_listener deco_request_mode_listener;
    struct wl_listener deco_destroy_listener;
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
static void output_destroy_notify(struct wl_listener *listener, void *data);
static void xdg_surface_commit_notify(struct wl_listener *listener, void *data);
static void xdg_shell_new_surface_notify(struct wl_listener *listener, void *data);
static void xdeco_mgr_new_toplevel_decoration_notify(struct wl_listener *listener,
                                           void *data);
static void surface_destroy_notify(struct wl_listener *listener, void *data);
static void deco_destroy_notify(struct wl_listener *listener, void *data);
static struct ewlc_output *get_next_output(int direction);
static void focus_client(struct ewlc_client *old, struct ewlc_client *c,
                         int lift);
static struct ewlc_client *focus_top(struct ewlc_output *o);
static void deco_request_mode_notify(struct wl_listener *listener, void *data);
static void surface_map_notify(struct wl_listener *listener, void *data);
static void move_resize(const Arg *arg);
static void pointer_focus(struct ewlc_client *c, struct wlr_surface *surface,
                          double sx, double sy, uint32_t time);
static void render_surface(struct wlr_surface *surface, int sx, int sy, void *data);
static void render_clients(struct ewlc_output *o, struct timespec *now);
static void output_frame_notify(struct wl_listener *listener, void *data);
static void resize(struct ewlc_client *c, int x, int y, int w, int h,
                   int interact);
static void scale_box(struct wlr_box *box, float scale);
static struct ewlc_client *get_active_client(void);
static void set_floating(struct ewlc_client *c, int floating);
static void set_output(struct ewlc_client *c, struct ewlc_output *o);
static void sigchld(int unused);
static void tile(struct ewlc_output *o);
static void surface_unmap_notify(struct wl_listener *listener, void *data);
static struct ewlc_client *get_client_at_point(double x, double y);
void ewlc_setup(emacs_env *env);
static bool is_visible_on(struct ewlc_client *c, struct ewlc_output *o);
static void apply_title(struct ewlc_client *c);

static void backend_new_output_notify(struct wl_listener *listener, void *data);

#ifdef XWAYLAND
static void xwayland_surface_request_activate_notify(struct wl_listener *listener,
                                                     void *data);
static void new_xwayland_surface_notify(struct wl_listener *listener,
                                        void *data);
static Atom get_atom(xcb_connection_t *xc, const char *name);
static void render_independents(struct wlr_output *output,
                                struct timespec *now);
static void update_window_type(struct ewlc_client *c);
static void xwayland_ready_notify(struct wl_listener *listener, void *data);
static struct ewlc_client *get_independent_at_point(double x, double y);
#endif

void e_message(emacs_env *env, char *msg_str);

/* global variables */
static const char broken[] = "broken";

struct ewlc_server server;
static struct ewlc_output *active_output;

#ifdef XWAYLAND
static Atom netatom[NetLast];
#endif

/* appearance */
static const int sloppyfocus = 1;        /* focus follows mouse */
static const unsigned int border_px = 1; /* border pixel of windows */
static const float root_color[] = {0.3, 0.3, 0.3, 1.0};
static const float border_color[] = {0.5, 0.5, 0.5, 1.0};
static const float focus_color[] = {1.0, 0.0, 0.0, 1.0};
static const int repeat_rate = 25;
static const int repeat_delay = 600;
#define MODKEY WLR_MODIFIER_ALT

/* outputs */
static const struct ewlc_output_rule output_rules[] = {
    /* name       master_ratio num_master scale  rotate/reflect */
    /* example of a HiDPI laptop monitor:
        { "eDP-1",    0.5,  1,      2,    WL_OUTPUT_TRANSFORM_NORMAL },
        */
    /* defaults */
    {NULL, 0.5, 1, 1, WL_OUTPUT_TRANSFORM_NORMAL},
};

static const Button buttons[] = {
    {MODKEY, BTN_LEFT, move_resize, {.ui = CUR_MOVE}},
    /* TODO: Fix this */
    {MODKEY, BTN_MIDDLE, ewlc_toggle_floating, {0}},
    {MODKEY, BTN_RIGHT, move_resize, {.ui = CUR_RESIZE}},
};

/* function implementations */



void xdeco_mgr_new_toplevel_decoration_notify(struct wl_listener *listener, void *data)
{
    struct wlr_xdg_toplevel_decoration_v1 *wlr_deco = data;
    struct ewlc_decoration *d = wlr_deco->data = calloc(1, sizeof(*d));

    d->deco_request_mode_listener.notify = deco_request_mode_notify;
    wl_signal_add(&wlr_deco->events.request_mode, &d->deco_request_mode_listener);

    d->deco_destroy_listener.notify = deco_destroy_notify;
    wl_signal_add(&wlr_deco->events.destroy, &d->deco_destroy_listener);

    deco_request_mode_notify(&d->deco_request_mode_listener, wlr_deco);
}

void deco_destroy_notify(struct wl_listener *listener, void *data)
{
    struct wlr_xdg_toplevel_decoration_v1 *wlr_deco = data;
    struct ewlc_decoration *d = wlr_deco->data;

    wl_list_remove(&d->deco_destroy_listener.link);
    wl_list_remove(&d->deco_request_mode_listener.link);
    free(d);
}

void deco_request_mode_notify(struct wl_listener *listener, void *data)
{
    struct wlr_xdg_toplevel_decoration_v1 *wlr_deco = data;
    wlr_xdg_toplevel_decoration_v1_set_mode(
        wlr_deco, WLR_XDG_TOPLEVEL_DECORATION_V1_MODE_SERVER_SIDE);
}

void sigchld(int unused)
{
    if (signal(SIGCHLD, sigchld) == SIG_ERR)
        EERROR("can't install SIGCHLD handler");
    while (0 < waitpid(-1, NULL, WNOHANG))
        ;
}

#ifdef XWAYLAND

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

void update_window_type(struct ewlc_client *c)
{
    size_t i;
    for (i = 0; i < c->surface.xwayland->window_type_len; i++)
        if (c->surface.xwayland->window_type[i] ==
                netatom[NetWMWindowTypeDialog] ||
            c->surface.xwayland->window_type[i] ==
                netatom[NetWMWindowTypeSplash] ||
            c->surface.xwayland->window_type[i] ==
                netatom[NetWMWindowTypeToolbar] ||
            c->surface.xwayland->window_type[i] ==
                netatom[NetWMWindowTypeUtility])
            c->is_floating = 1;
}

void xwayland_ready_notify(struct wl_listener *listener, void *data)
{
    xcb_connection_t *xc = xcb_connect(server.xwayland->display_name, NULL);
    int err = xcb_connection_has_error(xc);
    if (err) {
        fprintf(
            stderr,
            "xcb_connect to X server failed with code %d\n. Continuing with "
            "degraded functionality.\n",
            err);
        return;
    }

    /* Collect atoms we are interested in.  If get_atom returns 0, we will
     * not detect that window type. */
    netatom[NetWMWindowTypeDialog] = get_atom(xc, "_NET_WM_WINDOW_TYPE_DIALOG");
    netatom[NetWMWindowTypeSplash] = get_atom(xc, "_NET_WM_WINDOW_TYPE_SPLASH");
    netatom[NetWMWindowTypeUtility] =
        get_atom(xc, "_NET_WM_WINDOW_TYPE_TOOLBAR");
    netatom[NetWMWindowTypeToolbar] =
        get_atom(xc, "_NET_WM_WINDOW_TYPE_UTILITY");

    /* assign the one and only seat */
    wlr_xwayland_set_seat(server.xwayland, server.seat);

    xcb_disconnect(xc);
}

#endif

void ewlc_setup(emacs_env *env)
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
     * to dig your fingers in and play with their behavior if you want. Note
     * that the clients cannot set the selection directly without compositor
     * approval, see the setsel() function. */
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
    wl_list_init(&server.output_list);
    server.backend_new_output_listener.notify = backend_new_output_notify;
    wl_signal_add(&server.backend->events.new_output,
                  &server.backend_new_output_listener);

    /* Set up our client lists and the xdg-shell. The xdg-shell is a
     * Wayland protocol which is used for application windows. For more
     * detail on shells, refer to the article:
     *
     * https://drewdevault.com/2018/07/29/Wayland-shells.html
     */
    wl_list_init(&server.client_list);
    wl_list_init(&server.client_focus_list);
    wl_list_init(&server.client_stack_list);
    wl_list_init(&server.independent_list);

    server.xdg_shell = wlr_xdg_shell_create(server.display);
    server.xdg_shell_new_surface_listener.notify = xdg_shell_new_surface_notify;
    wl_signal_add(&server.xdg_shell->events.new_surface,
                  &server.xdg_shell_new_surface_listener);

    /* Use xdg_decoration protocol to negotiate server-side decorations */
    server.xdeco_mgr = wlr_xdg_decoration_manager_v1_create(server.display);

    server.xdeco_mgr_new_top_level_decoration_listener.notify =
        xdeco_mgr_new_toplevel_decoration_notify;
    wl_signal_add(&server.xdeco_mgr->events.new_toplevel_decoration,
                  &server.xdeco_mgr_new_top_level_decoration_listener);

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

    server.cursor_axis_listener.notify = cursor_axis_notify;
    server.cursor_button_listener.notify = cursor_button_notify;
    server.cursor_frame_listener.notify = cursor_frame_notify;
    server.cursor_motion_listener.notify = cursor_motion_notify;
    server.cursor_motion_absolute_listener.notify = cursor_motion_absolute_notify;

    wl_signal_add(&server.cursor->events.axis, &server.cursor_axis_listener);
    wl_signal_add(&server.cursor->events.button, &server.cursor_button_listener);
    wl_signal_add(&server.cursor->events.frame, &server.cursor_frame_listener);
    wl_signal_add(&server.cursor->events.motion, &server.cursor_motion_listener);
    wl_signal_add(&server.cursor->events.motion_absolute,
                  &server.cursor_motion_absolute_listener);

    /*
     * Configures a seat, which is a single "seat" at which a user sits and
     * operates the computer. This conceptually includes up to one keyboard,
     * pointer, touch, and drawing tablet device. We also rig up a listener to
     * let us know when new input devices are available on the backend.
     */
    wl_list_init(&server.keyboard_list);

    server.backend_new_input_listener.notify = backend_new_input_notify;
    wl_signal_add(&server.backend->events.new_input, &server.backend_new_input_listener);

    server.seat = wlr_seat_create(server.display, "seat0");

    server.seat_request_set_cursor_listener.notify = seat_request_set_cursor_notify;
    wl_signal_add(&server.seat->events.request_set_cursor,
                  &server.seat_request_set_cursor_listener);

    server.seat_request_set_selection_listener.notify =
        seat_request_set_selection_notify;
    wl_signal_add(&server.seat->events.request_set_selection,
                  &server.seat_request_set_selection_listener);

    server.seat_request_set_primary_selection_listener.notify =
        seat_request_set_primary_selection_notify;
    wl_signal_add(&server.seat->events.request_set_primary_selection,
                  &server.seat_request_set_primary_selection_listener);

#ifdef XWAYLAND
    /*
     * Initialise the XWayland X server.
     * It will be started when the first X client is started.
     */
    server.xwayland =
        wlr_xwayland_create(server.display, server.compositor, true);
    if (server.xwayland) {
        server.xwayland_ready_listener.notify = xwayland_ready_notify;
        wl_signal_add(&server.xwayland->events.ready, &server.xwayland_ready_listener);

        server.new_xwayland_surface_listener.notify = new_xwayland_surface_notify;
        wl_signal_add(&server.xwayland->events.new_surface,
                      &server.new_xwayland_surface_listener);

        setenv("DISPLAY", server.xwayland->display_name, true);
    } else {
        fprintf(stderr,
                "Failed to setup XWayland X server, continuing without it.\n");
    }
#endif
}

void ewlc_start(emacs_env *env)
{
    const char *socket;
    char startup_cmd[] = "alacritty";

    server.e_env = env;

    // Wayland requires XDG_RUNTIME_DIR for creating its communications
    // socket
    if (!getenv("XDG_RUNTIME_DIR"))
        ERROR("XDG_RUNTIME_DIR must be set");

    ewlc_setup(env);

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
    active_output = get_output_at_point(server, server.cursor->x, server.cursor->y);

    /* XXX hack to get cursor to display in its initial location (100, 100)
     * instead of (0, 0) and then jumping.  still may not be fully
     * initialized, as the image/coordinates are not transformed for the
     * output when displayed here */
    wlr_cursor_warp_closest(server.cursor, NULL, server.cursor->x,
                            server.cursor->y);
    wlr_xcursor_manager_set_cursor_image(server.cursor_mgr, "left_ptr",
                                         server.cursor);

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

    e_message(server.e_env, "ewlc_start: leaving");
}

int ewlc_cleanup(void)
{
    e_message(server.e_env, "ewlc_cleanup: enter");

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
