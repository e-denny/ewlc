CC = gcc
LD = gcc
EMACS_INC = /usr/include
CFLAGS ?= -g -Wall -Wextra -Wno-unused-parameter -Wno-sign-compare -Wno-error=unused-function
CFLAGS += -DXWAYLAND
CFLAGS += -fPIC -I. -I$(EMACS_INC) -DWLR_USE_UNSTABLE -std=c99 -Werror=declaration-after-statement


WAYLAND_PROTOCOLS=$(shell pkg-config --variable=pkgdatadir wayland-protocols)
WAYLAND_SCANNER=$(shell pkg-config --variable=wayland_scanner wayland-scanner)

PKGS = wlroots wayland-server xcb xkbcommon
CFLAGS += $(foreach p,$(PKGS),$(shell pkg-config --cflags $(p)))
LDLIBS += $(foreach p,$(PKGS),$(shell pkg-config --libs $(p)))
LDFLAGS = -shared
TARGET_LIB = ewlc.so

SRCS = module.c notify.c Fwl.c Fwlr_misc.c Fwlr_backend.c Fwlr_box.c Fwlr_renderer.c Fwlr_pointer.c Fwlr_cursor.c Fwlr_input_device.c Fwlr_keyboard.c Fwlr_output.c Fwlr_output_layout.c Fwlr_seat.c Fwlr_surface.c Fwlr_xcursor_manager.c Fwlr_xdg_shell.c Fwlr_xwayland.c Fewlc.c xdg-shell-protocol.c
OBJS = $(SRCS:.c=.o)

.PHONY:all

$(TARGET_LIB): $(OBJS)
	$(LD) $(LDFLAGS) $(LDLIBS) -o $@ $^

xdg-shell-protocol.h:
	$(WAYLAND_SCANNER) server-header \
		$(WAYLAND_PROTOCOLS)/stable/xdg-shell/xdg-shell.xml $@

xdg-shell-protocol.c: xdg-shell-protocol.h
	$(WAYLAND_SCANNER) private-code \
		$(WAYLAND_PROTOCOLS)/stable/xdg-shell/xdg-shell.xml $@

$(SRCS:.c=.d):%.d:%.c
	$(CC) $(CFLAGS) $(LDLIBS) -MM $< >$@

include $(SRCS:.c=.d)


all: $(TARGET_LIB)

.PHONY: clean

clean:
	rm -f $(OBJS) $(TARGET_LIB) $(SRCS:.c=.d) xdg-shell-protocol.h xdg-shell-protocol.c
