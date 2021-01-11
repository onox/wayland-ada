CFLAGS ?= -O2 -march=native

GPRBUILD = nice gprbuild -dm
GPRCLEAN = gprclean -q
GNATINSTALL = gprinstall

PREFIX ?= /usr

includedir = $(PREFIX)/include
gprdir     = $(PREFIX)/share/gpr
libdir     = $(PREFIX)/lib
alidir     = $(libdir)

installcmd = $(GNATINSTALL) -p \
	--sources-subdir=$(includedir) \
	--project-subdir=$(gprdir) \
	--lib-subdir=$(libdir) \
	--ali-subdir=$(alidir) \
	--prefix=$(PREFIX)

PROTOCOLS_DIR = ../wayland-protocols

.PHONY: scanner tools generate clean

all: libs

scanner:
	$(GPRBUILD) -P src/tools/wayland_ada_scanner/scanner.gpr -cargs $(CFLAGS)

tools: generate
	$(GPRBUILD) -P tools/tools.gpr -cargs $(CFLAGS)

libs: generate
	$(GPRBUILD) -P tools/wayland_egl.gpr -cargs $(CFLAGS)

generate: scanner
	mkdir -p generated
	./bin/wayland-ada-scanner /usr/share/wayland/wayland.xml
	./bin/wayland-ada-scanner $(PROTOCOLS_DIR)/stable/xdg-shell/xdg-shell.xml
	./bin/wayland-ada-scanner $(PROTOCOLS_DIR)/stable/presentation-time/presentation-time.xml
	./bin/wayland-ada-scanner $(PROTOCOLS_DIR)/stable/viewporter/viewporter.xml
	./bin/wayland-ada-scanner $(PROTOCOLS_DIR)/unstable/idle-inhibit/idle-inhibit-unstable-v1.xml
	./bin/wayland-ada-scanner $(PROTOCOLS_DIR)/unstable/xdg-decoration/xdg-decoration-unstable-v1.xml
	./bin/wayland-ada-scanner $(PROTOCOLS_DIR)/unstable/pointer-constraints/pointer-constraints-unstable-v1.xml
	./bin/wayland-ada-scanner $(PROTOCOLS_DIR)/unstable/pointer-gestures/pointer-gestures-unstable-v1.xml
	./bin/wayland-ada-scanner $(PROTOCOLS_DIR)/unstable/relative-pointer/relative-pointer-unstable-v1.xml

clean:
	$(GPRCLEAN) -r -P src/tools/wayland_ada_scanner/scanner.gpr
	$(GPRCLEAN) -P tools/tools.gpr
	rm -rf bin build generated

install:
	$(installcmd) -f --install-name='wayland-client-ada' -P tools/wayland_client.gpr
	$(installcmd) -f --install-name='wayland-protocols-ada' -P tools/wayland_protocols.gpr
	$(installcmd) -f --install-name='wayland-egl-ada' -P tools/wayland_egl.gpr

uninstall:
	$(installcmd) --uninstall --install-name='wayland-egl-ada' -P tools/wayland_egl.gpr
	$(installcmd) --uninstall --install-name='wayland-protocols-ada' -P tools/wayland_protocols.gpr
	$(installcmd) --uninstall --install-name='wayland-client-ada' -P tools/wayland_client.gpr
