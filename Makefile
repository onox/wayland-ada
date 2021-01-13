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

PROTOCOLS = ../../../wayland-protocols

GEN_PROTOCOLS = generated/protocols

SCANNER = ../../wayland_ada_scanner/alire/build/bin/wayland_ada_scanner

.PHONY: scanner tools libs clean

all: libs

scanner:
	cd wayland_ada_scanner && alr build

tools: protocols
	$(GPRBUILD) -P tools/tools.gpr -cargs $(CFLAGS)

libs: wayland protocols
	$(GPRBUILD) -P tools/wayland_egl.gpr -cargs $(CFLAGS)
	$(GPRBUILD) -P tools/wayland_protocols.gpr -cargs $(CFLAGS)

wayland: scanner
	cd wayland_client_ada && alr build

protocols: scanner wayland
	mkdir -p $(GEN_PROTOCOLS)
	cd $(GEN_PROTOCOLS) && $(SCANNER) $(PROTOCOLS)/stable/xdg-shell/xdg-shell.xml
	cd $(GEN_PROTOCOLS) && $(SCANNER) $(PROTOCOLS)/stable/presentation-time/presentation-time.xml
	cd $(GEN_PROTOCOLS) && $(SCANNER) $(PROTOCOLS)/stable/viewporter/viewporter.xml
	cd $(GEN_PROTOCOLS) && $(SCANNER) $(PROTOCOLS)/unstable/idle-inhibit/idle-inhibit-unstable-v1.xml
	cd $(GEN_PROTOCOLS) && $(SCANNER) $(PROTOCOLS)/unstable/xdg-decoration/xdg-decoration-unstable-v1.xml
	cd $(GEN_PROTOCOLS) && $(SCANNER) $(PROTOCOLS)/unstable/pointer-constraints/pointer-constraints-unstable-v1.xml
	cd $(GEN_PROTOCOLS) && $(SCANNER) $(PROTOCOLS)/unstable/pointer-gestures/pointer-gestures-unstable-v1.xml
	cd $(GEN_PROTOCOLS) && $(SCANNER) $(PROTOCOLS)/unstable/relative-pointer/relative-pointer-unstable-v1.xml

clean:
	cd wayland_client_ada && alr clean
	cd wayland_ada_scanner && alr clean
	$(GPRCLEAN) -P tools/tools.gpr
	$(GPRCLEAN) -P tools/wayland_egl.gpr
	$(GPRCLEAN) -P tools/wayland_protocols.gpr
	rm -rf bin build generated

install:
	$(installcmd) -f --install-name='wayland-client-ada' -P tools/wayland_client.gpr
	$(installcmd) -f --install-name='wayland-protocols-ada' -P tools/wayland_protocols.gpr
	$(installcmd) -f --install-name='wayland-egl-ada' -P tools/wayland_egl.gpr

uninstall:
	$(installcmd) --uninstall --install-name='wayland-egl-ada' -P tools/wayland_egl.gpr
	$(installcmd) --uninstall --install-name='wayland-protocols-ada' -P tools/wayland_protocols.gpr
	$(installcmd) --uninstall --install-name='wayland-client-ada' -P tools/wayland_client.gpr
