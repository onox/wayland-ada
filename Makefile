CFLAGS ?= -O2 -march=native

GPRBUILD = nice gprbuild -dm
GPRCLEAN = gprclean -q

.PHONY: scanner tools libs clean

all: libs

scanner:
	cd wayland_ada_scanner && alr build

tools: protocols
	$(GPRBUILD) -P tools/tools.gpr -cargs $(CFLAGS)

libs: wayland protocols
	cd wayland_egl_ada && alr build
	cd wayland_protocols_ada && alr build

wayland: scanner
	cd wayland_client_ada && alr build

protocols: scanner wayland
	cd wayland_protocols_ada && alr build

clean:
	cd wayland_egl_ada && alr clean
	cd wayland_protocols_ada && alr clean
	cd wayland_client_ada && alr clean
	cd wayland_ada_scanner && alr clean
	$(GPRCLEAN) -P tools/tools.gpr
	rm -rf bin build
