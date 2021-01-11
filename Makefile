CFLAGS ?= -O2 -march=native

GPRBUILD = nice gprbuild -dm
GPRCLEAN = gprclean -q

PROTOCOLS_DIR = ../wayland-protocols

.PHONY: build tools generate clean

all: generate

build:
	$(GPRBUILD) -P src/tools/wayland_ada_scanner/scanner.gpr -cargs $(CFLAGS)

tools: generate
	$(GPRBUILD) -P tools/tools.gpr -cargs $(CFLAGS)

generate: build
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
