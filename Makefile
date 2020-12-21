CFLAGS ?= -O2 -march=native

GPRBUILD = nice gprbuild -dm
GPRCLEAN = gprclean -q

.PHONY: build examples generate clean

all: generate

build:
	$(GPRBUILD) -P src/tools/wayland_ada_scanner/scanner.gpr -cargs $(CFLAGS)

examples: generate
	$(GPRBUILD) -P tools/examples.gpr -cargs $(CFLAGS)

generate: build
	./bin/wayland-ada-scanner /usr/share/wayland/wayland.xml

clean:
	$(GPRCLEAN) -r -P src/tools/wayland_ada_scanner/scanner.gpr
	$(GPRCLEAN) -P tools/examples.gpr
	rm -rf bin build
	rm src/wayland/wayland-protocols-*.ads
	rm src/wayland/wayland-protocols-*.adb
	rm src/wayland/wayland-enums-*.ads
