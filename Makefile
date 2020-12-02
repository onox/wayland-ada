CFLAGS ?= -O2 -march=native

GPRBUILD = nice gprbuild -dm
GPRCLEAN = gprclean -q

.PHONY: build examples generate clean

all: generate

build:
	$(GPRBUILD) -P src/tools/xml_parser/xml_parser.gpr -cargs $(CFLAGS)

examples: generate
	$(GPRBUILD) -P tools/examples.gpr -cargs $(CFLAGS)

generate: build
	./bin/xml_parser /usr/share/wayland/wayland.xml

clean:
	$(GPRCLEAN) -r -P src/tools/xml_parser/xml_parser.gpr
	$(GPRCLEAN) -P tools/examples.gpr
	rm -rf bin build
	rm src/wayland/wayland-client*.ads
	rm src/wayland/wayland-client*.adb
