.PHONY: build generate clean

all: generate

build:
	gprbuild -P xml_parser.gpr
generate: build
	./xml_parser /usr/share/wayland/wayland.xml
clean:
	gprclean -P xml_parser.gpr
	rm wayland-*.ads
	rm wayland-*.adb
