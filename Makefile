.PHONY: scanner info tools libs clean

all: libs

scanner:
	cd wayland_ada_scanner && alr build

info: tools
	cd wayland_ada_info && alr run

tools: protocols
	cd wayland_ada_info && alr build

libs: wayland protocols
	cd wayland_egl_ada && alr build
	cd wayland_cursor_ada && alr build
	cd wayland_protocols_ada && alr build

wayland: scanner
	cd wayland_client_ada && alr build

protocols: scanner wayland
	cd wayland_protocols_ada && alr build

clean:
	cd wayland_egl_ada && alr clean
	cd wayland_cursor_ada && alr clean
	cd wayland_protocols_ada && alr clean
	cd wayland_client_ada && alr clean
	cd wayland_ada_scanner && alr clean
	cd wayland_ada_info && alr clean
