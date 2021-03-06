[![Build status](https://github.com/onox/wayland-ada/actions/workflows/build.yaml/badge.svg)](https://github.com/onox/wayland-ada/actions/workflows/build.yaml)
[![License](https://img.shields.io/github/license/onox/wayland-ada.svg?color=blue)](https://github.com/onox/wayland-ada/blob/master/LICENSE)
[![GitHub release](https://img.shields.io/github/release/onox/wayland-ada.svg)](https://github.com/onox/waylanda-ada/releases/latest)
[![IRC](https://img.shields.io/badge/IRC-%23ada%20on%20libera.chat-orange.svg)](https://libera.chat)
[![Gitter chat](https://badges.gitter.im/gitterHQ/gitter.svg)](https://gitter.im/ada-lang/Lobby)

# wayland-ada

Ada 2012 bindings and tools for Wayland (client).

The following [Alire][url-alire] crates exist:

 - *wayland_ada_scanner*. A tool to generate thick bindings for various
   Wayland protocols.

 - *wayland_ada_info*. A tool to display information about interfaces
   advertised by the currently running Wayland compositor.

 - *wayland_client_ada*. Thick bindings for the core Wayland protocol.
   Additionally provides thin bindings for libwayland-client to be used
   by this crate and *wayland_protocols_ada*.

 - *wayland_protocols_ada*. Thick bindings for various stable and
   unstable protocols (see below) from the wayland-protocols project.

 - *wayland_egl_ada*. Bindings for libwayland-egl.

 - *wayland_cursor_ada*. Bindings for libwayland-cursor.

The following stable protocols are supported:

 - xdg-shell (This protocol replaces the wl_shell and wl_shell_surface interfaces)

 - presentation-time

 - viewporter

And the following unstable protocols:

 - idle-inhibit

 - xdg-decoration

 - pointer-constraints

 - pointer-gestures

 - relative-pointer

These crates and Wayland protocols are used by [AWT][url-awt], a window
toolkit that takes care of window management, locking and animating the
pointer cursor, and handling input devices.

## Building

Build and run the *wayland_ada_info* crate with Alire to display information
about Wayland interfaces.

## Dependencies

Requires Wayland 1.16 or higher and wayland-protocols 1.21 or higher.

## Thanks

Much thanks to [**@joakim-strandberg**][url-user-joakim-strandberg] for starting this project and writing
most of the wayland scanner.

## Contributing

Please read the [contributing guidelines][url-contributing] before opening
issues or pull requests.

## License

wayland-ada is distributed under the terms of the [Apache License 2.0][url-apache].

  [url-alire]: https://alire.ada.dev/
  [url-apache]: https://opensource.org/licenses/Apache-2.0
  [url-awt]: https://github.com/onox/awt
  [url-contributing]: /CONTRIBUTING.md
  [url-user-joakim-strandberg]: https://github.com/joakim-strandberg
