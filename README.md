# Ada binding to the Wayland Client library
The Wayland binding consists of two packages:
- Posix, It is a minimal Ada binding to the Ubuntu Posix API.
- Posix.Wayland, thick Ada binding to the Wayland Client API.

The Ada binding is only known to work on Ubuntu 18.04 due to a bug
in the Gnome Compositor that has a bugfix, but it has only found its way
into Ubuntu 18.04 but not 17.10.

To use the Ada binding in your project with "wayland_client.gpr".

Examples of how to use the Wayland Client Ada binding can be found in gpr-files that start with _example_:
> example_6_3_connect_to_server.gpr,
example_6_4_find_compositor_proxy.gpr

To build all executables "gprbuild -P all_executables.gpr".
Alternatively open all_executables.gpr with the GPS and then select Build -> Project -> Build All.

The Ada binding (package Posix.Wayland) is auto-generated from wayland.xml
by the application xml_parser.gpr.
It can be compiled by GNAT FSF version 7.2 and GNAT Community Edition 2017.



# Notes on the creation of the Wayland Ada binding

The thin ada binding (the nested package Wl_Thin in the private part of
the package Posix.Wayland) is auto-generated from wayland.xml and
then the thick Ada binding is manually built on top of it.

The reason the thick Ada binding is not also auto-generated is because it is
too complicated to auto-generate. For example, consider the request tag:
<interface name="wl_registry" version="1">
  ...
  <request name="bind">
  ...
</interface>
It indicates that the Bind subprogram should belong to the Registry interface.
This works in the thin Ada binding but in the thick one the Bind subprogram
belongs to all the other interfaces like for example Compisotor, Shm, Shell,
and others except for the Display interface.
There is no information in the file wayland.xml that indicates this, which is
correct since it only describes the C API, not how it should be wrapped
in a higher level language like Ada.

Another difficulty is auto-generating a thick Ada binding for event tags:
<interface name="wl_registry" version="1">
  ...
  <event name="global"> ... </event>
    ...
  <event name="global_remove"> ... </event>
  ...
</interface>
The corresponding Ada code in the thin Ada binding
contains access types (pointers) and
they should be hidden from the user in the thick Ada binding which makes
the code non-trivial to auto-generate.

There are more reasons for not auto-generating the thick Ada binding, but that
discussion is left for another day.

# Thanks to
- Dmitry Kazakov for Simple Components. They are used in the wayland.xml-file parser application for reading UTF8-characters.

- Brad Moore for providing Storage Pool (and Subpool) implementations available in the Deepend Open Source project.
