# Ada binding to the Wayland Client library
The Wayland binding consists of two packages:
- Thin binding in the "Wl_Thin" package. It is auto-generated from wayland.xml (programming language agnostic description of the API).
This is part is complete but untested. To use the thin binding in your project with "wayland_client_thin.gpr".
- Thick binding in the "Wl" package, manually crafted and built on top of the thin binding.
  This part is under construction. To use the thick binding in your project with "wayland_client.gpr".

To build all executables "gprbuild -P all_executables.gpr".
Alternatively open all_executables.gpr with the GPS and then select Build -> Project -> Build All.

Examples of how to use the Wayland Client Ada binding can be found in gpr-files that start with _example_:
> example_6_3_connect_to_server.gpr,
example_6_4_find_compositor_proxy.gpr

The thin Ada binding Wl_Thin is auto-generated from wayland.xml by the application xml_parser.gpr.
It can be compiled by GNAT FSF version 7.2 and GNAT Community Edition 2017.

# Thanks to
- Dmitry Kazakov for Simple Components. They are used in the wayland.xml-file parser application for reading UTF8-characters.

- Brad Moore for providing Storage Pool (and Subpool) implementations available in the Deepend Open Source project.
