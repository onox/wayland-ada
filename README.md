# Ada binding to the Wayland Client library
The Wayland binding consists of two packages:
- Posix, minimal Ada binding to the Ubuntu Posix API.
- Posix.Wayland_Client, thick Ada binding to the Wayland Client API.

The Ada binding is only known to work on Ubuntu 18.04 due to a bug
in the Gnome Compositor that has a bugfix, but it has only found its way
into Ubuntu 18.04 but not 17.10.

To use the Ada binding in your project with "wayland_client.gpr".

Examples of how to use the Wayland Client Ada binding can be found in gpr-files
that start with _example_. To build all executables
"gprbuild -P all_executables.gpr". Alternatively open all_executables.gpr
with the GPS and then select Build -> Project -> Build All.

The Ada binding (package Posix.Wayland_Client) has been auto-generated from
wayland.xml by the application xml_parser.gpr and
then subsequently manually edited.
All Ada source code can be compiled by GNAT FSF version 7.2 and
GNAT Community Edition 2017.

To do anything in Wayland one needs proxy objects to components
(called interfaces in Wayland terminology)
on the Wayland server. They are obtained from a _registry_.
A typical Wayland application starts by opening a connection to
the compositor interface (an object that is
in charge of combining different surfaces into one output).
A program to find a proxy for the compositor,
while listing the other registry objects on the command line (See section 6.4
at https://jan.newmarch.name/Wayland/ProgrammingClient/):
```ada
package Application is

   procedure Main;

end Application;

with Posix.Wayland_Client;
with Ada.Text_IO;
package body Application is

   package Wl renames Posix.Wayland_Client;

   procedure Put_Line (Text : String) renames Ada.Text_IO.Put_Line;

   procedure Global_Registry_Handler (Compositor : not null Wl.Compositor_Ptr;
                                      Registry   : Wl.Registry;
                                      Id         : Wl.Unsigned_32;
                                      Name       : String;
                                      Version    : Wl.Unsigned_32) is
   begin
      Put_Line ("Got a registry event for " & Name & " id" & Id'Image);

      if Name = "wl_compositor" then
         Compositor.Get_Proxy (Registry, Id, Version);
      end if;
   end Global_Registry_Handler;

   procedure Global_Registry_Remover (Data     : not null Wl.Compositor_Ptr;
                                      Registry : Wl.Registry;
                                      Id       : Wl.Unsigned_32) is
   begin
      Put_Line ("Got a registry losing event for" & Id'Image);
   end Global_Registry_Remover;

   package Registry_Events is new Wl.Registry_Events
     (Data_Type             => Wl.Compositor,
      Data_Ptr              => Wl.Compositor_Ptr,
      Global_Object_Added   => Global_Registry_Handler,
      Global_Object_Removed => Global_Registry_Remover);

   Display  : Wl.Display;
   Registry : Wl.Registry;

   Compositor : aliased Wl.Compositor;

   procedure Main is
   begin
      Display.Connect (Wl.Default_Display_Name);
      if not Display.Is_Connected then
         Put_Line ("Can't connect to display");
         return;
      end if;
      Put_Line ("Connected to display");

      Display.Get_Registry (Registry);
      if not Registry.Has_Proxy then
         Put_Line ("Can't get global registry object");
         return;
      end if;

      Registry_Events.Subscribe (Registry, Compositor'Access);
      Display.Dispatch;
      Display.Roundtrip;

      if Compositor.Has_Proxy then
         Put_Line ("Found compositor");
      else
         Put_Line ("Can't find compositor");
      end if;

      Registry.Destroy;
      Display.Disconnect;
      Put_Line ("Disconnected from display");
   end Main;

end Application;
```
The corresponding C code is:
```
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <wayland-client.h>

struct wl_display *display = NULL;
struct wl_compositor *compositor = NULL;

static void
global_registry_handler(void *data, struct wl_registry *registry, uint32_t id,
	       const char *interface, uint32_t version)
{
    printf("Got a registry event for %s id %d\n", interface, id);
    if (strcmp(interface, "wl_compositor") == 0)
        compositor = wl_registry_bind(registry,
				      id,
				      &wl_compositor_interface,
				      1);
}

static void
global_registry_remover(void *data, struct wl_registry *registry, uint32_t id)
{
    printf("Got a registry losing event for %d\n", id);
}

static const struct wl_registry_listener registry_listener = {
    global_registry_handler,
    global_registry_remover
};


int main(int argc, char **argv) {

    display = wl_display_connect(NULL);
    if (display == NULL) {
	fprintf(stderr, "Can't connect to display\n");
	exit(1);
    }
    printf("connected to display\n");

    struct wl_registry *registry = wl_display_get_registry(display);
    wl_registry_add_listener(registry, &registry_listener, NULL);

    wl_display_dispatch(display);
    wl_display_roundtrip(display);

    if (compositor == NULL) {
	fprintf(stderr, "Can't find compositor\n");
	exit(1);
    } else {
	fprintf(stderr, "Found compositor\n");
    }

    wl_display_disconnect(display);
    printf("disconnected from display\n");

    exit(0);
}
```
# The TODO list
 - Investigate the best way to expose events in the thick Ada binding.
 - Add xdg_shell support (which obsoletes wl_shell)
 - Add EGL support
 - Add Vulkan support (when available on Ubuntu)

Contact me if you want to help out: joakimds at kth dot se

# License
All software written by me (and potential contributors)
is released to the public domain or the software
license of your choice. This repository does however contain code that does
not originate from me like code from Dmitry Kazakov's Simple Components (Aida.UTF8).
These software treasures retain their original license
of their original authors.

# Notes on the creation of the Wayland Ada binding

The thin ada binding (the private package C_Binding.Wl_Thin) is auto-generated from wayland.xml and
then the thick Ada binding is manually built on top of it.

The reason the thick Ada binding is not also auto-generated is because it is
too complicated to auto-generate. For example, consider the request tag:
```
<interface name="wl_registry" version="1">
  ...
  <request name="bind">
  ...
</interface>
```
It indicates that the Bind subprogram should belong to the Registry interface.
This works in the thin Ada binding but in the thick one the Bind subprogram
belongs to all the other interfaces like for example Compisotor, Shm, Shell,
and others except for the Display interface.
There is no information in the file wayland.xml that indicates this, which is
correct since it only describes the C API, not how it should be wrapped
in a higher level language like Ada.

Another difficulty is auto-generating a thick Ada binding for event tags:
```
<interface name="wl_registry" version="1">
  ...
  <event name="global"> ... </event>
    ...
  <event name="global_remove"> ... </event>
  ...
</interface>
```
The corresponding Ada code in the thin Ada binding
contains access types (pointers) and
they should be hidden from the user in the thick Ada binding which makes
the code non-trivial to auto-generate.

There are more reasons for not auto-generating the thick Ada binding,
but stopping the discussion here.

# Thanks to
- Dmitry Kazakov for Simple Components. They are used in the wayland.xml-file parser application for reading UTF8-characters.
