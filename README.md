# Ada binding to the Wayland Client library
The Wayland binding consists of two packages:
- Posix, minimal Ada binding to the Ubuntu Posix API.
- Posix.Wayland, thick Ada binding to the Wayland Client API.

The Ada binding is only known to work on Ubuntu 18.04 due to a bug
in the Gnome Compositor that has a bugfix, but it has only found its way
into Ubuntu 18.04 but not 17.10.

To use the Ada binding in your project with "wayland_client.gpr".

Examples of how to use the Wayland Client Ada binding can be found in gpr-files
that start with _example_. To build all executables
"gprbuild -P all_executables.gpr". Alternatively open all_executables.gpr
with the GPS and then select Build -> Project -> Build All.

The Ada binding (package Posix.Wayland) is auto-generated from wayland.xml
by the application xml_parser.gpr. It can be compiled by
GNAT FSF version 7.2 and GNAT Community Edition 2017.

To do anything in Wayland one needs proxy objects to components
(called interfaces in Wayland terminology)
on the Wayland server. They are obtained from a _registry_.
A typical Wayland application starts by opening a connection to
the compositor interface (an object that is
in charge of combining different surfaces into one output).
A program to find a proxy for the compositor,
while listing the other registry objects on the command line (See section 6.4
at https://jan.newmarch.name/Wayland/ProgrammingClient/):
```
with Posix.Wayland;
with Ada.Text_IO;


procedure Example_6_4_Find_Compositor_Proxy is

   package Wl renames Posix.Wayland;

   procedure Put_Line (Text : String) renames Ada.Text_IO.Put_Line;

   type Compositor_Ptr is access all Wl.Compositor;

   procedure Global_Registry_Handler (Data     : not null Compositor_Ptr;
                                      Registry : Wl.Registry;
                                      Id       : Wl.Unsigned_32;
                                      Name     : String;
                                      Version  : Wl.Unsigned_32) is
   begin
      Put_Line ("Got a registry event for " & Name & " id" & Id'Image);

      if Name = "wl_compositor" then
         Data.Bind (Registry, Id, Version);
      end if;
   end Global_Registry_Handler;

   procedure Global_Registry_Remover (Data     : not null Compositor_Ptr;
                                      Registry : Wl.Registry;
                                      Id       : Wl.Unsigned_32) is
   begin
      Put_Line ("Got a registry losing event for" & Id'Image);
   end Global_Registry_Remover;

   Compositor : aliased Wl.Compositor;

   package Subscriber is new Wl.Registry_Objects_Subscriber
     (Data_T                => Compositor_Ptr,
      Data                  => Compositor'Unchecked_Access,
      Global_Object_Added   => Global_Registry_Handler,
      Global_Object_Removed => Global_Registry_Remover);

   Display    : Wl.Display;
   Registry   : Wl.Registry;

begin
   Display.Connect (Wl.Default_Display_Name);
   if not Display.Is_Connected then
      Put_Line ("Can't connect to display");
      return;
   end if;
   Put_Line ("Connected to display");

   Display.Get_Registry (Registry);
   if not Registry.Has_Registry_Object then
      Put_Line ("Can't get global registry object");
      return;
   end if;

   Subscriber.Start_Subscription (Registry);
   Display.Dispatch;
   Display.Roundtrip;

   if Compositor.Is_Bound then
      Put_Line ("Found compositor");
   else
      Put_Line ("Can't find compositor");
   end if;

   Registry.Destroy;
   Display.Disconnect;
   Put_Line ("Disconnected from display");
end Example_6_4_Find_Compositor_Proxy;
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
# Notes on the creation of the Wayland Ada binding

The thin ada binding (the nested package Wl_Thin in the private part of
the package Posix.Wayland) is auto-generated from wayland.xml and
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

There are more reasons for not auto-generating the thick Ada binding, but that
discussion is left for another day.

# Thanks to
- Dmitry Kazakov for Simple Components. They are used in the wayland.xml-file parser application for reading UTF8-characters.

- Brad Moore for providing Storage Pool (and Subpool) implementations available in the Deepend Open Source project.
