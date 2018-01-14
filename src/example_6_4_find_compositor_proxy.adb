with Wl;
with Ada.Text_IO;
with Interfaces.C.Strings;

-- See section 6.4 at:
-- https://jan.newmarch.name/Wayland/ProgrammingClient/
procedure Example_6_4_Find_Compositor_Proxy is

   procedure Global_Registry_Handler (Data        : Wl.Void_Ptr;
                                      Registry    : Wl.Registry_Ptr;
                                      Id          : Interfaces.Unsigned_32;
                                      Interface_V : Interfaces.C.Strings.chars_ptr;
                                      Version : Interfaces.Unsigned_32) is
   begin
      null;
      Ada.Text_IO.Put_Line ("Got a registry event for");
      --    printf("Got a registry event for %s id %d\n", interface, id);
--      if (strcmp(interface, "wl_compositor") == 0)
--          compositor = wl_registry_bind(registry,
--  				      id,
--  				      &wl_compositor_interface,
--  				      1);
   end Global_Registry_Handler;

   procedure Global_Registry_Remover(Data : Wl.Void_Ptr;
                                     Registry : Wl.Registry_T;
                                     Id : Interfaces.Unsigned_32) is
   begin
      Ada.Text_IO.Put_Line ("Got a registry losing event for");
--    printf("Got a registry losing event for %d\n", id);
   end;

--  static const struct wl_registry_listener registry_listener = {
--      global_registry_handler,
--      global_registry_remover
--  };


   procedure Get_Registry (Display : Wl.Display_T);

   procedure Connect_To_Wayland_Server is
   begin
      Get_Registry (Wl.Display_Connect (Wl.Default_Display_Name'Access));
   exception
      when Wl.Failed_To_Connect_Exception =>
         Ada.Text_IO.Put_Line ("Failed to connect to wayland server");
   end Connect_To_Wayland_Server;

   procedure Get_Registry (Display : Wl.Display_T) is
   begin
      Ada.Text_IO.Put_Line ("Success");

      declare
         Registry : Wl.Registry_T := Wl.Display_Get_Registry (Display);
      begin
         null;
      end;
      --      struct wl_registry *registry = wl_display_get_registry(display);
--      wl_registry_add_listener(registry, &registry_listener, NULL);
--
--      wl_display_dispatch(display);
--      wl_display_roundtrip(display);
--
--      if (compositor == NULL) {
--  	fprintf(stderr, "Can't find compositor\n");
--  	exit(1);
--      } else {
--  	fprintf(stderr, "Found compositor\n");
--      }
   end Get_Registry;

begin
   Connect_To_Wayland_Server;
end Example_6_4_Find_Compositor_Proxy;
