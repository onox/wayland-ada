with Wl;
with Ada.Text_IO;
with Interfaces.C.Strings;
with System;

-- See section 6.4 at:
-- https://jan.newmarch.name/Wayland/ProgrammingClient/
procedure Example_6_4_Find_Compositor_Proxy is

   use type Wl.Display_Ptr;
   use type Wl.Registry_Ptr;

   procedure Global_Registry_Handler (Data        : Wl.Void_Ptr;
                                      Registry    : Wl.Registry_Ptr;
                                      Id          : Interfaces.Unsigned_32;
                                      Interface_V : Interfaces.C.Strings.chars_ptr;
                                      Version     : Interfaces.Unsigned_32) with
     Convention => C;

   procedure Global_Registry_Handler (Data        : Wl.Void_Ptr;
                                      Registry    : Wl.Registry_Ptr;
                                      Id          : Interfaces.Unsigned_32;
                                      Interface_V : Interfaces.C.Strings.chars_ptr;
                                      Version     : Interfaces.Unsigned_32)
   is
      Temp : Interfaces.C.char_array := Interfaces.C.Strings.Value (Interface_V);
      Interface_Name : String := Interfaces.C.To_Ada (Temp);
   begin
      Ada.Text_IO.Put_Line ("Got a registry event for " & Interface_Name & "id" & Id'Image);
      --    printf("Got a registry event for %s id %d\n", interface, id);
--      if (strcmp(interface, "wl_compositor") == 0)
--          compositor = wl_registry_bind(registry,
--  				      id,
--  				      &wl_compositor_interface,
--  				      1);
   end Global_Registry_Handler;

   procedure Global_Registry_Remover(Data     : Wl.Void_Ptr;
                                     Registry : Wl.Registry_Ptr;
                                     Id       : Interfaces.Unsigned_32) with
     Convention => C;

   procedure Global_Registry_Remover(Data : Wl.Void_Ptr;
                                     Registry : Wl.Registry_Ptr;
                                     Id : Interfaces.Unsigned_32) is
   begin
      Ada.Text_IO.Put_Line ("Got a registry losing event for");
--    printf("Got a registry losing event for %d\n", id);
   end;

   procedure Get_Registry;

   Display : Wl.Display_Ptr;

   procedure Connect_To_Wayland_Server is
   begin
      Display := Wl.Display_Connect (Wl.Default_Display_Name'Access);
      if Display /= null then
         Ada.Text_IO.Put_Line ("Success");
         Get_Registry;
         Wl.Display_Disconnect (Display);
      else
         Ada.Text_IO.Put_Line ("Failed to connect to wayland server");
      end if;
   end Connect_To_Wayland_Server;

   pragma Unmodified (Display);

   procedure Use_Register;

   Registry : Wl.Registry_Ptr;

   procedure Get_Registry is
   begin
      Registry := Wl.Display_Get_Registry (Display);

      if Registry /= null then
         Use_Register;
         Wl.Registry_Destroy (Registry);
      else
         Ada.Text_IO.Put_Line ("Registry is null!!!!");
      end if;
   end Get_Registry;

   pragma Unmodified (Registry);

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
   procedure Use_Register
   is
      I : Interfaces.C.int;
      Listener : aliased Wl.Registry_Listener_T :=
        (
         Global        => Global_Registry_Handler'Unrestricted_Access,
         Global_Remove => Global_Registry_Remover'Unrestricted_Access
        );
   begin
      I := Wl.Registry_Add_Listener (Registry, Listener'Unchecked_Access, System.Null_Address);

      I := Wl.Display_Dispatch (Display);
      I := Wl.Display_Roundtrip (Display);
   end Use_Register;

begin
   Connect_To_Wayland_Server;
end Example_6_4_Find_Compositor_Proxy;
