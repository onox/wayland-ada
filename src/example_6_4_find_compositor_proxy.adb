with Wl;
with Ada.Text_IO;
with Interfaces.C.Strings;
with System;

-- See section 6.4 at:
-- https://jan.newmarch.name/Wayland/ProgrammingClient/
procedure Example_6_4_Find_Compositor_Proxy is

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

   procedure Global_Registry_Remover (Data     : Wl.Void_Ptr;
                                      Registry : Wl.Registry_Ptr;
                                      Id       : Interfaces.Unsigned_32) with
     Convention => C;

   procedure Global_Registry_Remover (Data : Wl.Void_Ptr;
                                      Registry : Wl.Registry_Ptr;
                                      Id : Interfaces.Unsigned_32) is
   begin
      Ada.Text_IO.Put_Line ("Got a registry losing event for");
--    printf("Got a registry losing event for %d\n", id);
   end;

   Display : Wl.Display_T;

   procedure Get_Registry;

   procedure Connect_To_Wayland_Server is
   begin
      Display.Connect (Wl.Default_Display_Name);
      if Display.Is_Connected then
         Ada.Text_IO.Put_Line ("Success");
         Get_Registry;
         Display.Disconnect;
      else
         Ada.Text_IO.Put_Line ("Failed to connect to wayland server");
      end if;

      pragma Assert (not Display.Is_Connected);
   end Connect_To_Wayland_Server;

   pragma Unmodified (Display);

   Registry : Wl.Registry_T;

   procedure Use_Register;

   procedure Get_Registry is
   begin
      Registry.Get (Display);
      if Registry.Has_Registry_Object then
         Use_Register;
         Registry.Destroy;
      else
         Ada.Text_IO.Put_Line ("Failed to retrieve Registry!!!!");
      end if;

      pragma Assert (not Registry.Has_Registry_Object);
   end Get_Registry;

   procedure Use_Register is
      I : Interfaces.C.int;

      Listener : aliased Wl.Registry_Listener_T :=
        (
         Global        => Global_Registry_Handler'Unrestricted_Access,
         Global_Remove => Global_Registry_Remover'Unrestricted_Access
        );
   begin
      I := Registry.Add_Listener (Listener'Unchecked_Access, System.Null_Address);

      Display.Dispatch;
      Display.Roundtrip;
   end Use_Register;

begin
   Connect_To_Wayland_Server;
end Example_6_4_Find_Compositor_Proxy;
