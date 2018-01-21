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

   procedure Get_Registry (Display : Wl.Display_T);

   procedure Connect_To_Wayland_Server is
   begin
      Get_Registry (Wl.Display_Connect (Wl.Default_Display_Name));
   exception
      when Wl.Display_Connection_Exception =>
         Ada.Text_IO.Put_Line ("Failed to connect to wayland server");
   end Connect_To_Wayland_Server;

   procedure Use_Register (Display : Wl.Display_T;
                           Registry : Wl.Registry_T);

   procedure Get_Registry (Display : Wl.Display_T) is
   begin
      Ada.Text_IO.Put_Line ("Success");

      Use_Register (Display, Wl.Display_Get_Registry (Display));
   exception
      when Wl.Registry_Exception =>
         Ada.Text_IO.Put_Line ("Failed to retrieve Registry!!!!");
   end Get_Registry;

   procedure Use_Register (Display  : Wl.Display_T;
                           Registry : Wl.Registry_T)
   is
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
