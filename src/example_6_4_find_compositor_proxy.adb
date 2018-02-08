with Wl;
with Ada.Text_IO;

-- See section 6.4 at:
-- https://jan.newmarch.name/Wayland/ProgrammingClient/
procedure Example_6_4_Find_Compositor_Proxy is

   Compositor : Wl.Compositor_T;

   procedure Global_Registry_Handler (Data        : Wl.Void_Ptr;
                                      Registry    : Wl.Registry_Ptr;
                                      Id          : Wl.Unsigned_32;
                                      Interface_V : Wl.chars_ptr;
                                      Version     : Wl.Unsigned_32) with
     Convention => C;

   procedure Global_Registry_Handler (Data        : Wl.Void_Ptr;
                                      Registry    : Wl.Registry_Ptr;
                                      Id          : Wl.Unsigned_32;
                                      Interface_V : Wl.chars_ptr;
                                      Version     : Wl.Unsigned_32)
   is
      Temp : Wl.char_array := Wl.Value (Interface_V);
      Interface_Name : String := Wl.To_Ada (Temp);
   begin
      Ada.Text_IO.Put_Line ("Got a registry event for " & Interface_Name & "id" & Id'Image);
      if Interface_Name = "wl_compositor" then
         Compositor.Bind (Registry, Id, 1);
      end if;
      --    printf("Got a registry event for %s id %d\n", interface, id);
--      if (strcmp(interface, "wl_compositor") == 0)
--          compositor = wl_registry_bind(registry,
--  				      id,
--  				      &wl_compositor_interface,
--  				      1);
   end Global_Registry_Handler;

   procedure Global_Registry_Remover (Data     : Wl.Void_Ptr;
                                      Registry : Wl.Registry_Ptr;
                                      Id       : Wl.Unsigned_32) with
     Convention => C;

   procedure Global_Registry_Remover (Data     : Wl.Void_Ptr;
                                      Registry : Wl.Registry_Ptr;
                                      Id       : Wl.Unsigned_32) is
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
         Ada.Text_IO.Put_Line ("Successfully connected to wayland server");
         Get_Registry;
         Display.Disconnect;
         Ada.Text_IO.Put_Line ("Disconnected from wayland server");
      else
         Ada.Text_IO.Put_Line ("Failed to connect to wayland server");
      end if;

      pragma Assert (not Display.Is_Connected);
   end Connect_To_Wayland_Server;

   pragma Unmodified (Display);

   Registry : Wl.Registry_T;

   procedure List_Global_Objects_Available_In_Registry;

   procedure Get_Registry is
   begin
      Registry.Get (Display);
      if Registry.Has_Registry_Object then
         List_Global_Objects_Available_In_Registry;
         Registry.Destroy;
      else
         Ada.Text_IO.Put_Line ("Failed to retrieve Registry!!!!");
      end if;

      pragma Assert (not Registry.Has_Registry_Object);
   end Get_Registry;

   pragma Unmodified (Registry);

   procedure Check_If_Compositor_Found;

   procedure List_Global_Objects_Available_In_Registry is
      I : Wl.int;

      Listener : aliased Wl.Registry_Listener_T :=
        (
         Global        => Global_Registry_Handler'Unrestricted_Access,
         Global_Remove => Global_Registry_Remover'Unrestricted_Access
        );
   begin
      I := Registry.Add_Listener (Listener'Unchecked_Access, Wl.Null_Address);

      Display.Dispatch;
      Display.Roundtrip;

      Check_If_Compositor_Found;
   end List_Global_Objects_Available_In_Registry;

   procedure Check_If_Compositor_Found is
   begin
      if Compositor.Is_Bound then
         Ada.Text_IO.Put_Line ("Found compositor");
      else
         Ada.Text_IO.Put_Line ("Can't find compositor");
      end if;
   end Check_If_Compositor_Found;

begin
   Connect_To_Wayland_Server;
end Example_6_4_Find_Compositor_Proxy;
