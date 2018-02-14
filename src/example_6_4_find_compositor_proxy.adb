with Wl;
with Ada.Text_IO;

-- See section 6.4 at:
-- https://jan.newmarch.name/Wayland/ProgrammingClient/
procedure Example_6_4_Find_Compositor_Proxy is

   package Subscriber is new Wl.Global_Objects_Subscriber;

   Compositor : Wl.Compositor_T;

   Display : Wl.Display_T;

   procedure Get_Registry;

   procedure Connect_To_Wayland_Server is
   begin
      Display.Connect (Wl.Default_Display_Name);
      if Display.Is_Connected then
         Ada.Text_IO.Put_Line ("Successfully connected to wayland server");
         Get_Registry;

--         Wl.Clear_Global_Objects;

         Display.Disconnect;
         Ada.Text_IO.Put_Line ("Disconnected from wayland server");
      else
         Ada.Text_IO.Put_Line ("Failed to connect to wayland server");
      end if;

      pragma Assert (not Display.Is_Connected);
   end Connect_To_Wayland_Server;

   pragma Unmodified (Display);

   Registry : Wl.Registry_T;

   procedure Get_Global_Objects_Available_In_Registry;

   procedure Get_Registry is
   begin
      Registry.Get (Display);
      if Registry.Has_Registry_Object then
         Get_Global_Objects_Available_In_Registry;
         Registry.Destroy;
      else
         Ada.Text_IO.Put_Line ("Failed to retrieve Registry!!!!");
      end if;

      pragma Assert (not Registry.Has_Registry_Object);
   end Get_Registry;

   procedure Check_If_Compositor_Found;

   procedure Get_Global_Objects_Available_In_Registry is
   begin
      Subscriber.Start_Subscription (Registry, Display);

      Check_If_Compositor_Found;
   end Get_Global_Objects_Available_In_Registry;

   pragma Unmodified (Registry);

   procedure Check_If_Compositor_Found is
   begin
      for Global_Object of Subscriber.Global_Objects loop
         if Global_Object.Interface_Name = "wl_compositor" then
            Compositor.Bind (Registry, Global_Object.Id, 1);
            exit;
         end if;
      end loop;

      if Compositor.Is_Bound then
         Ada.Text_IO.Put_Line ("Found compositor");
      else
         Ada.Text_IO.Put_Line ("Can't find compositor");
      end if;
   end Check_If_Compositor_Found;

begin
   Connect_To_Wayland_Server;
end Example_6_4_Find_Compositor_Proxy;
