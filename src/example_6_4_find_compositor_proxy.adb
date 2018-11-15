with Wayland_Client;
with Ada.Text_IO;

-- See section 6.4 at:
-- https://jan.newmarch.name/Wayland/ProgrammingClient/
procedure Example_6_4_Find_Compositor_Proxy is

   procedure Put_Line (Text : String) renames Ada.Text_IO.Put_Line;

   procedure Global_Registry_Handler
     (Compositor : not null Wayland_Client.Compositor_Ptr;
      Registry   : Wayland_Client.Registry;
      Id         : Wayland_Client.Unsigned_32;
      Name       : String;
      Version    : Wayland_Client.Unsigned_32) is
   begin
      Put_Line ("Got a registry event for " & Name & " id" & Id'Image);

      if Name = "wl_compositor" then
         Compositor.Get_Proxy (Registry, Id, Version);
      end if;
   end Global_Registry_Handler;

   procedure Global_Registry_Remover
     (Data     : not null Wayland_Client.Compositor_Ptr;
      Registry : Wayland_Client.Registry;
      Id       : Wayland_Client.Unsigned_32) is
   begin
      Put_Line ("Got a registry losing event for" & Id'Image);
   end Global_Registry_Remover;

   Compositor : aliased Wayland_Client.Compositor;

   package Subscriber is new Wayland_Client.Registry_Subscriber
     (Data_Type             => Wayland_Client.Compositor_Ptr,
      Data                  => Compositor'Unchecked_Access,
      Global_Object_Added   => Global_Registry_Handler,
      Global_Object_Removed => Global_Registry_Remover);

   Display  : Wayland_Client.Display;
   Registry : Wayland_Client.Registry;

begin
   Display.Connect (Wayland_Client.Default_Display_Name);
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

   Subscriber.Start_Subscription (Registry);
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
end Example_6_4_Find_Compositor_Proxy;
