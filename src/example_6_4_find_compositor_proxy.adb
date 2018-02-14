with Wl;
with Ada.Text_IO;

-- See section 6.4 at:
-- https://jan.newmarch.name/Wayland/ProgrammingClient/
procedure Example_6_4_Find_Compositor_Proxy is

   package Subscriber is new Wl.Global_Objects_Subscriber;

   Compositor : Wl.Compositor_T;
   Display    : Wl.Display_T;
   Registry   : Wl.Registry_T;

begin
   Display.Connect (Wl.Default_Display_Name);
   if not Display.Is_Connected then
      Ada.Text_IO.Put_Line ("Can't connect to display");
      return;
   end if;
   Ada.Text_IO.Put_Line ("Connected to display");

   Registry.Get (Display);
   if not Registry.Has_Registry_Object then
      Ada.Text_IO.Put_Line ("Can't get global registry object");
      return;
   end if;

   Subscriber.Start_Subscription (Registry, Display);

   for Global_Object of Subscriber.Global_Objects loop
      Ada.Text_IO.Put ("Got a registry event for ");
      Ada.Text_IO.Put_Line (Global_Object.Interface_Name & " id" & Global_Object.Id'Image);
      if Global_Object.Interface_Name = "wl_compositor" then
         Compositor.Bind (Registry, Global_Object.Id, 1);
      end if;
   end loop;

   if Compositor.Is_Bound then
      Ada.Text_IO.Put_Line ("Found compositor");
   else
      Ada.Text_IO.Put_Line ("Can't find compositor");
   end if;

   Registry.Destroy;
   Display.Disconnect;
   Ada.Text_IO.Put_Line ("Disconnected from display");
end Example_6_4_Find_Compositor_Proxy;
