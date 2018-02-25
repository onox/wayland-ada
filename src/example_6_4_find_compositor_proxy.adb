with Wl;
with Ada.Text_IO;

-- See section 6.4 at:
-- https://jan.newmarch.name/Wayland/ProgrammingClient/
procedure Example_6_4_Find_Compositor_Proxy is

   type Compositor_Ptr is access all Wl.Compositor_T;

   procedure Global_Registry_Handler (Data     : not null Compositor_Ptr;
                                      Registry : Wl.Registry_T;
                                      Id       : Wl.Unsigned_32;
                                      Name     : String;
                                      Version  : Wl.Unsigned_32) is
   begin
      Ada.Text_IO.Put_Line
        ("Got a registry event for " & Name & " id" & Id'Image);

      if Name = "wl_compositor" then
         Data.Bind (Registry, Id, Version);
      end if;
   end Global_Registry_Handler;

   procedure Global_Registry_Remover (Data     : not null Compositor_Ptr;
                                      Registry : Wl.Registry_T;
                                      Id       : Wl.Unsigned_32) is
   begin
      Ada.Text_IO.Put_Line ("Got a registry losing event for" & Id'Image);
   end Global_Registry_Remover;

   Compositor : aliased Wl.Compositor_T;

   package Subscriber is new Wl.Registry_Objects_Subscriber
     (Data_T                => Compositor_Ptr,
      Data                  => Compositor'Unchecked_Access,
      Global_Object_Added   => Global_Registry_Handler,
      Global_Object_Removed => Global_Registry_Remover);

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

   Subscriber.Start_Subscription (Registry);
   Display.Dispatch;
   Display.Roundtrip;

   if Compositor.Is_Bound then
      Ada.Text_IO.Put_Line ("Found compositor");
   else
      Ada.Text_IO.Put_Line ("Can't find compositor");
   end if;

   Registry.Destroy;
   Display.Disconnect;
   Ada.Text_IO.Put_Line ("Disconnected from display");
end Example_6_4_Find_Compositor_Proxy;
