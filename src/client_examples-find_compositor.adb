with Wayland_Client;

package body Client_Examples.Find_Compositor is

   use all type Wayland_Client.Call_Result_Code;

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

   package Registry_Events is new Wayland_Client.Registry_Events
     (Data_Type             => Wayland_Client.Compositor,
      Data_Ptr              => Wayland_Client.Compositor_Ptr,
      Global_Object_Added   => Global_Registry_Handler,
      Global_Object_Removed => Global_Registry_Remover);

   Display  : Wayland_Client.Display;
   Registry : Wayland_Client.Registry;

   Compositor : aliased Wayland_Client.Compositor;

   Call_Result : Wayland_Client.Call_Result_Code;

   procedure Run is
   begin
      Display.Connect;
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

      Call_Result := Registry_Events.Subscribe (Registry, Compositor'Access);
      case Call_Result is
         when Success =>
            Put_Line ("Successfully subscribed to registry events");
         when Error =>
            Put_Line ("Failed to subscribe to registry events");
            Display.Disconnect;
            return;
      end case;

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
   end Run;

end Client_Examples.Find_Compositor;
