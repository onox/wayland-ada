with Wayland.Client.Protocol;

package body Client_Examples.Find_Compositor is

   use all type Wayland.Client.Protocol.Call_Result_Code;

   type Data_Type is limited record
      Compositor : aliased Wayland.Client.Protocol.Compositor;
   end record;

   type Data_Ptr is access all Data_Type;

   Data : aliased Data_Type;

   procedure Global_Registry_Handler
     (Data       : not null Data_Ptr;
      Registry   : Wayland.Client.Protocol.Registry;
      Id         : Wayland.Unsigned_32;
      Name       : String;
      Version    : Wayland.Unsigned_32) is
   begin
      Put_Line ("Got a registry event for " & Name & " version" & Version'Image & " id" & Id'Image);

      if Name = "wl_compositor" then
         Data.Compositor.Bind (Registry, Id, Version);
      end if;
   end Global_Registry_Handler;

   procedure Global_Registry_Remover
     (Data     : not null Data_Ptr;
      Registry : Wayland.Client.Protocol.Registry;
      Id       : Wayland.Unsigned_32) is
   begin
      Put_Line ("Got a registry losing event for" & Id'Image);
   end Global_Registry_Remover;

   package Registry_Events is new Wayland.Client.Protocol.Registry_Events
     (Data_Type             => Data_Type,
      Data_Ptr              => Data_Ptr,
      Global_Object_Added   => Global_Registry_Handler,
      Global_Object_Removed => Global_Registry_Remover);

   Display  : Wayland.Client.Protocol.Display;
   Registry : Wayland.Client.Protocol.Registry;

   Call_Result : Wayland.Client.Protocol.Call_Result_Code;

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

      Call_Result := Registry_Events.Subscribe (Registry, Data'Access);
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

      if Data.Compositor.Has_Proxy then
         Put_Line ("Found compositor");
      else
         Put_Line ("Can't find compositor");
      end if;

      Registry.Destroy;
      Display.Disconnect;
      Put_Line ("Disconnected from display");
   end Run;

end Client_Examples.Find_Compositor;
