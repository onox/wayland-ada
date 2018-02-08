with Ada.Text_IO;

package body Wl is

   use type Wl_Thin.Proxy_Ptr;
   use type Wl_Thin.Display_Ptr;
   use type Wl_Thin.Registry_Ptr;

   procedure Connect (Display : in out Display_T;
                      Name    : Interfaces.C.Strings.char_array_access) is
   begin
      Display.My_Display := Wl_Thin.Display_Connect (Name);
   end Connect;

   procedure Disconnect (Display : in out Display_T) is
   begin
      if Display.My_Display /= null then
         Wl_Thin.Display_Disconnect (Display.My_Display);
      end if;
   end Disconnect;

   procedure Get (Registry : in out Registry_T;
                  Display  : Display_T) is
   begin
      Registry.My_Registry := Wl_Thin.Display_Get_Registry (Display.My_Display);
   end Get;

   procedure Destroy (Registry : in out Registry_T) is
   begin
      if Registry.My_Registry /= null then
         Wl_Thin.Registry_Destroy (Registry.My_Registry);
         Registry.My_Registry := null;
      end if;
   end Destroy;

   function Add_Listener (Registry : Registry_T;
                          Listener : Registry_Listener_Ptr;
                          Data     : Wl.Void_Ptr) return Interfaces.C.int is
   begin
      return Wl_Thin.Registry_Add_Listener (Registry.My_Registry, Listener, Data);
   end Add_Listener;

   function Dispatch (Display : Display_T) return Interfaces.C.int is
   begin
      return Wl_Thin.Display_Dispatch (Display.My_Display);
   end Dispatch;

   procedure Dispatch (Display : Display_T) is
      I : Interfaces.C.int;
      pragma Unreferenced (I);
   begin
      I := Display.Dispatch;
   end Dispatch;

   function Roundtrip (Display : Display_T) return Interfaces.C.int is
   begin
      return Wl_Thin.Display_Roundtrip (Display.My_Display);
   end Roundtrip;

   procedure Roundtrip (Display : Display_T) is
      I : Interfaces.C.int;
      pragma Unreferenced (I);
   begin
      I := Display.Roundtrip;
   end Roundtrip;

   procedure Bind (Compositor  : in out Compositor_T;
                   Registry    : Registry_Ptr;
                   Id          : Wl.Unsigned_32;
                   Version     : Wl.Unsigned_32)
   is
      P : Wl_Thin.Proxy_Ptr :=
        Wl_Thin.Registry_Bind (Registry    => Registry,
                               Name        => Id,
                               Interface_V => Wl_Thin.Compositor_Interface'Access,
                               New_Id      => Version);

   begin
      if P /= null then
         Compositor.My_Compositor := P.all'Access;
      end if;
   end Bind;

end Wl;
