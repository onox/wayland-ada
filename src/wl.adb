with Ada.Text_IO;

package body Wl is

   use type Wl_Thin.Display_Ptr;
   use type Wl_Thin.Registry_Ptr;

   function Display_Connect (Name : Interfaces.C.Strings.char_array_access) return Display_T is
   begin
      return This : Display_T do
         This.My_Display := Wl_Thin.Display_Connect (Name);
         if This.My_Display = null then
            raise Display_Connection_Exception;
         end if;
      end return;
   end Display_Connect;

   procedure Finalize (This : in out Display_T) is
   begin
      if This.My_Display /= null then
         Wl_Thin.Display_Disconnect (This.My_Display);
         Ada.Text_IO.Put_Line ("Disconnect from server!");
      end if;
   end Finalize;

   function Display_Get_Registry (Display : Display_T) return Registry_T is
   begin
      return This : Registry_T do
         This.My_Registry := Wl_Thin.Display_Get_Registry (Display.My_Display);
         if This.My_Registry = null then
            raise Registry_Exception;
         end if;
      end return;
   end Display_Get_Registry;

   procedure Finalize (This : in out Registry_T) is
   begin
      if This.My_Registry /= null then
         Wl_Thin.Registry_Destroy (This.My_Registry);
         Ada.Text_IO.Put_Line ("Destroy registry!");
         This.My_Registry := null;
      end if;
   end Finalize;

   function Registry_Add_Listener (Registry : Registry_T;
                                   Listener : Registry_Listener_Ptr;
                                   Data     : Wl.Void_Ptr) return Interfaces.C.int is
   begin
      return Wl_Thin.Registry_Add_Listener (Registry.My_Registry, Listener, Data);
   end Registry_Add_Listener;

   function Display_Dispatch (Display : Display_T) return Interfaces.C.int is
   begin
      return Wl_Thin.Display_Dispatch (Display.My_Display);
   end Display_Dispatch;

   procedure Display_Dispatch (Display : Display_T) is
      I : Interfaces.C.int;
      pragma Unreferenced (I);
   begin
      I := Display.Display_Dispatch;
   end Display_Dispatch;

   function Display_Roundtrip (Display : Display_T) return Interfaces.C.int is
   begin
      return Wl_Thin.Display_Dispatch (Display.My_Display);
   end Display_Roundtrip;

   procedure Display_Roundtrip (Display : Display_T) is
      I : Interfaces.C.int;
      pragma Unreferenced (I);
   begin
      I := Display.Display_Roundtrip;
   end Display_Roundtrip;

end Wl;
