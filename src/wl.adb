package body Wl is

   function wl_display_connect (Name : in Interfaces.C.Strings.chars_ptr) return Hidden_Display_Ptr with
     Convention => C,
     Import     => True;

   procedure wl_display_disconnect (Display : in Hidden_Display_Ptr) with
     Convention => C,
     Import     => True;

   function Display_Connect (Name       : in     Interfaces.C.Strings.char_array_access;
                             Is_Success :    out Boolean) return Display_T is
   begin
      return This : Display_T do
         This.Hidden_Display := wl_display_connect(Interfaces.C.Strings.To_Chars_Ptr (Name));
         Is_Success := This.Hidden_Display /= null;
      end return;
   end Display_Connect;

   procedure Adjust (This : in out Display_T) is
   begin
      if This.Hidden_Display /= null then
         This.Number_Of_Instances := This.Number_Of_Instances + 1;
      end if;
   end Adjust;

   procedure Finalize (This : in out Display_T) is
   begin
      if This.Number_Of_Instances = 2 and then This.Hidden_Display /= null then
         wl_display_disconnect (This.Hidden_Display);
         This.Hidden_Display := null;
      end if;
   end Finalize;

end Wl;
