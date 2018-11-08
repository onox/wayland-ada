package body C_Binding.Linux.Udev.List_Entries is

   procedure Next (LE : in out List_Entry) is
   begin
      LE.My_Ptr := Thin.Udev_List_Entry_Get_Next (LE.My_Ptr);
   end Next;

   function Name (LE : List_Entry) return String_Result is
      Text : constant Interfaces.C.Strings.Chars_Ptr
        := Thin.Udev_List_Entry_Get_Name (LE.My_Ptr);
   begin
      return Get_String_Result (Text, "List entry name failure");
   end Name;

   function Value (LE : List_Entry) return String_Result is
      Text : constant Interfaces.C.Strings.Chars_Ptr
        := Thin.Udev_List_Entry_Get_Value (LE.My_Ptr);
   begin
      return Get_String_Result (Text, "List entry value failure");
   end Value;

end C_Binding.Linux.Udev.List_Entries;
