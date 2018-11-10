package body C_Binding.Linux.Udev.List_Entries is

   function Udev_List_Entry_Get_Next
     (Arg1 : Udev_List_Entry_Ptr) return Udev_List_Entry_Ptr;
   pragma Import (C, Udev_List_Entry_Get_Next, "udev_list_entry_get_next");
   --  On success, return a pointer to the requested list entry.
   --  If no such entry can be found, or on failure, null is returned.

--     function Udev_List_Entry_Get_By_Name
--       (Arg1 : System.Address;
--        Arg2 : Interfaces.C.Strings.Chars_Ptr) return System.Address;
--     pragma Import
--       (C, Udev_List_Entry_Get_By_Name, "udev_list_entry_get_by_name");

   function Udev_List_Entry_Get_Name
     (Arg1 : Udev_List_Entry_Ptr) return Interfaces.C.Strings.Chars_Ptr;
   pragma Import (C, Udev_List_Entry_Get_Name, "udev_list_entry_get_name");
   --  Success, return a pointer to a constant string
   --  representing the requested value. The string is bound to
   --  the lifetime of the list entry itself. On failure, null is returned.

   function Udev_List_Entry_Get_Value
     (Arg1 : Udev_List_Entry_Ptr) return Interfaces.C.Strings.Chars_Ptr;
   pragma Import
     (C, Udev_List_Entry_Get_Value, "udev_list_entry_get_value");
   --  Success, return a pointer to a constant string
   --  representing the requested value. The string is bound to
   --  the lifetime of the list entry itself. On failure, null is returned.

   procedure Next (LE : in out List_Entry) is
   begin
      LE.My_Ptr := Udev_List_Entry_Get_Next (LE.My_Ptr);
   end Next;

   function Name (LE : List_Entry) return String_Result is
      Text : constant Interfaces.C.Strings.Chars_Ptr
        := Udev_List_Entry_Get_Name (LE.My_Ptr);
   begin
      return Get_String_Result (Text, "List entry name failure");
   end Name;

   function Value (LE : List_Entry) return String_Result is
      Text : constant Interfaces.C.Strings.Chars_Ptr
        := Udev_List_Entry_Get_Value (LE.My_Ptr);
   begin
      return Get_String_Result (Text, "List entry value failure");
   end Value;

end C_Binding.Linux.Udev.List_Entries;
