package body C_Binding.Linux.Udev.List_Entries is

   function Udev_List_Entry_Get_Next
     (Arg1 : Udev_List_Entry_Ptr) return Udev_List_Entry_Ptr;
   pragma Import (C, Udev_List_Entry_Get_Next, "udev_list_entry_get_next");
   --  On success, return a pointer to the requested list entry.
   --  If no such entry can be found, or on failure, null is returned.

   function Udev_List_Entry_Get_By_Name
     (Current : Udev_List_Entry_Ptr;
      Name    : C_String) return Udev_List_Entry_Ptr;
   pragma Import
     (C, Udev_List_Entry_Get_By_Name, "udev_list_entry_get_by_name");
   --  Returns the entry where name matched,
   --  null if no matching entry is found.

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

   procedure Next (List_Entry : in out List_Entries.List_Entry) is
   begin
      List_Entry.My_Ptr := Udev_List_Entry_Get_Next (List_Entry.My_Ptr);
   end Next;

   function Name (List_Entry : List_Entries.List_Entry) return String_Result is
      Text : constant Interfaces.C.Strings.Chars_Ptr
        := Udev_List_Entry_Get_Name (List_Entry.My_Ptr);
   begin
      return Get_String_Result (Text, "List entry name failure");
   end Name;

   function Value (List_Entry : List_Entries.List_Entry) return String_Result is
      Text : constant Interfaces.C.Strings.Chars_Ptr
        := Udev_List_Entry_Get_Value (List_Entry.My_Ptr);
   begin
      return Get_String_Result (Text, "List entry value failure");
   end Value;

   procedure Get_By_Name
     (
      Current : List_Entry;
      Name    : String;
      Found_Entry : out List_Entry
     ) is
   begin
      Found_Entry.My_Ptr
        := Udev_List_Entry_Get_By_Name (Current.My_Ptr, +Name);
   end Get_By_Name;

end C_Binding.Linux.Udev.List_Entries;
