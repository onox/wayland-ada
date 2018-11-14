package C_Binding.Linux.Udev.Hardware_Database is

private

   function Udev_Hwdb_New (Arg1 : System.Address) return System.Address;
   pragma Import (C, Udev_Hwdb_New, "udev_hwdb_new");

   function Udev_Hwdb_Ref (Arg1 : System.Address) return System.Address;
   pragma Import (C, Udev_Hwdb_Ref, "udev_hwdb_ref");

   function Udev_Hwdb_Unref (Arg1 : System.Address) return System.Address;
   pragma Import (C, Udev_Hwdb_Unref, "udev_hwdb_unref");

   function Udev_Hwdb_Get_Properties_List_Entry
     (Arg1 : System.Address;
      Arg2 : Interfaces.C.Strings.Chars_Ptr;
      Arg3 : Unsigned) return System.Address;
   pragma Import
     (C,
      Udev_Hwdb_Get_Properties_List_Entry,
      "udev_hwdb_get_properties_list_entry");

end C_Binding.Linux.Udev.Hardware_Database;
