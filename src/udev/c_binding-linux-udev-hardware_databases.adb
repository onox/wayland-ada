package body C_Binding.Linux.Udev.Hardware_Databases is

   function Udev_Hwdb_Ref (Hwdb : Udev_Hwdb_Ptr) return Udev_Hwdb_Ptr;
   pragma Import (C, Udev_Hwdb_Ref, "udev_hwdb_ref");

   function Udev_Hwdb_Unref (Hwdb : Udev_Hwdb_Ptr) return Udev_Hwdb_Ptr;
   pragma Import (C, Udev_Hwdb_Unref, "udev_hwdb_unref");

   function Udev_Hwdb_Get_Properties_List_Entry
     (Hwdb     : Udev_Hwdb_Ptr;
      Modalias : C_String;
      Flags    : Unsigned  --  Is unused
     ) return Udev_List_Entry_Ptr;
   pragma Import
     (C,
      Udev_Hwdb_Get_Properties_List_Entry,
      "udev_hwdb_get_properties_list_entry");
   --  Lookup a matching device in the hardware database. The lookup key
   --  is a modalias string, whose formats are defined for the Linux kernel
   --  modules. Examples are: pci:v00008086d00001C2D*, usb:v04F2pB221*.
   --  The first entry of a list of retrieved properties is returned.

   procedure Acquire
     (Original  : Database;
      Reference : out Database)
   is
   begin
      Reference.My_Ptr := Udev_Hwdb_Ref (Original.My_Ptr);
   end Acquire;

   function Exists (Database : Hardware_Databases.Database) return Boolean is
     (Database.My_Ptr /= null);

   procedure Delete
     (Database : in out Hardware_Databases.Database)
   is
   begin
      Database.My_Ptr := Udev_Hwdb_Unref (Database.My_Ptr);

      Database.My_Ptr := null;
      --  Is unnecessary, but static code analyzers cannot know
      --  Udev_Hwdb_Unref (..) always returns null.
   end Delete;

   procedure Properties_List_Entry
     (Database   : Hardware_Databases.Database;
      Modalias   : String;
      List_Entry : out List_Entries.List_Entry) is
   begin
      List_Entry_Base (List_Entry).My_Ptr
        := Udev_Hwdb_Get_Properties_List_Entry (Database.My_Ptr, +Modalias, 0);
   end Properties_List_Entry;

   procedure Finalize (Database : in out Hardware_Databases.Database) is
   begin
      if Database.Exists then
         Database.Delete;
      end if;
   end Finalize;

end C_Binding.Linux.Udev.Hardware_Databases;
