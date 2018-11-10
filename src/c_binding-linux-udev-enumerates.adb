package body C_Binding.Linux.Udev.Enumerates is

   use type int;

--     function Udev_Enumerate_Ref
--       (Arg1 : Udev_Enumerate_Ptr) return Udev_Enumerate_Ptr;
--     pragma Import (C, Udev_Enumerate_Ref, "udev_enumerate_ref");
   --  Acquire a udev enumerate object.
   --  Returns the argument that it was passed, unmodified.

   function Udev_Enumerate_Unref
     (Arg1 : Udev_Enumerate_Ptr) return Udev_Enumerate_Ptr;
   pragma Import (C, Udev_Enumerate_Unref, "udev_enumerate_unref");
   --  Release a udev enumerate object.
   --  Always returns null.

--     function Udev_Enumerate_Get_Udev
--       (Arg1 : System.Address) return System.Address;
--     pragma Import (C, Udev_Enumerate_Get_Udev, "udev_enumerate_get_udev");

   function Udev_Enumerate_New
     (Udev : Udev_Ptr) return Udev_Enumerate_Ptr;
   pragma Import (C, Udev_Enumerate_New, "udev_enumerate_new");
   --  Create a udev enumerate object.
   --  On success, returns a pointer to the allocated udev monitor.
   --  On failure, null is returned.

   function Udev_Enumerate_Add_Match_Subsystem
     (Enumerate : Udev_Enumerate_Ptr;
      Subsystem : C_String) return Int;
   pragma Import
     (C,
      Udev_Enumerate_Add_Match_Subsystem,
      "udev_enumerate_add_match_subsystem");

--     function Udev_Enumerate_Add_Nomatch_Subsystem
--       (Arg1 : System.Address;
--        Arg2 : Interfaces.C.Strings.Chars_Ptr) return Int;
--     pragma Import
--       (C,
--        Udev_Enumerate_Add_Nomatch_Subsystem,
--        "udev_enumerate_add_nomatch_subsystem");

--     function Udev_Enumerate_Add_Match_Sysattr
--       (Arg1 : System.Address;
--        Arg2 : Interfaces.C.Strings.Chars_Ptr;
--        Arg3 : Interfaces.C.Strings.Chars_Ptr) return Int;
--     pragma Import
--       (C,
--        Udev_Enumerate_Add_Match_Sysattr,
--        "udev_enumerate_add_match_sysattr");

--     function Udev_Enumerate_Add_Nomatch_Sysattr
--       (Arg1 : System.Address;
--        Arg2 : Interfaces.C.Strings.Chars_Ptr;
--        Arg3 : Interfaces.C.Strings.Chars_Ptr) return Int;
--     pragma Import
--       (C,
--        Udev_Enumerate_Add_Nomatch_Sysattr,
--        "udev_enumerate_add_nomatch_sysattr");

--     function Udev_Enumerate_Add_Match_Property
--       (Arg1 : System.Address;
--        Arg2 : Interfaces.C.Strings.Chars_Ptr;
--        Arg3 : Interfaces.C.Strings.Chars_Ptr) return Int;
--     pragma Import
--       (C,
--        Udev_Enumerate_Add_Match_Property,
--        "udev_enumerate_add_match_property");

--     function Udev_Enumerate_Add_Match_Sysname
--       (Arg1 : System.Address;
--        Arg2 : Interfaces.C.Strings.Chars_Ptr) return Int;
--     pragma Import
--       (C,
--        Udev_Enumerate_Add_Match_Sysname,
--        "udev_enumerate_add_match_sysname");

--     function Udev_Enumerate_Add_Match_Tag
--       (Arg1 : System.Address;
--        Arg2 : Interfaces.C.Strings.Chars_Ptr) return Int;
--     pragma Import
--       (C, Udev_Enumerate_Add_Match_Tag, "udev_enumerate_add_match_tag");

--     function Udev_Enumerate_Add_Match_Parent
--       (Arg1 : System.Address;
--        Arg2 : System.Address) return Int;
--     pragma Import
--       (C,
--        Udev_Enumerate_Add_Match_Parent,
--        "udev_enumerate_add_match_parent");

--     function Udev_Enumerate_Add_Match_Is_Initialized
--       (Arg1 : System.Address) return Int;
--     pragma Import
--       (C,
--        Udev_Enumerate_Add_Match_Is_Initialized,
--        "udev_enumerate_add_match_is_initialized");

--     function Udev_Enumerate_Add_Syspath
--       (Arg1 : System.Address;
--        Arg2 : Interfaces.C.Strings.Chars_Ptr) return Int;
--     pragma Import
--       (C, Udev_Enumerate_Add_Syspath, "udev_enumerate_add_syspath");

   function Udev_Enumerate_Scan_Devices
     (Enumerate : Udev_Enumerate_Ptr) return Int;
   pragma Import
     (C, Udev_Enumerate_Scan_Devices, "udev_enumerate_scan_devices");

--     function Udev_Enumerate_Scan_Subsystems
--       (Arg1 : System.Address) return Int;
--     pragma Import
--       (C, Udev_Enumerate_Scan_Subsystems, "udev_enumerate_scan_subsystems");

   function Udev_Enumerate_Get_List_Entry
     (Enumerate : Udev_Enumerate_Ptr) return Udev_List_Entry_Ptr;
   pragma Import
     (C, Udev_Enumerate_Get_List_Entry, "udev_enumerate_get_list_entry");
   --  On success, returns a pointer to the first entry in
   --  the list of found devices. If the list is empty,
   --  or on failure, null is returned.

   function Exists (Enum : Enumerate) return Boolean is
     (Enum.My_Ptr /= null);

   procedure Create
     (Enum    : out Enumerate;
      Context : Contexts.Context) is
   begin
      Enum.My_Ptr := Udev_Enumerate_New (Context_Base (Context).My_Ptr);
   end Create;

   procedure Delete (Enum : in out Enumerate) is
   begin
      Enum.My_Ptr := Udev_Enumerate_Unref (Enum.My_Ptr);
      Enum.My_Ptr := null;
      --  Is unnecessary, but static code analyzers cannot know
      --  Udev_Enumerate_Unref (..) always returns null.
   end Delete;

   function Add_Match_Subsystem
     (Enum      : Enumerate;
      Subsystem : String) return Success_Flag is
   begin
      if
        Udev_Enumerate_Add_Match_Subsystem (Enum.My_Ptr, +Subsystem) >= 0
      then
         return Success;
      else
         return Failure;
      end if;
   end Add_Match_Subsystem;

   function Scan_Devices
     (Enum : Enumerate) return Success_Flag is
   begin
      if Udev_Enumerate_Scan_Devices (Enum.My_Ptr) >= 0 then
         return Success;
      else
         return Failure;
      end if;
   end Scan_Devices;

   procedure Get_List_Entry (Enum : Enumerate;
                             LE   : out List_Entries.List_Entry) is
   begin
      List_Entry_Base (LE).My_Ptr
        := Udev_Enumerate_Get_List_Entry (Enum.My_Ptr);
   end Get_List_Entry;

end C_Binding.Linux.Udev.Enumerates;
