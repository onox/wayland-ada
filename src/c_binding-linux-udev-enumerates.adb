with C_Binding.Linux.Udev.Devices;

package body C_Binding.Linux.Udev.Enumerates is

   use type int;

   function Udev_Enumerate_Ref
     (Enumerate : Udev_Enumerate_Ptr) return Udev_Enumerate_Ptr;
   pragma Import (C, Udev_Enumerate_Ref, "udev_enumerate_ref");
   --  Acquire a udev enumerate object.
   --  Returns the argument that it was passed, unmodified.

   function Udev_Enumerate_Unref
     (Arg1 : Udev_Enumerate_Ptr) return Udev_Enumerate_Ptr;
   pragma Import (C, Udev_Enumerate_Unref, "udev_enumerate_unref");
   --  Release a udev enumerate object.
   --  Always returns null.

   function Udev_Enumerate_Get_Udev
     (Enumerate : Udev_Enumerate_Ptr) return Udev_Ptr;
   pragma Import (C, Udev_Enumerate_Get_Udev, "udev_enumerate_get_udev");

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

   function Udev_Enumerate_Add_Nomatch_Subsystem
     (Enum      : Udev_Enumerate_Ptr;
      Subsystem : C_String) return Int;
   pragma Import
     (C,
      Udev_Enumerate_Add_Nomatch_Subsystem,
      "udev_enumerate_add_nomatch_subsystem");
   --  0 on success, otherwise a negative error value.

   function Udev_Enumerate_Add_Match_Sysattr
     (Enum    : Udev_Enumerate_Ptr;
      Sysattr : C_String;
      Value   : C_String) return Int;
   pragma Import
     (C,
      Udev_Enumerate_Add_Match_Sysattr,
      "udev_enumerate_add_match_sysattr");

   function Udev_Enumerate_Add_Nomatch_Sysattr
     (Enum    : Udev_Enumerate_Ptr;
      Sysattr : C_String;
      Value   : C_String) return Int;
   pragma Import
     (C,
      Udev_Enumerate_Add_Nomatch_Sysattr,
      "udev_enumerate_add_nomatch_sysattr");

   function Udev_Enumerate_Add_Match_Property
     (Enum     : Udev_Enumerate_Ptr;
      Property : C_String;
      Value    : C_String) return Int;
   pragma Import
     (C,
      Udev_Enumerate_Add_Match_Property,
      "udev_enumerate_add_match_property");

   function Udev_Enumerate_Add_Match_Sysname
     (Enum    : Udev_Enumerate_Ptr;
      Sysname : C_String) return Int;
   pragma Import
     (C,
      Udev_Enumerate_Add_Match_Sysname,
      "udev_enumerate_add_match_sysname");

   function Udev_Enumerate_Add_Match_Tag
     (Enum : Udev_Enumerate_Ptr;
      Tag  : C_String) return Int;
   pragma Import
     (C, Udev_Enumerate_Add_Match_Tag, "udev_enumerate_add_match_tag");

   function Udev_Enumerate_Add_Match_Parent
     (Enum   : Udev_Enumerate_Ptr;
      Parent : Udev_Device_Ptr) return Int;
   pragma Import
     (C,
      Udev_Enumerate_Add_Match_Parent,
      "udev_enumerate_add_match_parent");

   function Udev_Enumerate_Add_Match_Is_Initialized
     (Enum : Udev_Enumerate_Ptr) return Int;
   pragma Import
     (C,
      Udev_Enumerate_Add_Match_Is_Initialized,
      "udev_enumerate_add_match_is_initialized");
   --  Match only devices which udev has set up already. This makes sure,
   --  that the device node permissions and context are properly set and that
   --  network devices are fully renamed.
   --
   --  Usually, devices which are found in the kernel but not already handled
   --  by udev, have still pending events. Services should subscribe to monitor
   --  events and wait for these devices to become ready, instead of using
   --  uninitialized devices.
   --
   --  For now, this will not affect devices which do not have a device node
   --  and are not network interfaces.

   function Udev_Enumerate_Add_Syspath
     (
      Enum    : Udev_Enumerate_Ptr;
      Syspath : C_String  --  path of a device
     ) return Int;
   pragma Import
     (C, Udev_Enumerate_Add_Syspath, "udev_enumerate_add_syspath");
   --  Add a device to the list of devices, to retrieve it back sorted
   --  in dependency order.

   function Udev_Enumerate_Scan_Devices
     (Enumerate : Udev_Enumerate_Ptr) return Int;
   pragma Import
     (C, Udev_Enumerate_Scan_Devices, "udev_enumerate_scan_devices");

   function Udev_Enumerate_Scan_Subsystems
     (Enumerate : Udev_Enumerate_Ptr) return Int;
   pragma Import
     (C, Udev_Enumerate_Scan_Subsystems, "udev_enumerate_scan_subsystems");

   function Udev_Enumerate_Get_List_Entry
     (Enumerate : Udev_Enumerate_Ptr) return Udev_List_Entry_Ptr;
   pragma Import
     (C, Udev_Enumerate_Get_List_Entry, "udev_enumerate_get_list_entry");
   --  On success, returns a pointer to the first entry in
   --  the list of found devices. If the list is empty,
   --  or on failure, null is returned.

   procedure Acquire
     (Original  : Enumerate;
      Reference : out Enumerate) is
   begin
      Reference.My_Ptr := Udev_Enumerate_Ref (Original.My_Ptr);
   end Acquire;

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

   function Scan_Subsystems
     (Enum : Enumerate) return Success_Flag is
   begin
      if Udev_Enumerate_Scan_Subsystems (Enum.My_Ptr) >= 0 then
         return Success;
      else
         return Failure;
      end if;
   end Scan_Subsystems;

   procedure Get_List_Entry
     (Enum : Enumerate;
      LE   : out List_Entries.List_Entry) is
   begin
      List_Entry_Base (LE).My_Ptr
        := Udev_Enumerate_Get_List_Entry (Enum.My_Ptr);
   end Get_List_Entry;

   procedure Context
     (Enum    : Enumerate;
      Context : out Contexts.Context) is
   begin
      Context_Base (Context).My_Ptr := Udev_Enumerate_Get_Udev (Enum.My_Ptr);
   end Context;

   function Add_Nomatch_Subsystem
     (Enum      : Enumerate;
      Subsystem : String) return Success_Flag is
   begin
      if
        Udev_Enumerate_Add_Nomatch_Subsystem
          (Enum.My_Ptr, +Subsystem) = 0
      then
         return Success;
      else
         return Failure;
      end if;
   end Add_Nomatch_Subsystem;

   function Add_Match_Sysattr
     (Enum    : Enumerate;
      Sysattr : String;
      Value   : String) return Success_Flag is
   begin
      if
        Udev_Enumerate_Add_Match_Sysattr
          (Enum.My_Ptr, +Sysattr, +Value) = 0
      then
         return Success;
      else
         return Failure;
      end if;
   end Add_Match_Sysattr;

   function Add_Nomatch_Sysattr
     (Enum    : Enumerate;
      Sysattr : String;
      Value   : String) return Success_Flag is
   begin
      if
        Udev_Enumerate_Add_Nomatch_Sysattr
          (Enum.My_Ptr, +Sysattr, +Value) = 0
      then
         return Success;
      else
         return Failure;
      end if;
   end Add_Nomatch_Sysattr;

   function Add_Match_Property
     (Enum     : Enumerate;
      Property : String;
      Value    : String) return Success_Flag is
   begin
      if
        Udev_Enumerate_Add_Match_Property
          (Enum.My_Ptr, +Property, +Value) = 0
      then
         return Success;
      else
         return Failure;
      end if;
   end Add_Match_Property;

   function Add_Match_Sysname
     (Enum    : Enumerate;
      Sysname : String) return Success_Flag is
   begin
      if
        Udev_Enumerate_Add_Match_Sysname
          (Enum.My_Ptr, +Sysname) = 0
      then
         return Success;
      else
         return Failure;
      end if;
   end Add_Match_Sysname;

   function Add_Match_Tag
     (Enum : Enumerate;
      Tag  : String) return Success_Flag is
   begin
      if
        Udev_Enumerate_Add_Match_Tag
          (Enum.My_Ptr, +Tag) = 0
      then
         return Success;
      else
         return Failure;
      end if;
   end Add_Match_Tag;

   function Add_Match_Parent
     (Enum   : Enumerate;
      Parent : Devices.Device) return Success_Flag is
   begin
      if
        Udev_Enumerate_Add_Match_Parent
          (Enum.My_Ptr, Device_Base (Parent).My_Ptr) = 0
      then
         return Success;
      else
         return Failure;
      end if;
   end Add_Match_Parent;

   function Add_Match_Is_Initialized
     (Enum   : Enumerate) return Success_Flag is
   begin
      if
        Udev_Enumerate_Add_Match_Is_Initialized (Enum.My_Ptr) = 0
      then
         return Success;
      else
         return Failure;
      end if;
   end Add_Match_Is_Initialized;

   function Add_Syspath
     (Enum    : Enumerate;
      Syspath : String) return Success_Flag is
   begin
      if
        Udev_Enumerate_Add_Syspath (Enum.My_Ptr, +Syspath) = 0
      then
         return Success;
      else
         return Failure;
      end if;
   end Add_Syspath;

end C_Binding.Linux.Udev.Enumerates;
