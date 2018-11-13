package body C_Binding.Linux.Udev.Devices is

   function Udev_Device_Ref
     (Arg1 : Udev_Device_Ptr) return Udev_Device_Ptr;
   pragma Import (C, Udev_Device_Ref, "udev_device_ref");

   function Udev_Device_Unref
     (Arg1 : Udev_Device_Ptr) return Udev_Device_Ptr;
   pragma Import (C, Udev_Device_Unref, "udev_device_unref");

   function Udev_Device_Get_Udev
     (Arg1 : Udev_Device_Ptr) return Udev_Ptr;
   pragma Import (C, Udev_Device_Get_Udev, "udev_device_get_udev");
   --  Retrieve the udev library context the device was created with.

   function Udev_Device_New_From_Syspath
     (Udev    : Udev_Ptr;
      Syspath : C_String) return Udev_Device_Ptr;
   pragma Import
     (C, Udev_Device_New_From_Syspath, "udev_device_new_from_syspath");
   --  Create the device object based on information found in /sys,
   --  annotated with properties from the udev-internal device database.
   --  A syspath is any subdirectory of /sys, with the restriction that a
   --  subdirectory of /sys/devices (or a symlink to one) represents a real
   --  device and as such must contain a uevent file.

   function Udev_Device_Get_Parent
     (Device : Udev_Device_Ptr) return Udev_Device_Ptr;
   pragma Import (C, Udev_Device_Get_Parent, "udev_device_get_parent");
   --  On success, returns a pointer to the parent device.
   --  No additional reference to this device is acquired,
   --  but the child device owns a reference to such a parent device.
   --  On failure, null is returned.

   function Udev_Device_Get_Parent_With_Subsystem_Devtype
     (Device    : Udev_Device_Ptr;
      Subsystem : C_String;
      Devtype   : C_String) return Udev_Device_Ptr;
   pragma Import
     (C,
      Udev_Device_Get_Parent_With_Subsystem_Devtype,
      "udev_device_get_parent_with_subsystem_devtype");
   --  On success, returns a pointer to the parent device.
   --  No additional reference to this device is acquired,
   --  but the child device owns a reference to such a parent device.
   --  On failure, null is returned.

   function Udev_Device_Get_Devpath
     (Arg1 : Udev_Device_Ptr) return Interfaces.C.Strings.Chars_Ptr;
   pragma Import (C, Udev_Device_Get_Devpath, "udev_device_get_devpath");

   function Udev_Device_Get_Subsystem
     (Device : Udev_Device_Ptr) return Interfaces.C.Strings.Chars_Ptr;
   pragma Import
     (C, Udev_Device_Get_Subsystem, "udev_device_get_subsystem");

   function Udev_Device_Get_Devtype
     (Arg1 : Udev_Device_Ptr) return Interfaces.C.Strings.Chars_Ptr;
   pragma Import (C, Udev_Device_Get_Devtype, "udev_device_get_devtype");

   function Udev_Device_Get_Syspath
     (Arg1 : Udev_Device_Ptr) return Interfaces.C.Strings.Chars_Ptr;
   pragma Import (C, Udev_Device_Get_Syspath, "udev_device_get_syspath");

   function Udev_Device_Get_Sysname
     (Arg1 : Udev_Device_Ptr) return Interfaces.C.Strings.Chars_Ptr;
   pragma Import (C, Udev_Device_Get_Sysname, "udev_device_get_sysname");

   function Udev_Device_Get_Sysnum
     (Device : Udev_Device_Ptr) return Interfaces.C.Strings.Chars_Ptr;
   pragma Import (C, Udev_Device_Get_Sysnum, "udev_device_get_sysnum");

   function Udev_Device_Get_Devnode
     (Device : Udev_Device_Ptr) return Interfaces.C.Strings.Chars_Ptr;
   pragma Import (C, Udev_Device_Get_Devnode, "udev_device_get_devnode");

   function Udev_Device_Get_Is_Initialized
     (Device : Udev_Device_Ptr) return Int;
   pragma Import
     (C, Udev_Device_Get_Is_Initialized, "udev_device_get_is_initialized");
   --  On success, returns either 1 or 0, depending on whether the passed
   --  device has already been initialized by udev or not.
   --  On failure, a negative error code is returned. Note that devices
   --  for which no udev rules are defined are never reported initialized.

   function Udev_Device_Get_Devlinks_List_Entry
     (Arg1 : System.Address) return System.Address;
   pragma Import
     (C,
      Udev_Device_Get_Devlinks_List_Entry,
      "udev_device_get_devlinks_list_entry");
   --  On success, returns a pointer to the first entry of the retrieved list.
   --  If that list is empty, or if an error occurred, NULL is returned.

--     function Udev_Device_Get_Properties_List_Entry
--       (Arg1 : System.Address) return System.Address;
--     pragma Import
--       (C,
--        Udev_Device_Get_Properties_List_Entry,
--        "udev_device_get_properties_list_entry");

--     function Udev_Device_Get_Tags_List_Entry
--       (Arg1 : System.Address) return System.Address;
--     pragma Import
--       (C,
--        Udev_Device_Get_Tags_List_Entry,
--        "udev_device_get_tags_list_entry");

--     function Udev_Device_Get_Sysattr_List_Entry
--       (Arg1 : System.Address) return System.Address;
--     pragma Import
--       (C,
--        Udev_Device_Get_Sysattr_List_Entry,
--        "udev_device_get_sysattr_list_entry");

--     function Udev_Device_Get_Property_Value
--       (
--        Arg1 : System.Address;
--        Arg2 : Interfaces.C.Strings.Chars_Ptr
--       ) return Interfaces.C.Strings.Chars_Ptr;
--     pragma Import
--       (C, Udev_Device_Get_Property_Value, "udev_device_get_property_value");

   function Udev_Device_Get_Driver
     (Device : Udev_Device_Ptr) return Interfaces.C.Strings.Chars_Ptr;
   pragma Import (C, Udev_Device_Get_Driver, "udev_device_get_driver");

--     function Udev_Device_Get_Devnum
--       (Arg1 : System.Address) return Unsigned_Long;
--     pragma Import (C, Udev_Device_Get_Devnum, "udev_device_get_devnum");

--     function Udev_Device_Get_Action
--       (Arg1 : System.Address) return Interfaces.C.Strings.Chars_Ptr;
--     pragma Import (C, Udev_Device_Get_Action, "udev_device_get_action");

--     function Udev_Device_Get_Seqnum
--       (Arg1 : System.Address) return Interfaces.Integer_64;
--     pragma Import (C, Udev_Device_Get_Seqnum, "udev_device_get_seqnum");

--     function Udev_Device_Get_Usec_Since_Initialized
--       (Arg1 : System.Address) return Interfaces.Integer_64;
--     pragma Import
--       (C,
--        Udev_Device_Get_Usec_Since_Initialized,
--        "udev_device_get_usec_since_initialized");

   function Udev_Device_Get_Sysattr_Value
     (
      Device : Udev_Device_Ptr;
      Name   : C_String
     ) return Interfaces.C.Strings.Chars_Ptr;
   pragma Import
     (C, Udev_Device_Get_Sysattr_Value, "udev_device_get_sysattr_value");

--     function Udev_Device_Set_Sysattr_Value
--       (Arg1 : System.Address;
--        Arg2 : Interfaces.C.Strings.Chars_Ptr;
--        Arg3 : Interfaces.C.Strings.Chars_Ptr) return Int;
--     pragma Import
--       (C, Udev_Device_Set_Sysattr_Value, "udev_device_set_sysattr_value");

--     function Udev_Device_Has_Tag
--       (Arg1 : System.Address;
--        Arg2 : Interfaces.C.Strings.Chars_Ptr) return Int;
--     pragma Import (C, Udev_Device_Has_Tag, "udev_device_has_tag");

   procedure Acquire (Original  : Device;
                      Reference : out Device) is
   begin
      Reference.My_Ptr := Udev_Device_Ref (Original.My_Ptr);
   end Acquire;

   procedure Get_Parent
     (Device : Devices.Device;
      Parent : out Devices.Device) is
   begin
      Parent.My_Ptr := Udev_Device_Get_Parent (Device.My_Ptr);
   end Get_Parent;

   procedure Get_Parent
     (Device    : Devices.Device;
      Subsystem : String;
      Devtype   : String;
      Parent    : out Devices.Device) is
   begin
      Parent.My_Ptr := Udev_Device_Get_Parent_With_Subsystem_Devtype
        (Device.My_Ptr, +Subsystem, +Devtype);
   end Get_Parent;

   function Exists (Device : Devices.Device) return Boolean is
     (Device.My_Ptr /= null);

   procedure Create
     (Device  : out Devices.Device;
      Context : Contexts.Context;
      Syspath : String) is
   begin
      Device.My_Ptr := Udev_Device_New_From_Syspath
        (Context_Base (Context).My_Ptr, +Syspath);
   end Create;

   procedure Delete (Device : in out Devices.Device) is
   begin
      Device.My_Ptr := Udev_Device_Unref (Device.My_Ptr);
      Device.My_Ptr := null;
      --  Is unnecessary, but static code analyzers cannot know
      --  Thin.Udev_Device_Unref (..) always returns null.
   end Delete;

   function Syspath (Device : Devices.Device) return String_Result is
      Text : constant Interfaces.C.Strings.Chars_Ptr
        := Udev_Device_Get_Syspath (Device.My_Ptr);
   begin
      return Get_String_Result (Text, "Syspath failure");
   end Syspath;

   function Devpath (Device : Devices.Device) return String_Result is
      Text : constant Interfaces.C.Strings.Chars_Ptr
        := Udev_Device_Get_Devpath (Device.My_Ptr);
   begin
      return Get_String_Result (Text, "Devpath failure");
   end Devpath;

   function Sysattr (Device : Devices.Device;
                     Name   : String) return String_Result is
      Text : constant Interfaces.C.Strings.Chars_Ptr
        := Udev_Device_Get_Sysattr_Value (Device.My_Ptr, +Name);
   begin
      return Get_String_Result (Text, "Sysattr failure");
   end Sysattr;

   function Driver (Device : Devices.Device) return String_Result is
      Text : constant Interfaces.C.Strings.Chars_Ptr
        := Udev_Device_Get_Driver (Device.My_Ptr);
   begin
      return Get_String_Result (Text, "Driver failure");
   end Driver;

   function Devtype (Device : Devices.Device) return String_Result is
      Text : constant Interfaces.C.Strings.Chars_Ptr
        := Udev_Device_Get_Devtype (Device.My_Ptr);
   begin
      return Get_String_Result (Text, "Devtype failure");
   end Devtype;

   function Sysname (Device : Devices.Device) return String_Result is
      Text : constant Interfaces.C.Strings.Chars_Ptr
        := Udev_Device_Get_Sysname (Device.My_Ptr);
   begin
      return Get_String_Result (Text, "Devtype failure");
   end Sysname;

   procedure Get_Context (Device  : Devices.Device;
                          Context : out Contexts.Context) is
   begin
      Context_Base (Context).My_Ptr := Udev_Device_Get_Udev (Device.My_Ptr);
   end Get_Context;

   function Subsystem (Device : Devices.Device) return String_Result is
      Text : constant Interfaces.C.Strings.Chars_Ptr
        := Udev_Device_Get_Subsystem (Device.My_Ptr);
   begin
      return Get_String_Result (Text, "Subsystem failure");
   end Subsystem;

   function Sysnum (Device : Devices.Device) return String_Result is
      Text : constant Interfaces.C.Strings.Chars_Ptr
        := Udev_Device_Get_Sysnum (Device.My_Ptr);
   begin
      return Get_String_Result (Text, "Sysnum failure");
   end Sysnum;

   function Devnode (Device : Devices.Device) return String_Result is
      Text : constant Interfaces.C.Strings.Chars_Ptr
        := Udev_Device_Get_Devnode (Device.My_Ptr);
   begin
      return Get_String_Result (Text, "Devnode failure");
   end Devnode;

   function Is_Initialized
     (Device : Devices.Device) return Initialization_Status is
      Result : Initialization_Status;
   begin
      case Udev_Device_Get_Is_Initialized (Device.My_Ptr) is
         when 1      => Result := Initialized;
         when 0      => Result := Not_Initialized;
         when others => Result := Unknown;
      end case;
      return Result;
   end Is_Initialized;

end C_Binding.Linux.Udev.Devices;
