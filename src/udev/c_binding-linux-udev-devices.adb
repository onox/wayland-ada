package body C_Binding.Linux.Udev.Devices is

   use type int;

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
     (Device : Udev_Device_Ptr) return Udev_List_Entry_Ptr;
   pragma Import
     (C,
      Udev_Device_Get_Devlinks_List_Entry,
      "udev_device_get_devlinks_list_entry");
   --  On success, returns a pointer to the first entry of the retrieved list.
   --  If that list is empty, or if an error occurred, null is returned.

   function Udev_Device_Get_Properties_List_Entry
     (Device : Udev_Device_Ptr) return Udev_List_Entry_Ptr;
   pragma Import
     (C,
      Udev_Device_Get_Properties_List_Entry,
      "udev_device_get_properties_list_entry");

   function Udev_Device_Get_Tags_List_Entry
     (Device : Udev_Device_Ptr) return Udev_List_Entry_Ptr;
   pragma Import
     (C,
      Udev_Device_Get_Tags_List_Entry,
      "udev_device_get_tags_list_entry");

   function Udev_Device_Get_Sysattr_List_Entry
     (Device : Udev_Device_Ptr) return Udev_List_Entry_Ptr;
   pragma Import
     (C,
      Udev_Device_Get_Sysattr_List_Entry,
      "udev_device_get_sysattr_list_entry");

   function Udev_Device_Get_Property_Value
     (
      Device : Udev_Device_Ptr;
      Key    : C_String
     ) return Interfaces.C.Strings.Chars_Ptr;
   pragma Import
     (C, Udev_Device_Get_Property_Value, "udev_device_get_property_value");
   --  On success, returns a pointer to a constant string of the requested
   --  value. On error, null is returned.

   function Udev_Device_Get_Driver
     (Device : Udev_Device_Ptr) return Interfaces.C.Strings.Chars_Ptr;
   pragma Import (C, Udev_Device_Get_Driver, "udev_device_get_driver");

   function Udev_Device_Get_Devnum
     (
      Device : Udev_Device_Ptr
     ) return Unsigned_Long;  --  Returnerar typen dev_t
   pragma Import (C, Udev_Device_Get_Devnum, "udev_device_get_devnum");
   --  On success, returns the device type of the passed device.
   --  On failure, a device type with minor and major number
   --  set to 0 is returned.

   function Udev_Device_Get_Action
     (Device : Udev_Device_Ptr) return Interfaces.C.Strings.Chars_Ptr;
   pragma Import (C, Udev_Device_Get_Action, "udev_device_get_action");

   function Udev_Device_Get_Seqnum
     (Device : Udev_Device_Ptr) return Interfaces.Integer_64;
   pragma Import (C, Udev_Device_Get_Seqnum, "udev_device_get_seqnum");
   --  This is only valid if the device was received through a monitor.
   --  Devices read from sys do not have a sequence number.
   --  Returns kernel event sequence number,
   --  or 0 if there is no sequence number available.

   function Udev_Device_Get_Usec_Since_Initialized
     (Device : Udev_Device_Ptr) return Interfaces.Integer_64;
   pragma Import
     (C,
      Udev_Device_Get_Usec_Since_Initialized,
      "udev_device_get_usec_since_initialized");
   --  Return the number of microseconds passed since udev set up
   --  the device for the first time.
   --  This is only implemented for devices with need to store properties
   --  in the udev database. All other devices return 0 here.
   --  Returns the number of microseconds since the device was first seen.

   function Udev_Device_Get_Sysattr_Value
     (
      Device : Udev_Device_Ptr;
      Name   : C_String
     ) return Interfaces.C.Strings.Chars_Ptr;
   pragma Import
     (C, Udev_Device_Get_Sysattr_Value, "udev_device_get_sysattr_value");
   --  The retrieved value is cached in the device. Repeated calls will
   --  return the same value and not open the attribute again.
   --  Returns content of a sys attribute file,
   --  or null if there is no sys attribute value.

   function Udev_Device_Set_Sysattr_Value
     (Device  : Udev_Device_Ptr;
      Sysattr : C_String;
      Value   : C_String) return Int;
   pragma Import
     (C, Udev_Device_Set_Sysattr_Value, "udev_device_set_sysattr_value");
   --  On success, returns an integer greater than, or equal to, 0.
   --  On failure, a negative error code is returned.

   function Udev_Device_Has_Tag
     (Device : Udev_Device_Ptr;
      Tag    : C_String) return Int;
   pragma Import (C, Udev_Device_Has_Tag, "udev_device_has_tag");
   --  On success, returns 1 or 0, depending on whether the device has
   --  the given tag or not.
   --  On failure, a negative error code is returned.

   procedure Acquire_Reference
     (Original  : Device;
      Reference : out Device) is
   begin
      Reference.My_Ptr := Udev_Device_Ref (Original.My_Ptr);
   end Acquire_Reference;

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

   procedure Create_Device
     (Device  : out Devices.Device;
      Context : Contexts.Context;
      Syspath : String) is
   begin
      Device.My_Ptr := Udev_Device_New_From_Syspath
        (Context_Base (Context).My_Ptr, +Syspath);
   end Create_Device;

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

   procedure Devlinks_List_Entry
     (Device     : Devices.Device;
      List_Entry : out List_Entries.List_Entry) is
   begin
      List_Entry_Base (List_Entry).My_Ptr
        := Udev_Device_Get_Devlinks_List_Entry (Device.My_Ptr);
   end Devlinks_List_Entry;

   procedure Properties_List_Entry
     (Device     : Devices.Device;
      List_Entry : out List_Entries.List_Entry) is
   begin
      List_Entry_Base (List_Entry).My_Ptr
        := Udev_Device_Get_Properties_List_Entry (Device.My_Ptr);
   end Properties_List_Entry;

   procedure Tags_List_Entry
     (Device     : Devices.Device;
      List_Entry : out List_Entries.List_Entry) is
   begin
      List_Entry_Base (List_Entry).My_Ptr
        := Udev_Device_Get_Tags_List_Entry (Device.My_Ptr);
   end Tags_List_Entry;

   procedure Sysattr_List_Entry
     (Device     : Devices.Device;
      List_Entry : out List_Entries.List_Entry) is
   begin
      List_Entry_Base (List_Entry).My_Ptr
        := Udev_Device_Get_Sysattr_List_Entry (Device.My_Ptr);
   end Sysattr_List_Entry;

   function Property_Value
     (Device : Devices.Device;
      Key    : String) return String_Result
   is
      Text : constant Interfaces.C.Strings.Chars_Ptr
        := Udev_Device_Get_Property_Value (Device.My_Ptr, +Key);
   begin
      return Get_String_Result (Text, "Property_Value failure");
   end Property_Value;

   function Devnum
     (Device : Devices.Device) return Interfaces.Unsigned_64 is
   begin
      return Interfaces.Unsigned_64 (Udev_Device_Get_Devnum (Device.My_Ptr));
   end Devnum;

   function Action (Device : Devices.Device) return String_Result is
      Text : constant Interfaces.C.Strings.Chars_Ptr
        := Udev_Device_Get_Action (Device.My_Ptr);
   begin
      return Get_String_Result (Text, "Action failure");
   end Action;

   function Sequence_Number (Device : Devices.Device) return Long_Integer is
   begin
      return Long_Integer (Udev_Device_Get_Seqnum (Device.My_Ptr));
   end Sequence_Number;

   function Microseconds_Since_Initialized
     (Device : Devices.Device) return Long_Integer is
   begin
      return Long_Integer
        (Udev_Device_Get_Usec_Since_Initialized (Device.My_Ptr));
   end Microseconds_Since_Initialized;

   function Set_Sysattr
     (Device  : Devices.Device;
      Sysattr : String;
      Value   : String) return Success_Flag is
   begin
      if
        Udev_Device_Set_Sysattr_Value
          (Device.My_Ptr, +Sysattr, +Value) >= 0
      then
         return Success;
      else
         return Failure;
      end if;
   end Set_Sysattr;

   function Has_Tag
     (Device : Devices.Device;
      Tag    : String) return Tag_Status
   is
      Result : Tag_Status;
   begin
      case Udev_Device_Has_Tag (Device.My_Ptr, +Tag) is
         when 1 => Result := Tag_Is_Present;
         when 0 => Result := Tag_is_Missing;
         when others => Result := Unknown;
      end case;
      return Result;
   end Has_Tag;

   procedure Finalize (Device : in out Devices.Device) is
   begin
      if Device.Exists then
         Device.Delete;
      end if;
   end Finalize;

end C_Binding.Linux.Udev.Devices;
