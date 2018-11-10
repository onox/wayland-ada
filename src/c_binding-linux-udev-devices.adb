package body C_Binding.Linux.Udev.Devices is

--     function Udev_Device_Ref
--       (Arg1 : Udev_Device_Ptr) return Udev_Device_Ptr;
--     pragma Import (C, Udev_Device_Ref, "udev_device_ref");

   function Udev_Device_Unref
     (Arg1 : Udev_Device_Ptr) return Udev_Device_Ptr;
   pragma Import (C, Udev_Device_Unref, "udev_device_unref");

--     function Udev_Device_Get_Udev
--       (Arg1 : System.Address) return System.Address;
--     pragma Import (C, Udev_Device_Get_Udev, "udev_device_get_udev");

   function Udev_Device_New_From_Syspath
     (Udev    : Udev_Ptr;
      Syspath : C_String) return Udev_Device_Ptr;
   pragma Import
     (C, Udev_Device_New_From_Syspath, "udev_device_new_from_syspath");

--     function Udev_Device_New_From_Devnum
--       (Arg1 : System.Address;
--        Arg2 : Char;
--        Arg3 : Unsigned_Long) return System.Address;
--     pragma Import
--       (C, Udev_Device_New_From_Devnum, "udev_device_new_from_devnum");

--     function Udev_Device_New_From_Subsystem_Sysname
--       (Arg1 : System.Address;
--        Arg2 : Interfaces.C.Strings.Chars_Ptr;
--        Arg3 : Interfaces.C.Strings.Chars_Ptr) return System.Address;
--     pragma Import
--       (C,
--        Udev_Device_New_From_Subsystem_Sysname,
--        "udev_device_new_from_subsystem_sysname");

--     function Udev_Device_New_From_Device_Id
--       (Arg1 : System.Address;
--        Arg2 : Interfaces.C.Strings.Chars_Ptr) return System.Address;
--     pragma Import
--       (C, Udev_Device_New_From_Device_Id, "udev_device_new_from_device_id");

--     function Udev_Device_New_From_Environment
--       (Arg1 : System.Address) return System.Address;
--     pragma Import
--       (C,
--        Udev_Device_New_From_Environment,
--        "udev_device_new_from_environment");

   function Udev_Device_Get_Parent
     (Device : Udev_Device_Ptr) return Udev_Device_Ptr;
   pragma Import (C, Udev_Device_Get_Parent, "udev_device_get_parent");

--     function Udev_Device_Get_Parent_With_Subsystem_Devtype
--       (Arg1 : System.Address;
--        Arg2 : Interfaces.C.Strings.Chars_Ptr;
--        Arg3 : Interfaces.C.Strings.Chars_Ptr) return System.Address;
--     pragma Import
--       (C,
--        Udev_Device_Get_Parent_With_Subsystem_Devtype,
--        "udev_device_get_parent_with_subsystem_devtype");

   function Udev_Device_Get_Devpath
     (Arg1 : Udev_Device_Ptr) return Interfaces.C.Strings.Chars_Ptr;
   pragma Import (C, Udev_Device_Get_Devpath, "udev_device_get_devpath");

--     function Udev_Device_Get_Subsystem
--       (Arg1 : System.Address) return Interfaces.C.Strings.Chars_Ptr;
--     pragma Import
--       (C, Udev_Device_Get_Subsystem, "udev_device_get_subsystem");

   function Udev_Device_Get_Devtype
     (Arg1 : Udev_Device_Ptr) return Interfaces.C.Strings.Chars_Ptr;
   pragma Import (C, Udev_Device_Get_Devtype, "udev_device_get_devtype");

   function Udev_Device_Get_Syspath
     (Arg1 : Udev_Device_Ptr) return Interfaces.C.Strings.Chars_Ptr;
   pragma Import (C, Udev_Device_Get_Syspath, "udev_device_get_syspath");

   function Udev_Device_Get_Sysname
     (Arg1 : Udev_Device_Ptr) return Interfaces.C.Strings.Chars_Ptr;
   pragma Import (C, Udev_Device_Get_Sysname, "udev_device_get_sysname");

--     function Udev_Device_Get_Sysnum
--       (Arg1 : System.Address) return Interfaces.C.Strings.Chars_Ptr;
--     pragma Import (C, Udev_Device_Get_Sysnum, "udev_device_get_sysnum");

--     function Udev_Device_Get_Devnode
--       (Arg1 : System.Address) return Interfaces.C.Strings.Chars_Ptr;
--     pragma Import (C, Udev_Device_Get_Devnode, "udev_device_get_devnode");

--     function Udev_Device_Get_Is_Initialized
--       (Arg1 : System.Address) return Int;
--     pragma Import
--       (C, Udev_Device_Get_Is_Initialized, "udev_device_get_is_initialized");

--     function Udev_Device_Get_Devlinks_List_Entry
--       (Arg1 : System.Address) return System.Address;
--     pragma Import
--       (C,
--        Udev_Device_Get_Devlinks_List_Entry,
--        "udev_device_get_devlinks_list_entry");

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

   procedure Get_Parent
     (Device : Devices.Device;
      Parent : out Devices.Device) is
   begin
      Parent.My_Ptr := Udev_Device_Get_Parent (Device.My_Ptr);
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

end C_Binding.Linux.Udev.Devices;
