with System;
with Interfaces.C.Strings;

--  Udev is "abbreviation" of Userspace /dev.
--  API for enumerating and introspecting local devices.
package C_Binding.Linux.Udev is

   subtype Max_Length is Natural range 0 .. 10_000;

   type String_Result
     (Is_Success : Boolean := False;
      Length     : Max_Length := 1)
   is record
      case Is_Success is
         when True  => Value : String (1 .. Length);
         when False => Error : String (1 .. Length);
      end case;
   end record;

   type Success_Flag is
     (
      Success,
      Failure
     );

   type Context;
   type Device;
   type Enumerate;
   type List_Entry;
   type Monitor;

   procedure Get_Parent
     (Device : Udev.Device;
      Parent : out Udev.Device);

   procedure Receive_Device (Monitor : Udev.Monitor;
                             Device  : out Udev.Device);

   type Enumerate is tagged limited private;

   procedure Create (Enum    : out Enumerate;
                     Context : Udev.Context);

   function Exists (Enum : Enumerate) return Boolean;

   procedure Delete (Enum : in out Enumerate) with
     Pre  => Enum.Exists,
     Post => not Enum.Exists;

   function Add_Match_Subsystem
     (Enum      : Enumerate;
      Subsystem : String) return Success_Flag;

   function Scan_Devices
     (Enum : Enumerate) return Success_Flag;

   procedure Get_List_Entry (Enum : Enumerate;
                             LE   : out List_Entry);

   type List_Entry is tagged limited private;

   function Exists (LE : List_Entry) return Boolean;

   procedure Next (LE : in out List_Entry) with
     Pre => LE.Exists;

   function Name  (LE : List_Entry) return String_Result;
   function Value (LE : List_Entry) return String_Result;

   type Device is tagged limited private;

   procedure Create
     (Device  : out Udev.Device;
      Context : Udev.Context;
      Syspath : String);
   --  A Syspath is any subdirectory of /sys, with the restriction
   --  that a subdirectory of /sys/devices (or a symlink to one) represents
   --  a real device and as such must contain a uevent file.

   function Exists (Device : Udev.Device) return Boolean;

   procedure Delete (Device : in out Udev.Device) with
     Pre  => Device.Exists,
     Post => not Device.Exists;

   function Syspath (Device : Udev.Device) return String_Result;
   function Devpath (Device : Udev.Device) return String_Result;
   function Sysattr (Device : Udev.Device;
                     Name   : String) return String_Result;
   function Driver  (Device : Udev.Device) return String_Result;
   function Devtype (Device : Udev.Device) return String_Result;
   function Sysname (Device : Udev.Device) return String_Result;

   type Context is tagged limited private;

   procedure Create (Context : out Udev.Context);

   function Exists (Context : Udev.Context) return Boolean;

   procedure Delete (Context : in out Udev.Context) with
     Pre  => Context.Exists,
     Post => not Context.Exists;

   procedure New_From_Netlink (Context : Udev.Context;
                               Name    : String;
                               Monitor : out Udev.Monitor);

   type Monitor is tagged limited private;

   function Exists (Monitor : Udev.Monitor) return Boolean;

   procedure Delete (Monitor : in out Udev.Monitor) with
     Pre  => Monitor.Exists,
     Post => not Monitor.Exists;

   function Filter_Add_Match_Subsystem_Devtype
     (Monitor : Udev.Monitor;
      Subsystem : String;
      Devtype   : access String) return Success_Flag;

   function Enable_Receiving (Monitor : Udev.Monitor) return Success_Flag;

   function Get_File_Descriptor (Monitor : Udev.Monitor) return Integer;

private

   Nul : constant Character := Character'Val (0);

   type C_String is new String with
     Dynamic_Predicate => C_String'Length > 0
     and then C_String (C_String'Last) = Nul;

   function "-" (Text : C_String) return String;
   -- Removes the last 'Nul' character and returns a normal String.

   function "+" (Text : String) return C_String;
   -- Appends a 'Nul' character to a standard String and returns a C_String.

   package Thin is

      subtype Int           is Interfaces.C.Int;
      subtype Char          is Interfaces.C.Char;
      subtype Unsigned      is Interfaces.C.Unsigned;
      subtype Unsigned_Long is Interfaces.C.Unsigned_Long;

      type Udev is null record;
      --  Udev context

      type Udev_Ptr is access Udev;

      function Udev_Ref (Udev : Udev_Ptr) return Udev_Ptr;
      pragma Import (C, Udev_Ref, "udev_ref");
      --  Acquire a udev context object.
      --  Returns the argument that it was passed, unmodified.

      function Udev_Unref (Udev : Udev_Ptr) return Udev_Ptr;
      pragma Import (C, Udev_Unref, "udev_unref");
      --  Release a udev context object.
      --  Always returns null.

      function Udev_New return Udev_Ptr;
      pragma Import (C, Udev_New, "udev_new");
      --  Create a udev context object
      --  On success, returns a pointer to the allocated udev context.
      --  On failure, null is returned.

      procedure Udev_Set_Log_Fn
        (Arg1 : System.Address; Arg2 : access procedure
           (Arg1 : System.Address;
            Arg2 : Int;
            Arg3 : Interfaces.C.Strings.Chars_Ptr;
            Arg4 : Int;
            Arg5 : Interfaces.C.Strings.Chars_Ptr;
            Arg6 : Interfaces.C.Strings.Chars_Ptr;
            Arg7 : access System.Address));
      pragma Import (C, Udev_Set_Log_Fn, "udev_set_log_fn");

      function Udev_Get_Log_Priority (Arg1 : System.Address) return Int;
      pragma Import (C, Udev_Get_Log_Priority, "udev_get_log_priority");

      procedure Udev_Set_Log_Priority (Arg1 : System.Address; Arg2 : Int);
      pragma Import (C, Udev_Set_Log_Priority, "udev_set_log_priority");

      function Udev_Get_Userdata
        (Arg1 : System.Address) return System.Address;
      pragma Import (C, Udev_Get_Userdata, "udev_get_userdata");

      procedure Udev_Set_Userdata
        (Arg1 : System.Address;
         Arg2 : System.Address);
      pragma Import (C, Udev_Set_Userdata, "udev_set_userdata");

      type Udev_List_Entry is null record;

      type Udev_List_Entry_Ptr is access Udev_List_Entry;

      function Udev_List_Entry_Get_Next
        (Arg1 : Udev_List_Entry_Ptr) return Udev_List_Entry_Ptr;
      pragma Import (C, Udev_List_Entry_Get_Next, "udev_list_entry_get_next");
      --  On success, return a pointer to the requested list entry.
      --  If no such entry can be found, or on failure, null is returned.

      function Udev_List_Entry_Get_By_Name
        (Arg1 : System.Address;
         Arg2 : Interfaces.C.Strings.Chars_Ptr) return System.Address;
      pragma Import
        (C, Udev_List_Entry_Get_By_Name, "udev_list_entry_get_by_name");

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

      type Udev_Device is null record;
      --  This object is opaque and must not be accessed by the caller via
      --  different means than functions provided by libudev.
      --  Initially, the reference count of the device is 1.
      --  You can acquire further references, and drop gained references via
      --  udev_device_ref() and udev_device_unref(). Once the reference count
      --  hits 0, the device object is destroyed and freed.

      type Udev_Device_Ptr is access Udev_Device;

      function Udev_Device_Ref
        (Arg1 : Udev_Device_Ptr) return Udev_Device_Ptr;
      pragma Import (C, Udev_Device_Ref, "udev_device_ref");

      function Udev_Device_Unref
        (Arg1 : Udev_Device_Ptr) return Udev_Device_Ptr;
      pragma Import (C, Udev_Device_Unref, "udev_device_unref");

      function Udev_Device_Get_Udev
        (Arg1 : System.Address) return System.Address;
      pragma Import (C, Udev_Device_Get_Udev, "udev_device_get_udev");

      function Udev_Device_New_From_Syspath
        (Udev    : Udev_Ptr;
         Syspath : C_String) return Udev_Device_Ptr;
      pragma Import
        (C, Udev_Device_New_From_Syspath, "udev_device_new_from_syspath");

      function Udev_Device_New_From_Devnum
        (Arg1 : System.Address;
         Arg2 : Char;
         Arg3 : Unsigned_Long) return System.Address;
      pragma Import
        (C, Udev_Device_New_From_Devnum, "udev_device_new_from_devnum");

      function Udev_Device_New_From_Subsystem_Sysname
        (Arg1 : System.Address;
         Arg2 : Interfaces.C.Strings.Chars_Ptr;
         Arg3 : Interfaces.C.Strings.Chars_Ptr) return System.Address;
      pragma Import
        (C,
         Udev_Device_New_From_Subsystem_Sysname,
         "udev_device_new_from_subsystem_sysname");

      function Udev_Device_New_From_Device_Id
        (Arg1 : System.Address;
         Arg2 : Interfaces.C.Strings.Chars_Ptr) return System.Address;
      pragma Import
        (C, Udev_Device_New_From_Device_Id, "udev_device_new_from_device_id");

      function Udev_Device_New_From_Environment
        (Arg1 : System.Address) return System.Address;
      pragma Import
        (C,
         Udev_Device_New_From_Environment,
         "udev_device_new_from_environment");

      function Udev_Device_Get_Parent
        (Device : Udev_Device_Ptr) return Udev_Device_Ptr;
      pragma Import (C, Udev_Device_Get_Parent, "udev_device_get_parent");

      function Udev_Device_Get_Parent_With_Subsystem_Devtype
        (Arg1 : System.Address;
         Arg2 : Interfaces.C.Strings.Chars_Ptr;
         Arg3 : Interfaces.C.Strings.Chars_Ptr) return System.Address;
      pragma Import
        (C,
         Udev_Device_Get_Parent_With_Subsystem_Devtype,
         "udev_device_get_parent_with_subsystem_devtype");

      function Udev_Device_Get_Devpath
        (Arg1 : Udev_Device_Ptr) return Interfaces.C.Strings.Chars_Ptr;
      pragma Import (C, Udev_Device_Get_Devpath, "udev_device_get_devpath");

      function Udev_Device_Get_Subsystem
        (Arg1 : System.Address) return Interfaces.C.Strings.Chars_Ptr;
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
        (Arg1 : System.Address) return Interfaces.C.Strings.Chars_Ptr;
      pragma Import (C, Udev_Device_Get_Sysnum, "udev_device_get_sysnum");

      function Udev_Device_Get_Devnode
        (Arg1 : System.Address) return Interfaces.C.Strings.Chars_Ptr;
      pragma Import (C, Udev_Device_Get_Devnode, "udev_device_get_devnode");

      function Udev_Device_Get_Is_Initialized
        (Arg1 : System.Address) return Int;
      pragma Import
        (C, Udev_Device_Get_Is_Initialized, "udev_device_get_is_initialized");

      function Udev_Device_Get_Devlinks_List_Entry
        (Arg1 : System.Address) return System.Address;
      pragma Import
        (C,
         Udev_Device_Get_Devlinks_List_Entry,
         "udev_device_get_devlinks_list_entry");

      function Udev_Device_Get_Properties_List_Entry
        (Arg1 : System.Address) return System.Address;
      pragma Import
        (C,
         Udev_Device_Get_Properties_List_Entry,
         "udev_device_get_properties_list_entry");

      function Udev_Device_Get_Tags_List_Entry
        (Arg1 : System.Address) return System.Address;
      pragma Import
        (C,
         Udev_Device_Get_Tags_List_Entry,
         "udev_device_get_tags_list_entry");

      function Udev_Device_Get_Sysattr_List_Entry
        (Arg1 : System.Address) return System.Address;
      pragma Import
        (C,
         Udev_Device_Get_Sysattr_List_Entry,
         "udev_device_get_sysattr_list_entry");

      function Udev_Device_Get_Property_Value
        (
         Arg1 : System.Address;
         Arg2 : Interfaces.C.Strings.Chars_Ptr
        ) return Interfaces.C.Strings.Chars_Ptr;
      pragma Import
        (C, Udev_Device_Get_Property_Value, "udev_device_get_property_value");

      function Udev_Device_Get_Driver
        (Device : Udev_Device_Ptr) return Interfaces.C.Strings.Chars_Ptr;
      pragma Import (C, Udev_Device_Get_Driver, "udev_device_get_driver");

      function Udev_Device_Get_Devnum
        (Arg1 : System.Address) return Unsigned_Long;
      pragma Import (C, Udev_Device_Get_Devnum, "udev_device_get_devnum");

      function Udev_Device_Get_Action
        (Arg1 : System.Address) return Interfaces.C.Strings.Chars_Ptr;
      pragma Import (C, Udev_Device_Get_Action, "udev_device_get_action");

      function Udev_Device_Get_Seqnum
        (Arg1 : System.Address) return Interfaces.Integer_64;
      pragma Import (C, Udev_Device_Get_Seqnum, "udev_device_get_seqnum");

      function Udev_Device_Get_Usec_Since_Initialized
        (Arg1 : System.Address) return Interfaces.Integer_64;
      pragma Import
        (C,
         Udev_Device_Get_Usec_Since_Initialized,
         "udev_device_get_usec_since_initialized");

      function Udev_Device_Get_Sysattr_Value
        (
         Device : Udev_Device_Ptr;
         Name   : C_String
        ) return Interfaces.C.Strings.Chars_Ptr;
      pragma Import
        (C, Udev_Device_Get_Sysattr_Value, "udev_device_get_sysattr_value");

      function Udev_Device_Set_Sysattr_Value
        (Arg1 : System.Address;
         Arg2 : Interfaces.C.Strings.Chars_Ptr;
         Arg3 : Interfaces.C.Strings.Chars_Ptr) return Int;
      pragma Import
        (C, Udev_Device_Set_Sysattr_Value, "udev_device_set_sysattr_value");

      function Udev_Device_Has_Tag
        (Arg1 : System.Address;
         Arg2 : Interfaces.C.Strings.Chars_Ptr) return Int;
      pragma Import (C, Udev_Device_Has_Tag, "udev_device_has_tag");

      type Udev_Monitor is null record;

      type Udev_Monitor_Ptr is access Udev_Monitor;

      function Udev_Monitor_Ref
        (Monitor : Udev_Monitor_Ptr) return Udev_Monitor_Ptr;
      pragma Import (C, Udev_Monitor_Ref, "udev_monitor_ref");
      --  Acquire a udev monitor object.

      function Udev_Monitor_Unref
        (Monitor : Udev_Monitor_Ptr) return Udev_Monitor_Ptr;
      pragma Import (C, Udev_Monitor_Unref, "udev_monitor_unref");
      --  Release a udev monitor object.

      function Udev_Monitor_Get_Udev
        (Arg1 : System.Address) return System.Address;
      pragma Import (C, Udev_Monitor_Get_Udev, "udev_monitor_get_udev");

      function Udev_Monitor_New_From_Netlink
        (Arg1 : Udev_Ptr;
         Arg2 : C_String) return Udev_Monitor_Ptr;
      pragma Import
        (C, Udev_Monitor_New_From_Netlink, "udev_monitor_new_from_netlink");
      --  Create a udev monitor object.
      --  On success, returns a pointer to the allocated udev monitor.
      --  On failure, null is returned.

      function Udev_Monitor_Enable_Receiving
        (Monitor : Udev_Monitor_Ptr) return Int;
      pragma Import
        (C, Udev_Monitor_Enable_Receiving, "udev_monitor_enable_receiving");

      function Udev_Monitor_Set_Receive_Buffer_Size
        (Arg1 : System.Address; Arg2 : Int) return Int;
      pragma Import
        (C,
         Udev_Monitor_Set_Receive_Buffer_Size,
         "udev_monitor_set_receive_buffer_size");

      function Udev_Monitor_Get_Fd (Arg1 : Udev_Monitor_Ptr) return Int;
      pragma Import (C, Udev_Monitor_Get_Fd, "udev_monitor_get_fd");

      function Udev_Monitor_Receive_Device
        (Monitor : Udev_Monitor_Ptr) return Udev_Device_Ptr;
      pragma Import
        (C, Udev_Monitor_Receive_Device, "udev_monitor_receive_device");

      function Udev_Monitor_Filter_Add_Match_Subsystem_Devtype
        (Monitor   : Udev_Monitor_Ptr;
         Subsystem : C_String;
         Devtype   : Interfaces.C.Strings.Chars_Ptr) return Int;
      pragma Import
        (C,
         Udev_Monitor_Filter_Add_Match_Subsystem_Devtype,
         "udev_monitor_filter_add_match_subsystem_devtype");

      function Udev_Monitor_Filter_Add_Match_Tag
        (Arg1 : System.Address;
         Arg2 : Interfaces.C.Strings.Chars_Ptr) return Int;
      pragma Import
        (C,
         Udev_Monitor_Filter_Add_Match_Tag,
         "udev_monitor_filter_add_match_tag");

      function Udev_Monitor_Filter_Update (Arg1 : System.Address) return Int;
      pragma Import
        (C, Udev_Monitor_Filter_Update, "udev_monitor_filter_update");

      function Udev_Monitor_Filter_Remove (Arg1 : System.Address) return Int;
      pragma Import
        (C, Udev_Monitor_Filter_Remove, "udev_monitor_filter_remove");

      type Udev_Enumerate is null record;

      type Udev_Enumerate_Ptr is access Udev_Enumerate;

      function Udev_Enumerate_Ref
        (Arg1 : Udev_Enumerate_Ptr) return Udev_Enumerate_Ptr;
      pragma Import (C, Udev_Enumerate_Ref, "udev_enumerate_ref");
      --  Acquire a udev enumerate object.
      --  Returns the argument that it was passed, unmodified.

      function Udev_Enumerate_Unref
        (Arg1 : Udev_Enumerate_Ptr) return Udev_Enumerate_Ptr;
      pragma Import (C, Udev_Enumerate_Unref, "udev_enumerate_unref");
      --  Release a udev enumerate object.
      --  Always returns null.

      function Udev_Enumerate_Get_Udev
        (Arg1 : System.Address) return System.Address;
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
        (Arg1 : System.Address;
         Arg2 : Interfaces.C.Strings.Chars_Ptr) return Int;
      pragma Import
        (C,
         Udev_Enumerate_Add_Nomatch_Subsystem,
         "udev_enumerate_add_nomatch_subsystem");

      function Udev_Enumerate_Add_Match_Sysattr
        (Arg1 : System.Address;
         Arg2 : Interfaces.C.Strings.Chars_Ptr;
         Arg3 : Interfaces.C.Strings.Chars_Ptr) return Int;
      pragma Import
        (C,
         Udev_Enumerate_Add_Match_Sysattr,
         "udev_enumerate_add_match_sysattr");

      function Udev_Enumerate_Add_Nomatch_Sysattr
        (Arg1 : System.Address;
         Arg2 : Interfaces.C.Strings.Chars_Ptr;
         Arg3 : Interfaces.C.Strings.Chars_Ptr) return Int;
      pragma Import
        (C,
         Udev_Enumerate_Add_Nomatch_Sysattr,
         "udev_enumerate_add_nomatch_sysattr");

      function Udev_Enumerate_Add_Match_Property
        (Arg1 : System.Address;
         Arg2 : Interfaces.C.Strings.Chars_Ptr;
         Arg3 : Interfaces.C.Strings.Chars_Ptr) return Int;
      pragma Import
        (C,
         Udev_Enumerate_Add_Match_Property,
         "udev_enumerate_add_match_property");

      function Udev_Enumerate_Add_Match_Sysname
        (Arg1 : System.Address;
         Arg2 : Interfaces.C.Strings.Chars_Ptr) return Int;
      pragma Import
        (C,
         Udev_Enumerate_Add_Match_Sysname,
         "udev_enumerate_add_match_sysname");

      function Udev_Enumerate_Add_Match_Tag
        (Arg1 : System.Address;
         Arg2 : Interfaces.C.Strings.Chars_Ptr) return Int;
      pragma Import
        (C, Udev_Enumerate_Add_Match_Tag, "udev_enumerate_add_match_tag");

      function Udev_Enumerate_Add_Match_Parent
        (Arg1 : System.Address;
         Arg2 : System.Address) return Int;
      pragma Import
        (C,
         Udev_Enumerate_Add_Match_Parent,
         "udev_enumerate_add_match_parent");

      function Udev_Enumerate_Add_Match_Is_Initialized
        (Arg1 : System.Address) return Int;
      pragma Import
        (C,
         Udev_Enumerate_Add_Match_Is_Initialized,
         "udev_enumerate_add_match_is_initialized");

      function Udev_Enumerate_Add_Syspath
        (Arg1 : System.Address;
         Arg2 : Interfaces.C.Strings.Chars_Ptr) return Int;
      pragma Import
        (C, Udev_Enumerate_Add_Syspath, "udev_enumerate_add_syspath");

      function Udev_Enumerate_Scan_Devices
        (Enumerate : Udev_Enumerate_Ptr) return Int;
      pragma Import
        (C, Udev_Enumerate_Scan_Devices, "udev_enumerate_scan_devices");

      function Udev_Enumerate_Scan_Subsystems
        (Arg1 : System.Address) return Int;
      pragma Import
        (C, Udev_Enumerate_Scan_Subsystems, "udev_enumerate_scan_subsystems");

      function Udev_Enumerate_Get_List_Entry
        (Enumerate : Udev_Enumerate_Ptr) return Udev_List_Entry_Ptr;
      pragma Import
        (C, Udev_Enumerate_Get_List_Entry, "udev_enumerate_get_list_entry");
      --  On success, returns a pointer to the first entry in
      --  the list of found devices. If the list is empty,
      --  or on failure, null is returned.

      function Udev_Queue_Ref (Arg1 : System.Address) return System.Address;
      pragma Import (C, Udev_Queue_Ref, "udev_queue_ref");

      function Udev_Queue_Unref (Arg1 : System.Address) return System.Address;
      pragma Import (C, Udev_Queue_Unref, "udev_queue_unref");

      function Udev_Queue_Get_Udev
        (Arg1 : System.Address) return System.Address;
      pragma Import (C, Udev_Queue_Get_Udev, "udev_queue_get_udev");

      function Udev_Queue_New (Arg1 : System.Address) return System.Address;
      pragma Import (C, Udev_Queue_New, "udev_queue_new");

      function Udev_Queue_Get_Kernel_Seqnum
        (Arg1 : System.Address) return Interfaces.Integer_64;
      pragma Import
        (C, Udev_Queue_Get_Kernel_Seqnum, "udev_queue_get_kernel_seqnum");

      function Udev_Queue_Get_Udev_Seqnum
        (Arg1 : System.Address) return Interfaces.Integer_64;
      pragma Import
        (C, Udev_Queue_Get_Udev_Seqnum, "udev_queue_get_udev_seqnum");

      function Udev_Queue_Get_Udev_Is_Active
        (Arg1 : System.Address) return Int;
      pragma Import
        (C, Udev_Queue_Get_Udev_Is_Active, "udev_queue_get_udev_is_active");

      function Udev_Queue_Get_Queue_Is_Empty
        (Arg1 : System.Address) return Int;
      pragma Import
        (C, Udev_Queue_Get_Queue_Is_Empty, "udev_queue_get_queue_is_empty");

      function Udev_Queue_Get_Seqnum_Is_Finished
        (Arg1 : System.Address;
         Arg2 : Interfaces.Integer_64) return Int;
      pragma Import
        (C,
         Udev_Queue_Get_Seqnum_Is_Finished,
         "udev_queue_get_seqnum_is_finished");

      function Udev_Queue_Get_Seqnum_Sequence_Is_Finished
        (Arg1 : System.Address;
         Arg2 : Interfaces.Integer_64;
         Arg3 : Interfaces.Integer_64) return Int;
      pragma Import
        (C,
         Udev_Queue_Get_Seqnum_Sequence_Is_Finished,
         "udev_queue_get_seqnum_sequence_is_finished");

      function Udev_Queue_Get_Fd (Arg1 : System.Address) return Int;
      pragma Import (C, Udev_Queue_Get_Fd, "udev_queue_get_fd");

      function Udev_Queue_Flush (Arg1 : System.Address) return Int;
      pragma Import (C, Udev_Queue_Flush, "udev_queue_flush");

      function Udev_Queue_Get_Queued_List_Entry
        (Arg1 : System.Address) return System.Address;
      pragma Import
        (C,
         Udev_Queue_Get_Queued_List_Entry,
         "udev_queue_get_queued_list_entry");

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

      function Udev_Util_Encode_String
        (Arg1 : Interfaces.C.Strings.Chars_Ptr;
         Arg2 : Interfaces.C.Strings.Chars_Ptr;
         Arg3 : Unsigned_Long) return Int;
      pragma Import (C, Udev_Util_Encode_String, "udev_util_encode_string");

   end Thin;

   use type Thin.Udev_Ptr;
   use type Thin.Udev_Device_Ptr;
   use type Thin.Udev_Enumerate_Ptr;
   use type Thin.Udev_List_Entry_Ptr;
   use type Thin.Udev_Monitor_Ptr;

   type Context is tagged limited record
      My_Ptr : Thin.Udev_Ptr;
   end record;

   function Exists (Context : Udev.Context) return Boolean is
     (Context.My_Ptr /= null);

   type Device is tagged limited record
      My_Ptr : Thin.Udev_Device_Ptr;
   end record;

   function Exists (Device : Udev.Device) return Boolean is
     (Device.My_Ptr /= null);

   type Enumerate is tagged limited record
      My_Ptr : Thin.Udev_Enumerate_Ptr;
   end record;

   function Exists (Enum : Enumerate) return Boolean is
     (Enum.My_Ptr /= null);

   type List_Entry is tagged limited record
      My_Ptr : Thin.Udev_List_Entry_Ptr;
   end record;

   function Exists (LE : List_Entry) return Boolean is
     (LE.My_Ptr /= null);

   type Monitor is tagged limited record
      My_Ptr : Thin.Udev_Monitor_Ptr;
   end record;

   function Exists (Monitor : Udev.Monitor) return Boolean is
     (Monitor.My_Ptr /= null);

end C_Binding.Linux.Udev;
