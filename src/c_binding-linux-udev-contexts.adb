with C_Binding.Linux.Udev.Devices;
with C_Binding.Linux.Udev.Monitors;
with C_Binding.Linux.Udev.Queues;
with System.Address_To_Access_Conversions;

package body C_Binding.Linux.Udev.Contexts is

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
     (Udev : Udev_Ptr;
      Arg2 : access procedure
        (Udev     : Udev_Ptr;
         Priority : Int;
         File     : Interfaces.C.Strings.Chars_Ptr;
         Line     : Int;
         Fn       : Interfaces.C.Strings.Chars_Ptr;
         Format   : Interfaces.C.Strings.Chars_Ptr;
         Args     : access System.Address));
   pragma Import (C, Udev_Set_Log_Fn, "udev_set_log_fn");
   --  The built-in logging writes to stderr. It can be overridden
   --  by a custom function, to plug log messages into
   --  the users' logging functionality.

   function Udev_Get_Log_Priority (Udev : Udev_Ptr) return Int;
   pragma Import (C, Udev_Get_Log_Priority, "udev_get_log_priority");

   procedure Udev_Set_Log_Priority (Udev : Udev_Ptr; Arg2 : Int);
   pragma Import (C, Udev_Set_Log_Priority, "udev_set_log_priority");

   function Udev_Get_Userdata
     (Udev : Udev_Ptr) return System.Address;
   pragma Import (C, Udev_Get_Userdata, "udev_get_userdata");
   --  Retrieve stored data pointer from library context. This might be
   --  useful to access from callbacks like a custom logging function.

   procedure Udev_Set_Userdata
     (Udev     : Udev_Ptr;
      Userdata : System.Address);
   pragma Import (C, Udev_Set_Userdata, "udev_set_userdata");
   --  Store custom userdata in the library context.

   function Udev_Monitor_New_From_Netlink
     (Arg1 : Udev_Ptr;
      Arg2 : C_String) return Udev_Monitor_Ptr;
   pragma Import
     (C, Udev_Monitor_New_From_Netlink, "udev_monitor_new_from_netlink");
   --  Create a udev monitor object.
   --  On success, returns a pointer to the allocated udev monitor.
   --  On failure, null is returned.

   function Udev_Device_New_From_Devnum
     (
      Udev : Udev_Ptr;      --  udev library context
      Arg2 : Char;          --  char or block device
      Arg3 : Unsigned_Long  --  device major/minor number
     ) return Udev_Device_Ptr;
   pragma Import
     (C, Udev_Device_New_From_Devnum, "udev_device_new_from_devnum");
   --  Create new udev device, and fill in information from the sys device
   --  and the udev database entry. The device is looked-up by its
   --  major/minor number and type. Character and block device numbers are
   --  not unique across the two types.
   --
   --  Returns a new udev device, or null, if it does not exist.
   --  The initial refcount is 1, and needs to be decremented to release
   --  the resources of the udev device.

   function Udev_Device_New_From_Subsystem_Sysname
     (Udev : Udev_Ptr;   --  udev library context
      Arg2 : C_String;   --  the subsystem of the device
      Arg3 : C_String    --  the name of the device
     ) return Udev_Device_Ptr;
   pragma Import
     (C,
      Udev_Device_New_From_Subsystem_Sysname,
      "udev_device_new_from_subsystem_sysname");
   --  Create new udev device, and fill in information from the sys device
   --  and the udev database entry. The device is looked up by the subsystem
   --  and name string of the device, like "mem" / "zero", or "block" / "sda".
   --
   --  Returns a new udev device, or null, if it does not exist.
   --
   --  The initial refcount is 1, and needs to be decremented to release
   --  the resources of the udev device.

   function Udev_Device_New_From_Device_Id
     (Udev : Udev_Ptr;
      Id   : C_String) return Udev_Device_Ptr;
   pragma Import
     (C, Udev_Device_New_From_Device_Id, "udev_device_new_from_device_id");

   function Udev_Device_New_From_Environment
     (Udev : Udev_Ptr) return Udev_Device_Ptr;
   pragma Import
     (C,
      Udev_Device_New_From_Environment,
      "udev_device_new_from_environment");

   function Udev_Queue_New (Udev : Udev_Ptr) return Udev_Queue_Ptr;
   pragma Import (C, Udev_Queue_New, "udev_queue_new");

   procedure Acquire
     (Original  : Contexts.Context;
      Reference : out Contexts.Context) is
   begin
      Reference.My_Ptr := Udev_Ref (Original.My_Ptr);
   end Acquire;

   procedure Create
     (Context : out Contexts.Context) is
   begin
      Context.My_Ptr := Udev_New;
   end Create;

   procedure Delete (Context : in out Contexts.Context) is
   begin
      Context.My_Ptr := Udev_Unref (Context.My_Ptr);

      Context.My_Ptr := null;
      --  Is unnecessary, but static code analyzers cannot know
      --  Udev_Unref (..) always returns null.
   end Delete;

   procedure New_From_Netlink (Context : Contexts.Context;
                               Name    : String;
                               Monitor : out Monitors.Monitor) is
   begin
      Monitor_Base (Monitor).My_Ptr
        := Udev_Monitor_New_From_Netlink (Context.My_Ptr, +Name);
   end New_From_Netlink;

   function Log_Priority (Context : Contexts.Context) return Integer is
   begin
      return Integer (Udev_Get_Log_Priority (Context.My_Ptr));
   end Log_Priority;

   procedure Set_Log_Priority
     (Context : Contexts.Context;
      Value   : Integer) is
   begin
      Udev_Set_Log_Priority (Context.My_Ptr, int (Value));
   end Set_Log_Priority;

   package body Logging is

      procedure Internal_Callback
        (Udev     : Udev_Ptr;
         Priority : Int;
         File     : Interfaces.C.Strings.Chars_Ptr;
         Line     : Int;
         Fn       : Interfaces.C.Strings.Chars_Ptr;
         Format   : Interfaces.C.Strings.Chars_Ptr;
         Args     : access System.Address) with
        Convention => C;

      procedure Internal_Callback
        (Udev     : Udev_Ptr;
         Priority : Int;
         File     : Interfaces.C.Strings.Chars_Ptr;
         Line     : Int;
         Fn       : Interfaces.C.Strings.Chars_Ptr;
         Format   : Interfaces.C.Strings.Chars_Ptr;
         Args     : access System.Address)
      is
         pragma Unreferenced (Args);  --  What to do with this?

         C : constant Context := (My_Ptr => Udev);
      begin
         Log (C, Integer (Priority), -File, Integer (Line), -Fn, -Format);
      end Internal_Callback;

      procedure Redirect_Logs (Context : Contexts.Context) is
      begin
         Udev_Set_Log_Fn (Context.My_Ptr, Internal_Callback'Access);
      end Redirect_Logs;

   end Logging;

   package body Custom_Data is

      package Conversions is new System.Address_To_Access_Conversions
        (Data_Type);

      procedure Set_Userdata
        (Context : Contexts.Context;
         Data    : Data_Ptr) is
      begin
         Udev_Set_Userdata
           (Context.My_Ptr, Conversions.To_Address (Data.all'Access));
      end Set_Userdata;

      function Get_Userdata
        (Context : Contexts.Context) return Data_Ptr is
      begin
         return Data_Ptr
           (Conversions.To_Pointer (Udev_Get_Userdata (Context.My_Ptr)));
      end Get_Userdata;

   end Custom_Data;

   procedure New_Device_From_Devnum
     (Context       : Contexts.Context;
      Block_Device  : Character;
      Device_Number : Interfaces.Unsigned_64;
      Device        : out Devices.Device) is
   begin
      Device_Base (Device).My_Ptr := Udev_Device_New_From_Devnum
        (Context.My_Ptr,
         Char (Block_Device),
         unsigned_long (Device_Number));
   end New_Device_From_Devnum;

   procedure New_Device_From_Subsystem_Sysname
     (Context   : Contexts.Context;
      Subsystem : String;
      Sysname   : String;
      Device    : out Devices.Device) is
   begin
      Device_Base (Device).My_Ptr := Udev_Device_New_From_Subsystem_Sysname
        (Context.My_Ptr,
         +Subsystem,
         +Sysname);
   end New_Device_From_Subsystem_Sysname;

   procedure New_Device_From_Device_Id
     (Context : Contexts.Context;
      Id      : String;
      Device  : out Devices.Device) is
   begin
      Device_Base (Device).My_Ptr := Udev_Device_New_From_Device_Id
        (Context.My_Ptr,
         +Id);
   end New_Device_From_Device_Id;

   procedure New_Device_From_Environment
     (Context : Contexts.Context;
      Device  : out Devices.Device) is
   begin
      Device_Base (Device).My_Ptr := Udev_Device_New_From_Environment
        (Context.My_Ptr);
   end New_Device_From_Environment;

   procedure New_Queue
     (Context : Contexts.Context;
      Queue   : out Queues.Queue) is
   begin
      Queue_Base (Queue).My_Ptr := Udev_Queue_New (Context.My_Ptr);
   end New_Queue;

end C_Binding.Linux.Udev.Contexts;
