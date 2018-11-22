with C_Binding.Linux.Udev.Devices;
with C_Binding.Linux.Udev.Enumerates;
with C_Binding.Linux.Udev.Monitors;
with C_Binding.Linux.Udev.List_Entries;
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

   function Udev_Enumerate_New
     (Udev : Udev_Ptr) return Udev_Enumerate_Ptr;
   pragma Import (C, Udev_Enumerate_New, "udev_enumerate_new");
   --  Create a udev enumerate object.
   --  On success, returns a pointer to the allocated udev monitor.
   --  On failure, null is returned.

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

   procedure Acquire_Reference
     (Original  : Contexts.Context;
      Reference : out Contexts.Context) is
   begin
      Reference.My_Ptr := Udev_Ref (Original.My_Ptr);
   end Acquire_Reference;

   procedure Create_Context
     (Context : out Contexts.Context) is
   begin
      Context.My_Ptr := Udev_New;
   end Create_Context;

   procedure Delete (Context : in out Contexts.Context) is
   begin
      Context.My_Ptr := Udev_Unref (Context.My_Ptr);

      Context.My_Ptr := null;
      --  Is unnecessary, but static code analyzers cannot know
      --  Udev_Unref (..) always returns null.
   end Delete;

   procedure Create_Enumerate
     (Context : Contexts.Context;
      Enum    : out Enumerates.Enumerate) is
   begin
      Enumerate_Base (Enum).My_Ptr := Udev_Enumerate_New (Context.My_Ptr);
   end Create_Enumerate;

   procedure Create_Monitor
     (Context : Contexts.Context;
      Name    : String;
      Monitor : out Monitors.Monitor) is
   begin
      Monitor_Base (Monitor).My_Ptr
        := Udev_Monitor_New_From_Netlink (Context.My_Ptr, +Name);
   end Create_Monitor;

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

         C : Context;
      begin
         C.My_Ptr := Udev;
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

   procedure Create_Device
     (Context       : Contexts.Context;
      Block_Device  : Character;
      Device_Number : Interfaces.Unsigned_64;
      Device        : out Devices.Device) is
   begin
      Device_Base (Device).My_Ptr := Udev_Device_New_From_Devnum
        (Context.My_Ptr,
         Char (Block_Device),
         unsigned_long (Device_Number));
   end Create_Device;

   procedure Create_Device
     (Context   : Contexts.Context;
      Subsystem : String;
      Sysname   : String;
      Device    : out Devices.Device) is
   begin
      Device_Base (Device).My_Ptr := Udev_Device_New_From_Subsystem_Sysname
        (Context.My_Ptr,
         +Subsystem,
         +Sysname);
   end Create_Device;

   procedure Create_Device
     (Context : Contexts.Context;
      Id      : String;
      Device  : out Devices.Device) is
   begin
      Device_Base (Device).My_Ptr := Udev_Device_New_From_Device_Id
        (Context.My_Ptr,
         +Id);
   end Create_Device;

   procedure Create_Device
     (Context : Contexts.Context;
      Device  : out Devices.Device) is
   begin
      Device_Base (Device).My_Ptr := Udev_Device_New_From_Environment
        (Context.My_Ptr);
   end Create_Device;

   procedure Finalize (Context : in out Contexts.Context) is
   begin
      if Context.Exists then
         Context.Delete;
      end if;
   end Finalize;

   procedure Generic_List_Devices
     (Subsystem : access constant String := null;
      Devtype   : access constant String := null)
   is
      procedure Create_Context;
      procedure Create_Enumerate;
      procedure Match_Subsystem;
      procedure Match_Devtype;
      procedure Scan_Devices;
      procedure List_Devices;

      Context   : Udev.Contexts.Context;
      Enumerate : Udev.Enumerates.Enumerate;

      procedure Create_Context is
      begin
         Udev.Contexts.Create_Context (Context);
         if Context.Exists then
            Create_Enumerate;
         else
            Handle_Error ("Failed to create udev context");
         end if;
      end Create_Context;

      procedure Create_Enumerate is
      begin
         Context.Create_Enumerate (Enumerate);
         if Enumerate.Exists then
            Match_Subsystem;
         else
            Handle_Error ("Failed to create udev enumerate");
         end if;
      end Create_Enumerate;

      procedure Match_Subsystem is
         Result : Success_Flag;
      begin
         if Subsystem /= null then
            Result := Enumerate.Add_Match_Subsystem (Subsystem.all);
         else
            Result := Success;
         end if;
         if Result = Success then
            Match_Devtype;
         else
            Handle_Error ("Failed to match subsystem "  & Subsystem.all);
         end if;
      end Match_Subsystem;

      procedure Match_Devtype is
         Result : Success_Flag;
      begin
         if Devtype /= null then
            Result := Enumerate.Add_Match_Property ("DEVTYPE", Devtype.all);
         else
            Result := Success;
         end if;
         if Result = Success then
            Scan_Devices;
         else
            Handle_Error ("Failed to match devtype "  & Devtype.all);
         end if;
      end Match_Devtype;

      procedure Scan_Devices is
         Result : Udev.Success_Flag;
      begin
         Result := Enumerate.Scan_Devices;
         if Result = Success then
            List_Devices;
         else
            Handle_Error ("Failed to scan devices");
         end if;
      end Scan_Devices;

      procedure List_Devices is

         type Temp_Node;

         type Device_Node
           (Text : not null access constant String;
            Parent : access Temp_Node) is null record;
         --  Only the root node has Parent = null

         type Temp_Node (Parent : not null access Device_Node) is null record;

         procedure Extract_Device_Name
           (List_Entry    : in out Udev.List_Entries.List_Entry;
            Prev_Node     : access Device_Node;
            Entries_Count : in out Natural)
         is
            Name : aliased constant String := List_Entry.Name.Value;
            Intermediary : aliased Temp_Node (Prev_Node);
            Node : aliased Device_Node (Name'Access, Intermediary'Access);
         begin
            --  Put_Line (Name);
            List_Entry.Next;
            if List_Entry.Exists then
               Entries_Count := Entries_Count + 1;
               Extract_Device_Name
                 (List_Entry,
                  Node'Access,
                  Entries_Count);
            else
               declare
                  Current_Node : access Device_Node := Node'Access;

                  function Get_String return access constant String is
                     Temp : constant access constant String
                       := Current_Node.Text;
                  begin
                     if Current_Node.Parent /= null then
                        Current_Node := Current_Node.Parent.Parent;
                     end if;
                     return Temp;
                  end Get_String;

                  Devices : constant Device_Array
                    (1 .. Device_Index (Entries_Count))
                      := (others => (Name => Get_String));
               begin
                  Handle_Devices (Devices);
               end;
            end if;
         end Extract_Device_Name;

         List_Entry : Udev.List_Entries.List_Entry;
         Entries_Count : Natural := 2;
      begin
         Enumerate.Get_List_Entry (List_Entry);
         if List_Entry.Exists then
            declare
               Name : aliased constant String := List_Entry.Name.Value;
               Node : aliased Device_Node (Name'Access, null);
            begin
               --  Put_Line (Name);
               List_Entry.Next;
               if List_Entry.Exists then
                  Extract_Device_Name
                    (List_Entry,
                     Node'Access,
                     Entries_Count);
               else
                  declare
                     Devices : constant Device_Array (1 .. 1)
                       := (1 => (Name => Name'Access));
                  begin
                     Handle_Devices (Devices);
                  end;
               end if;
            end;
         else
            declare
               Devices : Device_Array (1 .. 0);
            begin
               Handle_Devices (Devices);
            end;
         end if;
      end List_Devices;

   begin
      Create_Context;
   end Generic_List_Devices;

end C_Binding.Linux.Udev.Contexts;
