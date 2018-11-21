limited with C_Binding.Linux.Udev.Devices;
limited with C_Binding.Linux.Udev.Enumerates;
limited with C_Binding.Linux.Udev.Monitors;

package C_Binding.Linux.Udev.Contexts is

   type Context;

   procedure Acquire_Reference
     (Original  : Contexts.Context;
      Reference : out Contexts.Context) with
     Pre => Contexts.Exists (Original);
   --  Acquire a reference to an existing udev context object.
   --  The reference count to Original goes up by 1.

   procedure Create_Context (Context : out Contexts.Context);
   --  Create udev context object. This reads the udev configuration file,
   --  and fills in the default values.
   --
   --  On success, Context.Exists = True.
   --  On failure, Context.Exists = False.
   --
   --  When successful, the reference count is 1.

   type Context is new Context_Base with private with
     Default_Initial_Condition => not Context.Exists;
   --  Represents a udev context object that may or may not exist.
   --  Before you can do anything with Udev the application must
   --  have a "connection" to Udev. This type represents such a "connection".

   function Exists (Context : Contexts.Context) return Boolean;

   procedure Delete (Context : in out Contexts.Context) with
     Pre  => Context.Exists,
     Post => not Context.Exists;
   --  Delete a udev context object.
   --  The reference count drops by 1.
   --  Once the reference count hits 0,
   --  the context object is destroyed and freed.

   procedure Create_Enumerate
     (Context : Contexts.Context;
      Enum    : out Enumerates.Enumerate);

   procedure Create_Monitor
     (Context : Contexts.Context;
      Name    : String;
      Monitor : out Monitors.Monitor);
   --  Create a udev monitor object.
   --  On success, Monitor.Exists = True.
   --  On failure, Monitor.Exists = False.

   procedure Create_Device
     (Context       : Contexts.Context;
      Block_Device  : Character;
      Device_Number : Interfaces.Unsigned_64;
      Device        : out Devices.Device);

   procedure Create_Device
     (Context   : Contexts.Context;
      Subsystem : String;
      Sysname   : String;
      Device    : out Devices.Device);
   --  To see list of valid Subsystem names execute "ls /sys/class/"

   procedure Create_Device
     (Context : Contexts.Context;
      Id      : String;
      Device  : out Devices.Device);

   procedure Create_Device
     (Context : Contexts.Context;
      Device  : out Devices.Device);

   function Log_Priority (Context : Contexts.Context) return Integer;

   procedure Set_Log_Priority
     (Context : Contexts.Context;
      Value   : Integer);

   generic
      with procedure Log
        (Context  : Contexts.Context;
         Priority : Integer;
         File     : String;
         Line     : Integer;
         Fn       : String;
         Format   : String);
   package Logging is

      procedure Redirect_Logs (Context : Contexts.Context);

   end Logging;

   generic
      type Data_Type (<>) is limited private;
      type Data_Ptr is not null access all Data_Type;
   package Custom_Data is

      procedure Set_Userdata
        (Context : Contexts.Context;
         Data    : Data_Ptr) with
        Pre => Context.Exists;

      function Get_Userdata
        (Context : Contexts.Context) return Data_Ptr;

   end Custom_Data;

   type Device_Index is new Positive;

   Empty_String : aliased String := "";

   type Device_Name
     (Name : not null access constant String)
   is limited null record;

   type Device_Array is
     array (Device_Index range <>) of Device_Name (Empty_String'Access);

   generic
      with procedure Handle_Error (Error_Message : String);
      with procedure Handle_Devices (Devices : Device_Array);
   procedure Generic_List_Devices;

private

   type Context is new Context_Base with null record;

   overriding
   procedure Finalize (Context : in out Contexts.Context);

   function Exists (Context : Contexts.Context) return Boolean is
     (Context.My_Ptr /= null);

end C_Binding.Linux.Udev.Contexts;
