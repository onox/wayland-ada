limited with C_Binding.Linux.Udev.Monitors;
limited with C_Binding.Linux.Udev.Devices;
limited with C_Binding.Linux.Udev.Queues;
limited with C_Binding.Linux.Udev.Hardware_Databases;

package C_Binding.Linux.Udev.Contexts is

   type Context;

   procedure Acquire
     (Original  : Contexts.Context;
      Reference : out Contexts.Context) with
     Pre => Contexts.Exists (Original);
   --  Acquire a reference to an existing udev context object.
   --  The reference count to Original goes up by 1.

   type Context is new Context_Base with private with
     Default_Initial_Condition => not Context.Exists;
   --  Represents a proxy to a udev context object that may or may not exist.

   procedure Create (Context : out Contexts.Context);
   --  Create a udev context object.
   --  On success, Context.Exists = True.
   --  On failure, Context.Exists = False.
   --  When successful, the reference count is 1.

   function Exists (Context : Contexts.Context) return Boolean;

   procedure Delete (Context : in out Contexts.Context) with
     Pre  => Context.Exists,
     Post => not Context.Exists;
   --  Delete a udev context object.
   --  The reference count drops by 1.
   --  Once the reference count hits 0,
   --  the context object is destroyed and freed.

   procedure New_From_Netlink
     (Context : Contexts.Context;
      Name    : String;
      Monitor : out Monitors.Monitor);
   --  Create a udev monitor object.
   --  On success, Monitor.Exists = True.
   --  On failure, Monitor.Exists = False.

   procedure New_Device_From_Devnum
     (Context       : Contexts.Context;
      Block_Device  : Character;
      Device_Number : Interfaces.Unsigned_64;
      Device        : out Devices.Device);

   procedure New_Device_From_Subsystem_Sysname
     (Context   : Contexts.Context;
      Subsystem : String;
      Sysname   : String;
      Device    : out Devices.Device);

   procedure New_Device_From_Device_Id
     (Context : Contexts.Context;
      Id      : String;
      Device  : out Devices.Device);

   procedure New_Device_From_Environment
     (Context : Contexts.Context;
      Device  : out Devices.Device);

   procedure New_Queue
     (Context : Contexts.Context;
      Queue   : out Queues.Queue);

   procedure New_Hardware_Database
     (Context  : Contexts.Context;
      Database : out Hardware_Databases.Database);

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

private

   type Context is new Context_Base with null record;

   function Exists (Context : Contexts.Context) return Boolean is
     (Context.My_Ptr /= null);

end C_Binding.Linux.Udev.Contexts;
