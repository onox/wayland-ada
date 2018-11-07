package body Udev is

   use type Interfaces.C.Strings.chars_ptr;
   use type Interfaces.C.int;

   function "-" (Text : C_String) return String is
   begin
      return String (Text (Text'First .. Text'Last - 1));
   end "-";

   function "+" (Text : String) return C_String is
   begin
      return C_String (Text & Nul);
   end "+";

   procedure Create
     (Context : out Udev.Context) is
   begin
      Context.My_Ptr := Thin.Udev_New;
   end Create;

   procedure Delete (Context : in out Udev.Context) is
   begin
      Context.My_Ptr := Thin.Udev_Unref (Context.My_Ptr);

      Context.My_Ptr := null;
      --  Is unnecessary, but static code analyzers cannot know
      --  Thin.Udev_Unref (..) always returns null.
   end Delete;

   procedure Create
     (Device  : out Udev.Device;
      Context : Udev.Context;
      Syspath : String) is
   begin
      Device.My_Ptr := Thin.Udev_Device_New_From_Syspath
        (Context.My_Ptr, +Syspath);
   end Create;

   procedure Delete (Device : in out Udev.Device) is
   begin
      Device.My_Ptr := Thin.Udev_Device_Unref (Device.My_Ptr);
      Device.My_Ptr := null;
      --  Is unnecessary, but static code analyzers cannot know
      --  Thin.Udev_Device_Unref (..) always returns null.
   end Delete;

   function Get_String_Result
     (Text  : Interfaces.C.Strings.Chars_Ptr;
      Error : String) return String_Result is
   begin
      if Text /= Interfaces.C.Strings.Null_Ptr then
         declare
            Result : String := Interfaces.C.Strings.Value (Text);
         begin
            return
                (Is_Success => True,
                 Length     => Max_Length (Result'Length),
                 Value      => Result);
         end;
      else
         return
           (Is_Success => False,
            Length     => Max_Length (Error'Length),
            Error      => Error);
      end if;
   end Get_String_Result;

   function Syspath (Device : Udev.Device) return String_Result is
      Text : Interfaces.C.Strings.Chars_Ptr
        := Thin.Udev_Device_Get_Syspath (Device.My_Ptr);
   begin
      return Get_String_Result (Text, "Syspath failure");
   end Syspath;

   function Devpath (Device : Udev.Device) return String_Result is
      Text : Interfaces.C.Strings.Chars_Ptr
        := Thin.Udev_Device_Get_Devpath (Device.My_Ptr);
   begin
      return Get_String_Result (Text, "Devpath failure");
   end Devpath;

   function Sysattr (Device : Udev.Device;
                     Name   : String) return String_Result is
      Text : Interfaces.C.Strings.Chars_Ptr
        := Thin.Udev_Device_Get_Sysattr_Value (Device.My_Ptr, +Name);
   begin
      return Get_String_Result (Text, "Sysattr failure");
   end Sysattr;

   procedure Get_Parent
     (Device : Udev.Device;
      Parent : out Udev.Device) is
   begin
      Parent.My_Ptr := Thin.Udev_Device_Get_Parent (Device.My_Ptr);
   end Get_Parent;

   function Driver (Device : Udev.Device) return String_Result is
      Text : Interfaces.C.Strings.Chars_Ptr
        := Thin.Udev_Device_Get_Driver (Device.My_Ptr);
   begin
      return Get_String_Result (Text, "Driver failure");
   end Driver;

   function Devtype (Device : Udev.Device) return String_Result is
      Text : Interfaces.C.Strings.Chars_Ptr
        := Thin.Udev_Device_Get_Devtype (Device.My_Ptr);
   begin
      return Get_String_Result (Text, "Devtype failure");
   end Devtype;

   function Sysname (Device : Udev.Device) return String_Result is
      Text : Interfaces.C.Strings.Chars_Ptr
        := Thin.Udev_Device_Get_Sysname (Device.My_Ptr);
   begin
      return Get_String_Result (Text, "Devtype failure");
   end Sysname;

   procedure Create
     (Enum    : out Enumerate;
      Context : Udev.Context) is
   begin
      Enum.My_Ptr := Thin.Udev_Enumerate_New (Context.My_Ptr);
   end Create;

   procedure Delete (Enum : in out Enumerate) is
   begin
      Enum.My_Ptr := Thin.Udev_Enumerate_Unref (Enum.My_Ptr);
      Enum.My_Ptr := null;
      --  Is unnecessary, but static code analyzers cannot know
      --  Thin.Udev_Enumerate_Unref (..) always returns null.
   end Delete;

   function Add_Match_Subsystem
     (Enum      : Enumerate;
      Subsystem : String) return Success_Flag is
   begin
      if
        Thin.Udev_Enumerate_Add_Match_Subsystem (Enum.My_Ptr, +Subsystem) >= 0
      then
         return Success;
      else
         return Failure;
      end if;
   end Add_Match_Subsystem;

   function Scan_Devices
     (Enum : Enumerate) return Success_Flag is
   begin
      if Thin.Udev_Enumerate_Scan_Devices (Enum.My_Ptr) >= 0 then
         return Success;
      else
         return Failure;
      end if;
   end Scan_Devices;

   procedure Get_List_Entry (Enum : Enumerate;
                             LE   : out List_Entry) is
   begin
      LE.My_Ptr := Thin.Udev_Enumerate_Get_List_Entry (Enum.My_Ptr);
   end Get_List_Entry;

   procedure Next (LE : in out List_Entry) is
   begin
      LE.My_Ptr := Thin.Udev_List_Entry_Get_Next (LE.My_Ptr);
   end Next;

   function Name (LE : List_Entry) return String_Result is
      Text : Interfaces.C.Strings.Chars_Ptr
        := Thin.Udev_List_Entry_Get_Name (LE.My_Ptr);
   begin
      return Get_String_Result (Text, "List entry name failure");
   end Name;

   function Value (LE : List_Entry) return String_Result is
      Text : Interfaces.C.Strings.Chars_Ptr
        := Thin.Udev_List_Entry_Get_Value (LE.My_Ptr);
   begin
      return Get_String_Result (Text, "List entry value failure");
   end Value;

   procedure New_From_Netlink (Context : Udev.Context;
                               Name    : String;
                               Monitor : out Udev.Monitor) is
   begin
      Monitor.My_Ptr
        := Thin.Udev_Monitor_New_From_Netlink (Context.My_Ptr, +Name);
   end New_From_Netlink;

   procedure Delete (Monitor : in out Udev.Monitor) is
   begin
      Monitor.My_Ptr := Thin.Udev_Monitor_Unref (Monitor.My_Ptr);
      Monitor.My_Ptr := null;
      --  Is unnecessary, but static code analyzers cannot know
      --  Thin.Udev_Monitor_Unref (..) always returns null.
   end Delete;

   function Filter_Add_Match_Subsystem_Devtype
     (Monitor : Udev.Monitor;
      Subsystem : String;
      Devtype   : access String) return Success_Flag is
   begin
      if Devtype /= null then
         declare
            Chars : aliased Interfaces.C.char_array
              := Interfaces.C.To_C (Devtype.all);
         begin
            if
              Thin.Udev_Monitor_Filter_Add_Match_Subsystem_Devtype
                (Monitor.My_Ptr,
                 +Subsystem,
                 Interfaces.C.Strings.To_Chars_Ptr (Chars'Unchecked_Access))
                  >= 0
            then
               return Success;
            else
               return Failure;
            end if;
         end;
      else
         if
           Thin.Udev_Monitor_Filter_Add_Match_Subsystem_Devtype
             (Monitor.My_Ptr, +Subsystem, Interfaces.C.Strings.Null_Ptr) >= 0
         then
            return Success;
         else
            return Failure;
         end if;
      end if;
   end Filter_Add_Match_Subsystem_Devtype;

   function Enable_Receiving (Monitor : Udev.Monitor) return Success_Flag is
   begin
      if Thin.Udev_Monitor_Enable_Receiving (Monitor.My_Ptr) >= 0 then
         return Success;
      else
         return Failure;
      end if;
   end Enable_Receiving;

   function Get_File_Descriptor (Monitor : Udev.Monitor) return Integer is
   begin
      return Integer (Thin.Udev_Monitor_Get_Fd (Monitor.My_Ptr));
   end Get_File_Descriptor;

   procedure Receive_Device (Monitor : Udev.Monitor;
                             Device  : out Udev.Device) is
   begin
      Device.My_Ptr := Thin.Udev_Monitor_Receive_Device (Monitor.My_Ptr);
   end Receive_Device;

end Udev;
