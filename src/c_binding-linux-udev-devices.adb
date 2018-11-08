with C_Binding.Linux.Udev.Contexts;

package body C_Binding.Linux.Udev.Devices is

   --package body Bug is

      procedure Get_Parent
        (Device : Devices.Device;
         Parent : out Devices.Device) is
      begin
         Parent.My_Ptr := Thin.Udev_Device_Get_Parent (Device.My_Ptr);
      end Get_Parent;

   --end Bug;

   procedure Create
     (Device  : out Devices.Device;
      Context : Contexts.Context;
      Syspath : String) is
   begin
      Device.My_Ptr := Thin.Udev_Device_New_From_Syspath
        (Context_Base (Context).My_Ptr, +Syspath);
   end Create;

   procedure Delete (Device : in out Devices.Device) is
   begin
      Device.My_Ptr := Thin.Udev_Device_Unref (Device.My_Ptr);
      Device.My_Ptr := null;
      --  Is unnecessary, but static code analyzers cannot know
      --  Thin.Udev_Device_Unref (..) always returns null.
   end Delete;

   function Syspath (Device : Devices.Device) return String_Result is
      Text : constant Interfaces.C.Strings.Chars_Ptr
        := Thin.Udev_Device_Get_Syspath (Device.My_Ptr);
   begin
      return Get_String_Result (Text, "Syspath failure");
   end Syspath;

   function Devpath (Device : Devices.Device) return String_Result is
      Text : constant Interfaces.C.Strings.Chars_Ptr
        := Thin.Udev_Device_Get_Devpath (Device.My_Ptr);
   begin
      return Get_String_Result (Text, "Devpath failure");
   end Devpath;

   function Sysattr (Device : Devices.Device;
                     Name   : String) return String_Result is
      Text : constant Interfaces.C.Strings.Chars_Ptr
        := Thin.Udev_Device_Get_Sysattr_Value (Device.My_Ptr, +Name);
   begin
      return Get_String_Result (Text, "Sysattr failure");
   end Sysattr;

   function Driver (Device : Devices.Device) return String_Result is
      Text : constant Interfaces.C.Strings.Chars_Ptr
        := Thin.Udev_Device_Get_Driver (Device.My_Ptr);
   begin
      return Get_String_Result (Text, "Driver failure");
   end Driver;

   function Devtype (Device : Devices.Device) return String_Result is
      Text : constant Interfaces.C.Strings.Chars_Ptr
        := Thin.Udev_Device_Get_Devtype (Device.My_Ptr);
   begin
      return Get_String_Result (Text, "Devtype failure");
   end Devtype;

   function Sysname (Device : Devices.Device) return String_Result is
      Text : constant Interfaces.C.Strings.Chars_Ptr
        := Thin.Udev_Device_Get_Sysname (Device.My_Ptr);
   begin
      return Get_String_Result (Text, "Devtype failure");
   end Sysname;

end C_Binding.Linux.Udev.Devices;
