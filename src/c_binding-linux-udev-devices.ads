with C_Binding.Linux.Udev.Contexts;

package C_Binding.Linux.Udev.Devices is

   type Device;

   procedure Get_Parent
     (Device : Devices.Device;
      Parent : out Devices.Device);

   type Device is new Device_Base with private;

   procedure Create
     (Device  : out Devices.Device;
      Context : Contexts.Context;
      Syspath : String);
   --  A Syspath is any subdirectory of /sys, with the restriction
   --  that a subdirectory of /sys/devices (or a symlink to one) represents
   --  a real device and as such must contain a uevent file.

   function Exists (Device : Devices.Device) return Boolean;

   procedure Delete (Device : in out Devices.Device) with
     Pre  => Device.Exists,
     Post => not Device.Exists;

   function Syspath (Device : Devices.Device) return String_Result with
     Pre  => Device.Exists;

   function Devpath (Device : Devices.Device) return String_Result with
     Pre  => Device.Exists;

   function Sysattr (Device : Devices.Device;
                     Name   : String) return String_Result with
     Pre  => Device.Exists;

   function Driver  (Device : Devices.Device) return String_Result with
     Pre  => Device.Exists;

   function Devtype (Device : Devices.Device) return String_Result with
     Pre  => Device.Exists;

   function Sysname (Device : Devices.Device) return String_Result with
     Pre  => Device.Exists;

private

   type Device is new Device_Base with null record;

end C_Binding.Linux.Udev.Devices;
