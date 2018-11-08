limited with C_Binding.Linux.Udev.Contexts;

package C_Binding.Linux.Udev.Devices is

   type Device;

   package Bug is

      procedure Get_Parent
        (Device : Devices.Device;
         Parent : out Devices.Device);

   end Bug;

   type Device is new Device_Base with null record;

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

   function Syspath (Device : Devices.Device) return String_Result;
   function Devpath (Device : Devices.Device) return String_Result;
   function Sysattr (Device : Devices.Device;
                     Name   : String) return String_Result;
   function Driver  (Device : Devices.Device) return String_Result;
   function Devtype (Device : Devices.Device) return String_Result;
   function Sysname (Device : Devices.Device) return String_Result;

private

   function Exists (Device : Devices.Device) return Boolean is
     (Device.My_Ptr /= null);

end C_Binding.Linux.Udev.Devices;
