with C_Binding.Linux.Udev.Devices;

package C_Binding.Linux.Udev.Monitors is

   type Monitor is new Monitor_Base with null record;

   function Exists (Monitor : Monitors.Monitor) return Boolean;

   procedure Delete (Monitor : in out Monitors.Monitor) with
     Pre  => Monitor.Exists,
     Post => not Monitor.Exists;

   function Filter_Add_Match_Subsystem_Devtype
     (Monitor : Monitors.Monitor;
      Subsystem : String;
      Devtype   : access String) return Success_Flag;

   function Enable_Receiving (Monitor : Monitors.Monitor) return Success_Flag;

   function Get_File_Descriptor (Monitor : Monitors.Monitor) return Integer;

   procedure Receive_Device (Monitor : Monitors.Monitor;
                             Device  : out Devices.Device);
private

   function Exists (Monitor : Monitors.Monitor) return Boolean is
     (Monitor.My_Ptr /= null);

end C_Binding.Linux.Udev.Monitors;
