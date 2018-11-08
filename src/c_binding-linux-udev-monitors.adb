package body C_Binding.Linux.Udev.Monitors is

   use type int;

   procedure Delete (Monitor : in out Monitors.Monitor) is
   begin
      Monitor.My_Ptr := Thin.Udev_Monitor_Unref (Monitor.My_Ptr);
      Monitor.My_Ptr := null;
      --  Is unnecessary, but static code analyzers cannot know
      --  Thin.Udev_Monitor_Unref (..) always returns null.
   end Delete;

   function Filter_Add_Match_Subsystem_Devtype
     (Monitor : Monitors.Monitor;
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

   function Enable_Receiving
     (Monitor : Monitors.Monitor) return Success_Flag is
   begin
      if Thin.Udev_Monitor_Enable_Receiving (Monitor.My_Ptr) >= 0 then
         return Success;
      else
         return Failure;
      end if;
   end Enable_Receiving;

   function Get_File_Descriptor (Monitor : Monitors.Monitor) return Integer is
   begin
      return Integer (Thin.Udev_Monitor_Get_Fd (Monitor.My_Ptr));
   end Get_File_Descriptor;

   procedure Receive_Device (Monitor : Monitors.Monitor;
                             Device  : out Devices.Device) is
   begin
      Device_Base (Device).My_Ptr
        := Thin.Udev_Monitor_Receive_Device (Monitor.My_Ptr);
   end Receive_Device;

end C_Binding.Linux.Udev.Monitors;
