package body C_Binding.Linux.Udev.Monitors is

   use type int;

--     function Udev_Monitor_Ref
--       (Monitor : Udev_Monitor_Ptr) return Udev_Monitor_Ptr;
--     pragma Import (C, Udev_Monitor_Ref, "udev_monitor_ref");
   --  Acquire a udev monitor object.

   function Udev_Monitor_Unref
     (Monitor : Udev_Monitor_Ptr) return Udev_Monitor_Ptr;
   pragma Import (C, Udev_Monitor_Unref, "udev_monitor_unref");
   --  Release a udev monitor object.

--     function Udev_Monitor_Get_Udev
--       (Arg1 : System.Address) return System.Address;
--     pragma Import (C, Udev_Monitor_Get_Udev, "udev_monitor_get_udev");

   function Udev_Monitor_Enable_Receiving
     (Monitor : Udev_Monitor_Ptr) return Int;
   pragma Import
     (C, Udev_Monitor_Enable_Receiving, "udev_monitor_enable_receiving");

--     function Udev_Monitor_Set_Receive_Buffer_Size
--       (Arg1 : System.Address; Arg2 : Int) return Int;
--     pragma Import
--       (C,
--        Udev_Monitor_Set_Receive_Buffer_Size,
--        "udev_monitor_set_receive_buffer_size");

   function Udev_Monitor_Get_Fd (Arg1 : Udev_Monitor_Ptr) return Int;
   pragma Import (C, Udev_Monitor_Get_Fd, "udev_monitor_get_fd");

   function Udev_Monitor_Receive_Device
     (Monitor : Udev_Monitor_Ptr) return Udev_Device_Ptr;
   pragma Import
     (C, Udev_Monitor_Receive_Device, "udev_monitor_receive_device");

   function Udev_Monitor_Filter_Add_Match_Subsystem_Devtype
     (Monitor   : Udev_Monitor_Ptr;
      Subsystem : C_String;
      Devtype   : Interfaces.C.Strings.Chars_Ptr) return Int;
   pragma Import
     (C,
      Udev_Monitor_Filter_Add_Match_Subsystem_Devtype,
      "udev_monitor_filter_add_match_subsystem_devtype");

--     function Udev_Monitor_Filter_Add_Match_Tag
--       (Arg1 : System.Address;
--        Arg2 : Interfaces.C.Strings.Chars_Ptr) return Int;
--     pragma Import
--       (C,
--        Udev_Monitor_Filter_Add_Match_Tag,
--        "udev_monitor_filter_add_match_tag");

--     function Udev_Monitor_Filter_Update (Arg1 : System.Address) return Int;
--     pragma Import
--       (C, Udev_Monitor_Filter_Update, "udev_monitor_filter_update");

--     function Udev_Monitor_Filter_Remove (Arg1 : System.Address) return Int;
--     pragma Import
--       (C, Udev_Monitor_Filter_Remove, "udev_monitor_filter_remove");

   procedure Delete (Monitor : in out Monitors.Monitor) is
   begin
      Monitor.My_Ptr := Udev_Monitor_Unref (Monitor.My_Ptr);
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
              Udev_Monitor_Filter_Add_Match_Subsystem_Devtype
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
           Udev_Monitor_Filter_Add_Match_Subsystem_Devtype
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
      if Udev_Monitor_Enable_Receiving (Monitor.My_Ptr) >= 0 then
         return Success;
      else
         return Failure;
      end if;
   end Enable_Receiving;

   function Get_File_Descriptor (Monitor : Monitors.Monitor) return Integer is
   begin
      return Integer (Udev_Monitor_Get_Fd (Monitor.My_Ptr));
   end Get_File_Descriptor;

   procedure Receive_Device (Monitor : Monitors.Monitor;
                             Device  : out Devices.Device) is
   begin
      Device_Base (Device).My_Ptr
        := Udev_Monitor_Receive_Device (Monitor.My_Ptr);
   end Receive_Device;

end C_Binding.Linux.Udev.Monitors;
