package body C_Binding.Linux.Udev.Monitors is

   use type int;

   function Udev_Monitor_Ref
     (Monitor : Udev_Monitor_Ptr) return Udev_Monitor_Ptr;
   pragma Import (C, Udev_Monitor_Ref, "udev_monitor_ref");
   --  Acquire a udev monitor object.

   function Udev_Monitor_Unref
     (Monitor : Udev_Monitor_Ptr) return Udev_Monitor_Ptr;
   pragma Import (C, Udev_Monitor_Unref, "udev_monitor_unref");
   --  Release a udev monitor object.

   function Udev_Monitor_Get_Udev
     (Monitor : Udev_Monitor_Ptr) return Udev_Ptr;
   pragma Import (C, Udev_Monitor_Get_Udev, "udev_monitor_get_udev");

   function Udev_Monitor_Enable_Receiving
     (Monitor : Udev_Monitor_Ptr) return Int;
   pragma Import
     (C, Udev_Monitor_Enable_Receiving, "udev_monitor_enable_receiving");

   function Udev_Monitor_Set_Receive_Buffer_Size
     (
      Monitor : Udev_Monitor_Ptr;
      Size    : Int  --  the size in bytes
     ) return Int;
   pragma Import
     (C,
      Udev_Monitor_Set_Receive_Buffer_Size,
      "udev_monitor_set_receive_buffer_size");
   --  Set the size of the kernel socket buffer. This call needs
   --  the appropriate privileges to succeed.
   --
   --  Returns 0 on success, otherwise -1 on error.

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

   function Udev_Monitor_Filter_Add_Match_Tag
     (Monitor : Udev_Monitor_Ptr;
      Tag     : C_String) return Int;
   pragma Import
     (C,
      Udev_Monitor_Filter_Add_Match_Tag,
      "udev_monitor_filter_add_match_tag");
   --  This filter is efficiently executed inside the kernel, and libudev
   --  subscribers will usually not be woken up for devices which do not match.
   --
   --  The filter must be installed before the monitor is
   --  switched to listening mode.
   --
   --  Returns 0 on success, otherwise a negative error value.

   function Udev_Monitor_Filter_Update
     (Monitor : Udev_Monitor_Ptr) return Int;
   pragma Import
     (C, Udev_Monitor_Filter_Update, "udev_monitor_filter_update");
   --  Update the installed socket filter. This is only needed,
   --  if the filter was removed or changed.
   --
   --  Returns 0 on success, otherwise a negative error value.

   function Udev_Monitor_Filter_Remove
     (Monitor : Udev_Monitor_Ptr) return Int;
   pragma Import
     (C, Udev_Monitor_Filter_Remove, "udev_monitor_filter_remove");
   --  Remove all filters from monitor.
   --
   --  Returns 0 on success, otherwise a negative error value.

   function Exists (Monitor : Monitors.Monitor) return Boolean is
     (Monitor.My_Ptr /= null);

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

   procedure Acquire
     (Original  : Monitor;
      Reference : out Monitor) is
   begin
      Reference.My_Ptr := Udev_Monitor_Ref (Original.My_Ptr);
   end Acquire;

   procedure Context
     (Monitor : Monitors.Monitor;
      Context : out Contexts.Context) is
   begin
      Context_Base (Context).My_Ptr := Udev_Monitor_Get_Udev (Monitor.My_Ptr);
   end Context;

   function Set_Receive_Buffer_Size
     (Monitor : Monitors.Monitor;
      Size    : Integer) return Success_Flag is
   begin
      if
        Udev_Monitor_Set_Receive_Buffer_Size
          (Monitor.My_Ptr, int (Size)) = 0
      then
         return Success;
      else
         return Failure;
      end if;
   end Set_Receive_Buffer_Size;

   function Filter_Add_Match_Tag
     (
      Monitor : Monitors.Monitor;
      Tag     : String
     ) return Success_Flag is
   begin
      if
        Udev_Monitor_Filter_Add_Match_Tag (Monitor.My_Ptr, +Tag) = 0
      then
         return Success;
      else
         return Failure;
      end if;
   end Filter_Add_Match_Tag;

   function Filter_Update (Monitor : Monitors.Monitor) return Success_Flag is
   begin
      if
        Udev_Monitor_Filter_Update (Monitor.My_Ptr) = 0
      then
         return Success;
      else
         return Failure;
      end if;
   end Filter_Update;

   function Filter_Remove (Monitor : Monitors.Monitor) return Success_Flag is
   begin
      if
        Udev_Monitor_Filter_Remove (Monitor.My_Ptr) = 0
      then
         return Success;
      else
         return Failure;
      end if;
   end Filter_Remove;

   procedure Finalize (Monitor : in out Monitors.Monitor) is
   begin
      if Monitor.Exists then
         Monitor.Delete;
      end if;
   end Finalize;

end C_Binding.Linux.Udev.Monitors;
