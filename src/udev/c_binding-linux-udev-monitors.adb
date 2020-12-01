--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2018 - 2019 Joakim Strandberg <joakim@mequinox.se>
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.

package body C_Binding.Linux.Udev.Monitors is

   use type int;

   function Udev_Monitor_Ref
     (Monitor : Udev_Monitor_Ptr) return Udev_Monitor_Ptr;
   pragma Import (C, Udev_Monitor_Ref, "udev_monitor_ref");

   function Udev_Monitor_Unref
     (Monitor : Udev_Monitor_Ptr) return Udev_Monitor_Ptr;
   pragma Import (C, Udev_Monitor_Unref, "udev_monitor_unref");

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
      Size    : Int
     ) return Int;
   pragma Import
     (C,
      Udev_Monitor_Set_Receive_Buffer_Size,
      "udev_monitor_set_receive_buffer_size");

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

   function Udev_Monitor_Filter_Update
     (Monitor : Udev_Monitor_Ptr) return Int;
   pragma Import
     (C, Udev_Monitor_Filter_Update, "udev_monitor_filter_update");

   function Udev_Monitor_Filter_Remove
     (Monitor : Udev_Monitor_Ptr) return Int;
   pragma Import
     (C, Udev_Monitor_Filter_Remove, "udev_monitor_filter_remove");

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

   procedure Acquire_Reference
     (Original  : Monitor;
      Reference : out Monitor) is
   begin
      Reference.My_Ptr := Udev_Monitor_Ref (Original.My_Ptr);
   end Acquire_Reference;

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
