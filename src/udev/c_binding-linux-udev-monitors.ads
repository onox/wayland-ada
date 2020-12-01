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

with C_Binding.Linux.Udev.Devices;
with C_Binding.Linux.Udev.Contexts;

package C_Binding.Linux.Udev.Monitors is

   type Monitor;

    procedure Acquire_Reference
     (Original  : Monitor;
      Reference : out Monitor) with
     Pre => Monitors.Exists (Original);

   type Monitor is new Monitor_Base with private;

   function Exists (Monitor : Monitors.Monitor) return Boolean;

   procedure Delete (Monitor : in out Monitors.Monitor) with
     Pre  => Monitor.Exists,
     Post => not Monitor.Exists;

   function Filter_Add_Match_Subsystem_Devtype
     (Monitor   : Monitors.Monitor;
      Subsystem : String;
      Devtype   : access String) return Success_Flag;

   function Enable_Receiving (Monitor : Monitors.Monitor) return Success_Flag;

   function Get_File_Descriptor (Monitor : Monitors.Monitor) return Integer;

   procedure Receive_Device
     (Monitor : Monitors.Monitor;
      Device  : out Devices.Device);

   procedure Context
     (Monitor : Monitors.Monitor;
      Context : out Contexts.Context);

   function Set_Receive_Buffer_Size
     (
      Monitor : Monitors.Monitor;
      Size    : Integer
     ) return Success_Flag;

   function Filter_Add_Match_Tag
     (
      Monitor : Monitors.Monitor;
      Tag     : String
     ) return Success_Flag;

   function Filter_Update
     (
      Monitor : Monitors.Monitor
     ) return Success_Flag;

   function Filter_Remove
     (
      Monitor : Monitors.Monitor
     ) return Success_Flag;

private

   type Monitor is new Monitor_Base with null record;

   overriding
   procedure Finalize (Monitor : in out Monitors.Monitor);

end C_Binding.Linux.Udev.Monitors;
