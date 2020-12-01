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

with C_Binding.Linux.Udev.Contexts;
with C_Binding.Linux.Udev.List_Entries;

package C_Binding.Linux.Udev.Devices is

   type Device;

   procedure Get_Parent
     (Device : Devices.Device;
      Parent : out Devices.Device);

   procedure Get_Parent
     (Device    : Devices.Device;
      Subsystem : String;
      Devtype   : String;
      Parent    : out Devices.Device);

   procedure Acquire_Reference
     (Original  : Device;
      Reference : out Device) with
     Pre => Devices.Exists (Original);

   procedure Create_Device
     (Device  : out Devices.Device;
      Context : Contexts.Context;
      Syspath : String);

   type Device is new Device_Base with private;

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

   procedure Get_Context
     (Device  : Devices.Device;
      Context : out Contexts.Context) with
     Pre => Device.Exists;

   function Subsystem
     (Device : Devices.Device) return String_Result with
     Pre  => Device.Exists;

   function Sysnum
     (Device : Devices.Device) return String_Result with
     Pre  => Device.Exists;

   function Devnode
     (Device : Devices.Device) return String_Result with
     Pre  => Device.Exists;

   type Initialization_Status is
     (
      Initialized,
      Not_Initialized,
      Unknown
     );

   function Is_Initialized
     (Device : Devices.Device) return Initialization_Status with
     Pre  => Device.Exists;

   procedure Devlinks_List_Entry
     (Device     : Devices.Device;
      List_Entry : out List_Entries.List_Entry) with
     Pre => Device.Exists;

   procedure Properties_List_Entry
     (Device     : Devices.Device;
      List_Entry : out List_Entries.List_Entry) with
     Pre => Device.Exists;

   procedure Tags_List_Entry
     (Device     : Devices.Device;
      List_Entry : out List_Entries.List_Entry) with
     Pre => Device.Exists;

   procedure Sysattr_List_Entry
     (Device     : Devices.Device;
      List_Entry : out List_Entries.List_Entry) with
     Pre => Device.Exists;

   function Property_Value
     (Device : Devices.Device;
      Key    : String) return String_Result with
     Pre  => Device.Exists;

   function Devnum
     (Device : Devices.Device) return Interfaces.Unsigned_64 with
     Pre  => Device.Exists;
   --  TODO: dev_t, how to handle?

   function Action (Device : Devices.Device) return String_Result with
     Pre  => Device.Exists;

   function Sequence_Number (Device : Devices.Device) return Long_Integer with
     Pre  => Device.Exists;

   function Microseconds_Since_Initialized
     (Device : Devices.Device) return Long_Integer with
     Pre  => Device.Exists;

   function Set_Sysattr
     (Device  : Devices.Device;
      Sysattr : String;
      Value   : String) return Success_Flag with
     Pre  => Device.Exists;

   type Tag_Status is
     (
      Tag_Is_Present,
      Tag_is_Missing,

      Unknown
     );

   function Has_Tag
     (Device : Devices.Device;
      Tag    : String) return Tag_Status with
     Pre  => Device.Exists;

private

   type Device is new Device_Base with null record;

   overriding
   procedure Finalize (Device : in out Devices.Device);

end C_Binding.Linux.Udev.Devices;
