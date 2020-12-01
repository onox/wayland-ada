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

limited with C_Binding.Linux.Udev.Devices;
limited with C_Binding.Linux.Udev.Enumerates;
limited with C_Binding.Linux.Udev.Monitors;

package C_Binding.Linux.Udev.Contexts is

   type Context;

   procedure Acquire_Reference
     (Original  : Contexts.Context;
      Reference : out Contexts.Context) with
     Pre => Contexts.Exists (Original);

   procedure Create_Context (Context : out Contexts.Context);


   type Context is new Context_Base with private with
     Default_Initial_Condition => not Context.Exists;

   function Exists (Context : Contexts.Context) return Boolean;

   procedure Delete (Context : in out Contexts.Context) with
     Pre  => Context.Exists,
     Post => not Context.Exists;

   procedure Create_Enumerate
     (Context : Contexts.Context;
      Enum    : out Enumerates.Enumerate);

   procedure Create_Monitor
     (Context : Contexts.Context;
      Name    : String;
      Monitor : out Monitors.Monitor);

   procedure Create_Device
     (Context       : Contexts.Context;
      Block_Device  : Character;
      Device_Number : Interfaces.Unsigned_64;
      Device        : out Devices.Device);

   procedure Create_Device
     (Context   : Contexts.Context;
      Subsystem : String;
      Sysname   : String;
      Device    : out Devices.Device);

   procedure Create_Device
     (Context : Contexts.Context;
      Id      : String;
      Device  : out Devices.Device);

   procedure Create_Device
     (Context : Contexts.Context;
      Device  : out Devices.Device);

   function Log_Priority (Context : Contexts.Context) return Integer;

   procedure Set_Log_Priority
     (Context : Contexts.Context;
      Value   : Integer);

   generic
      with procedure Log
        (Context  : Contexts.Context;
         Priority : Integer;
         File     : String;
         Line     : Integer;
         Fn       : String;
         Format   : String);
   package Logging is

      procedure Redirect_Logs (Context : Contexts.Context);

   end Logging;

   generic
      type Data_Type (<>) is limited private;
      type Data_Ptr is not null access all Data_Type;
   package Custom_Data is

      procedure Set_Userdata
        (Context : Contexts.Context;
         Data    : Data_Ptr) with
        Pre => Context.Exists;

      function Get_Userdata
        (Context : Contexts.Context) return Data_Ptr;

   end Custom_Data;

   type Device_Index is new Positive;

   Empty_String : aliased String := "";

   type Device_Name
     (Name : not null access constant String)
   is limited null record;

   type Device_Array is
     array (Device_Index range <>) of Device_Name (Empty_String'Access);

   generic
      with procedure Handle_Error (Error_Message : String);
      with procedure Handle_Devices (Devices : Device_Array);
   procedure Generic_List_Devices
     (Subsystem : access constant String := null;
      Devtype   : access constant String := null);

private

   type Context is new Context_Base with null record;

   overriding
   procedure Finalize (Context : in out Contexts.Context);

   function Exists (Context : Contexts.Context) return Boolean is
     (Context.My_Ptr /= null);

end C_Binding.Linux.Udev.Contexts;
