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

with Interfaces.C.Strings;

private with Ada.Finalization;

package C_Binding.Linux.Udev is
   pragma Elaborate_Body;

   pragma Linker_Options ("-L/lib/x86_64-linux-gnu/");
   pragma Linker_Options ("-ludev");

   type Context_Base    is abstract tagged limited private;
   type Device_Base     is abstract tagged limited private;
   type Enumerate_Base  is abstract tagged limited private;
   type Monitor_Base    is abstract tagged limited private;
   type List_Entry_Base is abstract tagged limited private;
   type Queue_Base      is abstract tagged limited private;

   type Hwdb_Base       is abstract tagged limited private;
   --  Abbreviations are generally avoided when working with Ada,
   --  but hwdb (short for hardware database) is used here because this type
   --  can never used directly by a user of this API.

   subtype Success_Flag is C_Binding.Success_Flag;

private

   function Get_String_Result
     (Text  : Interfaces.C.Strings.Chars_Ptr;
      Error : String) return String_Result;

   type Udev_Context is null record;

   type Udev_Ptr is access Udev_Context;

   type Udev_List_Entry is null record;

   type Udev_List_Entry_Ptr is access Udev_List_Entry;

   type Udev_Device is null record;

   type Udev_Device_Ptr is access Udev_Device;

   type Udev_Enumerate is null record;

   type Udev_Enumerate_Ptr is access Udev_Enumerate;

   type Udev_Monitor is null record;

   type Udev_Monitor_Ptr is access Udev_Monitor;

   type Udev_Queue is null record;

   type Udev_Queue_Ptr is access Udev_Queue;

   type Udev_Hwdb is null record;

   type Udev_Hwdb_Ptr is access Udev_Hwdb;

   function Udev_Util_Encode_String
     (Arg1 : Interfaces.C.Strings.Chars_Ptr;
      Arg2 : Interfaces.C.Strings.Chars_Ptr;
      Arg3 : Unsigned_Long) return Int;
   pragma Import (C, Udev_Util_Encode_String, "udev_util_encode_string");
   --  TODO What to do with this C-function?

   type Monitor_Base is new Ada.Finalization.Limited_Controlled with record
      My_Ptr : Udev_Monitor_Ptr;
   end record;

   type Context_Base is new Ada.Finalization.Limited_Controlled with record
      My_Ptr : Udev_Ptr;
   end record;

   type Device_Base is new Ada.Finalization.Limited_Controlled with record
      My_Ptr : Udev_Device_Ptr;
   end record;

   type Enumerate_Base is new Ada.Finalization.Limited_Controlled with record
      My_Ptr : Udev_Enumerate_Ptr;
   end record;

   type List_Entry_Base is tagged limited record
      My_Ptr : Udev_List_Entry_Ptr;
   end record;

   type Queue_Base is new Ada.Finalization.Limited_Controlled with record
      My_Ptr : Udev_Queue_Ptr;
   end record;

   type Hwdb_Base is new Ada.Finalization.Limited_Controlled with record
      My_Ptr : Udev_Hwdb_Ptr;
   end record;

end C_Binding.Linux.Udev;
