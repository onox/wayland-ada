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

package body C_Binding.Linux.Udev.List_Entries is

   function Udev_List_Entry_Get_Next
     (Arg1 : Udev_List_Entry_Ptr) return Udev_List_Entry_Ptr;
   pragma Import (C, Udev_List_Entry_Get_Next, "udev_list_entry_get_next");

   function Udev_List_Entry_Get_By_Name
     (Current : Udev_List_Entry_Ptr;
      Name    : C_String) return Udev_List_Entry_Ptr;
   pragma Import
     (C, Udev_List_Entry_Get_By_Name, "udev_list_entry_get_by_name");

   function Udev_List_Entry_Get_Name
     (Arg1 : Udev_List_Entry_Ptr) return Interfaces.C.Strings.Chars_Ptr;
   pragma Import (C, Udev_List_Entry_Get_Name, "udev_list_entry_get_name");

   function Udev_List_Entry_Get_Value
     (Arg1 : Udev_List_Entry_Ptr) return Interfaces.C.Strings.Chars_Ptr;
   pragma Import
     (C, Udev_List_Entry_Get_Value, "udev_list_entry_get_value");

   function Exists (List_Entry : List_Entries.List_Entry) return Boolean is
     (List_Entry.My_Ptr /= null);

   procedure Next (List_Entry : in out List_Entries.List_Entry) is
   begin
      List_Entry.My_Ptr := Udev_List_Entry_Get_Next (List_Entry.My_Ptr);
   end Next;

   function Name (List_Entry : List_Entries.List_Entry) return String_Result is
      Text : constant Interfaces.C.Strings.Chars_Ptr
        := Udev_List_Entry_Get_Name (List_Entry.My_Ptr);
   begin
      return Get_String_Result (Text, "List entry name failure");
   end Name;

   function Value (List_Entry : List_Entries.List_Entry) return String_Result is
      Text : constant Interfaces.C.Strings.Chars_Ptr
        := Udev_List_Entry_Get_Value (List_Entry.My_Ptr);
   begin
      return Get_String_Result (Text, "List entry value failure");
   end Value;

   procedure Get_By_Name
     (
      Current : List_Entry;
      Name    : String;
      Found_Entry : out List_Entry
     ) is
   begin
      Found_Entry.My_Ptr
        := Udev_List_Entry_Get_By_Name (Current.My_Ptr, +Name);
   end Get_By_Name;

end C_Binding.Linux.Udev.List_Entries;
