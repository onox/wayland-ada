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

package C_Binding.Linux.Udev.List_Entries is

   type List_Entry;

   procedure Get_By_Name
     (
      Current : List_Entry;
      Name    : String;
      Found_Entry : out List_Entry
     ) with
       Pre => List_Entries.Exists (Current);

   type List_Entry is new List_Entry_Base with private;

   function Exists (List_Entry : List_Entries.List_Entry) return Boolean;

   procedure Next (List_Entry : in out List_Entries.List_Entry) with
     Pre => List_Entry.Exists;

   function Name  (List_Entry : List_Entries.List_Entry) return String_Result;
   function Value (List_Entry : List_Entries.List_Entry) return String_Result;

private

   type List_Entry is new List_Entry_Base with null record;

end C_Binding.Linux.Udev.List_Entries;
