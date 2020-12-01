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

with C_Binding.Linux.Udev.List_Entries;

package C_Binding.Linux.Udev.Hardware_Databases is
   pragma Obsolescent;

   type Database;

   procedure Acquire
     (Original  : Database;
      Reference : out Database) with
     Pre => Hardware_Databases.Exists (Original);

   type Database is new Hwdb_Base with private;

   function Exists (Database : Hardware_Databases.Database) return Boolean;

   procedure Delete (Database : in out Hardware_Databases.Database) with
     Pre  => Database.Exists,
     Post => not Database.Exists;

   procedure Properties_List_Entry
     (Database   : Hardware_Databases.Database;
      Modalias   : String;
      List_Entry : out List_Entries.List_Entry);

private

   type Database is new Hwdb_Base with null record;

   overriding
   procedure Finalize (Database : in out Hardware_Databases.Database);

end C_Binding.Linux.Udev.Hardware_Databases;
