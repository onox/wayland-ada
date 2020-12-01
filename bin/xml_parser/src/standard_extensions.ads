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

with Ada.Strings.Fixed;
with Ada.Text_IO;

package Standard_Extensions is

   function Trim (Source : String) return String is
     (Ada.Strings.Fixed.Trim (Source, Ada.Strings.Both));

   procedure Put
     (File : Ada.Text_IO.File_Type;
      Item : String) renames Ada.Text_IO.Put;

   procedure Put_Line
     (File : Ada.Text_IO.File_Type;
      Item : String) renames Ada.Text_IO.Put_Line;

   procedure New_Line
     (File    : Ada.Text_IO.File_Type;
      Spacing : Ada.Text_IO.Positive_Count := 1) renames Ada.Text_IO.New_Line;

   procedure Put_Line (Item : String) renames Ada.Text_IO.Put_Line;

end Standard_Extensions;
