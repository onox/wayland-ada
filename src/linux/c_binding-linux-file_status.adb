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

package body C_Binding.Linux.File_Status is

   procedure Get_File_Status
     (File : in     C_Binding.Linux.Files.File;
      This : in out Status)
   is
      Result : constant Interfaces.C.int :=
        C_Get_File_Status
          (Fd     => File.My_File_Descriptor,
           Status => This.My_Status'Access);
   begin
      This.My_Is_Valid := Result = 0;
   end Get_File_Status;

end C_Binding.Linux.File_Status;
