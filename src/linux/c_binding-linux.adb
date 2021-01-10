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

package body C_Binding.Linux is

   use type Ada.Streams.Stream_Element_Offset;
   use type Interfaces.C.unsigned;
   use type Interfaces.Unsigned_32;

   function Convert_Unchecked is new Ada.Unchecked_Conversion
     (Source => Interfaces.C.int,
      Target => O_FLag);

   function Convert_Unchecked is new Ada.Unchecked_Conversion
     (Source => O_FLag,
      Target => Interfaces.C.int);

   procedure Set_File_Descriptor_Flag_Non_Blocking
     (File_Descriptor : in out Interfaces.C.int)
   is
      Temp : O_FLag := Convert_Unchecked (File_Descriptor);
   begin
      Temp := Temp or O_NONBLOCK;
      File_Descriptor := Convert_Unchecked (Temp);
   end Set_File_Descriptor_Flag_Non_Blocking;

   function Poll (Descriptor : Integer; Timeout : Duration) return Integer is
      File_Descriptors : constant C_Binding.Linux.Poll_File_Descriptor_Array
        := (1 => (Descriptor => Descriptor,
                  Events     => C_Binding.Linux.POLLIN,
                  Revents    => 0));
   begin
      return Poll (File_Descriptors, Integer (Timeout * 1e3));
      --  TODO If 'Result = -1 and errno in EINT | EAGAIN then poll again
   end Poll;

end C_Binding.Linux;
