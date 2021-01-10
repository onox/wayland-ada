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

with Interfaces.C;

package body Wayland.Posix is

   type Unsigned_16 is mod 2 ** 16
     with Size => 16;

   type Poll_File_Descriptor is record
      Descriptor : aliased Integer := -1;
      Events     : aliased Unsigned_16 := 0;
      Revents    : aliased Unsigned_16 := 0;
   end record with
     Convention => C_Pass_By_Copy;

   type Poll_File_Descriptor_Array is
     array (Positive range <>) of Poll_File_Descriptor
   with Convention => C;

   function C_Poll
     (File_Descriptors        : Poll_File_Descriptor_Array;
      File_Descriptors_Length : Interfaces.C.unsigned_long;
      Timeout                 : Integer) return Integer
   with Import, Convention => C, External_Name => "poll";

   function Poll
     (File_Descriptors : Poll_File_Descriptor_Array;
      Timeout          : Integer) return Integer
   is (C_Poll (File_Descriptors, File_Descriptors'Length, Timeout));

   Poll_In  : constant := 1;
   Poll_Out : constant := 4;

   Error_Interrupt : constant := 4;
   Error_Again     : constant := 11;

   function Errno return Interfaces.C.int
     with Import, Convention => C, External_Name => "get_errno";

   function Error_Number return Integer is (Integer (Errno));

   function Poll
     (Descriptor : Integer;
      Timeout    : Duration;
      Mode       : Poll_Mode) return Integer
   is
      File_Descriptors : constant Poll_File_Descriptor_Array
        := (1 => (Descriptor => Descriptor,
                  Events     => (case Mode is
                                   when Input  => Poll_In,
                                   when Output => Poll_Out),
                  Revents    => 0));
      Milliseconds : constant Integer := Integer (Timeout * 1e3);
   begin
      while True loop
         declare
            Result : constant Integer := Poll (File_Descriptors, Milliseconds);
         begin
            if Result /= -1 or else Errno not in Error_Interrupt | Error_Again then
               return Result;
            end if;
         end;
      end loop;
   end Poll;

   function Poll
     (Descriptor : Integer;
      Timeout    : Duration) return Integer
   is (Poll (Descriptor, Timeout, Input));

end Wayland.Posix;
