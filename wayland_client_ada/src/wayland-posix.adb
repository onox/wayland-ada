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

   type Returned_Event_Bits is record
      Input    : Boolean := False;
      Priority : Boolean := False;
      Output   : Boolean := False;
      Error    : Boolean := False;
      Hang_Up  : Boolean := False;
      Invalid  : Boolean := False;
      Unused   : Unused_Type;
   end record;

   for Returned_Event_Bits use record
      Input    at 0 range 0 .. 0;
      Priority at 0 range 1 .. 1;
      Output   at 0 range 2 .. 2;
      Error    at 0 range 3 .. 3;
      Hang_Up  at 0 range 4 .. 4;
      Invalid  at 0 range 5 .. 5;
      Unused   at 0 range 6 .. 15;
   end record;
   for Returned_Event_Bits'Size use Interfaces.C.short'Size;

   use type Interfaces.C.int;

   type Poll_File_Descriptor is record
      Descriptor : Interfaces.C.int    := -1;
      Requested  : Event_Bits          := (others => False);
      Returned   : Returned_Event_Bits := (others => False);
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
     (File_Descriptors : in out Poll_File_Descriptor_Array;
      Timeout          : Integer) return Integer
   is (C_Poll (File_Descriptors, File_Descriptors'Length, Timeout));

   Error_Interrupt : constant := 4;
   Error_Again     : constant := 11;

   function Errno return Interfaces.C.int
     with Import, Convention => C, External_Name => "get_errno";

   function Error_Number return Integer is (Integer (Errno));

   function Poll
     (Events  : Requested_Event_Array;
      Timeout : Duration) return Returned_Event_Array
   is
      Milliseconds : constant Integer := Integer (Timeout * 1e3);

      File_Descriptors : Poll_File_Descriptor_Array (Events'Range);
   begin
      for Index in File_Descriptors'Range loop
         File_Descriptors (Index) :=
           (Descriptor => Interfaces.C.int (Events (Index).FD),
            Requested  => Events (Index).Events,
            others     => <>);
      end loop;

      while True loop
         declare
            Count : constant Integer := Poll (File_Descriptors, Milliseconds);
         begin
            if Count /= -1 or else Errno not in Error_Interrupt | Error_Again then
               return Result : Returned_Event_Array (File_Descriptors'Range) do
                  for Index in Result'Range loop
                     declare
                        FD : Poll_File_Descriptor renames File_Descriptors (Index);
                     begin
                        if Count = -1
                          or else (FD.Returned.Error or FD.Returned.Hang_Up or FD.Returned.Invalid)
                        then
                           Result (Index) :=
                             (Is_Success => False,
                              FD         => File_Descriptor (FD.Descriptor));
                        else
                           Result (Index) :=
                             (Is_Success => True,
                              FD         => File_Descriptor (FD.Descriptor),
                              Events     =>
                                (Input    => FD.Returned.Input,
                                 Priority => FD.Returned.Priority,
                                 Output   => FD.Returned.Output,
                                 Unused   => False));
                        end if;
                     end;
                  end loop;
               end return;
            end if;
         end;
      end loop;
      raise Program_Error;
   end Poll;

end Wayland.Posix;
