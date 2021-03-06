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

private package Wayland.Posix is
   pragma Preelaborate;

   type Event_Bits is record
      Input    : Boolean := False;
      Priority : Boolean := False;
      Output   : Boolean := False;
      Unused   : Unused_Type;
   end record;

   type Requested_Event is record
      FD     : File_Descriptor;
      Events : Event_Bits;
   end record;

   type Returned_Event (Is_Success : Boolean := False) is record
      FD     : File_Descriptor;
      case Is_Success is
         when True  => Events : Event_Bits;
         when False => null;
      end case;
   end record;

   type Requested_Event_Array is array (Positive range <>) of Requested_Event;

   type Returned_Event_Array is array (Positive range <>) of Returned_Event;

   function Poll
     (Events  : Requested_Event_Array;
      Timeout : Duration) return Returned_Event_Array;
   --  Wait for data to become readable or writable on the file descriptor

   function Error_Number return Integer;

private

   for Event_Bits use record
      Input    at 0 range 0 .. 0;
      Priority at 0 range 1 .. 1;
      Output   at 0 range 2 .. 2;
      Unused   at 0 range 3 .. 15;
   end record;
   for Event_Bits'Size use Interfaces.C.short'Size;

end Wayland.Posix;
