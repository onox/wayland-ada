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

private with Interfaces.C;

private with System;

package Wayland is
   pragma Pure;

   type Unsigned_32 is mod 2 ** Integer'Size
     with Size => Integer'Size;

   type Unsigned_64 is mod 2 ** (Unsigned_32'Size * 2)
     with Size => Unsigned_32'Size * 2;

   type Unsigned_32_Array is array (Positive range <>) of Unsigned_32
     with Convention => C;

   type Fixed is delta 2.0 ** (-8) range -(2.0 ** 23) .. +(2.0 ** 23 - 1.0)
     with Small => 2.0 ** (-8),
          Size  => Integer'Size;

   type File_Descriptor is new Integer;

   type Call_Result_Code is (Success, Error);

   type Optional_Result (Is_Success : Boolean := False) is record
      case Is_Success is
         when True  => Count : Natural;
         when False => null;
      end case;
   end record;

   subtype Unused_Type is Boolean range False .. False;

private

   subtype Void_Ptr is System.Address;

   type Wayland_Array is record
      Size  : Interfaces.C.size_t;
      Alloc : Interfaces.C.size_t;
      Data  : Void_Ptr;
   end record
     with Convention => C;

end Wayland;
