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

   pragma Linker_Options ("-lwayland-client");
   --  Added this linker option here to avoid adding it
   --  to each gpr file that with's this Wayland Ada binding.

   Nul : constant Character := Character'Val (0);

   type Unsigned_32 is mod 2 ** Integer'Size
     with Size => Integer'Size;

   type Unsigned_32_Array is array (Positive range <>) of Unsigned_32
     with Convention => C;

   type Fixed is delta 2.0 ** (-8) range -(2.0 ** 23) .. +(2.0 ** 23 - 1.0)
     with Small => 2.0 ** (-8),
          Size  => Integer'Size;

private

   subtype Void_Ptr is System.Address;

   type Wayland_Array is record
      Size  : Interfaces.C.size_t;
      Alloc : Interfaces.C.size_t;
      Data  : Void_Ptr;
   end record
     with Convention => C;

end Wayland;
