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

with System;

with Interfaces.C.Strings;
private with Ada.Unchecked_Conversion;

package C_Binding is
   pragma Preelaborate;

   subtype Void_Ptr is System.Address;

   type Success_Flag is (Success, Failure);

   subtype Max_String_Length is Natural range 0 .. 10_000;

   type String_Result
     (Is_Success : Boolean := False;
      Length     : Max_String_Length := 1) is
   record
      case Is_Success is
         when True  => Value : String (1 .. Length);
         when False => Error : String (1 .. Length);
      end case;
   end record;

private

   Nul : constant Character := Character'Val (0);

   type C_String is new String with
     Dynamic_Predicate => C_String'Length > 0
     and then C_String (C_String'Last) = Nul;

   function "-" (Text : C_String) return String;
   --  Removes the last 'Nul' character and returns a normal String

   function "+" (Text : String) return C_String;
   --  Appends a 'Nul' character to a standard String and returns a C_String

   subtype char is Interfaces.C.char;
   subtype unsigned_long is Interfaces.C.unsigned_long;
   subtype unsigned is Interfaces.C.unsigned;
   subtype int is Interfaces.C.int;
   subtype long is Interfaces.C.long;
   subtype Unsigned_32 is Interfaces.Unsigned_32;
   subtype Unsigned_16 is Interfaces.Unsigned_16;

   subtype chars_ptr is Interfaces.C.Strings.chars_ptr;

   function "-" (Chars : chars_ptr) return String;
   --  Removes the last Character'Val (0) character and returns a String

end C_Binding;
