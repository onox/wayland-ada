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

package body C_Binding is

   function "-" (Text : C_String) return String is
   begin
      return String (Text (Text'First .. Text'Last - 1));
   end "-";

   function "+" (Text : String) return C_String is
   begin
      return C_String (Text & Nul);
   end "+";

   function "-" (Chars : chars_ptr) return String is
      A : constant Interfaces.C.char_array
        := Interfaces.C.Strings.Value (Chars);
   begin
      return Interfaces.C.To_Ada (A);
   end "-";

end C_Binding;
