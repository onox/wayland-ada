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

package body C_Binding.Linux.Udev is

   use type Interfaces.C.Strings.chars_ptr;

   function Get_String_Result
     (Text  : Interfaces.C.Strings.Chars_Ptr;
      Error : String) return String_Result is
   begin
      if Text /= Interfaces.C.Strings.Null_Ptr then
         declare
            Result : constant String := Interfaces.C.Strings.Value (Text);
         begin
            return
                (Is_Success => True,
                 Length     => Max_String_Length (Result'Length),
                 Value      => Result);
         end;
      else
         return
           (Is_Success => False,
            Length     => Max_String_Length (Error'Length),
            Error      => Error);
      end if;
   end Get_String_Result;

end C_Binding.Linux.Udev;
