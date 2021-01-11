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

with Ada.Strings.Fixed;

package body Aida with SPARK_Mode is

   function To_String (This : Integer) return String is
      function Trim (Value : String) return String is (Ada.Strings.Fixed.Trim (Value, Ada.Strings.Both));
   begin
      return Trim (This'Image);
   end To_String;

   procedure To_Integer
     (Source     : String;
      Target     : out Integer;
      Has_Failed : out Boolean) is
   begin
      Target := Integer'Value (Source);
      Has_Failed := False;
   exception
      when Constraint_Error =>
         Has_Failed := True;
   end To_Integer;

   procedure Initialize (This    : in out Call_Result;
                         Code_1 : Integer;
                         Code_2 : Integer) is
   begin
      This.Code_1 := Code_1;
      This.Code_2 := Code_2;
      This.My_Has_Failed := True;
   end Initialize;

   function Message (This : Call_Result) return String is
   begin
      return To_String (This.Code_1) & ", " & To_String (This.Code_2);
   end Message;

end Aida;
