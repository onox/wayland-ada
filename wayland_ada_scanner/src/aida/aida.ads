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

package Aida with SPARK_Mode is
   pragma Preelaborate;

   function To_String (This : Integer) return String
     with Global => null,
          Post   =>
       (if This < 0 then
          To_String'Result'Length >= 2 and To_String'Result'Length <= 11
            else
              To_String'Result'Length >= 1 and To_String'Result'Length <= 10);

   procedure To_Integer
     (Source     : String;
      Target     : out Integer;
      Has_Failed : out Boolean)
   with Global => null;

   type Call_Result is tagged limited private
     with Default_Initial_Condition => Call_Result.Has_Failed = False;

   procedure Initialize
     (This   : in out Call_Result;
      Code_1 : Integer;
      Code_2 : Integer)
   with Global     => null,
        Pre'Class  => not Has_Failed (This),
        Post'Class => This.Has_Failed;

   function Has_Failed (This : Call_Result) return Boolean
     with Global => null;

   function Message (This : Call_Result) return String
     with Global => null;

private

   type Call_Result is tagged limited
      record
         Code_1     : Integer := 0;
         Code_2     : Integer := 0;
         My_Has_Failed : Boolean := False;
      end record;

   function Has_Failed (This : Call_Result) return Boolean is
     (This.My_Has_Failed);

end Aida;
