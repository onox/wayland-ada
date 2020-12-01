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

with Ada.Containers;

use all type Ada.Containers.Count_Type;

generic
   type Argument_Type (<>) is limited private;
   with procedure Start_Tag
     (Argument    : in out Argument_Type;
      Tag_Name    : in     String;
      Call_Result : in out Aida.Call_Result);

   with procedure End_Tag
     (Argument    : in out Argument_Type;
      Tag_Name    : in     String;
      Call_Result : in out Aida.Call_Result);
   --  It is the responsibility of the implementor of End_Tag to verify
   --  that the tag name corresponds to the expected tag name.

   with procedure Text
     (Argument    : in out Argument_Type;
      Value       : in     String;
      Call_Result : in out Aida.Call_Result);

   with procedure Attribute
     (Argument        : in out Argument_Type;
      Attribute_Name  : in     String;
      Attribute_Value : in     String;
      Call_Result     : in out Aida.Call_Result);

   with procedure Comment
     (Argument    : in out Argument_Type;
      Value       : in     String;
      Call_Result : in out Aida.Call_Result);

   with procedure CDATA
     (Argument    : in out Argument_Type;
      Value       : in     String;
      Call_Result : in out Aida.Call_Result);

procedure Aida.XML_SAX_Parse
  (Argument    : in out Argument_Type;
   Contents    : in     String;
   Call_Result : in out Aida.Call_Result) with
  Global => null,
  Pre    =>
    (not Call_Result.Has_Failed and Contents'Length > 0 and
       Contents'Last < Integer'Last - 4);
pragma Annotate (GNATProve, Terminating, Aida.XML_SAX_Parse);
