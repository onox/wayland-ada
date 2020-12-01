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

with Wayland_XML;
with Aida;
with Ada.Containers.Vectors;

pragma Elaborate_All (Wayland_XML);
pragma Elaborate_All (Aida);

package Xml_Parser_Utils is

   function Adaify_Name (Old_Name : String) return String;

   -- If the variable name is "Interface" then it will be recognized as
   -- a reserved word in Ada and it will be suffixed with "_V" resulting
   -- in "Interface_V".
   function Adaify_Variable_Name (Old_Name : String) return String;

   function Arg_Type_As_String (Arg_Tag : Wayland_XML.Arg_Tag) return String;

   function Number_Of_Args
     (Request_Tag : aliased Wayland_XML.Request_Tag) return Natural;

   function Is_New_Id_Argument_Present
     (Request_Tag : aliased Wayland_XML.Request_Tag) return Boolean;

   function Is_Interface_Specified
     (Request_Tag : aliased Wayland_XML.Request_Tag) return Boolean with
     Pre => Is_New_Id_Argument_Present (Request_Tag);

   Interface_Not_Found_Exception : exception;

   -- will raise Interface_Not_Found_Exception is pre-condition is not met.
   function Find_Specified_Interface
     (Request_Tag : aliased Wayland_XML.Request_Tag) return String with
     Pre => Is_Interface_Specified (Request_Tag);

   function Is_Request_Destructor
     (Request_Tag : aliased Wayland_XML.Request_Tag) return Boolean;

   function Exists_Destructor
     (Interface_Tag : aliased Wayland_XML.Interface_Tag) return Boolean;

   function Exists_Any_Event_Tag
     (Interface_Tag : aliased Wayland_XML.Interface_Tag) return Boolean;

   function Remove_Tabs (Text : String) return String;

   type Interval is record
      First : Positive;
      Last  : Natural;
   end record;

   package Interval_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Interval,
      "="          => "=");

   type Intervals_Ref
     (Element : not null access constant Interval_Vectors.Vector)
   is limited null record with Implicit_Dereference => Element;

   type Interval_Identifier is tagged limited private;

   function Intervals
     (This : aliased Interval_Identifier) return Intervals_Ref with
     Global => null;

   function Make (Text : String) return Interval_Identifier with
     Global => null;

private

   type Interval_Identifier is tagged limited record
      My_Intervals : aliased Interval_Vectors.Vector;
   end record;

   function Intervals
     (This : aliased Interval_Identifier) return Intervals_Ref
   is ((Element => This.My_Intervals'Access));

end Xml_Parser_Utils;
