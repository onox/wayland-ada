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

with Ada.Containers.Vectors;

package Aida.Deepend.XML_DOM_Parser is

   type Attribute is limited private;

   function Name (This : Attribute) return String with
     Global => null;

   function Value (This : Attribute) return String with
     Global => null;

   type Attribute_Ptr is access all Attribute;

   type Attribute_Index is new Positive;

   package Attribute_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Attribute_Index,
      Element_Type => Attribute_Ptr,
      "="          => "=");

   type Node_Kind_Id is
     (
      Node_Kind_Tag,
      Node_Kind_Comment,
      Node_Kind_CDATA,
      Node_Kind_Text
     );

   type Node;
   type Node_Ptr is access all Node;

   package Node_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Node_Ptr);

   type Attributes_Ref
     (
      Element : not null access constant Attribute_Vectors.Vector
     )
   is limited null record with Implicit_Dereference => Element;

   type Child_Nodes_Ref
     (
      Element : not null access constant Node_Vectors.Vector
     )
   is limited null record with Implicit_Dereference => Element;

   type XML_Tag is private;

   function Attributes (This : aliased XML_Tag) return Attributes_Ref with
     Global => null;

   function Child_Nodes (This : aliased XML_Tag) return Child_Nodes_Ref with
     Global => null;

   function Name (This : XML_Tag) return String with
     Global => null;

   type Node (Id : Node_Kind_Id := Node_Kind_Tag) is record
      case Id is
         when Node_Kind_Tag  =>
            Tag : aliased XML_Tag;
         when Node_Kind_Comment |
              Node_Kind_CDATA |
              Node_Kind_Text =>
            Text : not null String_Ptr := Empty_String'Access;
            --  It would be cool to specify that this can only be set once.
            --  To be improved in the future!
      end case;
   end record;

   procedure Parse
     (XML_Message : String;
      Call_Result : in out Aida.Call_Result;
      Root_Node   :    out Node_Ptr) with
     Global => null,
     Pre    =>
       (not Call_Result.Has_Failed and
          XML_Message'Length > 0 and XML_Message'Last < Integer'Last - 4);

private

   type Attribute is limited record
      My_Name  : String_Ptr;
      My_Value : String_Ptr;
   end record;

   type XML_Tag is record
      My_Name        : String_Ptr;
      My_Child_Nodes : aliased Node_Vectors.Vector;
      My_Attributes  : aliased Attribute_Vectors.Vector;
   end record;

end Aida.Deepend.XML_DOM_Parser;
