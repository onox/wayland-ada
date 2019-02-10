with Dynamic_Pools;
with Ada.Containers.Vectors;
with Aida.Deepend_XML_SAX_Parser;

pragma Elaborate_All (Dynamic_Pools);
pragma Elaborate_All (Aida.Deepend_XML_SAX_Parser);

package Aida.Deepend_XML_DOM_Parser is

   Default_Subpool : Dynamic_Pools.Dynamic_Pool (0);
   -- Allocations are done in subpools, not the default subpool

   type String_Ptr is access all String with
     Storage_Pool => Default_Subpool;

   Empty_String : aliased String := "";

   type Attribute is limited record
      Name  : String_Ptr;
      Value : String_Ptr;
   end record;

   type Attribute_Ptr is access all Attribute with
     Storage_Pool => Default_Subpool;

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
   type Node_Ptr is access all Node with Storage_Pool => Default_Subpool;

   package Node_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Node_Ptr);

   type XML_Tag is record
      Name        : String_Ptr;
      Child_Nodes : aliased Node_Vectors.Vector;
      Attributes  : aliased Attribute_Vectors.Vector;
   end record;

   type Node (Id : Node_Kind_Id := Node_Kind_Tag) is record
      case Id is
         when Node_Kind_Tag  =>
            Tag : aliased XML_Tag;
         when Node_Kind_Comment |
              Node_Kind_CDATA |
              Node_Kind_Text =>
            Text : not null String_Ptr := Empty_String'Access;
      end case;
   end record;

   procedure Parse
     (Subpool     : in out Dynamic_Pools.Subpool_Handle;
      XML_Message : String;
      Call_Result : in out Aida.Call_Result;
      Root_Node   :    out Node_Ptr) with
     Global => null,
     Pre    =>
       (not Call_Result.Has_Failed and
          XML_Message'Length > 0 and XML_Message'Last < Integer'Last - 4);

private

   type State_T is
     (
      Expecting_Object_Start, -- seems to only apply to the root start tag
      --  Expecting_Attribute_Or_Text_Or_Comment_Or_CDATA_Or_Object_Start_Or_Object_End,
      Expecting_Default, -- Attribute_Or_Text_Or_Comment_Or_CDATA_Or_Object_Start_Or_Object_End
      End_State
     );

   type SAX_Parser is new Aida.Deepend_XML_SAX_Parser.SAX_Parser with record
      Current_Nodes : Node_Vectors.Vector;
      -- The current node is the last Node pointed to in the container

      Root_Node : Node_Ptr := null;
      State     : State_T := Expecting_Object_Start;
      Subpool   : Dynamic_Pools.Subpool_Handle;
   end record;

   overriding
   procedure Handle_Start_Tag
     (This        : in out SAX_Parser;
      Tag_Name    : String;
      Call_Result : in out Aida.Call_Result) with
     Global => null,
     Pre    => not Call_Result.Has_Failed;

   overriding
   procedure Handle_End_Tag
     (This        : in out SAX_Parser;
      Tag_Name    : String;
      Call_Result : in out Aida.Call_Result) with
     Global => null,
     Pre    => not Call_Result.Has_Failed;

   overriding
   procedure Handle_Text
     (This        : in out SAX_Parser;
      Value       : String;
      Call_Result : in out Aida.Call_Result) with
     Global => null,
     Pre    => not Call_Result.Has_Failed;

   overriding
   procedure Handle_Attribute
     (This            : in out SAX_Parser;
      Attribute_Name  : String;
      Attribute_Value : String;
      Call_Result     : in out Aida.Call_Result) with
     Global => null,
     Pre    => not Call_Result.Has_Failed;

   overriding
   procedure Handle_Comment
     (This        : in out SAX_Parser;
      Value       : String;
      Call_Result : in out Aida.Call_Result) with
     Global => null,
     Pre    => not Call_Result.Has_Failed;

   overriding
   procedure Handle_CDATA
     (This        : in out SAX_Parser;
      Value       : String;
      Call_Result : in out Aida.Call_Result) with
     Global => null,
     Pre    => not Call_Result.Has_Failed;

end Aida.Deepend_XML_DOM_Parser;
