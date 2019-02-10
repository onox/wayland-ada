with Aida.Deepend_XML_SAX_Parser;

pragma Elaborate_All (Aida.Deepend_XML_SAX_Parser);

package body Aida.Deepend_XML_DOM_Parser is

   procedure Handle_Start_Tag
     (This        : in out SAX_Parser;
      Tag_Name    : String;
      Call_Result : in out Aida.Call_Result) is
   begin
      case This.State is
         when Expecting_Object_Start =>
            if
              Tag_Name'Length > 0 and
              This.Current_Nodes.Is_Empty
            then
               declare
                  Current_Node : not null Node_Ptr
                    := new (This.Subpool) Node;
               begin
                  Current_Node.Tag.Name
                    := new (This.Subpool) String'(Tag_Name);
                  This.Current_Nodes.Append (Current_Node);
                  This.Root_Node := Current_Node;
               end;
               This.State := Expecting_Default;
            else
               Call_Result.Initialize (-2132671123, 1966624808);
            end if;
         when Expecting_Default =>
            if
              Tag_Name'Length > 0
            then
               declare
                  Current_Node : not null Node_Ptr
                    := new (This.Subpool) Node;
               begin
                  Current_Node.Tag.Name
                    := new (This.Subpool) String'(Tag_Name);

                  if
                    This.Current_Nodes.Constant_Reference
                      (This.Current_Nodes.Last_Index).all.Id = Node_Kind_Tag
                  then
                     This.Current_Nodes.Constant_Reference
                       (This.Current_Nodes.Last_Index).all.Tag.
                       Child_Nodes.Append (Current_Node);

                     This.Current_Nodes.Append (Current_Node);
                  else
                     Call_Result.Initialize (1695756105, 1714042669);
                  end if;
               end;
            else
               Call_Result.Initialize (-0416079960, -1464855808);
            end if;
         when End_State =>
            Call_Result.Initialize (0561631589, 0761077416);
      end case;
   end Handle_Start_Tag;

   procedure Handle_End_Tag
     (This        : in out SAX_Parser;
      Tag_Name    : String;
      Call_Result : in out Aida.Call_Result) is
   begin
      case This.State is
         when Expecting_Default =>
            if not This.Current_Nodes.Is_Empty and then
              (This.Current_Nodes.Constant_Reference
                 (This.Current_Nodes.Last_Index).all.Id = Node_Kind_Tag)
            then
               if This.Current_Nodes.Constant_Reference
                 (This.Current_Nodes.Last_Index).all.Tag.Name.all = Tag_Name
               then
                  This.Current_Nodes.Delete_Last;
                  if This.Current_Nodes.Is_Empty then
                     This.State := End_State;
                  end if;
               else
                  Call_Result.Initialize (-0316487383, -2063296151);
               end if;
            else
               Call_Result.Initialize (-1355522791, 1675536860);
            end if;
         when Expecting_Object_Start |
              End_State =>
            Call_Result.Initialize (-0728861922, -0299445966);
      end case;
   end Handle_End_Tag;

   procedure Handle_Text
     (This        : in out SAX_Parser;
      Value       : String;
      Call_Result : in out Aida.Call_Result) is
   begin
      case This.State is
         when Expecting_Default =>
            if
              Value'Length = 0 or
              (Value'Length > 0 and then
                 (for all I in Value'Range =>
                      Value (I) = ' ' or
                      Value (I) = Character'Val (10) or
                      Value (I) = Character'Val (13)))
            then
               null;
            elsif
              not This.Current_Nodes.Is_Empty
            then
               declare
                  Current_Node : not null Node_Ptr
                    := new (This.Subpool) Node (Node_Kind_Text);
               begin
                  Current_Node.all
                    := (Id   => Node_Kind_Text,
                        Text => new (This.Subpool) String'(Value));

                  if
                    This.Current_Nodes.Constant_Reference
                      (This.Current_Nodes.Last_Index).all.Id = Node_Kind_Tag
                  then
                     This.Current_Nodes.Constant_Reference
                       (This.Current_Nodes.Last_Index).all.Tag.
                       Child_Nodes.Append (Current_Node);
                  else
                     Call_Result.Initialize (-0944309962, -0212130363);
                  end if;
               end;
            else
               Call_Result.Initialize (0536156601, 0921613311);
            end if;
         when Expecting_Object_Start |
              End_State =>
            Call_Result.Initialize (0240750889, 1723362921);
      end case;
   end Handle_Text;

   procedure Handle_Attribute
     (This            : in out SAX_Parser;
      Attribute_Name  : String;
      Attribute_Value : String;
      Call_Result     : in out Aida.Call_Result) is
   begin
      case This.State is
         when Expecting_Default =>
            if not This.Current_Nodes.Is_Empty then
               if
                 Attribute_Name'Length > 0 and Attribute_Value'Length > 0
               then
                  declare
                     Attr : not null Attribute_Ptr
                       := new (This.Subpool) Attribute;
                  begin
                     Attr.Name  := new (This.Subpool) String'(Attribute_Name);
                     Attr.Value := new (This.Subpool) String'(Attribute_Value);

                     if
                       This.Current_Nodes.Constant_Reference
                       (This.Current_Nodes.Last_Index).all.Id = Node_Kind_Tag
                     then
                        This.Current_Nodes.Constant_Reference
                          (This.Current_Nodes.Last_Index).all.Tag.
                          Attributes.Append (Attr);
                     else
                        Call_Result.Initialize (0612916249, -0250963769);
                     end if;
                  end;
               else
                  Call_Result.Initialize (-1091502024, -1483543078);
               end if;
            else
               Call_Result.Initialize (-0372407662, -1139199208);
            end if;
         when Expecting_Object_Start |
              End_State =>
            Call_Result.Initialize (1103012185, 0319457400);
      end case;
   end Handle_Attribute;

   procedure Handle_Comment
     (This        : in out SAX_Parser;
      Value       : String;
      Call_Result : in out Aida.Call_Result) is
   begin
      case This.State is
         when Expecting_Default =>
            if
              not This.Current_Nodes.Is_Empty
            then
               if
                 Value'Length > 0
               then
                  declare
                     Current_Node : Node_Ptr
                       := new (This.Subpool) Node (Node_Kind_Comment);
                  begin
                     Current_Node.all
                       := (Id   => Node_Kind_Comment,
                           Text => new (This.Subpool) String'(Value));

                     if
                       This.Current_Nodes.Constant_Reference
                         (This.Current_Nodes.Last_Index).all.Id = Node_Kind_Tag
                     then
                        This.Current_Nodes.Constant_Reference
                          (This.Current_Nodes.Last_Index).all.Tag.
                          Child_Nodes.Append (Current_Node);
                     else
                        Call_Result.Initialize (2066772500, 1193932906);
                     end if;
                  end;
               else
                  Call_Result.Initialize (1366102371, 1421674126);
               end if;
            else
               Call_Result.Initialize (0845969060, 0639006566);
            end if;
         when Expecting_Object_Start |
              End_State =>
            Call_Result.Initialize (-1373186804, -0874315849);
      end case;
   end Handle_Comment;

   procedure Handle_CDATA
     (This        : in out SAX_Parser;
      Value       : String;
      Call_Result : in out Aida.Call_Result) is
   begin
      case This.State is
         when Expecting_Default =>
            if
              not This.Current_Nodes.Is_Empty
            then
               if
                 Value'Length > 0
               then
                  declare
                     Current_Node : Node_Ptr
                       := new (This.Subpool) Node (Node_Kind_CDATA);
                  begin
                     Current_Node.all
                       := (Id   => Node_Kind_CDATA,
                           Text => new (This.Subpool) String'(Value));

                     if
                       This.Current_Nodes.Constant_Reference
                         (This.Current_Nodes.Last_Index).all.Id = Node_Kind_Tag
                     then
                        This.Current_Nodes.Constant_Reference
                          (This.Current_Nodes.Last_Index).all.Tag.
                          Child_Nodes.Append (Current_Node);
                     else
                        Call_Result.Initialize (-2021174626, -1403249390);
                     end if;
                  end;
               else
                  Call_Result.Initialize (1915730777, 1973598725);
               end if;
            else
               Call_Result.Initialize (-0076965217, 0193355440);
            end if;
         when Expecting_Object_Start |
              End_State =>
            Call_Result.Initialize (0698504230, -0963685542);
      end case;
   end Handle_CDATA;

   procedure Parse
     (Subpool     : in out Dynamic_Pools.Subpool_Handle;
      XML_Message : String;
      Call_Result : in out Aida.Call_Result;
      Root_Node   :    out Node_Ptr)
   is
      Parser : SAX_Parser;
   begin
      Parser.Subpool := Subpool;
      Parser.Parse (XML_Message,
                        Call_Result);

      Root_Node := Parser.Root_Node;
   end Parse;

end Aida.Deepend_XML_DOM_Parser;
