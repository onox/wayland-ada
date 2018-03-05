package body Wayland_XML is

   procedure Set_Summary (This    : in out Arg_Tag;
                          Value   : String;
                          Subpool : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Summary := (Exists => True,
                          Value  => new (Subpool) String'(Value));
   end Set_Summary;

   procedure Set_Interface_Attribute (This    : in out Arg_Tag;
                                      Value   : String;
                                      Subpool : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Interface_Attribute := (Exists => True,
                                      Value  => new (Subpool) String'(Value));
   end Set_Interface_Attribute;

   procedure Set_Type_Attribute (This    : in out Arg_Tag;
                                 Value   : String)
   is
   begin
      if Value = "int" then
         This.My_Type_Attribute := (Exists => True,
                                    Value  => Type_Integer);
      elsif Value = "uint" then
         This.My_Type_Attribute := (Exists => True,
                                    Value  => Type_Unsigned_Integer);
      elsif Value = "string" then
         This.My_Type_Attribute := (Exists => True,
                                    Value  => Type_String);
      elsif Value = "fd" then
         This.My_Type_Attribute := (Exists => True,
                                    Value  => Type_FD);
      elsif Value = "new_id" then
         This.My_Type_Attribute := (Exists => True,
                                    Value  => Type_New_Id);
      elsif Value = "object" then
         This.My_Type_Attribute := (Exists => True,
                                    Value  => Type_Object);
      elsif Value = "fixed" then
         This.My_Type_Attribute := (Exists => True,
                                    Value  => Type_Fixed);
      elsif Value = "array" then
         This.My_Type_Attribute := (Exists => True,
                                    Value  => Type_Array);
      else
         raise TYPE_ATTRIBUTE_EXCEPTION with Value;
      end if;

   end Set_Type_Attribute;

   procedure Set_Name (This    : in out Arg_Tag;
                       Value   : String;
                       Subpool : Dynamic_Pools.Subpool_Handle) is
   begin
      This.My_Name := (Exists => True,
                       Value  => new (Subpool) String'(Value));
   end Set_Name;

   procedure Set_Allow_Null (This  : in out Arg_Tag;
                             Value : Boolean)
   is
   begin
      This.My_Allow_Null := (Exists => True,
                             Value  => Value);
   end Set_Allow_Null;

   procedure Set_Enum (This    : in out Arg_Tag;
                       Value   : String;
                       Subpool : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Enum := (Exists => True,
                       Value  => new (Subpool) String'(Value));
   end Set_Enum;

   procedure Set_Text (This    : in out Copyright_Tag;
                       Value   : String;
                       Subpool : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Text := (Exists => True,
                       Value  => new (Subpool) String'(Value));
   end Set_Text;

   procedure Set_Text (This    : in out Description_Tag;
                       Value   : String;
                       Subpool : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Text := (Exists => True,
                       Value  => new (Subpool) String'(Value));
   end Set_Text;

   procedure Set_Summary (This    : in out Description_Tag;
                          Value   : String;
                          Subpool : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Summary := (Exists => True,
                          Value  => new (Subpool) String'(Value));
   end Set_Summary;

   procedure Set_Value (This  : in out Entry_Tag;
                        Value : Entry_Value)
   is
   begin
      This.My_Value := (Exists => True,
                        Value  => Value);
   end Set_Value;

   procedure Set_Summary (This    : in out Entry_Tag;
                          Value   : String;
                          Subpool : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Summary := (Exists => True,
                          Value  => new (Subpool) String'(Value));
   end Set_Summary;

   procedure Set_Name (This    : in out Entry_Tag;
                       Value   : String;
                       Subpool : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Name := (Exists => True,
                       Value  => new (Subpool) String'(Value));
   end Set_Name;

   procedure Set_Since (This  : in out Entry_Tag;
                        Value : Version_Number) is
   begin
      This.My_Since := (Exists => True,
                        Value  => Value);
   end Set_Since;

   procedure Set_Name (This    : in out Enum_Tag;
                       Value   : String;
                       Subpool : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Name := (Exists => True,
                       Value  => new (Subpool) String'(Value));
   end Set_Name;

   procedure Set_Bitfield (This  : in out Enum_Tag;
                           Value : Boolean) is
   begin
      This.My_Bitfield := (Exists => True,
                           Value  => Value);
   end Set_Bitfield;

   procedure Set_Since (This  : in out Enum_Tag;
                        Value : Version_Number)
   is
   begin
      This.My_Since := (Exists => True,
                        Value  => Value);
   end Set_Since;

   procedure Append_Child (This  : in out Enum_Tag;
                           Item  : not null Wayland_XML.Description_Tag_Ptr)
   is
      Child : Enum_Child := (Child_Description, Item);
   begin
      This.My_Children.Append (Child);
   end Append_Child;

   procedure Append_Child (This  : in out Enum_Tag;
                           Item  : not null Entry_Tag_Ptr)
   is
      Child : Enum_Child := (Child_Entry, Item);
   begin
      This.My_Children.Append (Child);
   end Append_Child;

   procedure Set_Name (This    : in out Event_Tag;
                       Value   : String;
                       Subpool : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Name := (Exists => True,
                       Value  => new (Subpool) String'(Value));
   end Set_Name;

   procedure Set_Since_Attribute (This  : in out Event_Tag;
                                  Value : Version_Number)
   is
   begin
      This.My_Since_Attribute := (Exists => True,
                                  Value  => Value);
   end Set_Since_Attribute;

   procedure Append_Child (This  : in out Event_Tag;
                           Item  : not null Wayland_XML.Description_Tag_Ptr)
   is
      C : Event_Child := (Child_Description, Item);
   begin
      This.My_Children.Append (C);
   end Append_Child;

   procedure Append_Child (This  : in out Event_Tag;
                           Item  : not null Wayland_XML.Arg_Tag_Ptr)
   is
      C : Event_Child := (Child_Arg, Item);
   begin
      This.My_Children.Append (C);
   end Append_Child;

   procedure Set_Name (This    : in out Request_Tag;
                       Value   : String;
                       Subpool : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Name := (Exists => True,
                       Value  => new (Subpool) String'(Value));
   end Set_Name;

   procedure Append_Child (This  : in out Request_Tag;
                           Item  : not null Description_Tag_Ptr)
   is
      C : Request_Child := (Child_Description, Item);
   begin
      This.My_Children.Append (C);
   end Append_Child;

   procedure Append_Child (This  : in out Request_Tag;
                           Item  : not null Wayland_XML.Arg_Tag_Ptr)
   is
      C : Request_Child := (Child_Arg, Item);
   begin
      This.My_Children.Append (C);
   end Append_Child;

   procedure Set_Type_Attribute (This    : in out Request_Tag;
                                 Value   : String;
                                 Subpool : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Type_Attribute := (Exists => True,
                                 Value  => new (Subpool) String'(Value));
   end Set_Type_Attribute;

   procedure Set_Since (This  : in out Request_Tag;
                        Value : Version_Number) is
   begin
      This.My_Since := (Exists => True,
                        Value  => Value);
   end Set_Since;

   function Exists_Description (This : Request_Tag) return Boolean is
      N : Aida.Nat32_T := 0;
   begin
      for Child of This.Children loop
         if
           Child.Kind_Id = Child_Description and then
           Child.Description_Tag.Exists_Text
         then
            N := N + 1;
         end if;
      end loop;

      return N = 1;
   end Exists_Description;

   function Description (This : Request_Tag) return String is
      C : Request_Child;
   begin
      for Child of This.Children loop
         if Child.Kind_Id = Child_Description then
            C := Child;
            exit;
         end if;
      end loop;

      return C.Description_Tag.Text;
   end Description;

   procedure Set_Name (This    : in out Interface_Tag;
                       Value   : String;
                       Subpool : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Name := (Exists => True,
                       Value  => new (Subpool) String'(Value));
   end Set_Name;

   procedure Set_Version (This  : in out Interface_Tag;
                          Value : Version_Number)
   is
   begin
      This.My_Version := (Exists => True,
                          Value  => Value);
   end Set_Version;

   procedure Append_Child (This  : in out Interface_Tag;
                           Item  : not null Description_Tag_Ptr)
   is
      C : Interface_Child := (Child_Description, Item);
   begin
      This.My_Children.Append (C);
   end Append_Child;

   procedure Append_Child (This  : in out Interface_Tag;
                           Item  : not null Request_Tag_Ptr)
   is
      C : Interface_Child := (Child_Request, Item);
   begin
      This.My_Children.Append (C);
   end Append_Child;

   procedure Append_Child (This  : in out Interface_Tag;
                           Item  : not null Event_Tag_Ptr)
   is
      C : Interface_Child := (Child_Event, Item);
   begin
      This.My_Children.Append (C);
   end Append_Child;

   procedure Append_Child (This  : in out Interface_Tag;
                           Item  : not null Enum_Tag_Ptr)
   is
      C : Interface_Child := (Child_Enum, Item);
   begin
      This.My_Children.Append (C);
   end Append_Child;

   procedure Set_Name (This    : in out Protocol_Tag;
                       Value   : String;
                       Subpool : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Name := (Exists => True,
                       Value  => new (Subpool) String'(Value));
   end Set_Name;

   procedure Append_Child (This  : in out Protocol_Tag;
                           Item  : not null Wayland_XML.Copyright_Ptr)
   is
      C : Protocol_Child := (Kind_Id       => Child_Copyright,
                             Copyright_Tag => Item);
   begin
      This.My_Children.Append (C);
   end Append_Child;

   procedure Append_Child (This  : in out Protocol_Tag;
                           Item  : not null Interface_Tag_Ptr)
   is
      C : Protocol_Child := (Kind_Id       => Child_Interface,
                             Interface_Tag => Item);
   begin
      This.My_Children.Append (C);
   end Append_Child;

end Wayland_XML;
