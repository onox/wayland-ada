package body Wayland_XML.Arg_Tag is

   procedure Set_Summary (This    : in out Arg_Tag_T;
                          Value   : Aida.String_T;
                          Subpool : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Summary := (Exists => True,
                          Value  => new (Subpool) Aida.String_T'(Value));
   end Set_Summary;

   procedure Set_Interface_Attribute (This    : in out Arg_Tag_T;
                                      Value   : Aida.String_T;
                                      Subpool : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Interface_Attribute := (Exists => True,
                                      Value  => new (Subpool) Aida.String_T'(Value));
   end Set_Interface_Attribute;

   procedure Set_Type_Attribute (This    : in out Arg_Tag_T;
                                 Value   : Aida.String_T;
                                 Subpool : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Type_Attribute := (Exists => True,
                                 Value  => new (Subpool) Aida.String_T'(Value));
   end Set_Type_Attribute;

   procedure Set_Name (This    : in out Arg_Tag_T;
                       Value   : Aida.String_T;
                       Subpool : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Name := (Exists => True,
                       Value  => new (Subpool) Aida.String_T'(Value));
   end Set_Name;

   procedure Set_Allow_Null (This  : in out Arg_Tag_T;
                             Value : Boolean)
   is
   begin
      This.My_Allow_Null := (Exists => True,
                             Value  => Value);
   end Set_Allow_Null;

   procedure Set_Enum (This    : in out Arg_Tag_T;
                       Value   : Aida.String_T;
                       Subpool : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Enum := (Exists => True,
                       Value  => new (Subpool) Aida.String_T'(Value));
   end Set_Enum;

end Wayland_XML.Arg_Tag;
