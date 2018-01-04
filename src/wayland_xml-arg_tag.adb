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

end Wayland_XML.Arg_Tag;
