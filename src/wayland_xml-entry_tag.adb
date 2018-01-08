package body Wayland_XML.Entry_Tag is

   procedure Set_Value (This  : in out Entry_Tag_T;
                        Value : Value_T)
   is
   begin
      This.My_Value := (Exists => True,
                        Value  => Value);
   end Set_Value;

   procedure Set_Summary (This    : in out Entry_Tag_T;
                          Value   : Aida.String_T;
                          Subpool : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Summary := (Exists => True,
                          Value  => new (Subpool) Aida.String_T'(Value));
   end Set_Summary;

   procedure Set_Name (This    : in out Entry_Tag_T;
                       Value   : Aida.String_T;
                       Subpool : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Name := (Exists => True,
                       Value  => new (Subpool) Aida.String_T'(Value));
   end Set_Name;

   procedure Set_Since (This  : in out Entry_Tag_T;
                        Value : Version_T) is
   begin
      This.My_Since := (Exists => True,
                        Value  => Value);
   end Set_Since;

end Wayland_XML.Entry_Tag;
