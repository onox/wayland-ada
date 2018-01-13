package body Wayland_XML.Description_Tag is

   procedure Set_Text (This    : in out Description_Tag_T;
                       Value   : String;
                       Subpool : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Text := (Exists => True,
                       Value  => new (Subpool) String'(Value));
   end Set_Text;

   procedure Set_Summary (This    : in out Description_Tag_T;
                          Value   : String;
                          Subpool : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Summary := (Exists => True,
                          Value  => new (Subpool) String'(Value));
   end Set_Summary;

end Wayland_XML.Description_Tag;
