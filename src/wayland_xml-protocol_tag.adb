package body Wayland_XML.Protocol_Tag is

   procedure Set_Name (This    : in out Protocol_Tag_T;
                       Value   : Aida.String_T;
                       Subpool : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Name := (Exists => True,
                       Value  => new (Subpool) Aida.String_T'(Value));
   end Set_Name;

   procedure Append_Child (This  : in out Protocol_Tag_T;
                           Item  : Child_T)
   is
   begin
      This.My_Children.Append (Item);
   end Append_Child;

end Wayland_XML.Protocol_Tag;
