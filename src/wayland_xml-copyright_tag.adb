package body Wayland_XML.Copyright_Tag is

   procedure Set_Text (This    : in out Copyright_Tag_T;
                       Value   : String;
                       Subpool : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Text := (Exists => True,
                       Value  => new (Subpool) String'(Value));
   end Set_Text;

end Wayland_XML.Copyright_Tag;
