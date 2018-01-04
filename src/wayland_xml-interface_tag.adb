package body Wayland_XML.Interface_Tag is

   procedure Set_Name (This    : in out Interface_Tag_T;
                       Value   : Aida.String_T;
                       Subpool : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Name := (Exists => True,
                       Value  => new (Subpool) Aida.String_T'(Value));
   end Set_Name;

   procedure Set_Version (This  : in out Interface_Tag_T;
                          Value : Version_T)
   is
   begin
      This.My_Version := (Exists => True,
                          Value  => Value);
   end Set_Version;

end Wayland_XML.Interface_Tag;
