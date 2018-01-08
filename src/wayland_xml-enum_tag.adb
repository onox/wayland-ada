package body Wayland_XML.Enum_Tag is

   procedure Set_Name (This    : in out Enum_Tag_T;
                       Value   : Aida.String_T;
                       Subpool : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Name := (Exists => True,
                       Value  => new (Subpool) Aida.String_T'(Value));
   end Set_Name;

   procedure Set_Bitfield (This  : in out Enum_Tag_T;
                           Value : Boolean) is
   begin
      This.My_Bitfield := (Exists => True,
                           Value  => Value);
   end Set_Bitfield;

   procedure Set_Since (This  : in out Enum_Tag_T;
                        Value : Version_T)
   is
   begin
      This.My_Since := (Exists => True,
                        Value  => Value);
   end Set_Since;

   procedure Append_Child (This  : in out Enum_Tag_T;
                           Item  : not null Wayland_XML.Description_Tag.Description_Tag_Ptr)
   is
      Child : Child_T := (Child_Description, Item);
   begin
      This.My_Children.Append (Child);
   end Append_Child;

   procedure Append_Child (This  : in out Enum_Tag_T;
                           Item  : not null Wayland_XML.Entry_Tag.Entry_Tag_Ptr)
   is
      Child : Child_T := (Child_Entry, Item);
   begin
      This.My_Children.Append (Child);
   end Append_Child;

end Wayland_XML.Enum_Tag;
