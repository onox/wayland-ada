package body Wayland_XML.Event_Tag is

   procedure Set_Name (This    : in out Event_Tag_T;
                       Value   : Aida.String_T;
                       Subpool : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Name := (Exists => True,
                       Value  => new (Subpool) Aida.String_T'(Value));
   end Set_Name;

   procedure Set_Since_Attribute (This  : in out Event_Tag_T;
                                  Value : Version_T)
   is
   begin
      This.My_Since_Attribute := (Exists => True,
                                  Value  => Value);
   end Set_Since_Attribute;

   procedure Append_Child (This  : in out Event_Tag_T;
                           Item  : not null Wayland_XML.Description_Tag.Description_Tag_Ptr)
   is
      Child : Child_T := (Child_Description, Item);
   begin
      This.My_Children.Append (Child);
   end Append_Child;

   procedure Append_Child (This  : in out Event_Tag_T;
                           Item  : not null Wayland_XML.Arg_Tag.Arg_Tag_Ptr)
   is
      Child : Child_T := (Child_Arg, Item);
   begin
      This.My_Children.Append (Child);
   end Append_Child;

end Wayland_XML.Event_Tag;
