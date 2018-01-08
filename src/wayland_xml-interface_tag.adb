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

   procedure Append_Child (This  : in out Interface_Tag_T;
                           Item  : not null Wayland_XML.Description_Tag.Description_Tag_Ptr)
   is
      Child : Child_T := (Child_Description, Item);
   begin
      This.My_Children.Append (Child);
   end Append_Child;

   procedure Append_Child (This  : in out Interface_Tag_T;
                           Item  : not null Wayland_XML.Request_Tag.Request_Tag_Ptr)
   is
      Child : Child_T := (Child_Request, Item);
   begin
      This.My_Children.Append (Child);
   end Append_Child;

   procedure Append_Child (This  : in out Interface_Tag_T;
                           Item  : not null Wayland_XML.Event_Tag.Event_Tag_Ptr)
   is
      Child : Child_T := (Child_Event, Item);
   begin
      This.My_Children.Append (Child);
   end Append_Child;

   procedure Append_Child (This  : in out Interface_Tag_T;
                           Item  : not null Wayland_XML.Enum_Tag.Enum_Tag_Ptr)
   is
      Child : Child_T := (Child_Enum, Item);
   begin
      This.My_Children.Append (Child);
   end Append_Child;

end Wayland_XML.Interface_Tag;
