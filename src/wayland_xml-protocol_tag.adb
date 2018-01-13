package body Wayland_XML.Protocol_Tag is

   procedure Set_Name (This    : in out Protocol_Tag_T;
                       Value   : String;
                       Subpool : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Name := (Exists => True,
                       Value  => new (Subpool) String'(Value));
   end Set_Name;

   procedure Append_Child (This  : in out Protocol_Tag_T;
                           Item  : Child_T)
   is
   begin
      This.My_Children.Append (Item);
   end Append_Child;

   procedure Append_Child (This  : in out Protocol_Tag_T;
                           Item  : not null Wayland_XML.Copyright_Tag.Copyright_Ptr)
   is
      Child : Child_T := (Kind_Id       => Child_Copyright,
                          Copyright_Tag => Item);
   begin
      This.My_Children.Append (Child);
   end Append_Child;

   procedure Append_Child (This  : in out Protocol_Tag_T;
                           Item  : not null Wayland_XML.Interface_Tag.Interface_Tag_Ptr)
   is
      Child : Child_T := (Kind_Id       => Child_Interface,
                          Interface_Tag => Item);
   begin
      This.My_Children.Append (Child);
   end Append_Child;

end Wayland_XML.Protocol_Tag;
