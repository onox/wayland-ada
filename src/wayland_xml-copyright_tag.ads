package Wayland_XML.Copyright_Tag is

   type Copyright_Tag_T is tagged limited private;

   procedure Set_Text (This    : in out Copyright_Tag_T;
                       Value   : String;
                       Subpool : Dynamic_Pools.Subpool_Handle) with
     Global => null,
     Pre    => not This.Exists_Text,
     Post   => This.Exists_Text and This.Text = Value;

   function Text (This : Copyright_Tag_T) return String with
     Global => null,
     Pre    => This.Exists_Text;

   function Exists_Text (This : Copyright_Tag_T) return Boolean with
     Global => null;

   type Copyright_Ptr is access all Copyright_Tag_T with Storage_Pool => Default_Subpool;

private

   type Copyright_Tag_T is tagged limited record
      My_Text : Nullable_String_Ptr;
   end record;

   function Text (This : Copyright_Tag_T) return String is (This.My_Text.Value.all);

   function Exists_Text (This : Copyright_Tag_T) return Boolean is (This.My_Text.Exists);

end Wayland_XML.Copyright_Tag;
