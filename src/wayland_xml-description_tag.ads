package Wayland_XML.Description_Tag is

   type Description_Tag_T is tagged limited private;

   procedure Set_Text (This    : in out Description_Tag_T;
                       Value   : String;
                       Subpool : Dynamic_Pools.Subpool_Handle) with
     Global => null,
     Pre    => not This.Exists_Text,
     Post   => This.Exists_Text and This.Text = Value;

   function Text (This : Description_Tag_T) return String with
     Global => null,
     Pre    => This.Exists_Text;

   function Exists_Text (This : Description_Tag_T) return Boolean with
     Global => null;

   procedure Set_Summary (This    : in out Description_Tag_T;
                          Value   : String;
                          Subpool : Dynamic_Pools.Subpool_Handle) with
     Global => null,
     Pre    => not This.Exists_Summary,
     Post   => This.Exists_Summary and This.Summary = Value;

   function Summary (This : Description_Tag_T) return String with
     Global => null,
     Pre    => This.Exists_Summary;

   function Exists_Summary (This : Description_Tag_T) return Boolean with
     Global => null;

   type Description_Tag_Ptr is access all Description_Tag_T with Storage_Pool => Default_Subpool;

private

   type Description_Tag_T is tagged limited record
      My_Text    : Nullable_String_Ptr;
      My_Summary : Nullable_String_Ptr;
   end record;

   function Text (This : Description_Tag_T) return String is (This.My_Text.Value.all);

   function Exists_Text (This : Description_Tag_T) return Boolean is (This.My_Text.Exists);

   function Summary (This : Description_Tag_T) return String is (This.My_Summary.Value.all);

   function Exists_Summary (This : Description_Tag_T) return Boolean is (This.My_Summary.Exists);

end Wayland_XML.Description_Tag;
