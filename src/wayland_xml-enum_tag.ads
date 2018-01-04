package Wayland_XML.Enum_Tag is

   type Enum_Tag_T is tagged limited private;

   procedure Set_Name (This    : in out Enum_Tag_T;
                       Value   : Aida.String_T;
                       Subpool : Dynamic_Pools.Subpool_Handle) with
     Global => null,
     Pre    => not This.Exists_Name,
     Post   => This.Exists_Name and This.Name = Value;

   function Name (This : Enum_Tag_T) return Aida.String_T with
     Global => null,
     Pre    => This.Exists_Name;

   function Exists_Name (This : Enum_Tag_T) return Boolean with
     Global => null;

   type Enum_Tag_Ptr is access all Enum_Tag_T with Storage_Pool => Default_Subpool;

private

   type Enum_Tag_T is tagged limited record
      My_Name : Nullable_String_Ptr;
   end record;

   function Name (This : Enum_Tag_T) return Aida.String_T is (This.My_Name.Value.all);

   function Exists_Name (This : Enum_Tag_T) return Boolean is (This.My_Name.Exists);

end Wayland_XML.Enum_Tag;
