package Wayland_XML.Arg_Tag is

   type Arg_Tag_T is tagged limited private;

   procedure Set_Name (This    : in out Arg_Tag_T;
                       Value   : Aida.String_T;
                       Subpool : Dynamic_Pools.Subpool_Handle) with
     Global => null,
     Pre    => not This.Exists_Name,
     Post   => This.Exists_Name and This.Name = Value;

   function Name (This : Arg_Tag_T) return Aida.String_T with
     Global => null,
     Pre    => This.Exists_Name;

   function Exists_Name (This : Arg_Tag_T) return Boolean with
     Global => null;

   procedure Set_Type_Attribute (This    : in out Arg_Tag_T;
                                 Value   : Aida.String_T;
                                 Subpool : Dynamic_Pools.Subpool_Handle) with
     Global => null,
     Pre    => not This.Exists_Type_Attribute,
     Post   => This.Exists_Type_Attribute and This.Type_Attribute = Value;

   function Type_Attribute (This : Arg_Tag_T) return Aida.String_T with
     Global => null,
     Pre    => This.Exists_Type_Attribute;

   function Exists_Type_Attribute (This : Arg_Tag_T) return Boolean with
     Global => null;

   procedure Set_Interface_Attribute (This    : in out Arg_Tag_T;
                                      Value   : Aida.String_T;
                                      Subpool : Dynamic_Pools.Subpool_Handle) with
     Global => null,
     Pre    => not This.Exists_Interface_Attribute,
     Post   => This.Exists_Interface_Attribute and This.Interface_Attribute = Value;

   function Interface_Attribute (This : Arg_Tag_T) return Aida.String_T with
     Global => null,
     Pre    => This.Exists_Interface_Attribute;

   function Exists_Interface_Attribute (This : Arg_Tag_T) return Boolean with
     Global => null;

   procedure Set_Summary (This    : in out Arg_Tag_T;
                          Value   : Aida.String_T;
                          Subpool : Dynamic_Pools.Subpool_Handle) with
     Global => null,
     Pre    => not This.Exists_Summary,
     Post   => This.Exists_Summary and This.Summary = Value;

   function Summary (This : Arg_Tag_T) return Aida.String_T with
     Global => null,
     Pre    => This.Exists_Summary;

   function Exists_Summary (This : Arg_Tag_T) return Boolean with
     Global => null;

   type Arg_Tag_Ptr is access all Arg_Tag_T with Storage_Pool => Default_Subpool;

private

   type Arg_Tag_T is tagged limited record
      My_Name                : Nullable_String_Ptr;
      My_Type_Attribute      : Nullable_String_Ptr;
      My_Interface_Attribute : Nullable_String_Ptr;
      My_Summary             : Nullable_String_Ptr;
   end record;

   function Name (This : Arg_Tag_T) return Aida.String_T is (This.My_Name.Value.all);

   function Exists_Name (This : Arg_Tag_T) return Boolean is (This.My_Name.Exists);

   function Type_Attribute (This : Arg_Tag_T) return Aida.String_T is (This.My_Type_Attribute.Value.all);

   function Exists_Type_Attribute (This : Arg_Tag_T) return Boolean is (This.My_Type_Attribute.Exists);

   function Interface_Attribute (This : Arg_Tag_T) return Aida.String_T is (This.My_Interface_Attribute.Value.all);

   function Exists_Interface_Attribute (This : Arg_Tag_T) return Boolean is (This.My_Interface_Attribute.Exists);

   function Summary (This : Arg_Tag_T) return Aida.String_T is (This.My_Summary.Value.all);

   function Exists_Summary (This : Arg_Tag_T) return Boolean is (This.My_Summary.Exists);

end Wayland_XML.Arg_Tag;
