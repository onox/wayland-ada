package Wayland_XML.Arg_Tag is

   type Type_Attribute_T is (
                             Type_Integer,
                             Type_Unsigned_Integer,
                             Type_String,
                             Type_FD,
                             Type_New_Id,
                             Type_Object,
                             Type_Fixed,
                             Type_Array
                             );

   TYPE_ATTRIBUTE_EXCEPTION : exception;

   type Arg_Tag_T is tagged limited private;

   procedure Set_Name (This    : in out Arg_Tag_T;
                       Value   : String;
                       Subpool : Dynamic_Pools.Subpool_Handle) with
     Global => null,
     Pre    => not This.Exists_Name,
     Post   => This.Exists_Name and This.Name = Value;

   function Name (This : Arg_Tag_T) return String with
     Global => null,
     Pre    => This.Exists_Name;

   function Exists_Name (This : Arg_Tag_T) return Boolean with
     Global => null;

   -- raises TYPE_ATTRIBUTE_EXCEPTION if Value cannot be interpreted
   procedure Set_Type_Attribute (This    : in out Arg_Tag_T;
                                 Value   : String) with
     Global => null,
     Pre    => not This.Exists_Type_Attribute,
     Post   => This.Exists_Type_Attribute;

   function Type_Attribute (This : Arg_Tag_T) return Type_Attribute_T with
     Global => null,
     Pre    => This.Exists_Type_Attribute;

   function Exists_Type_Attribute (This : Arg_Tag_T) return Boolean with
     Global => null;

   procedure Set_Interface_Attribute (This    : in out Arg_Tag_T;
                                      Value   : String;
                                      Subpool : Dynamic_Pools.Subpool_Handle) with
     Global => null,
     Pre    => not This.Exists_Interface_Attribute,
     Post   => This.Exists_Interface_Attribute and This.Interface_Attribute = Value;

   function Interface_Attribute (This : Arg_Tag_T) return String with
     Global => null,
     Pre    => This.Exists_Interface_Attribute;

   function Exists_Interface_Attribute (This : Arg_Tag_T) return Boolean with
     Global => null;

   procedure Set_Summary (This    : in out Arg_Tag_T;
                          Value   : String;
                          Subpool : Dynamic_Pools.Subpool_Handle) with
     Global => null,
     Pre    => not This.Exists_Summary,
     Post   => This.Exists_Summary and This.Summary = Value;

   function Summary (This : Arg_Tag_T) return String with
     Global => null,
     Pre    => This.Exists_Summary;

   function Exists_Summary (This : Arg_Tag_T) return Boolean with
     Global => null;

   procedure Set_Allow_Null (This  : in out Arg_Tag_T;
                             Value : Boolean) with
     Global => null,
     Pre    => not This.Exists_Allow_Null,
     Post   => This.Exists_Allow_Null and This.Allow_Null = Value;

   function Allow_Null (This : Arg_Tag_T) return Boolean with
     Global => null,
     Pre    => This.Exists_Allow_Null;

   function Exists_Allow_Null (This : Arg_Tag_T) return Boolean with
     Global => null;

   procedure Set_Enum (This    : in out Arg_Tag_T;
                       Value   : String;
                       Subpool : Dynamic_Pools.Subpool_Handle) with
     Global => null,
     Pre    => not This.Exists_Enum,
     Post   => This.Exists_Enum and This.Enum = Value;

   function Enum (This : Arg_Tag_T) return String with
     Global => null,
     Pre    => This.Exists_Enum;

   function Exists_Enum (This : Arg_Tag_T) return Boolean with
     Global => null;

   type Arg_Tag_Ptr is access all Arg_Tag_T with Storage_Pool => Default_Subpool;

private

   type Nullable_Allow_Null_T (Exists : Boolean := False) is record
      case Exists is
         when True => Value : Boolean;
         when False => null;
      end case;
   end record;

   type Nullable_Type_Attribute_T (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : Type_Attribute_T;
         when False => null;
      end case;
   end record;


   type Arg_Tag_T is tagged limited record
      My_Name                : Nullable_String_Ptr;
      My_Type_Attribute      : Nullable_Type_Attribute_T;
      My_Interface_Attribute : Nullable_String_Ptr;
      My_Summary             : Nullable_String_Ptr;
      My_Allow_Null          : Nullable_Allow_Null_T;
      My_Enum                : Nullable_String_Ptr;
   end record;

   function Name (This : Arg_Tag_T) return String is (This.My_Name.Value.all);

   function Exists_Name (This : Arg_Tag_T) return Boolean is (This.My_Name.Exists);

   function Type_Attribute (This : Arg_Tag_T) return Type_Attribute_T is (This.My_Type_Attribute.Value);

   function Exists_Type_Attribute (This : Arg_Tag_T) return Boolean is (This.My_Type_Attribute.Exists);

   function Interface_Attribute (This : Arg_Tag_T) return String is (This.My_Interface_Attribute.Value.all);

   function Exists_Interface_Attribute (This : Arg_Tag_T) return Boolean is (This.My_Interface_Attribute.Exists);

   function Summary (This : Arg_Tag_T) return String is (This.My_Summary.Value.all);

   function Exists_Summary (This : Arg_Tag_T) return Boolean is (This.My_Summary.Exists);

   function Allow_Null (This : Arg_Tag_T) return Boolean is (This.My_Allow_Null.Value);

   function Exists_Allow_Null (This : Arg_Tag_T) return Boolean is (This.My_Allow_Null.Exists);

   function Enum (This : Arg_Tag_T) return String is (This.My_Enum.Value.all);

   function Exists_Enum (This : Arg_Tag_T) return Boolean is (This.My_Enum.Exists);

end Wayland_XML.Arg_Tag;
