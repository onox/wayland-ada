package Wayland_XML.Entry_Tag is

   type Value_T is new Aida.Nat32_T;

   type Entry_Tag_T is tagged limited private;

   procedure Set_Name (This  : in out Entry_Tag_T;
                       Value   : Aida.String_T;
                       Subpool : Dynamic_Pools.Subpool_Handle) with
     Global => null,
     Pre    => not This.Exists_Name,
     Post   => This.Exists_Name and This.Name = Value;

   function Name (This : Entry_Tag_T) return Aida.String_T with
     Global => null,
     Pre    => This.Exists_Name;

   function Exists_Name (This : Entry_Tag_T) return Boolean with
     Global => null;

   procedure Set_Summary (This  : in out Entry_Tag_T;
                          Value   : Aida.String_T;
                          Subpool : Dynamic_Pools.Subpool_Handle) with
     Global => null,
     Pre    => not This.Exists_Summary,
     Post   => This.Exists_Summary and This.Summary = Value;

   function Summary (This : Entry_Tag_T) return Aida.String_T with
     Global => null,
     Pre    => This.Exists_Summary;

   function Exists_Summary (This : Entry_Tag_T) return Boolean with
     Global => null;

   procedure Set_Value (This  : in out Entry_Tag_T;
                        Value : Value_T) with
     Global => null,
     Pre    => not This.Exists_Value,
     Post   => This.Exists_Value and This.Value = Value;

   function Value (This : Entry_Tag_T) return Value_T with
     Global => null,
     Pre    => This.Exists_Value;

   function Exists_Value (This : Entry_Tag_T) return Boolean with
     Global => null;


   procedure Set_Since (This  : in out Entry_Tag_T;
                        Value : Version_T) with
     Global => null,
     Pre    => not This.Exists_Since,
     Post   => This.Exists_Since and This.Since = Value;

   function Since (This : Entry_Tag_T) return Version_T with
     Global => null,
     Pre    => This.Exists_Since;

   function Exists_Since (This : Entry_Tag_T) return Boolean with
     Global => null;

   type Entry_Tag_Ptr is access all Entry_Tag_T with Storage_Pool => Default_Subpool;

private

   type Nullable_Value_T (Exists : Boolean := False) is record
      case Exists is
      when True => Value : Value_T;
         when False => null;
      end case;
   end record;

   type Entry_Tag_T is tagged limited record
      My_Name    : Nullable_String_Ptr;
      My_Value   : Nullable_Value_T;
      My_Summary : Nullable_String_Ptr;
      My_Since   : Nullable_Version_T;
   end record;

   function Name (This : Entry_Tag_T) return Aida.String_T is (This.My_Name.Value.all);

   function Exists_Name (This : Entry_Tag_T) return Boolean is (This.My_Name.Exists);

   function Value (This : Entry_Tag_T) return Value_T is (This.My_Value.Value);

   function Exists_Value (This : Entry_Tag_T) return Boolean is (This.My_Value.Exists);

   function Summary (This : Entry_Tag_T) return Aida.String_T is (This.My_Summary.Value.all);

   function Exists_Summary (This : Entry_Tag_T) return Boolean is (This.My_Summary.Exists);

   function Since (This : Entry_Tag_T) return Version_T is (This.My_Since.Value);

   function Exists_Since (This : Entry_Tag_T) return Boolean is (This.My_Since.Exists);

end Wayland_XML.Entry_Tag;
