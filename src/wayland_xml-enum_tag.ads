with Wayland_XML.Entry_Tag;
with Wayland_XML.Description_Tag;

with Ada.Containers.Vectors;

package Wayland_XML.Enum_Tag is

   type Child_Kind_Id_T is (
                            Child_Dummy,
                            Child_Description,
                            Child_Entry
                           );

   type Child_T (Kind_Id : Child_Kind_Id_T := Child_Dummy) is record
      case Kind_Id is
         when Child_Dummy       => Dummy           : not null String_Ptr := Empty_String'Access;
         when Child_Description => Description_Tag : not null Wayland_XML.Description_Tag.Description_Tag_Ptr;
         when Child_Entry       => Entry_Tag       : not null Wayland_XML.Entry_Tag.Entry_Tag_Ptr;
      end case;
   end record;

   package Child_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                        Element_Type => Child_T,
                                                        "="          => "=");

   type Children_Ref (E : not null access constant Child_Vectors.Vector) is limited null record with
     Implicit_Dereference => E;

   type Enum_Tag_T is tagged limited private;

   function Children (This : aliased Enum_Tag_T) return Children_Ref;

   procedure Append_Child (This : in out Enum_Tag_T;
                           Item : not null Wayland_XML.Description_Tag.Description_Tag_Ptr);

   procedure Append_Child (This : in out Enum_Tag_T;
                           Item : not null Wayland_XML.Entry_Tag.Entry_Tag_Ptr);

   procedure Set_Name (This    : in out Enum_Tag_T;
                       Value   : String;
                       Subpool : Dynamic_Pools.Subpool_Handle) with
     Global => null,
     Pre    => not This.Exists_Name,
     Post   => This.Exists_Name and This.Name = Value;

   function Name (This : Enum_Tag_T) return String with
     Global => null,
     Pre    => This.Exists_Name;

   function Exists_Name (This : Enum_Tag_T) return Boolean with
     Global => null;

   procedure Set_Bitfield (This  : in out Enum_Tag_T;
                           Value : Boolean) with
     Global => null,
     Pre    => not This.Exists_Bitfield,
     Post   => This.Exists_Bitfield and This.Bitfield = Value;

   function Bitfield (This : Enum_Tag_T) return Boolean with
     Global => null,
     Pre    => This.Exists_Bitfield;

   function Exists_Bitfield (This : Enum_Tag_T) return Boolean with
     Global => null;

   procedure Set_Since (This  : in out Enum_Tag_T;
                        Value : Version_T) with
     Global => null,
     Pre    => not This.Exists_Since,
     Post   => This.Exists_Since and This.Since = Value;

   function Since (This : Enum_Tag_T) return Version_T with
     Global => null,
     Pre    => This.Exists_Since;

   function Exists_Since (This : Enum_Tag_T) return Boolean with
     Global => null;

   type Enum_Tag_Ptr is access all Enum_Tag_T with Storage_Pool => Default_Subpool;

private

   type Enum_Tag_T is tagged limited record
      My_Name     : Nullable_String_Ptr;
      My_Bitfield : Nullable_Boolean_T;
      My_Since    : Nullable_Version_T;
      My_Children : aliased Child_Vectors.Vector;
   end record;

   function Name (This : Enum_Tag_T) return String is (This.My_Name.Value.all);

   function Exists_Name (This : Enum_Tag_T) return Boolean is (This.My_Name.Exists);

   function Bitfield (This : Enum_Tag_T) return Boolean is (This.My_Bitfield.Value);

   function Exists_Bitfield (This : Enum_Tag_T) return Boolean is (This.My_Bitfield.Exists);

   function Since (This : Enum_Tag_T) return Version_T is (This.My_Since.Value);

   function Exists_Since (This : Enum_Tag_T) return Boolean is (This.My_Since.Exists);

   function Children (This : aliased Enum_Tag_T) return Children_Ref is ((E => This.My_Children'Access));

end Wayland_XML.Enum_Tag;
