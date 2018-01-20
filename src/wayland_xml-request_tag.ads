with Wayland_XML.Description_Tag;
with Wayland_XML.Arg_Tag;

with Ada.Containers.Vectors;

package Wayland_XML.Request_Tag is

   type Child_Kind_Id_T is (
                            Child_Dummy,
                            Child_Description,
                            Child_Arg
                           );

   type Child_T (Kind_Id : Child_Kind_Id_T := Child_Dummy) is record
      case Kind_Id is
         when Child_Dummy       => Dummy           : not null String_Ptr := Empty_String'Access;
         when Child_Description => Description_Tag : not null Wayland_XML.Description_Tag.Description_Tag_Ptr;
         when Child_Arg         => Arg_Tag         : not null Wayland_XML.Arg_Tag.Arg_Tag_Ptr;
      end case;
   end record;

   package Child_Vectors is new Ada.Containers.Vectors (Index_Type   => Aida.Pos32_T,
                                                        Element_Type => Child_T,
                                                        "="          => "=");

   type Children_Ref (E : not null access constant Child_Vectors.Vector) is limited null record with
     Implicit_Dereference => E;

   type Request_Tag_T is tagged limited private;

   function Children (This : aliased Request_Tag_T) return Children_Ref;

   procedure Append_Child (This : in out Request_Tag_T;
                           Item : not null Wayland_XML.Description_Tag.Description_Tag_Ptr);

   procedure Append_Child (This : in out Request_Tag_T;
                           Item : not null Wayland_XML.Arg_Tag.Arg_Tag_Ptr);

   procedure Set_Name (This    : in out Request_Tag_T;
                       Value   : String;
                       Subpool : Dynamic_Pools.Subpool_Handle) with
     Global         => null,
       Pre'Class    => not This.Exists_Name,
         Post'Class => This.Exists_Name and This.Name = Value;

   function Name (This : Request_Tag_T) return String with
     Global => null,
     Pre    => This.Exists_Name;

   function Exists_Name (This : Request_Tag_T) return Boolean with
     Global => null;

   procedure Set_Type_Attribute (This    : in out Request_Tag_T;
                                 Value   : String;
                                 Subpool : Dynamic_Pools.Subpool_Handle) with
     Global => null,
     Pre    => not This.Exists_Type_Attribute,
     Post   => This.Exists_Type_Attribute and This.Type_Attribute = Value;

   function Type_Attribute (This : Request_Tag_T) return String with
     Global => null,
     Pre    => This.Exists_Type_Attribute;

   function Exists_Type_Attribute (This : Request_Tag_T) return Boolean with
     Global => null;

   procedure Set_Since (This  : in out Request_Tag_T;
                        Value : Version_T) with
     Global => null,
     Pre    => not This.Exists_Since,
     Post   => This.Exists_Since and This.Since = Value;

   function Since (This : Request_Tag_T) return Version_T with
     Global => null,
     Pre    => This.Exists_Since;

   function Since_As_Pos32 (This : Request_Tag_T) return Aida.Pos32_T with
     Global => null,
     Pre    => This.Exists_Since;

   function Exists_Since (This : Request_Tag_T) return Boolean with
     Global => null;

   type Request_Tag_Ptr is access all Request_Tag_T with Storage_Pool => Default_Subpool;

private

   type Nullable_Since_T (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : Version_T;
         when False => null;
      end case;
   end record;

   type Request_Tag_T is tagged limited record
      My_Name           : Nullable_String_Ptr;
      My_Children       : aliased Child_Vectors.Vector;
      My_Type_Attribute : Nullable_String_Ptr;
      My_Since          : Nullable_Since_T;
   end record;

   function Name (This : Request_Tag_T) return String is (This.My_Name.Value.all);

   function Exists_Name (This : Request_Tag_T) return Boolean is (This.My_Name.Exists);

   function Children (This : aliased Request_Tag_T) return Children_Ref is ((E => This.My_Children'Access));

   function Type_Attribute (This : Request_Tag_T) return String is (This.My_Type_Attribute.Value.all);

   function Exists_Type_Attribute (This : Request_Tag_T) return Boolean is (This.My_Type_Attribute.Exists);

   function Since (This : Request_Tag_T) return Version_T is (This.My_Since.Value);

   function Since_As_Pos32 (This : Request_Tag_T) return Aida.Pos32_T is (Aida.Pos32_T (This.My_Since.Value));

   function Exists_Since (This : Request_Tag_T) return Boolean is (This.My_Since.Exists);

end Wayland_XML.Request_Tag;
