with Wayland_XML.Description_Tag;
with Wayland_XML.Arg_Tag;

with Ada.Containers.Vectors;

package Wayland_XML.Event_Tag is

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

   package Child_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                        Element_Type => Child_T,
                                                        "="          => "=");

   type Children_Ref (E : not null access constant Child_Vectors.Vector) is limited null record with
     Implicit_Dereference => E;

   type Event_Tag_T is tagged limited private;

   function Children (This : aliased Event_Tag_T) return Children_Ref;

   procedure Append_Child (This : in out Event_Tag_T;
                           Item : not null Wayland_XML.Description_Tag.Description_Tag_Ptr);

   procedure Append_Child (This : in out Event_Tag_T;
                           Item : not null Wayland_XML.Arg_Tag.Arg_Tag_Ptr);

   procedure Set_Name (This    : in out Event_Tag_T;
                       Value   : Aida.String_T;
                       Subpool : Dynamic_Pools.Subpool_Handle) with
     Global => null,
     Pre    => not This.Exists_Name,
     Post   => This.Exists_Name and This.Name = Value;

   function Name (This : Event_Tag_T) return Aida.String_T with
     Global => null,
     Pre    => This.Exists_Name;

   function Exists_Name (This : Event_Tag_T) return Boolean with
     Global => null;

   procedure Set_Since_Attribute (This  : in out Event_Tag_T;
                                  Value : Version_T) with
     Global => null,
     Pre    => not This.Exists_Since_Attribute,
     Post   => This.Exists_Since_Attribute and This.Since_Attribute = Value;

   function Since_Attribute (This : Event_Tag_T) return Version_T with
     Global => null,
     Pre    => This.Exists_Since_Attribute;

   function Exists_Since_Attribute (This : Event_Tag_T) return Boolean with
     Global => null;

   type Event_Tag_Ptr is access all Event_Tag_T with Storage_Pool => Default_Subpool;

private

   type Nullable_Since_Attribute_T (Exists : Boolean := False) is record
      case Exists is
         when True => Value : Version_T;
         when False => null;
      end case;
   end record;

   type Event_Tag_T is tagged limited record
      My_Name            : Nullable_String_Ptr;
      My_Since_Attribute : Nullable_Since_Attribute_T;
      My_Children        : aliased Child_Vectors.Vector;
   end record;

   function Name (This : Event_Tag_T) return Aida.String_T is (This.My_Name.Value.all);

   function Exists_Name (This : Event_Tag_T) return Boolean is (This.My_Name.Exists);

   function Since_Attribute (This : Event_Tag_T) return Version_T is (This.My_Since_Attribute.Value);

   function Exists_Since_Attribute (This : Event_Tag_T) return Boolean is (This.My_Since_Attribute.Exists);

   function Children (This : aliased Event_Tag_T) return Children_Ref is ((E => This.My_Children'Access));

end Wayland_XML.Event_Tag;
