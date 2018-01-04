with Ada.Containers.Vectors;

package Wayland_XML.Protocol_Tag is

   type Child_Kind_Id_T is (
                            Child_Dummy
                           );

   type Child_T (Kind_Id : Child_Kind_Id_T := Child_Dummy) is record
      case Kind_Id is
         when Child_Dummy => Dummy : not null String_Ptr := Empty_String'Access;
      end case;
   end record;

   package Child_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                        Element_Type => Child_T,
                                                        "="          => "=");

   type Children_Ref (E : not null access constant Child_Vectors.Vector) is limited null record with
     Implicit_Dereference => E;

   type Protocol_Tag_T is tagged limited private;

   procedure Set_Name (This    : in out Protocol_Tag_T;
                       Value   : Aida.String_T;
                       Subpool : Dynamic_Pools.Subpool_Handle) with
     Global => null,
     Pre    => not This.Exists_Name,
     Post   => This.Exists_Name and This.Name = Value;

   function Name (This : Protocol_Tag_T) return Aida.String_T with
     Global => null,
     Pre    => This.Exists_Name;

   function Exists_Name (This : Protocol_Tag_T) return Boolean with
     Global => null;

   function Children (This : aliased Protocol_Tag_T) return Children_Ref;

   procedure Append_Child (This : in out Protocol_Tag_T;
                           Item : Child_T);

   type Protocol_Tag_Ptr is access all Protocol_Tag_T with Storage_Pool => Default_Subpool;

private

   type Protocol_Tag_T is tagged limited record
      My_Name     : Nullable_String_Ptr;
      My_Children : aliased Child_Vectors.Vector;
   end record;

   function Name (This : Protocol_Tag_T) return Aida.String_T is (This.My_Name.Value.all);

   function Exists_Name (This : Protocol_Tag_T) return Boolean is (This.My_Name.Exists);

   function Children (This : aliased Protocol_Tag_T) return Children_Ref is ((E => This.My_Children'Access));

end Wayland_XML.Protocol_Tag;
