with Ada.Containers.Vectors;

with Wayland_XML.Description_Tag;
with Wayland_XML.Request_Tag;
with Wayland_XML.Event_Tag;
with Wayland_XML.Enum_Tag;

package Wayland_XML.Interface_Tag is

   type Child_Kind_Id_T is (
                            Child_Dummy,
                            Child_Description,
                            Child_Request,
                            Child_Event,
                            Child_Enum
                           );

   type Child_T (Kind_Id : Child_Kind_Id_T := Child_Dummy) is record
      case Kind_Id is
         when Child_Dummy       => Dummy           : not null String_Ptr := Empty_String'Access;
         when Child_Description => Description_Tag : not null Wayland_XML.Description_Tag.Description_Tag_Ptr;
         when Child_Request     => Request_Tag     : not null Wayland_XML.Request_Tag.Request_Tag_Ptr;
         when Child_Event       => Event_Tag       : not null Wayland_XML.Event_Tag.Event_Tag_Ptr;
         when Child_Enum        => Enum_Tag        : not null Wayland_XML.Enum_Tag.Enum_Tag_Ptr;
      end case;
   end record;

   package Child_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                        Element_Type => Child_T,
                                                        "="          => "=");

   type Children_Ref (E : not null access constant Child_Vectors.Vector) is limited null record with
     Implicit_Dereference => E;

   type Interface_Tag_T is tagged limited private;

   procedure Set_Name (This    : in out Interface_Tag_T;
                       Value   : String;
                       Subpool : Dynamic_Pools.Subpool_Handle) with
     Global => null,
     Pre    => not This.Exists_Name,
     Post   => This.Exists_Name and This.Name = Value;

   function Name (This : Interface_Tag_T) return String with
     Global => null,
     Pre    => This.Exists_Name;

   function Exists_Name (This : Interface_Tag_T) return Boolean with
     Global => null;

   procedure Set_Version (This  : in out Interface_Tag_T;
                          Value : Version_T) with
     Global => null,
     Pre    => not This.Exists_Version,
     Post   => This.Exists_Version and This.Version = Value;

   function Version (This : Interface_Tag_T) return Version_T with
     Global => null,
     Pre    => This.Exists_Version;

   function Exists_Version (This : Interface_Tag_T) return Boolean with
     Global => null;

   function Children (This : aliased Interface_Tag_T) return Children_Ref;

   procedure Append_Child (This : in out Interface_Tag_T;
                           Item : not null Wayland_XML.Description_Tag.Description_Tag_Ptr);

   procedure Append_Child (This : in out Interface_Tag_T;
                           Item : not null Wayland_XML.Request_Tag.Request_Tag_Ptr);

   procedure Append_Child (This : in out Interface_Tag_T;
                           Item : not null Wayland_XML.Event_Tag.Event_Tag_Ptr);

   procedure Append_Child (This : in out Interface_Tag_T;
                           Item : not null Wayland_XML.Enum_Tag.Enum_Tag_Ptr);

   type Interface_Tag_Ptr is access all Interface_Tag_T with Storage_Pool => Default_Subpool;

private

   type Nullable_Version_T (Exists : Boolean := False) is record
      case Exists is
         when True => Value : Version_T;
         when False => null;
      end case;
   end record;

   type Interface_Tag_T is tagged limited record
      My_Name     : Nullable_String_Ptr;
      My_Version  : Nullable_Version_T;
      My_Children : aliased Child_Vectors.Vector;
   end record;

   function Name (This : Interface_Tag_T) return String is (This.My_Name.Value.all);

   function Exists_Name (This : Interface_Tag_T) return Boolean is (This.My_Name.Exists);

   function Version (This : Interface_Tag_T) return Version_T is (This.My_Version.Value);

   function Exists_Version (This : Interface_Tag_T) return Boolean is (This.My_Version.Exists);

   function Children (This : aliased Interface_Tag_T) return Children_Ref is ((E => This.My_Children'Access));

end Wayland_XML.Interface_Tag;
