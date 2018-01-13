with Dynamic_Pools;
with Aida.Deepend_XML_DOM_Parser;

package Wayland_XML is

   Default_Subpool : Dynamic_Pools.Dynamic_Pool renames Aida.Deepend_XML_DOM_Parser.Default_Subpool;

   type String_Ptr is access all String with Storage_Pool => Default_Subpool;

   Empty_String : aliased String := "";

   type Version_T is new Aida.Pos32_T;

private

   type Nullable_String_Ptr (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : not null String_Ptr := Empty_String'Access;
         when False => null;
      end case;
   end record;

   type Nullable_Boolean_T (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : Boolean;
         when False => null;
      end case;
   end record;

   type Nullable_Version_T (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : Version_T;
         when False => null;
      end case;
   end record;

end Wayland_XML;
