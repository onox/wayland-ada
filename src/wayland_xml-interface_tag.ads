package Wayland_XML.Interface_Tag is

   type Version_T is new Aida.Pos32_T;

   type Interface_Tag_T is tagged limited private;

   procedure Set_Name (This    : in out Interface_Tag_T;
                       Value   : Aida.String_T;
                       Subpool : Dynamic_Pools.Subpool_Handle) with
     Global => null,
     Pre    => not This.Exists_Name,
     Post   => This.Exists_Name and This.Name = Value;

   function Name (This : Interface_Tag_T) return Aida.String_T with
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

   type Interface_Tag_Ptr is access all Interface_Tag_T with Storage_Pool => Default_Subpool;

private

   type Nullable_Version_T (Exists : Boolean := False) is record
      case Exists is
         when True => Value : Version_T;
         when False => null;
      end case;
   end record;

   type Interface_Tag_T is tagged limited record
      My_Name    : Nullable_String_Ptr;
      My_Version : Nullable_Version_T;
   end record;

   function Name (This : Interface_Tag_T) return Aida.String_T is (This.My_Name.Value.all);

   function Exists_Name (This : Interface_Tag_T) return Boolean is (This.My_Name.Exists);

   function Version (This : Interface_Tag_T) return Version_T is (This.My_Version.Value);

   function Exists_Version (This : Interface_Tag_T) return Boolean is (This.My_Version.Exists);

end Wayland_XML.Interface_Tag;
