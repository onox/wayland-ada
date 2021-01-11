with Ada.Containers.Vectors;
with Aida.Deepend;

package Wayland_XML is

   subtype String_Ptr is Aida.Deepend.String_Ptr;

   Empty_String : aliased String := Aida.Deepend.Empty_String;

   type Version_Number is new Positive;

   type Arg_Type_Attribute is
     (Type_Integer,
      Type_Unsigned_Integer,
      Type_String,
      Type_FD,
      Type_New_Id,
      Type_Object,
      Type_Fixed,
      Type_Array);

   TYPE_ATTRIBUTE_EXCEPTION : exception;

   type Arg_Tag is limited private;

   procedure Set_Name
     (This    : in out Arg_Tag;
      Value   :        String) with
      Global => null,
      Pre    => not Exists_Name (This),
      Post   => Exists_Name (This) and Name (This) = Value;

   function Name (This : Arg_Tag) return String with
      Global => null,
      Pre    => Exists_Name (This);

   function Exists_Name (This : Arg_Tag) return Boolean with
      Global => null;

      -- raises TYPE_ATTRIBUTE_EXCEPTION if Value cannot be interpreted
   procedure Set_Type_Attribute (This : in out Arg_Tag; Value : String) with
      Global => null,
      Pre    => not Exists_Type_Attribute (This),
      Post   => Exists_Type_Attribute (This);

   function Type_Attribute (This : Arg_Tag) return Arg_Type_Attribute with
      Global => null,
      Pre    => Exists_Type_Attribute (This);

   function Exists_Type_Attribute (This : Arg_Tag) return Boolean with
      Global => null;

   procedure Set_Interface_Attribute
     (This    : in out Arg_Tag;
      Value   :        String) with
      Global => null,
      Pre    => not Exists_Interface_Attribute (This),
      Post   => Exists_Interface_Attribute (This) and
      Interface_Attribute (This) = Value;

   function Interface_Attribute (This : Arg_Tag) return String with
      Global => null,
      Pre    => Exists_Interface_Attribute (This);

   function Exists_Interface_Attribute (This : Arg_Tag) return Boolean with
      Global => null;

   procedure Set_Summary
     (This    : in out Arg_Tag;
      Value   :        String) with
      Global => null,
      Pre    => not Exists_Summary (This),
      Post   => Exists_Summary (This) and Summary (This) = Value;

   function Summary (This : Arg_Tag) return String with
      Global => null,
      Pre    => Exists_Summary (This);

   function Exists_Summary (This : Arg_Tag) return Boolean with
      Global => null;

   procedure Set_Allow_Null (This : in out Arg_Tag; Value : Boolean) with
      Global => null,
      Pre    => not Exists_Allow_Null (This),
      Post   => Exists_Allow_Null (This) and Allow_Null (This) = Value;

   function Allow_Null (This : Arg_Tag) return Boolean with
      Global => null,
      Pre    => Exists_Allow_Null (This);

   function Exists_Allow_Null (This : Arg_Tag) return Boolean with
      Global => null;

   procedure Set_Enum
     (This    : in out Arg_Tag;
      Value   :        String) with
      Global => null,
      Pre    => not Exists_Enum (This),
      Post   => Exists_Enum (This) and Enum (This) = Value;

   function Enum (This : Arg_Tag) return String with
      Global => null,
      Pre    => Exists_Enum (This);

   function Exists_Enum (This : Arg_Tag) return Boolean with
      Global => null;

   type Arg_Tag_Ptr is access all Arg_Tag;

   type Copyright_Tag is limited private;

   procedure Set_Text
     (This    : in out Copyright_Tag;
      Value   :        String) with
      Global => null,
      Pre    => not Exists_Text (This),
      Post   => Exists_Text (This) and Text (This) = Value;

   function Text (This : Copyright_Tag) return String with
      Global => null,
      Pre    => Exists_Text (This);

   function Exists_Text (This : Copyright_Tag) return Boolean with
      Global => null;

   type Copyright_Ptr is access all Copyright_Tag;

   type Description_Tag is limited private;

   procedure Set_Text
     (This    : in out Description_Tag;
      Value   :        String) with
      Global => null,
      Pre    => not Exists_Text (This),
      Post   => Exists_Text (This) and Text (This) = Value;

   function Text (This : Description_Tag) return String with
      Global => null,
      Pre    => Exists_Text (This);

   function Exists_Text (This : Description_Tag) return Boolean with
      Global => null;

   procedure Set_Summary
     (This    : in out Description_Tag;
      Value   :        String) with
      Global => null,
      Pre    => not Exists_Summary (This),
      Post   => Exists_Summary (This) and Summary (This) = Value;

   function Summary (This : Description_Tag) return String with
      Global => null,
      Pre    => Exists_Summary (This);

   function Exists_Summary (This : Description_Tag) return Boolean with
      Global => null;

   type Description_Tag_Ptr is access all Description_Tag;

   type Entry_Value is new Natural;

   type Entry_Tag is limited private;

   procedure Set_Name
     (This    : in out Entry_Tag;
      Value   :        String) with
      Global => null,
      Pre    => not Exists_Name (This),
      Post   => Exists_Name (This) and Name (This) = Value;

   function Name (This : Entry_Tag) return String with
      Global => null,
      Pre    => Exists_Name (This);

   function Exists_Name (This : Entry_Tag) return Boolean with
      Global => null;

   procedure Set_Summary
     (This    : in out Entry_Tag;
      Value   :        String) with
      Global => null,
      Pre    => not Exists_Summary (This),
      Post   => Exists_Summary (This) and Summary (This) = Value;

   function Summary (This : Entry_Tag) return String with
      Global => null,
      Pre    => Exists_Summary (This);

   function Exists_Summary (This : Entry_Tag) return Boolean with
      Global => null;

   procedure Set_Value (This : in out Entry_Tag; V : Entry_Value) with
      Global => null,
      Pre    => not Exists_Value (This),
      Post   => Exists_Value (This) and Value (This) = V;

   function Value (This : Entry_Tag) return Entry_Value with
      Global => null,
      Pre    => Exists_Value (This);

   function Value_As_String (This : Entry_Tag) return String with
      Global => null,
      Pre    => Exists_Value (This);

   function Exists_Value (This : Entry_Tag) return Boolean with
      Global => null;

   procedure Set_Since (This : in out Entry_Tag; Value : Version_Number) with
      Global => null,
      Pre    => not Exists_Since (This),
      Post   => Exists_Since (This) and Since (This) = Value;

   function Since (This : Entry_Tag) return Version_Number with
      Global => null,
      Pre    => Exists_Since (This);

   function Exists_Since (This : Entry_Tag) return Boolean with
      Global => null;

   type Entry_Tag_Ptr is access all Entry_Tag;

   type Enum_Child_Kind_Id is (Child_Dummy, Child_Description, Child_Entry);

   type Enum_Child (Kind_Id : Enum_Child_Kind_Id := Child_Dummy) is record
      case Kind_Id is
         when Child_Dummy =>
            Dummy : not null String_Ptr := Empty_String'Access;
         when Child_Description =>
            Description_Tag : not null Description_Tag_Ptr;
         when Child_Entry =>
            Entry_Tag : not null Entry_Tag_Ptr;
      end case;
   end record;

   package Enum_Child_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Enum_Child,
      "="          => "=");

   function Is_Smaller_Enum_Value (Left, Right : Enum_Child) return Boolean is
     (Value (Left.Entry_Tag.all) < Value (Right.Entry_Tag.all));

   package Enum_Child_Sorting is new Enum_Child_Vectors.Generic_Sorting (Is_Smaller_Enum_Value);

   type Enum_Children_Ref
     (E : not null access constant Enum_Child_Vectors.Vector)
   is limited null record with
      Implicit_Dereference => E;

   type Enum_Tag is limited private;

   function Children (This : aliased Enum_Tag) return Enum_Children_Ref;
   function Entries (This : aliased Enum_Tag) return Enum_Children_Ref;

   procedure Sort_Entries (This : in out Enum_Tag);

   procedure Append_Child
     (This : in out Enum_Tag;
      Item :        not null Wayland_XML.Description_Tag_Ptr);

   procedure Append_Child
     (This : in out Enum_Tag;
      Item :        not null Entry_Tag_Ptr);

   procedure Set_Name
     (This    : in out Enum_Tag;
      Value   :        String) with
      Global => null,
      Pre    => not Exists_Name (This),
      Post   => Exists_Name (This) and Name (This) = Value;

   function Name (This : Enum_Tag) return String with
      Global => null,
      Pre    => Exists_Name (This);

   function Exists_Name (This : Enum_Tag) return Boolean with
      Global => null;

   procedure Set_Bitfield (This : in out Enum_Tag; Value : Boolean) with
      Global => null,
      Pre    => not Exists_Bitfield (This),
      Post   => Exists_Bitfield (This) and Bitfield (This) = Value;

   function Bitfield (This : Enum_Tag) return Boolean with
      Global => null,
      Pre    => Exists_Bitfield (This);

   function Exists_Bitfield (This : Enum_Tag) return Boolean with
      Global => null;

   procedure Set_Since (This : in out Enum_Tag; Value : Version_Number) with
      Global => null,
      Pre    => not Exists_Since (This),
      Post   => Exists_Since (This) and Since (This) = Value;

   function Since (This : Enum_Tag) return Version_Number with
      Global => null,
      Pre    => Exists_Since (This);

   function Exists_Since (This : Enum_Tag) return Boolean with
      Global => null;

   type Enum_Tag_Ptr is access all Enum_Tag;

   type Event_Child_Kind_Id is (Child_Dummy, Child_Description, Child_Arg);

   type Event_Child (Kind_Id : Event_Child_Kind_Id := Child_Dummy) is record
      case Kind_Id is
         when Child_Dummy =>
            Dummy : not null String_Ptr := Empty_String'Access;
         when Child_Description =>
            Description_Tag : not null Description_Tag_Ptr;
         when Child_Arg =>
            Arg_Tag : not null Arg_Tag_Ptr;
      end case;
   end record;

   package Event_Child_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Event_Child,
      "="          => "=");

   type Event_Children_Ref
     (E : not null access constant Event_Child_Vectors.Vector)
   is limited null record with
      Implicit_Dereference => E;

   type Event_Tag is limited private;

   function Children (This : aliased Event_Tag) return Event_Children_Ref;

   procedure Append_Child
     (This : in out Event_Tag;
      Item :        not null Description_Tag_Ptr);

   procedure Append_Child
     (This : in out Event_Tag;
      Item :        not null Wayland_XML.Arg_Tag_Ptr);

   procedure Set_Name
     (This    : in out Event_Tag;
      Value   :        String) with
      Global => null,
      Pre    => not Exists_Name (This),
      Post   => Exists_Name (This) and Name (This) = Value;

   function Name (This : Event_Tag) return String with
      Global => null,
      Pre    => Exists_Name (This);

   function Exists_Name (This : Event_Tag) return Boolean with
      Global => null;

   procedure Set_Since_Attribute
     (This  : in out Event_Tag;
      Value :        Version_Number) with
     Global => null,
     Pre    => not Exists_Since_Attribute (This),
     Post   =>
       Exists_Since_Attribute (This) and Since_Attribute (This) = Value;

   function Since_Attribute (This : Event_Tag) return Version_Number with
      Global => null,
      Pre    => Exists_Since_Attribute (This);

   function Since_Attribute_As_Pos32
     (This : Event_Tag) return Positive with
      Global => null,
      Pre    => Exists_Since_Attribute (This);

   function Exists_Since_Attribute (This : Event_Tag) return Boolean with
      Global => null;

   type Event_Tag_Ptr is access all Event_Tag;

   type Request_Child_Kind_Id is (Child_Dummy, Child_Description, Child_Arg);

   type Request_Child
     (Kind_Id : Request_Child_Kind_Id := Child_Dummy)
   is record
      case Kind_Id is
         when Child_Dummy =>
            Dummy : not null String_Ptr := Empty_String'Access;
         when Child_Description =>
            Description_Tag : not null Description_Tag_Ptr;
         when Child_Arg =>
            Arg_Tag : not null Arg_Tag_Ptr;
      end case;
   end record;

   package Request_Child_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Request_Child,
      "="          => "=");

   type Request_Children_Ref
     (E : not null access constant Request_Child_Vectors.Vector)
   is limited null record with
      Implicit_Dereference => E;

   type Request_Tag is limited private;

   function Children (This : aliased Request_Tag) return Request_Children_Ref;

   procedure Append_Child
     (This : in out Request_Tag;
      Item :        not null Description_Tag_Ptr);

   procedure Append_Child
     (This : in out Request_Tag;
      Item :        not null Arg_Tag_Ptr);

   procedure Set_Name
     (This    : in out Request_Tag;
      Value   :        String) with
     Global => null,
     Pre    => not Exists_Name (This),
     Post   => Exists_Name (This) and Name (This) = Value;

   function Name (This : Request_Tag) return String with
      Global => null,
      Pre    => Exists_Name (This);

   function Exists_Name (This : Request_Tag) return Boolean with
      Global => null;

   procedure Set_Type_Attribute
     (This    : in out Request_Tag;
      Value   :        String) with
      Global => null,
      Pre    => not Exists_Type_Attribute (This),
      Post   => Exists_Type_Attribute (This) and Type_Attribute (This) = Value;

   function Type_Attribute (This : Request_Tag) return String with
      Global => null,
      Pre    => Exists_Type_Attribute (This);

   function Exists_Type_Attribute (This : Request_Tag) return Boolean with
      Global => null;

   procedure Set_Since (This : in out Request_Tag; Value : Version_Number) with
      Global => null,
      Pre    => not Exists_Since (This),
      Post   => Exists_Since (This) and Since (This) = Value;

   function Since (This : Request_Tag) return Version_Number with
      Global => null,
      Pre    => Exists_Since (This);

   function Since_As_Pos32 (This : Request_Tag) return Positive with
      Global => null,
      Pre    => Exists_Since (This);

   function Exists_Since (This : Request_Tag) return Boolean with
      Global => null;

   function Description (This : aliased Request_Tag) return String with
      Global => null,
      Pre    => Exists_Description (This);

      -- Returns true if there is one unique description.
   function Exists_Description (This : aliased Request_Tag) return Boolean with
      Global => null;

   type Request_Tag_Ptr is access all Request_Tag;

   type Interface_Child_Kind_Id is
     (Child_Dummy, Child_Description, Child_Request, Child_Event, Child_Enum);

   type Interface_Child
     (Kind_Id : Interface_Child_Kind_Id := Child_Dummy)
   is record
      case Kind_Id is
         when Child_Dummy =>
            Dummy : not null String_Ptr := Empty_String'Access;
         when Child_Description =>
            Description_Tag : not null Description_Tag_Ptr;
         when Child_Request =>
            Request_Tag : not null Request_Tag_Ptr;
         when Child_Event =>
            Event_Tag : not null Event_Tag_Ptr;
         when Child_Enum =>
            Enum_Tag : not null Enum_Tag_Ptr;
      end case;
   end record;

   package Interface_Child_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Interface_Child,
      "="          => "=");

   type Interface_Children_Ref
     (E : not null access constant Interface_Child_Vectors.Vector)
   is limited null record with
      Implicit_Dereference => E;

   type Interface_Tag is limited private;

   procedure Set_Name
     (This    : in out Interface_Tag;
      Value   :        String) with
      Global => null,
      Pre    => not Exists_Name (This),
      Post   => Exists_Name (This) and Name (This) = Value;

   function Name (This : Interface_Tag) return String with
      Global => null,
      Pre    => Exists_Name (This);

   function Exists_Name (This : Interface_Tag) return Boolean with
      Global => null;

   procedure Set_Version (This : in out Interface_Tag; Value : Version_Number) with
      Global => null,
      Pre    => not Exists_Version (This),
      Post   => Exists_Version (This) and Version (This) = Value;

   function Version (This : Interface_Tag) return Version_Number with
      Global => null,
      Pre    => Exists_Version (This);

   function Exists_Version (This : Interface_Tag) return Boolean with
      Global => null;

   function Children
     (This : aliased Interface_Tag) return Interface_Children_Ref;

   procedure Append_Child
     (This : in out Interface_Tag;
      Item :        not null Description_Tag_Ptr);

   procedure Append_Child
     (This : in out Interface_Tag;
      Item :        not null Request_Tag_Ptr);

   procedure Append_Child
     (This : in out Interface_Tag;
      Item :        not null Event_Tag_Ptr);

   procedure Append_Child
     (This : in out Interface_Tag;
      Item :        not null Enum_Tag_Ptr);

   type Interface_Tag_Ptr is access all Interface_Tag;

   type Protocol_Child_Kind_Id is
     (Child_Dummy, Child_Copyright, Child_Interface);

   type Protocol_Child
     (Kind_Id : Protocol_Child_Kind_Id := Child_Dummy)
   is record
      case Kind_Id is
         when Child_Dummy =>
            Dummy : not null String_Ptr := Empty_String'Access;
         when Child_Copyright =>
            Copyright_Tag : not null Copyright_Ptr;
         when Child_Interface =>
            Interface_Tag : not null Interface_Tag_Ptr;
      end case;
   end record;

   package Protocol_Child_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Protocol_Child,
      "="          => "=");

   type Protocol_Children_Ref
     (E : not null access constant Protocol_Child_Vectors.Vector)
   is limited null record with
      Implicit_Dereference => E;

   type Protocol_Tag is limited private;

   procedure Set_Name
     (This    : in out Protocol_Tag;
      Value   :        String) with
      Global => null,
      Pre    => not Exists_Name (This),
      Post   => Exists_Name (This) and Name (This) = Value;

   function Name (This : Protocol_Tag) return String with
      Global => null,
      Pre    => Exists_Name (This);

   function Exists_Name (This : Protocol_Tag) return Boolean with
      Global => null;

   function Children
     (This : aliased Protocol_Tag) return Protocol_Children_Ref;

   procedure Append_Child
     (This : in out Protocol_Tag;
      Item :        not null Copyright_Ptr);

   procedure Append_Child
     (This : in out Protocol_Tag;
      Item :        not null Interface_Tag_Ptr);

   type Protocol_Tag_Ptr is access all Protocol_Tag;

private

   type Nullable_String_Ptr (Exists : Boolean := False) is record
      case Exists is
         when True =>
            Value : not null String_Ptr := Empty_String'Access;
         when False =>
            null;
      end case;
   end record;

   type Nullable_Boolean (Exists : Boolean := False) is record
      case Exists is
         when True =>
            Value : Boolean;
         when False =>
            null;
      end case;
   end record;

   type Nullable_Version (Exists : Boolean := False) is record
      case Exists is
         when True =>
            Value : Version_Number;
         when False =>
            null;
      end case;
   end record;

   type Nullable_Allow_Null (Exists : Boolean := False) is record
      case Exists is
         when True =>
            Value : Boolean;
         when False =>
            null;
      end case;
   end record;

   type Nullable_Type_Attribute (Exists : Boolean := False) is record
      case Exists is
         when True =>
            Value : Arg_Type_Attribute;
         when False =>
            null;
      end case;
   end record;

   type Arg_Tag is limited record
      My_Name                : Nullable_String_Ptr;
      My_Type_Attribute      : Nullable_Type_Attribute;
      My_Interface_Attribute : Nullable_String_Ptr;
      My_Summary             : Nullable_String_Ptr;
      My_Allow_Null          : Nullable_Allow_Null;
      My_Enum                : Nullable_String_Ptr;
   end record;

   function Name (This : Arg_Tag) return String is (This.My_Name.Value.all);

   function Exists_Name
     (This : Arg_Tag) return Boolean is
     (This.My_Name.Exists);

   function Type_Attribute
     (This : Arg_Tag) return Arg_Type_Attribute is
     (This.My_Type_Attribute.Value);

   function Exists_Type_Attribute
     (This : Arg_Tag) return Boolean is
     (This.My_Type_Attribute.Exists);

   function Interface_Attribute
     (This : Arg_Tag) return String is
     (This.My_Interface_Attribute.Value.all);

   function Exists_Interface_Attribute
     (This : Arg_Tag) return Boolean is
     (This.My_Interface_Attribute.Exists);

   function Summary
     (This : Arg_Tag) return String is
     (This.My_Summary.Value.all);

   function Exists_Summary
     (This : Arg_Tag) return Boolean is
     (This.My_Summary.Exists);

   function Allow_Null
     (This : Arg_Tag) return Boolean is
     (This.My_Allow_Null.Value);

   function Exists_Allow_Null
     (This : Arg_Tag) return Boolean is
     (This.My_Allow_Null.Exists);

   function Enum (This : Arg_Tag) return String is (This.My_Enum.Value.all);

   function Exists_Enum
     (This : Arg_Tag) return Boolean is
     (This.My_Enum.Exists);

   type Copyright_Tag is limited record
      My_Text : Nullable_String_Ptr;
   end record;

   function Text
     (This : Copyright_Tag) return String is
     (This.My_Text.Value.all);

   function Exists_Text
     (This : Copyright_Tag) return Boolean is
     (This.My_Text.Exists);

   type Description_Tag is limited record
      My_Text    : Nullable_String_Ptr;
      My_Summary : Nullable_String_Ptr;
   end record;

   function Text
     (This : Description_Tag) return String is
     (This.My_Text.Value.all);

   function Exists_Text
     (This : Description_Tag) return Boolean is
     (This.My_Text.Exists);

   function Summary
     (This : Description_Tag) return String is
     (This.My_Summary.Value.all);

   function Exists_Summary
     (This : Description_Tag) return Boolean is
     (This.My_Summary.Exists);

   type Nullable_Entry_Value (Exists : Boolean := False) is record
      case Exists is
         when True =>
            Value : Entry_Value;
         when False =>
            null;
      end case;
   end record;

   type Entry_Tag is limited record
      My_Name    : Nullable_String_Ptr;
      My_Value   : Nullable_Entry_Value;
      My_Summary : Nullable_String_Ptr;
      My_Since   : Nullable_Version;
   end record;

   function Name (This : Entry_Tag) return String is (This.My_Name.Value.all);

   function Exists_Name
     (This : Entry_Tag) return Boolean is
     (This.My_Name.Exists);

   function Value
     (This : Entry_Tag) return Entry_Value is
     (This.My_Value.Value);

   function Value_As_String
     (This : Entry_Tag) return String is
     (Aida.To_String (Natural (This.My_Value.Value)));

   function Exists_Value
     (This : Entry_Tag) return Boolean is
     (This.My_Value.Exists);

   function Summary
     (This : Entry_Tag) return String is
     (This.My_Summary.Value.all);

   function Exists_Summary
     (This : Entry_Tag) return Boolean is
     (This.My_Summary.Exists);

   function Since (This : Entry_Tag) return Version_Number is (This.My_Since.Value);

   function Exists_Since
     (This : Entry_Tag) return Boolean is
     (This.My_Since.Exists);

   type Enum_Tag is limited record
      My_Name     : Nullable_String_Ptr;
      My_Bitfield : Nullable_Boolean;
      My_Since    : Nullable_Version;
      My_Children : aliased Enum_Child_Vectors.Vector;
      My_Entries : aliased Enum_Child_Vectors.Vector;
   end record;

   function Name (This : Enum_Tag) return String is (This.My_Name.Value.all);

   function Exists_Name
     (This : Enum_Tag) return Boolean is
     (This.My_Name.Exists);

   function Bitfield
     (This : Enum_Tag) return Boolean is
     (This.My_Bitfield.Value);

   function Exists_Bitfield
     (This : Enum_Tag) return Boolean is
     (This.My_Bitfield.Exists);

   function Since (This : Enum_Tag) return Version_Number is (This.My_Since.Value);

   function Exists_Since
     (This : Enum_Tag) return Boolean is
     (This.My_Since.Exists);

   function Children
     (This : aliased Enum_Tag) return Enum_Children_Ref is
     ((E => This.My_Children'Access));

   function Entries
     (This : aliased Enum_Tag) return Enum_Children_Ref is
     ((E => This.My_Entries'Access));

   type Nullable_Since_Attribute (Exists : Boolean := False) is record
      case Exists is
         when True =>
            Value : Version_Number;
         when False =>
            null;
      end case;
   end record;

   type Event_Tag is limited record
      My_Name            : Nullable_String_Ptr;
      My_Since_Attribute : Nullable_Since_Attribute;
      My_Children        : aliased Event_Child_Vectors.Vector;
   end record;

   function Name (This : Event_Tag) return String is (This.My_Name.Value.all);

   function Exists_Name
     (This : Event_Tag) return Boolean is
     (This.My_Name.Exists);

   function Since_Attribute
     (This : Event_Tag) return Version_Number is
     (This.My_Since_Attribute.Value);

   function Since_Attribute_As_Pos32
     (This : Event_Tag) return Positive is
     (Positive (This.My_Since_Attribute.Value));

   function Exists_Since_Attribute
     (This : Event_Tag) return Boolean is
     (This.My_Since_Attribute.Exists);

   function Children
     (This : aliased Event_Tag) return Event_Children_Ref is
     ((E => This.My_Children'Access));

   type Nullable_Since_T (Exists : Boolean := False) is record
      case Exists is
         when True =>
            Value : Version_Number;
         when False =>
            null;
      end case;
   end record;

   type Request_Tag is limited record
      My_Name           : Nullable_String_Ptr;
      My_Children       : aliased Request_Child_Vectors.Vector;
      My_Type_Attribute : Nullable_String_Ptr;
      My_Since          : Nullable_Since_T;
   end record;

   function Name
     (This : Request_Tag) return String is
     (This.My_Name.Value.all);

   function Exists_Name
     (This : Request_Tag) return Boolean is
     (This.My_Name.Exists);

   function Children
     (This : aliased Request_Tag) return Request_Children_Ref is
     ((E => This.My_Children'Access));

   function Type_Attribute
     (This : Request_Tag) return String is
     (This.My_Type_Attribute.Value.all);

   function Exists_Type_Attribute
     (This : Request_Tag) return Boolean is
     (This.My_Type_Attribute.Exists);

   function Since
     (This : Request_Tag) return Version_Number is
     (This.My_Since.Value);

   function Since_As_Pos32
     (This : Request_Tag) return Positive is
     (Positive (This.My_Since.Value));

   function Exists_Since
     (This : Request_Tag) return Boolean is
     (This.My_Since.Exists);

   type Interface_Tag is limited record
      My_Name     : Nullable_String_Ptr;
      My_Version  : Nullable_Version;
      My_Children : aliased Interface_Child_Vectors.Vector;
   end record;

   function Name
     (This : Interface_Tag) return String is
     (This.My_Name.Value.all);

   function Exists_Name
     (This : Interface_Tag) return Boolean is
     (This.My_Name.Exists);

   function Version
     (This : Interface_Tag) return Version_Number is
     (This.My_Version.Value);

   function Exists_Version
     (This : Interface_Tag) return Boolean is
     (This.My_Version.Exists);

   function Children
     (This : aliased Interface_Tag) return Interface_Children_Ref is
     ((E => This.My_Children'Access));

   type Protocol_Tag is limited record
      My_Name     : Nullable_String_Ptr;
      My_Children : aliased Protocol_Child_Vectors.Vector;
   end record;

   function Name
     (This : Protocol_Tag) return String is
     (This.My_Name.Value.all);

   function Exists_Name
     (This : Protocol_Tag) return Boolean is
     (This.My_Name.Exists);

   function Children
     (This : aliased Protocol_Tag) return Protocol_Children_Ref is
     ((E => This.My_Children'Access));

end Wayland_XML;
