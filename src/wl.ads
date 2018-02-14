with Interfaces.C.Strings;
with System;

private with Wl_Thin;
with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

package Wl is

   type Display_T;
   type Registry_T;

   subtype int is Interfaces.C.int;

   subtype char_array is Interfaces.C.char_array;

   subtype chars_ptr is Interfaces.C.Strings.chars_ptr;

   subtype Unsigned_32 is Interfaces.Unsigned_32;

   function Value (Item : chars_ptr) return char_array renames Interfaces.C.Strings.Value;

   function To_Ada (Item     : char_array;
                    Trim_Nul : Boolean := True) return String renames Interfaces.C.To_Ada;

   subtype Void_Ptr is System.Address;

   Null_Address : Void_Ptr renames System.Null_Address;

   Default_Display_Name : constant Interfaces.C.Strings.char_array_access;

   package Global_Object is

      type Global_Object_T is tagged private;

      function Data (Global_Object : Global_Object_T) return Wl.Void_Ptr;

      function Id (Global_Object : Global_Object_T) return Wl.Unsigned_32;

      function Interface_Name (Global_Object : Global_Object_T) return String;

      function Version (Global_Object : Global_Object_T) return Wl.Unsigned_32;

      function Make (
                     Data        : Wl.Void_Ptr;
                     Id          : Wl.Unsigned_32;
                     Interface_V : String;
                     Version     : Wl.Unsigned_32
                    )
                     return Global_Object_T;

   private

      type Global_Object_T is tagged record
         My_Data        : Wl.Void_Ptr;
         My_Id          : Wl.Unsigned_32;
         My_Interface_V : Ada.Strings.Unbounded.Unbounded_String;
         My_Version     : Wl.Unsigned_32;
      end record;

      function Data (Global_Object : Global_Object_T) return Wl.Void_Ptr is (Global_Object.My_Data);

      function Id (Global_Object : Global_Object_T) return Wl.Unsigned_32 is (Global_Object.My_Id);

      function Interface_Name (Global_Object : Global_Object_T) return String is (Ada.Strings.Unbounded.To_String (Global_Object.My_Interface_V));

      function Version (Global_Object : Global_Object_T) return Wl.Unsigned_32 is (Global_Object.My_Version);

   end Global_Object;

   subtype Global_Object_T is Global_Object.Global_Object_T;

   package Global_Object_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                                Element_Type => Global_Object_T,
                                                                "="          => Global_Object."=");

   type Global_Objects_Ref (E : not null access constant Global_Object_Vectors.Vector) is limited null record with
     Implicit_Dereference => E;

   generic
   package Global_Objects_Subscriber is

      function Global_Objects return Global_Objects_Ref;

      procedure Start_Subscription (Registry : in out Registry_T;
                                    Display  : in     Display_T);-- with
--       Global => null,
--       Pre    => not Registry.Has_Started_Subscription and Registry.Has_Registry_Object,
--       Post   => Registry.Has_Started_Subscription;

   end Global_Objects_Subscriber;

   type Compositor_T is tagged limited private;

   function Is_Bound (Compositor : Compositor_T) return Boolean with
     Global => null;

   procedure Bind (Compositor  : in out Compositor_T;
                   Registry    : Registry_T;
                   Id          : Wl.Unsigned_32;
                   Version     : Wl.Unsigned_32) with
     Global => null,
     Pre    => Has_Registry_Object (Registry);

   type Registry_T is tagged limited private;

   function Has_Registry_Object (Registry : Registry_T) return Boolean with
     Global => null;

   procedure Get (Registry : in out Registry_T;
                  Display  : Display_T) with
     Global => null,
     Pre    => Is_Connected (Display) and not Registry.Has_Registry_Object;

   function Has_Started_Subscription (Registry : Registry_T) return Boolean with
     Global => null;

--     procedure Start_Subscription (Registry : in out Registry_T;
--                                   Display  : in     Display_T) with
--       Global => null,
--       Pre    => not Registry.Has_Started_Subscription and Registry.Has_Registry_Object,
--       Post   => Registry.Has_Started_Subscription;

   procedure Destroy (Registry : in out Registry_T) with
     Global => null,
     Pre    => Registry.Has_Registry_Object,
     Post   => not Registry.Has_Registry_Object;

   type Display_T is tagged limited private with
     Default_Initial_Condition => not Display_T.Is_Connected;

   function Is_Connected (Display : Display_T) return Boolean with
     Global => null;

   procedure Connect (Display : in out Display_T;
                      Name    : Interfaces.C.Strings.char_array_access) with
     Global => null,
     Pre    => not Display.Is_Connected;
   -- Attempts connecting with the Wayland server.

   function Dispatch (Display : Display_T) return Interfaces.C.int with
     Global => null,
     Pre    => Display.Is_Connected;

   procedure Dispatch (Display : Display_T) with
     Global => null,
     Pre    => Display.Is_Connected;

   function Roundtrip (Display : Display_T) return Interfaces.C.int with
     Global => null,
     Pre    => Display.Is_Connected;

   procedure Roundtrip (Display : Display_T) with
     Global => null,
     Pre    => Display.Is_Connected;

   procedure Disconnect (Display : in out Display_T) with
     Global => null,
     Pre    => Display.Is_Connected,
     Post   => not Display.Is_Connected;

--     type Message_T is limited record
--        Name      : Interfaces.C.Strings.chars_ptr;
--        Signature : Interfaces.C.Strings.chars_ptr;
--        Interfaces : Void_Ptr; -- Can be improved upon.
--     end record with
--       Convention => C_Pass_By_Copy;

--   type

--     type Interface_T is limited record
--        Name         : Interfaces.C.Strings.chars_ptr;
--        Version      : Interfaces.C.int;
--        Method_Count : Interfaces.C.int;
--        Methods      : Void_Ptr; -- Can be improved upon.
--        Event_Count  : Interfaces.C.int;
--        Events       : Void_Ptr; -- Can be improved upon.
--     end record with
--       Convention => C_Pass_By_Copy;

private

   use type Wl_Thin.Display_Ptr;
   use type Wl_Thin.Registry_Ptr;
   use type Wl_Thin.Compositor_Ptr;

   Default_Display_Name : constant Interfaces.C.Strings.char_array_access := Wl_Thin.Default_Display_Name'Access;

   type Display_T is tagged limited record
      My_Display : Wl_Thin.Display_Ptr;
   end record;

   function Is_Connected (Display : Display_T) return Boolean is (Display.My_Display /= null);

   type Registry_T is tagged limited record
      My_Registry                 : Wl_Thin.Registry_Ptr;
      My_Has_Started_Subscription : Boolean := False;
   end record;

   function Has_Registry_Object (Registry : Registry_T) return Boolean is (Registry.My_Registry /= null);

   function Has_Started_Subscription (Registry : Registry_T) return Boolean is (Registry.My_Has_Started_Subscription);

   type Compositor_T is tagged limited record
      My_Compositor : Wl_Thin.Compositor_Ptr;
   end record;

   function Is_Bound (Compositor : Compositor_T) return Boolean is (Compositor.My_Compositor /= null);

end Wl;
