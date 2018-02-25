with Interfaces.C.Strings;
with System;

private with Wl_Thin;
with Ada.Strings.Unbounded;

package Wl is

   type Display_T;
   type Registry_T;

   subtype int is Interfaces.C.int;

   subtype char_array is Interfaces.C.char_array;

   subtype chars_ptr is Interfaces.C.Strings.chars_ptr;

   subtype Unsigned_32 is Interfaces.Unsigned_32;

   function Value (Item : chars_ptr) return char_array renames Interfaces.C.Strings.Value;

   function Value (C : chars_ptr) return String renames Interfaces.C.Strings.Value;

   function To_Ada (Item     : char_array;
                    Trim_Nul : Boolean := True) return String renames Interfaces.C.To_Ada;

   subtype Void_Ptr is System.Address;

   Null_Address : Void_Ptr renames System.Null_Address;

   Default_Display_Name : constant Interfaces.C.Strings.char_array_access;

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

   generic
      type Data_T is private;
      Data : Data_T;
      with procedure Global_Object_Added (Data     : Data_T;
                                          Registry : Registry_T;
                                          Id       : Unsigned_32;
                                          Name     : String;
                                          Version  : Unsigned_32);

      with procedure Global_Object_Removed (Data     : Data_T;
                                            Registry : Registry_T;
                                            Id       : Unsigned_32);
   package Registry_Objects_Subscriber is

      -- Starts subcription to global objects addded and removed events.
      -- To stop subscription, call Registry.Destroy.
      procedure Start_Subscription (Registry : in out Registry_T);

   end Registry_Objects_Subscriber;

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
