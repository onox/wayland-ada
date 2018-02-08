with Interfaces.C.Strings;
with System;

with Wl_Thin;

package Wl is

   type Display_T;
   type Registry_T;

   --
   -- Constructors
   --

   --
   -- Type and subprogram declarations
   --

   subtype Void_Ptr is Wl_Thin.Void_Ptr;

   Default_Display_Name : Interfaces.C.Strings.char_array_access := Wl_Thin.Default_Display_Name'Access;

   subtype Registry_Ptr is Wl_Thin.Registry_Ptr;

   subtype Registry_Global_Subprogram_Ptr is Wl_Thin.Registry_Global_Subprogram_Ptr;

   subtype Registry_Global_Remove_Subprogram_Ptr is Wl_Thin.Registry_Global_Remove_Subprogram_Ptr;

   subtype Registry_Listener_T is Wl_Thin.Registry_Listener_T;

   subtype Registry_Listener_Ptr is Wl_Thin.Registry_Listener_Ptr;

   type Registry_T is tagged limited private;

   function Has_Registry_Object (Registry : Registry_T) return Boolean with
     Global => null;

   procedure Get (Registry : in out Registry_T;
                  Display  : Display_T) with
     Global => null,
     Pre    => Is_Connected (Display) and not Registry.Has_Registry_Object;

   function Add_Listener (Registry : Registry_T;
                          Listener : Registry_Listener_Ptr;
                          Data     : Wl.Void_Ptr) return Interfaces.C.int;

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

   type Display_T is tagged limited record
      My_Display : Wl_Thin.Display_Ptr;
   end record;

   function Is_Connected (Display : Display_T) return Boolean is (Display.My_Display /= null);

   type Registry_T is tagged limited record
      My_Registry : Wl_Thin.Registry_Ptr;
   end record;

   function Has_Registry_Object (Registry : Registry_T) return Boolean is (Registry.My_Registry /= null);

end Wl;
