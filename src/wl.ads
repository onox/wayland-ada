with Interfaces.C.Strings;
with System;

private with Ada.Finalization;

with Wl_Thin;

package Wl is

   type Display_T;
   type Registry_T;

   --
   -- Constructors
   --

   function Display_Connect (Name : Interfaces.C.Strings.char_array_access) return Display_T;

   function Display_Get_Registry (Display : Display_T) return Registry_T;

   --
   -- Type and subprogram declarations
   --

   subtype Void_Ptr is Wl_Thin.Void_Ptr;

   Default_Display_Name : Interfaces.C.Strings.char_array_access := Wl_Thin.Default_Display_Name'Access;

   Display_Connection_Exception : exception;

   Registry_Exception : exception;

   subtype Registry_Ptr is Wl_Thin.Registry_Ptr;

   subtype Registry_Global_Subprogram_Ptr is Wl_Thin.Registry_Global_Subprogram_Ptr;

   subtype Registry_Global_Remove_Subprogram_Ptr is Wl_Thin.Registry_Global_Remove_Subprogram_Ptr;

   subtype Registry_Listener_T is Wl_Thin.Registry_Listener_T;

   subtype Registry_Listener_Ptr is Wl_Thin.Registry_Listener_Ptr;

   type Registry_T is tagged limited private;

   function Add_Listener (Registry : Registry_T;
                          Listener : Registry_Listener_Ptr;
                          Data     : Wl.Void_Ptr) return Interfaces.C.int;

   type Display_T is tagged limited private;

   function Dispatch (Display : Display_T) return Interfaces.C.int;

   procedure Dispatch (Display : Display_T);

   function Roundtrip (Display : Display_T) return Interfaces.C.int;

   procedure Roundtrip (Display : Display_T);

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

   type Display_T is new Ada.Finalization.Limited_Controlled with record
      My_Display : Wl_Thin.Display_Ptr;
   end record;

   overriding
   procedure Finalize (This : in out Display_T);

   type Registry_T is new Ada.Finalization.Limited_Controlled with record
      My_Registry : Wl_Thin.Registry_Ptr;
   end record;

   overriding
   procedure Finalize (This : in out Registry_T);

end Wl;
