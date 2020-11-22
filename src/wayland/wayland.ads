with System;

package Wayland is
   pragma Pure;

   Nul : constant Character := Character'Val (0);

   type Unsigned_32 is mod 2 ** 32
     with Size => 32;

   type Fixed is new Integer;

   type C_String is new String
     with Dynamic_Predicate => C_String'Length > 0 and then C_String (C_String'Last) = Nul;

   subtype Void_Ptr is System.Address;

   type Wayland_Array_T is record
      Size  : Unsigned_32;
      Alloc : Unsigned_32;
      Data  : Void_Ptr;
   end record
     with Convention => C_Pass_By_Copy;
   --   TODO: Remove the trailing _T from the name of this type

private

   function "+" (Text : String) return C_String is (C_String (Text & Nul));
   --  Appends a 'Nul' character to a standard String and returns a C_String

end Wayland;
