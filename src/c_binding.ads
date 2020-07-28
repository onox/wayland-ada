with System;

private with Interfaces.C.Strings;
private with Ada.Unchecked_Conversion;

with Wayland.API;

--  Contains type definitions common to all Ada bindings to C libraries.
package C_Binding with Preelaborate is

   subtype Void_Ptr is System.Address;

   type Success_Flag is
     (
      Success,
      Failure
     );

   subtype Max_String_Length is Natural range 0 .. 10_000;

   type String_Result
     (Is_Success : Boolean := False;
      Length     : Max_String_Length := 1)
   is record
      case Is_Success is
         when True  => Value : String (1 .. Length);
         when False => Error : String (1 .. Length);
      end case;
   end record;

private

   Nul : constant Character := Character'Val (0);

   subtype C_String is Wayland.API.C_String with
     Dynamic_Predicate => C_String'Length > 0
     and then C_String (C_String'Last) = Nul;

   function "-" (Text : C_String) return String;
   -- Removes the last 'Nul' character and returns a normal String.

   function "+" (Text : String) return C_String;
   -- Appends a 'Nul' character to a standard String and returns a C_String.

   subtype char is Interfaces.C.char;
   subtype unsigned_long is Interfaces.C.unsigned_long;
   subtype unsigned is Interfaces.C.unsigned;
   subtype int is Interfaces.C.int;
   subtype long is Interfaces.C.long;
--   subtype Unsigned_32 is Interfaces.Unsigned_32;
   subtype Unsigned_32 is Wayland.API.Unsigned_32;
   subtype Unsigned_16 is Interfaces.Unsigned_16;

   subtype chars_ptr is Interfaces.C.Strings.chars_ptr;

   --     type Chars_Ref (E : not null access constant chars_ptr) is
   --
   --     function "+" (Text : String) return chars_ptr;
   --     -- Appends a Character'Val (0) character to a standard String.
   --
   function "-" (Chars : chars_ptr) return String;
   -- Removes the last Character'Val (0) character and returns a String.

end C_Binding;
