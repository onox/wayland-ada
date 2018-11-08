with Interfaces.C.Strings;
with System;

--  Contains type definitions common to all Ada bindings to C libraries.
package C_Binding with Preelaborate is

   subtype char is Interfaces.C.char;
   subtype unsigned_long is Interfaces.C.unsigned_long;
   subtype unsigned is Interfaces.C.unsigned;
   subtype int is Interfaces.C.int;
   subtype long is Interfaces.C.long;
   subtype Unsigned_32 is Interfaces.Unsigned_32;
   subtype Unsigned_16 is Interfaces.Unsigned_16;

   subtype Void_Ptr is System.Address;

   Nul : constant Character := Character'Val (0);

   type C_String is new String with
     Dynamic_Predicate => C_String'Length > 0
     and then C_String (C_String'Last) = Nul;

   function "-" (Text : C_String) return String;
   -- Removes the last 'Nul' character and returns a normal String.

   function "+" (Text : String) return C_String;
   -- Appends a 'Nul' character to a standard String and returns a C_String.

private

   subtype chars_ptr is Interfaces.C.Strings.chars_ptr;

--     function "+" (Text : String) return chars_ptr;
   -- Appends a Character'Val (0) character to a standard String.

end C_Binding;
