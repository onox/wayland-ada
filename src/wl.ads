with Interfaces.C.Strings;

private with Ada.Finalization;

package Wl is

   pragma Linker_Options ("-lwayland-client");
   -- Added this linker option here to avoid added it to each gpr file that with's
   -- this Ada binding to Wayland. If the wayland client lib changes its name it
   -- means there is only one place one needs to update.

   Default_Display_Name : aliased Interfaces.C.char_array := Interfaces.C.To_C ("wayland-0");

   type Display_T is private;

   function Display_Connect (Name       : in     Interfaces.C.Strings.char_array_access;
                             Is_Success :    out Boolean) return Display_T;

private

   type Hidden_Display_T is null record;

   type Hidden_Display_Ptr is access all Hidden_Display_T;

   type Display_T is new Ada.Finalization.Controlled with record
      Hidden_Display      : Hidden_Display_Ptr := null;
      Number_Of_Instances : Natural := 0;
   end record;

--     overriding
--     procedure Initialize (This : in out Display_T);

   overriding
   procedure Adjust (This : in out Display_T);

   overriding
   procedure Finalize (This : in out Display_T);

end Wl;
