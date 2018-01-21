with Wl;
with Ada.Text_IO;

-- See section 6.3 at:
-- https://jan.newmarch.name/Wayland/ProgrammingClient/
procedure Example_6_3_Connect_To_Server is

   procedure Do_Something_With_Display (Display : Wl.Display_T);

   procedure Connect_To_Display is
   begin
      Do_Something_With_Display (Wl.Display_Connect (Wl.Default_Display_Name));
   exception
      when Wl.Display_Connection_Exception =>
         Ada.Text_IO.Put_Line ("Failure");
   end Connect_To_Display;

   procedure Do_Something_With_Display (Display : Wl.Display_T) is
      pragma Unreferenced (Display);
   begin
      Ada.Text_IO.Put_Line ("Success");
   end Do_Something_With_Display;

begin
   Connect_To_Display;
end Example_6_3_Connect_To_Server;
