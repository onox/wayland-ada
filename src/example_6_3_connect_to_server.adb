with Wl;
with Ada.Text_IO;

-- See section 6.3 at:
-- https://jan.newmarch.name/Wayland/ProgrammingClient/
procedure Example_6_3_Connect_To_Server is
begin
   declare
      Display : Wl.Display_T := Wl.Display_Connect (Wl.Default_Display_Name'Access);
   begin
      Ada.Text_IO.Put_Line ("Success");
   end;
exception
   when Wl.Failed_To_Connect_Exception =>
      Ada.Text_IO.Put_Line ("Failure");
end Example_6_3_Connect_To_Server;
