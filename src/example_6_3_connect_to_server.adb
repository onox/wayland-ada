with Wl;
with Ada.Text_IO;

-- See section 6.3 at:
-- https://jan.newmarch.name/Wayland/ProgrammingClient/
procedure Example_6_3_Connect_To_Server is
   use type Wl.Display_Ptr;

   Display : Wl.Display_Ptr := Wl.Display_Connect (Wl.Default_Display_Name'Access);
begin
   if Display /= null then
      Ada.Text_IO.Put_Line ("Success");
   else
      Ada.Text_IO.Put_Line ("Failure");
   end if;
end Example_6_3_Connect_To_Server;
