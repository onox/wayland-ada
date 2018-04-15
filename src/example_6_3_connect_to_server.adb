with Posix.Wayland;
with Ada.Text_IO;

-- See section 6.3 at:
-- https://jan.newmarch.name/Wayland/ProgrammingClient/
procedure Example_6_3_Connect_To_Server is
   package Wl renames Posix.Wayland;

   Display : Wl.Display;
begin
   Display.Connect (Wl.Default_Display_Name);
   if not Display.Is_Connected then
      Ada.Text_IO.Put_Line ("Can't connect to display");
      return;
   end if;
   Ada.Text_IO.Put_Line ("Connected to display");

   Display.Disconnect;
   Ada.Text_IO.Put_Line ("Disconnected from display");
end Example_6_3_Connect_To_Server;
