with Posix.Wayland_Client;
with Ada.Text_IO;

-- See section 6.3 at:
-- https://jan.newmarch.name/Wayland/ProgrammingClient/
package body Client_Examples.Connect_To_Server is

   procedure Run is
      package Wl renames Posix.Wayland_Client;

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
   end Run;

end Client_Examples.Connect_To_Server;
