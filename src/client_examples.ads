with Posix.Wayland_Client;
with Ada.Text_IO;

package Client_Examples is

   package Wl renames Posix.Wayland_Client;

   procedure Put_Line (Text : String) renames Ada.Text_IO.Put_Line;

end Client_Examples;
