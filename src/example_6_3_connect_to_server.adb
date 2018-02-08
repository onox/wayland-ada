with Wl;
with Ada.Text_IO;

-- See section 6.3 at:
-- https://jan.newmarch.name/Wayland/ProgrammingClient/
procedure Example_6_3_Connect_To_Server is

   Display : Wl.Display_T;

   procedure Do_Something;

   procedure Connect_To_Wayland_Server is
   begin
      Display.Connect (Wl.Default_Display_Name);
      if Display.Is_Connected then
         Ada.Text_IO.Put_Line ("Success");
         Do_Something;
         Display.Disconnect;
      else
         Ada.Text_IO.Put_Line ("Failure");
      end if;

      pragma Assert (not Display.Is_Connected);
   end Connect_To_Wayland_Server;

   pragma Unmodified (Display);

   procedure Do_Something is
   begin
      null;
   end Do_Something;

begin
   Connect_To_Wayland_Server;
end Example_6_3_Connect_To_Server;
