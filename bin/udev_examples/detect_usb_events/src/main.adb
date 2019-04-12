with C_Binding;
with Udev.Contexts;
with Udev.Devices;
with Udev.Monitors;
with Ada.Text_IO;
with Linux.Posix_Select;

--  This application detects USB events.
--  Build the executable and run this application.
--  Take a USB and insert it into the computer.
--  When it is detected this application will print "Result received!"
--  on standard out. Note that inserting one USB device may trigger
--  several USB events.
procedure Main is

   procedure Put_Line (Text : String) renames Ada.Text_IO.Put_Line;

   use all type Udev.Success_Flag;

   use all type Linux.Posix_Select.Call_Select_Result_Id;
   use all type Linux.Posix_Select.File_Descriptor_Set;

   Context : Udev.Contexts.Context;
   Monitor : Udev.Monitors.Monitor;

   procedure Create_Udev_Context;
   procedure Create_Monitor;
   procedure Add_Mach_Subsystem;
   procedure Enable_Receiving;

   procedure Create_Udev_Context is
   begin
      Udev.Contexts.Create_Context (Context);
      if Context.Exists then
         Create_Monitor;
      else
         Put_Line ("Failed to create udev context");
      end if;
   end Create_Udev_Context;

   procedure Create_Monitor is
   begin
      Context.Create_Monitor ("udev", Monitor);
      if Monitor.Exists then
         Put_Line ("Monitor success");
         Add_Mach_Subsystem;
      else
         Put_Line ("Monitor failure");
      end if;
   end Create_Monitor;

   procedure Add_Mach_Subsystem is
      Flag : Udev.Success_Flag;
   begin
--        Flag := Monitor.Filter_Add_Match_Subsystem_Devtype
--          ("net", null);
      Flag := Monitor.Filter_Add_Match_Subsystem_Devtype
        ("usb", null);
      if Flag = Success then
         Put_Line ("Filter add match success");
         Enable_Receiving;
      else
         Put_Line ("Filter add match failure");
      end if;
   end Add_Mach_Subsystem;

   procedure Enable_Receiving is
      Fd   : Integer;
      Flag : Udev.Success_Flag;
      Fds  : aliased Linux.Posix_Select.File_Descriptor_Set;
      Tv   : aliased Linux.Posix_Select.Time_Value;

      Device : Udev.Devices.Device;
   begin
      Flag := Monitor.Enable_Receiving;
      if Flag = Success then
         Put_Line ("Enable receiving success");
         Fd := Monitor.Get_File_Descriptor;

         loop
            Clear (Fds);
            Set_File_Descriptor (Fds, Fd);
            Tv  := (0, 0);

            declare
               Select_Result : constant Linux.Posix_Select.Call_Select_Result
                 := Linux.Posix_Select.Call_Select
                   (Fd + 1, Fds'Access, null, null, Tv);
            begin
               case Select_Result.Id is
                  when Select_Success =>
                     Put_Line ("Result received!");
                     Monitor.Receive_Device (Device);
                     if Device.Exists then
                        Device.Delete;
                     end if;
                  when Select_Timeout =>
                     null;
                     --  Put_Line ("No response");
                  when Select_Failure =>
                     Put_Line ("Error");
               end case;
               --  Put_Line ("Will loop again");
               delay 0.1;
            end;
         end loop;

      else
         Put_Line ("Enable receiving failure");
      end if;
   end Enable_Receiving;

begin
   Create_Udev_Context;
end Main;
