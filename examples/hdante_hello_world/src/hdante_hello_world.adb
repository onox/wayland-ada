--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2018 - 2019 Joakim Strandberg <joakim@mequinox.se>
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.

with System;

with Ada.Exceptions;
with Ada.Real_Time;
with Ada.Text_IO;

with C_Binding.Linux.Files;
with C_Binding.Linux.File_Status;
with C_Binding.Linux.Memory_Maps;

with Wayland.Protocols.Client;
with Wayland.Protocols.Xdg_Shell;
with Wayland.Protocols.Presentation_Time;
with Wayland.Protocols.Viewporter;
with Wayland.Protocols.Idle_Inhibit_Unstable_V1;
with Wayland.Protocols.Xdg_Decoration_Unstable_V1;

with Wayland.Enums.Client;
with Wayland.Enums.Presentation_Time;
with Wayland.Enums.Viewporter;
with Wayland.Enums.Idle_Inhibit_Unstable_V1;
with Wayland.Enums.Xdg_Decoration_Unstable_V1;

-- sudo apt install libwayland-dev
-- This is a wayland hello world application. It uses the wayland
-- client library and the wayland protocol to display a window.
--
-- Original code that was translated from C to Ada:
-- https://github.com/hdante/hello_wayland
-- https://hdante.wordpress.com/2014/07/08/the-hello-wayland-tutorial/
package body Hdante_Hello_World is

   procedure Put_Line (Value : String) renames Ada.Text_IO.Put_Line;

   subtype Unsigned_32 is Wayland.Unsigned_32;
   subtype Fixed is Wayland.Fixed;

   use type Unsigned_32;

   package Linux renames C_Binding.Linux;

   Wayland_Error : exception;

   use all type Linux.Files.File_Mode;
   use all type Linux.Files.File_Permission;
   use all type Linux.File_Status.Status;
   use all type Linux.Memory_Maps.Memory_Map;
   use all type Linux.Memory_Maps.Memory_Protection;

   use all type Wayland.Call_Result_Code;

   Done : Boolean := False;

   type Data_Type is limited record
      Compositor : Wayland.Protocols.Client.Compositor;
      Keyboard   : Wayland.Protocols.Client.Keyboard;
      Pointer    : Wayland.Protocols.Client.Pointer;
      Seat       : Wayland.Protocols.Client.Seat;
      Shm        : Wayland.Protocols.Client.Shm;
      Surface    : Wayland.Protocols.Client.Surface;

      XDG_Shell    : Wayland.Protocols.Xdg_Shell.Xdg_Wm_Base;
      XDG_Surface  : Wayland.Protocols.Xdg_Shell.Xdg_Surface;
      XDG_Toplevel : Wayland.Protocols.Xdg_Shell.Xdg_Toplevel;

      Presentation : Wayland.Protocols.Presentation_Time.Presentation;
      Feedback     : Wayland.Protocols.Presentation_Time.Presentation_Feedback;
      Has_Feedback : Boolean := False;

      Decorator  : Wayland.Protocols.Xdg_Decoration_Unstable_V1.Decoration_Manager_V1;
      Decoration : Wayland.Protocols.Xdg_Decoration_Unstable_V1.Toplevel_Decoration_V1;

      Capabilities : Wayland.Enums.Client.Seat_Capability := (others => False);
   end record;

   Data : Data_Type;

   Buffer : Wayland.Protocols.Client.Buffer;

   procedure Buffer_Release (Buffer : in out Wayland.Protocols.Client.Buffer'Class) is
   begin
      Buffer.Destroy;
      Put_Line ("Destroyed buffer");
   end Buffer_Release;

   package Buffer_Events is new Wayland.Protocols.Client.Buffer_Events
     (Release => Buffer_Release);

   Width  : constant := 320;
   Height : constant := 200;
   --     CURSOR_WIDTH : constant := 100;
   --     CURSOR_HEIGHT : constant := 59;
   --     CURSOR_HOT_SPOT_X : constant := 10;
   --     CURSOR_HOT_SPOT_Y : constant := 35;

   File_Name : constant String := "./examples/hdante_hello_world/hello_world_image.bin";

   procedure Draw_Frame is
      Pool   : Wayland.Protocols.Client.Shm_Pool;

      Image       : Linux.Files.File;
      File_Status : Linux.File_Status.Status;
      Memory_Map  : Linux.Memory_Maps.Memory_Map;
   begin
      Linux.Files.Open
        (Image,
         File_Name,
         Mode        => Read_Write,
         Permissions =>
           (Owner_Read  => True,
            Group_Read  => True,
            Others_Read => True,
            others      => False));

      if Linux.Files.Is_Closed (Image) then
         raise Wayland_Error with "Failed to open surface image";
      end if;

      Get_File_Status (Image, File_Status);

      if not Is_Valid (File_Status) then
         raise Wayland_Error with "File does not exist";
      end if;

      Get_Map_Memory
        (Image,
         System.Null_Address,
         Size (File_Status),
         Page_Can_Be_Read,
         Linux.Memory_Maps.MAP_SHARED, 0, Memory_Map);

      if not Memory_Map.Has_Mapping then
         Linux.Files.Close (Image);
         raise Wayland_Error with "Failed to memory map image";
      end if;

      Data.Shm.Create_Pool (Image, Integer (Size (File_Status)), Pool);

      if not Pool.Has_Proxy then
         raise Wayland_Error with "No shm pool";
      end if;

      Pool.Create_Buffer
        (0,
         Width,
         Height,
         Width * 4,
         Wayland.Enums.Client.Xrgb_8888,
         Buffer);

      Memory_Map.Unmap_Memory;
      Pool.Destroy;

      if not Buffer.Has_Proxy then
         raise Wayland_Error with "No buffer";
      end if;

      if Buffer_Events.Subscribe (Buffer) = Error then
         raise Wayland_Error with "Failed to subscribe to buffer events";
      end if;
   end Draw_Frame;

   procedure Seat_Capabilities
     (Seat         : in out Wayland.Protocols.Client.Seat'Class;
      Capabilities : Wayland.Enums.Client.Seat_Capability) is
   begin
      Data.Capabilities := Capabilities;

      Put_Line ("Seat capabilities:");
      if Capabilities.Pointer then
         Put_Line (" pointer");
      end if;

      if Capabilities.Keyboard then
         Put_Line (" keyboard");
      end if;

      if Capabilities.Touch then
         Put_Line (" touch");
      end if;
   end Seat_Capabilities;

   procedure Seat_Name
     (Seat : in out Wayland.Protocols.Client.Seat'Class;
      Name : String) is
   begin
      Put_Line ("Seat name: " & Name);
   end Seat_Name;

   package Seat_Events is new Wayland.Protocols.Client.Seat_Events
     (Seat_Capabilities => Seat_Capabilities,
      Seat_Name         => Seat_Name);

   procedure Base_Ping
     (Xdg_Wm_Base : in out Wayland.Protocols.Xdg_Shell.Xdg_Wm_Base'Class;
      Serial      : Unsigned_32) is
   begin
      Put_Line ("xdg_wm_base ping");
      Xdg_Wm_Base.Pong (Serial);
   end Base_Ping;

   package Base_Events is new Wayland.Protocols.Xdg_Shell.Xdg_Wm_Base_Events
     (Ping => Base_Ping);

   procedure Presentation_Synchronized_Output
     (Presentation_Feedback : in out Wayland.Protocols.Presentation_Time.Presentation_Feedback'Class;
      Output                : Wayland.Protocols.Client.Output'Class) is
   begin
      Put_Line ("presentation output");
   end Presentation_Synchronized_Output;

   procedure Presentation_Presented
     (Presentation_Feedback : in out Wayland.Protocols.Presentation_Time.Presentation_Feedback'Class;
      Timestamp             : Duration;
      Refresh               : Duration;
      Counter               : Wayland.Unsigned_64;
      Flags                 : Wayland.Enums.Presentation_Time.Presentation_Feedback_Kind) is
   begin
      Put_Line ("presentation presented: ts:" & Timestamp'Image & " refresh:" & Refresh'Image & " counter:" & Counter'Image);
      Put_Line ("  flags:");
      Put_Line ("          vsync: " & Flags.Vsync'Image);
      Put_Line ("       hw clock: " & Flags.Hw_Clock'Image);
      Put_Line ("  hw completion: " & Flags.Hw_Completion'Image);
      Put_Line ("      zero copy: " & Flags.Zero_Copy'Image);

      Data.Has_Feedback := False;
   end Presentation_Presented;

   procedure Presentation_Discarded
     (Presentation_Feedback : in out Wayland.Protocols.Presentation_Time.Presentation_Feedback'Class) is
   begin
      Put_Line ("presentation discarded");

      Data.Has_Feedback := False;
   end Presentation_Discarded;

   package Presentation_Events is new Wayland.Protocols.Presentation_Time.Presentation_Feedback_Events
     (Synchronized_Output => Presentation_Synchronized_Output,
      Presented           => Presentation_Presented,
      Discarded           => Presentation_Discarded);

   procedure Global_Registry_Handler
     (Registry : in out Wayland.Protocols.Client.Registry'Class;
      Id       : Unsigned_32;
      Name     : String;
      Version  : Unsigned_32) is
   begin
      if Name = Wayland.Protocols.Client.Compositor_Interface.Name then
         Data.Compositor.Bind (Registry, Id, Unsigned_32'Min (Version, 4));

         if not Data.Compositor.Has_Proxy then
            raise Wayland_Error with "No compositor";
         end if;
         Put_Line ("Got compositor proxy");
      elsif Name = Wayland.Protocols.Client.Shm_Interface.Name then
         Data.Shm.Bind (Registry, Id, Unsigned_32'Min (Version, 1));

         if not Data.Shm.Has_Proxy then
            raise Wayland_Error with "No shm";
         end if;
         Put_Line ("Got shm proxy");
      elsif Name = Wayland.Protocols.Xdg_Shell.Xdg_Wm_Base_Interface.Name then
         Data.XDG_Shell.Bind (Registry, Id, Unsigned_32'Min (Version, 1));

         if not Data.XDG_Shell.Has_Proxy then
            raise Wayland_Error with "No xdg_wm_base";
         end if;
         Put_Line ("Got xdg_wm_base proxy");

         if Base_Events.Subscribe (Data.XDG_Shell) = Error then
            raise Wayland_Error with "Failed to subscribe to xdg_wm_base events";
         end if;
      elsif Name = Wayland.Protocols.Client.Seat_Interface.Name then
         Data.Seat.Bind (Registry, Id, Unsigned_32'Min (Version, 6));

         if not Data.Seat.Has_Proxy then
            raise Wayland_Error with "No seat";
         end if;
         Put_Line ("Got seat proxy");

         if Seat_Events.Subscribe (Data.Seat) = Error then
            raise Wayland_Error with "Failed to subscribe to seat events";
         end if;
      elsif Name = Wayland.Protocols.Presentation_Time.Presentation_Interface.Name then
         Data.Presentation.Bind (Registry, Id, Unsigned_32'Min (Version, 1));

         if not Data.Presentation.Has_Proxy then
            raise Wayland_Error with "No presentation";
         end if;
      elsif Name = Wayland.Protocols.Xdg_Decoration_Unstable_V1.Decoration_Manager_V1_Interface.Name then
         Data.Decorator.Bind (Registry, Id, Unsigned_32'Min (Version, 1));

         if not Data.Decorator.Has_Proxy then
            raise Wayland_Error with "No xdg_decoration_manager";
         end if;
      end if;
   end Global_Registry_Handler;

   procedure Global_Registry_Remover
     (Registry : in out Wayland.Protocols.Client.Registry'Class;
      Id       : Unsigned_32) is
   begin
      Put_Line ("Got a registry losing event for" & Id'Image);
   end Global_Registry_Remover;

   package Registry_Events is new Wayland.Protocols.Client.Registry_Events
     (Global_Object_Added   => Global_Registry_Handler,
      Global_Object_Removed => Global_Registry_Remover);

   procedure XDG_Surface_Configure
     (Xdg_Surface : in out Wayland.Protocols.Xdg_Shell.Xdg_Surface'Class;
      Serial      : Unsigned_32) is
   begin
      Put_Line ("Configure xdg_surface");
      Xdg_Surface.Ack_Configure (Serial);

      Draw_Frame;
      Data.Surface.Attach (Buffer, 0, 0);  --  FIXME if Done then should use null to detach
      Data.Surface.Commit;

      if not Data.Has_Feedback then
         Data.Presentation.Feedback (Data.Surface, Data.Feedback);

         if Presentation_Events.Subscribe (Data.Feedback) = Error then
            raise Wayland_Error with "Failed to subscribe to presentation feedback events";
         end if;
         Data.Has_Feedback := True;
      end if;
   end XDG_Surface_Configure;

   procedure XDG_Toplevel_Configure
     (Xdg_Toplevel : in out Wayland.Protocols.Xdg_Shell.Xdg_Toplevel'Class;
      Width        : Natural;
      Height       : Natural;
      States       : Wayland.Unsigned_32_Array) is
   begin
      Put_Line ("Configure xdg_toplevel" & Width'Image & Height'Image);

      if States'Length > 0 then
         Put_Line ("Has" & Integer'Image (States'Length) & " states:");
         for S of States loop
            Put_Line (S'Image);
         end loop;
      end if;
   end XDG_Toplevel_Configure;

   procedure XDG_Toplevel_Close
     (Xdg_Toplevel : in out Wayland.Protocols.Xdg_Shell.Xdg_Toplevel'Class) is
   begin
      Put_Line ("Close xdg_toplevel");
      Done := True;
   end XDG_Toplevel_Close;

   procedure XDG_Toplevel_Decoration_Configure
     (Decoration : in out Wayland.Protocols.Xdg_Decoration_Unstable_V1.Toplevel_Decoration_V1'Class;
      Mode       : Wayland.Enums.Xdg_Decoration_Unstable_V1.Toplevel_Decoration_V1_Mode) is
   begin
      Put_Line ("xdg_toplevel_decoration: " & Mode'Image);
   end XDG_Toplevel_Decoration_Configure;

   package XDG_Surface_Events is new Wayland.Protocols.Xdg_Shell.Xdg_Surface_Events
     (Configure => XDG_Surface_Configure);

   package XDG_Toplevel_Events is new Wayland.Protocols.Xdg_Shell.Xdg_Toplevel_Events
     (Configure => XDG_Toplevel_Configure,
      Close     => XDG_Toplevel_Close);

   package XDG_Toplevel_Decoration_Events is new Wayland.Protocols.Xdg_Decoration_Unstable_V1.Toplevel_Decoration_V1_Events
     (Configure => XDG_Toplevel_Decoration_Configure);

   procedure Pointer_Enter
     (Pointer   : in out Wayland.Protocols.Client.Pointer'Class;
      Serial    : Unsigned_32;
      Surface   : Wayland.Protocols.Client.Surface;
      Surface_X : Fixed;
      Surface_Y : Fixed) is
   begin
      Put_Line ("Pointer enter: " & Surface_X'Image & Surface_Y'Image);
   end Pointer_Enter;

   procedure Pointer_Leave
     (Pointer : in out Wayland.Protocols.Client.Pointer'Class;
      Serial  : Unsigned_32;
      Surface : Wayland.Protocols.Client.Surface) is
   begin
      Put_Line ("Pointer leave");
   end Pointer_Leave;

   procedure Pointer_Motion
     (Pointer : in out Wayland.Protocols.Client.Pointer'Class;
      Time      : Unsigned_32;
      Surface_X : Fixed;
      Surface_Y : Fixed) is
   begin
      Put_Line ("Pointer motion:" & Time'Image & Surface_X'Image & Surface_Y'Image);
   end Pointer_Motion;

   procedure Pointer_Button
     (Pointer : in out Wayland.Protocols.Client.Pointer'Class;
      Serial  : Unsigned_32;
      Time    : Unsigned_32;
      Button  : Unsigned_32;
      State   : Wayland.Enums.Client.Pointer_Button_State) is
   begin
      Put_Line ("Pointer button: " & Time'Image & " " & Button'Image & " " & State'Image);
   end Pointer_Button;

   procedure Pointer_Axis
     (Pointer : in out Wayland.Protocols.Client.Pointer'Class;
      Time    : Unsigned_32;
      Axis    : Wayland.Enums.Client.Pointer_Axis;
      Value   : Fixed) is
   begin
      Put_Line ("Pointer axis: " & Time'Image & Axis'Image & Value'Image);
   end Pointer_Axis;

   procedure Pointer_Frame
     (Pointer : in out Wayland.Protocols.Client.Pointer'Class) is
   begin
      Put_Line ("Pointer frame");
   end Pointer_Frame;

   procedure Pointer_Axis_Source
     (Pointer     : in out Wayland.Protocols.Client.Pointer'Class;
      Axis_Source : Wayland.Enums.Client.Pointer_Axis_Source) is
   begin
      Put_Line ("Pointer axis source:" & Axis_Source'Image);
   end Pointer_Axis_Source;

   procedure Pointer_Axis_Stop
     (Pointer : in out Wayland.Protocols.Client.Pointer'Class;
      Time    : Unsigned_32;
      Axis    : Wayland.Enums.Client.Pointer_Axis) is
   begin
      Put_Line ("Pointer axis stop:" & Time'Image & Axis'Image);
   end Pointer_Axis_Stop;

   procedure Pointer_Axis_Discrete
     (Pointer  : in out Wayland.Protocols.Client.Pointer'Class;
      Axis     : Wayland.Enums.Client.Pointer_Axis;
      Discrete : Integer) is
   begin
      Put_Line ("Pointer axis discrete:" & Axis'Image & Discrete'Image);
   end Pointer_Axis_Discrete;

   package Pointer_Events is new Wayland.Protocols.Client.Pointer_Events
     (Pointer_Enter           => Pointer_Enter,
      Pointer_Leave           => Pointer_Leave,
      Pointer_Motion          => Pointer_Motion,
      Pointer_Button          => Pointer_Button,
      Pointer_Scroll          => Pointer_Axis,
      Pointer_Frame           => Pointer_Frame,
      Pointer_Scroll_Source   => Pointer_Axis_Source,
      Pointer_Scroll_Stop     => Pointer_Axis_Stop,
      Pointer_Scroll_Discrete => Pointer_Axis_Discrete);

   procedure Keyboard_Keymap
     (Keyboard : in out Wayland.Protocols.Client.Keyboard'Class;
      Format   : Wayland.Enums.Client.Keyboard_Keymap_Format;
      Fd       : Integer;
      Size     : Unsigned_32) is
   begin
      Put_Line ("Keyboard keymap: " & Format'Image & Size'Image);
   end Keyboard_Keymap;

   procedure Keyboard_Enter
     (Keyboard : in out Wayland.Protocols.Client.Keyboard'Class;
      Serial   : Unsigned_32;
      Surface  : Wayland.Protocols.Client.Surface;
      Keys     : Wayland.Unsigned_32_Array) is
   begin
      Put_Line ("Keyboard focus gained");

      if Keys'Length > 0 then
         Put_Line ("Pressed" & Integer'Image (Keys'Length) & " keys:");
         for K of Keys loop
            Put_Line (K'Image);
         end loop;
      end if;
   end Keyboard_Enter;

   procedure Keyboard_Leave
     (Keyboard : in out Wayland.Protocols.Client.Keyboard'Class;
      Serial   : Unsigned_32;
      Surface  : Wayland.Protocols.Client.Surface) is
   begin
      Put_Line ("Keyboard focus lost");
   end Keyboard_Leave;

   procedure Keyboard_Key
     (Keyboard : in out Wayland.Protocols.Client.Keyboard'Class;
      Serial   : Unsigned_32;
      Time     : Unsigned_32;
      Key      : Unsigned_32;
      State    : Wayland.Enums.Client.Keyboard_Key_State) is
   begin
      Put_Line ("Keyboard key" & Key'Image & " " & State'Image & " at" & Time'Image);
   end Keyboard_Key;

   procedure Keyboard_Modifiers
     (Keyboard       : in out Wayland.Protocols.Client.Keyboard'Class;
      Serial         : Unsigned_32;
      Mods_Depressed : Unsigned_32;
      Mods_Latched   : Unsigned_32;
      Mods_Locked    : Unsigned_32;
      Group          : Unsigned_32) is
   begin
      Put_Line ("Keyboard mods:" & Mods_Depressed'Image & " " & Mods_Latched'Image & " " & Mods_Locked'Image & " " & Group'Image);
   end Keyboard_Modifiers;

   package Keyboard_Events is new Wayland.Protocols.Client.Keyboard_Events
     (Keymap      => Keyboard_Keymap,
      Enter       => Keyboard_Enter,
      Leave       => Keyboard_Leave,
      Key         => Keyboard_Key,
      Modifiers   => Keyboard_Modifiers);

   --  TODO Add Surface_Events and Output_Events

   Display  : Wayland.Protocols.Client.Display;
   Registry : Wayland.Protocols.Client.Registry;

   procedure Run is
      use all type Wayland.Protocols.Client.Check_For_Events_Status;

      use all type Ada.Real_Time.Time;
      use all type Ada.Real_Time.Time_Span;

      Events_Status : Wayland.Protocols.Client.Check_For_Events_Status;
      Timestamp     : Ada.Real_Time.Time := Clock;
   begin
      Display.Connect;
      if not Display.Is_Connected then
         raise Wayland_Error with "No connection to Wayland compositor";
      end if;
      Put_Line ("Connected to Wayland compositor");

      Display.Get_Registry (Registry);

      if not Registry.Has_Proxy then
         raise Wayland_Error with "No registry";
      end if;

      if Registry_Events.Subscribe (Registry) = Error then
         raise Wayland_Error with "Failed to subscribe to registry events";
      end if;

      Display.Dispatch;
      Display.Roundtrip;

      if Data.Capabilities.Pointer then
         Data.Seat.Get_Pointer (Data.Pointer);

         if not Data.Pointer.Has_Proxy then
            raise Wayland_Error with "No pointer";
         end if;

         if Pointer_Events.Subscribe (Data.Pointer) = Error then
            raise Wayland_Error with "Failed to subscribe to pointer events";
         end if;
      end if;

      if Data.Capabilities.Keyboard then
         Data.Seat.Get_Keyboard (Data.Keyboard);

         if not Data.Keyboard.Has_Proxy then
            raise Wayland_Error with "No keyboard";
         end if;

         if Keyboard_Events.Subscribe (Data.Keyboard) = Error then
            raise Wayland_Error with "Failed to subscribe to keyboard events";
         end if;
      end if;

      Data.Compositor.Create_Surface (Data.Surface);

      if not Data.Surface.Has_Proxy then
         raise Wayland_Error with "Failed to create wl_surface";
      end if;
      Put_Line ("Created wl_surface");

      Put_Line ("Getting xdg_surface via xdg_wm_base + wl_surface...");
      Data.XDG_Shell.Get_Surface (Data.Surface, Data.XDG_Surface);

      if not Data.XDG_Surface.Has_Proxy then
         raise Wayland_Error with "Failed to create xdg_surface";
      end if;
      Put_Line ("Got xdg_surface");

      if XDG_Surface_Events.Subscribe (Data.XDG_Surface) = Error then
         raise Wayland_Error with "Failed to subscribe to xdg_surface events";
      end if;
      Put_Line ("Successfully subscribed to xdg_surface events");

      Data.XDG_Surface.Get_Toplevel (Data.XDG_Toplevel);

      if not Data.XDG_Toplevel.Has_Proxy then
         raise Wayland_Error with "Failed to create xdg_toplevel";
      end if;
      Put_Line ("Got xdg_toplevel");

      if XDG_Toplevel_Events.Subscribe (Data.XDG_Toplevel) = Error then
         raise Wayland_Error with "Failed to subscribe to xdg_toplevel events";
      end if;
      Put_Line ("Successfully subscribed to xdg_toplevel events");

      Data.XDG_Toplevel.Set_Title ("Wayland with Ada");
      Data.XDG_Toplevel.Set_App_Id ("wayland.ada");

      if Data.Decorator.Has_Proxy then
         Data.Decorator.Get_Toplevel_Decoration (Data.XDG_Toplevel, Data.Decoration);

         if not Data.Decoration.Has_Proxy then
            raise Wayland_Error with "Failed to create xdg_toplevel_decoration";
         end if;
         Put_Line ("Got xdg_toplevel_decoration");

         if XDG_Toplevel_Decoration_Events.Subscribe (Data.Decoration) = Error then
            raise Wayland_Error with "Failed to subscribe to xdg_toplevel_decoration events";
         end if;
         Put_Line ("Successfully subscribed to xdg_toplevel_decoration events");

         Data.Decoration.Set_Mode (Wayland.Enums.Xdg_Decoration_Unstable_V1.Server_Side);
      else
         Put_Line ("No xdg_decoration_manager");
      end if;

      Data.Surface.Commit;

      --  Event dispatching method 1
      if True then
         while not Done and then Display.Dispatch.Is_Success loop
            null;
         end loop;
      end if;

      --  Event dispatching method 2
      while not Done loop
         while Display.Prepare_Read = Error loop
            if not Display.Dispatch_Pending.Is_Success then
               raise Wayland_Error with "failed dispatching pending events";
            end if;
         end loop;
         Display.Flush;

         declare
            Events_Interval : constant Ada.Real_Time.Time_Span := Seconds (1);

            Timeout : constant Integer
              := Integer (To_Duration ((Timestamp + Events_Interval) - Clock) * 1_000);
            --  The timeout to check for events in millisends
         begin
            Events_Status := Display.Check_For_Events (if Timeout > 0 then Timeout else 0);
         end;

         case Events_Status is
            when Events_Need_Processing =>
               if Display.Read_Events = Error then
                  raise Wayland_Error with "failed reading events";
               end if;

               if not Display.Dispatch_Pending.Is_Success then
                  raise Wayland_Error with "failed dispatching pending events";
               end if;
            when No_Events =>
               Put_Line ("loop");
               Display.Cancel_Read;
               Timestamp := Clock;
            when Error =>
               Display.Cancel_Read;
               raise Wayland_Error with "failed checking for events";
         end case;
      end loop;

      Display.Disconnect;
   exception
      when E : others =>
         Put_Line ("Error: " & Ada.Exceptions.Exception_Message (E));
         if Display.Is_Connected then
            Display.Disconnect;
         end if;
   end Run;

end Hdante_Hello_World;
