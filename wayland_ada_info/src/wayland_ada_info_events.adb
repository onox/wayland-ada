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

with Ada.Containers.Vectors;
with Ada.Characters.Latin_1;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Wayland.Enums.Client;
with Wayland.Protocols.Client;
with Wayland.Protocols.Presentation_Time;

package body Wayland_Ada_Info_Events is

   package WE renames Wayland.Enums;
   package WP renames Wayland.Protocols;

   procedure Put_Line (Value : String) renames Ada.Text_IO.Put_Line;
   procedure Put (Value : String) renames Ada.Text_IO.Put;

   package L1 renames Ada.Characters.Latin_1;
   package SU renames Ada.Strings.Unbounded;

   function "+" (Value : SU.Unbounded_String) return String renames SU.To_String;
   function "+" (Value : String) return SU.Unbounded_String renames SU.To_Unbounded_String;

   Wayland_Error : exception;

   use Wayland;
   use type SU.Unbounded_String;

   use all type WP.Client.Keyboard;
   use all type WP.Client.Seat;
   use all type WP.Client.Output;
   use type WE.Client.Shm_Format;

   Compositor : WP.Client.Compositor;
   Shm        : WP.Client.Shm;
   Display    : WP.Client.Display;
   Registry   : WP.Client.Registry;

   Presentation : WP.Presentation_Time.Presentation;

   type Interface_Data is record
      Name    : SU.Unbounded_String;
      Id      : Unsigned_32;
      Version : Unsigned_32;
   end record;

   type Seat_Data is limited record
      Keyboard : WP.Client.Keyboard;
      Seat     : WP.Client.Seat;

      Name         : SU.Unbounded_String;
      Capabilities : WE.Client.Seat_Capability := (others => False);

      Keyboard_Rate  : Integer := Integer'First;
      Keyboard_Delay : Integer := Integer'First;
   end record;

   type Output_Data is limited record
      Output : WP.Client.Output;

      --  Geometry
      X, Y            : Integer;
      Physical_Width  : Natural;
      Physical_Height : Natural;
      Subpixel        : WE.Client.Output_Subpixel;
      Make            : SU.Unbounded_String;
      Model           : SU.Unbounded_String;
      Transform       : WE.Client.Output_Transform;

      --  Mode
      Flags   : WE.Client.Output_Mode;
      Width   : Natural;
      Height  : Natural;
      Refresh : Positive;

      --  Scale
      Factor  : Positive := 1;
   end record;

   package Interface_Vectors is new Ada.Containers.Vectors
     (Positive, Interface_Data);

   package Format_Vectors is new Ada.Containers.Vectors
     (Positive, WE.Client.Shm_Format);

   Interfaces : Interface_Vectors.Vector;
   Formats    : Format_Vectors.Vector;

   --  Arbitrary maximum of 4 seats
   Seats : array (1 .. 4) of Seat_Data;
   Seat_First_Index : Natural := Seats'First;
   Seat_Last_Index  : Natural := Seats'First - 1;

   --  Arbitrary maximum of 12 outputs
   Outputs : array (1 .. 12) of Output_Data;
   Output_First_Index : Natural := Outputs'First;
   Output_Last_Index  : Natural := Outputs'First - 1;

   Clock : Unsigned_32;

   procedure Image (Data : Interface_Data) is
   begin
      Put_Line
        ("interface: '" & (+Data.Name) & "', " &
         "version:" & Data.Version'Image & ", " &
         "name:" & Data.Id'Image);
   end Image;

   procedure Image (Data : Seat_Data) is
   begin
      if not Data.Seat.Has_Proxy then
         return;
      end if;

      Put_Line (L1.HT & "name: " & (+Data.Name));

      if Data.Capabilities.Pointer or
         Data.Capabilities.Keyboard or
         Data.Capabilities.Touch
      then
         Put (L1.HT & "capabilities:");

         if Data.Capabilities.Pointer then
            Put (" pointer");
         end if;

         if Data.Capabilities.Keyboard then
            Put (" keyboard");
         end if;

         if Data.Capabilities.Touch then
            Put (" touch");
         end if;

         Put_Line ("");
      end if;

      if not Data.Keyboard.Has_Proxy then
         return;
      end if;

      Put_Line (L1.HT & "keyboard repeat rate:" & Data.Keyboard_Rate'Image);
      Put_Line (L1.HT & "keyboard repeat delay:" & Data.Keyboard_Delay'Image);
   end Image;

   procedure Image (Data : Output_Data) is
   begin
      if not Data.Output.Has_Proxy then
         return;
      end if;

      Put_Line (L1.HT &
        "x:" & Data.X'Image & ", " &
        "y:" & Data.Y'Image & ", " &
        "scale:" & Data.Factor'Image);
      Put_Line (L1.HT & "physical size:" &
        Data.Physical_Width'Image & " x" & Data.Physical_Height'Image & " mm");

      Put_Line (L1.HT & "make: '" & (+Data.Make) & "', model: '" & (+Data.Model) & "'");
      Put_Line (L1.HT &
        "subpixel_orientation: " & Data.Subpixel'Image & ", " &
        "output_transform: " & Data.Transform'Image);

      Put_Line (L1.HT & "mode:");
      Put_Line (L1.HT & L1.HT & "size:" &
        Data.Width'Image & " x" & Data.Height'Image & " px, " &
        "refresh:" & Data.Refresh'Image & " mHz");

      if Data.Flags.Current or Data.Flags.Preferred then
         Put (L1.HT & L1.HT & "flags:");

         if Data.Flags.Current then
            Put (" current");
         end if;

         if Data.Flags.Preferred then
            Put (" preferred");
         end if;

         Put_Line ("");
      end if;
   end Image;

   procedure Shm_Format
     (Shm    : in out WP.Client.Shm'Class;
      Format : WE.Client.Shm_Format) is
   begin
      Formats.Append (Format);
   end Shm_Format;

   procedure Keyboard_Repeat_Info
     (Keyboard : in out WP.Client.Keyboard'Class;
      Rate     : Integer;
      Delay_V  : Integer) is
   begin
      for E of Seats loop
         if E.Keyboard = Keyboard then
            E.Keyboard_Rate  := Rate;
            E.Keyboard_Delay := Delay_V;
         end if;
      end loop;
   end Keyboard_Repeat_Info;

   package Keyboard_Events is new WP.Client.Keyboard_Events
     (Repeat_Info => Keyboard_Repeat_Info);

   procedure Seat_Capabilities
     (Seat         : in out WP.Client.Seat'Class;
      Capabilities : WE.Client.Seat_Capability) is
   begin
      for E of Seats loop
         if E.Seat = Seat then
            E.Capabilities := Capabilities;

            if Capabilities.Keyboard then
               Seat.Get_Keyboard (E.Keyboard);

               if not E.Keyboard.Has_Proxy then
                  raise Wayland_Error with "No keyboard";
               end if;

               if Keyboard_Events.Subscribe (E.Keyboard) = Error then
                  E.Keyboard.Destroy;
                  raise Wayland_Error with "Failed to subscribe to keyboard events";
               end if;
            end if;
         end if;
      end loop;
   end Seat_Capabilities;

   procedure Seat_Name
     (Seat : in out WP.Client.Seat'Class;
      Name : String) is
   begin
      for E of Seats loop
         if E.Seat = Seat then
            E.Name := +Name;
         end if;
      end loop;
   end Seat_Name;

   procedure Output_Geometry
     (Output          : in out WP.Client.Output'Class;
      X, Y            : Integer;
      Physical_Width  : Integer;
      Physical_Height : Integer;
      Subpixel        : WE.Client.Output_Subpixel;
      Make            : String;
      Model           : String;
      Transform       : WE.Client.Output_Transform) is
   begin
      for E of Outputs loop
         if E.Output = Output then
            E.X := X;
            E.Y := Y;
            E.Physical_Width := Physical_Width;
            E.Physical_Height := Physical_Height;
            E.Subpixel := Subpixel;
            E.Make := +Make;
            E.Model := +Model;
            E.Transform := Transform;
         end if;
      end loop;
   end Output_Geometry;

   procedure Output_Mode
     (Output  : in out WP.Client.Output'Class;
      Flags   : WE.Client.Output_Mode;
      Width   : Integer;
      Height  : Integer;
      Refresh : Integer) is
   begin
      for E of Outputs loop
         if E.Output = Output then
            E.Flags := Flags;
            E.Width := Width;
            E.Height := Height;
            E.Refresh := Refresh;
         end if;
      end loop;
   end Output_Mode;

   procedure Output_Scale
     (Output : in out WP.Client.Output'Class;
      Factor : Integer) is
   begin
      for E of Outputs loop
         if E.Output = Output then
            E.Factor := Factor;
         end if;
      end loop;
   end Output_Scale;

   procedure Presentation_Clock
     (Presentation : in out WP.Presentation_Time.Presentation'Class;
      Id           : Unsigned_32) is
   begin
      Clock := Id;
   end Presentation_Clock;

   package Shm_Events is new WP.Client.Shm_Events
     (Format => Shm_Format);

   package Seat_Events is new WP.Client.Seat_Events
     (Seat_Capabilities => Seat_Capabilities,
      Seat_Name         => Seat_Name);

   package Output_Events is new WP.Client.Output_Events
     (Geometry => Output_Geometry,
      Mode     => Output_Mode,
      Scale    => Output_Scale);

   package Presentation_Events is new WP.Presentation_Time.Presentation_Events
     (Clock => Presentation_Clock);

   procedure Global_Registry_Handler
     (Registry   : in out WP.Client.Registry'Class;
      Id         : Unsigned_32;
      Name       : String;
      Version    : Unsigned_32) is
   begin
      Interfaces.Append ((Name => +Name, Id => Id, Version => Version));

      if Name = WP.Client.Compositor_Interface.Name then
         Compositor.Bind (Registry, Id, Unsigned_32'Min (Version, 4));
      elsif Name = WP.Client.Shm_Interface.Name then
         Shm.Bind (Registry, Id, Unsigned_32'Min (Version, 1));

         if not Shm.Has_Proxy then
            raise Wayland_Error with "No shm";
         end if;

         if Shm_Events.Subscribe (Shm) = Error then
            Shm.Destroy;
            raise Wayland_Error with "Failed to subscribe to shm events";
         end if;
      elsif Name = WP.Client.Seat_Interface.Name then
         declare
            Seat : WP.Client.Seat renames Seats (Seat_Last_Index + 1).Seat;
         begin
            Seat.Bind (Registry, Id, Unsigned_32'Min (Version, 6));

            if not Seat.Has_Proxy then
               raise Wayland_Error with "No seat";
            end if;

            if Seat_Events.Subscribe (Seat) = Error then
               Seat.Destroy;
               raise Wayland_Error with "Failed to subscribe to seat events";
            end if;

            Seat_Last_Index := Seat_Last_Index + 1;
         end;
      elsif Name = WP.Client.Output_Interface.Name then
         declare
            Output : WP.Client.Output renames Outputs (Output_Last_Index + 1).Output;
         begin
            Output.Bind (Registry, Id, Unsigned_32'Min (Version, 3));

            if not Output.Has_Proxy then
               raise Wayland_Error with "No output";
            end if;

            if Output_Events.Subscribe (Output) = Error then
               Output.Destroy;
               raise Wayland_Error with "Failed to subscribe to output events";
            end if;

            Output_Last_Index := Output_Last_Index + 1;
         end;
      elsif Name = WP.Presentation_Time.Presentation_Interface.Name then
         Presentation.Bind (Registry, Id, Unsigned_32'Min (Version, 1));

         if not Presentation.Has_Proxy then
            raise Wayland_Error with "No presentation";
         end if;

         if Presentation_Events.Subscribe (Presentation) = Error then
            Presentation.Destroy;
            raise Wayland_Error with "Failed to subscribe to presentation events";
         end if;
      end if;
   end Global_Registry_Handler;

   package Registry_Events is new WP.Client.Registry_Events
     (Global_Object_Added => Global_Registry_Handler);

   procedure Run is
   begin
      Display.Connect;

      if not Display.Is_Connected then
         raise Wayland_Error with "Not connected to display";
      end if;

      Display.Get_Registry (Registry);

      if not Registry.Has_Proxy then
         raise Wayland_Error with "No global registry";
      end if;

      if Registry_Events.Subscribe (Registry) = Error then
         Registry.Destroy;
         raise Wayland_Error with "Failed to subscribe to registry events";
      end if;

      Display.Roundtrip;
      Display.Roundtrip;
      Display.Roundtrip;

      for E of Interfaces loop
         Image (E);

         if E.Name = WP.Client.Shm_Interface.Name then
            if not Formats.Is_Empty then
               Put (L1.HT & "formats:");
               for Format of Formats loop
                  Put (" " & Format'Image);
               end loop;
               Put_Line ("");
            end if;
         elsif E.Name = WP.Client.Seat_Interface.Name then
            Image (Seats (Seat_First_Index));

            pragma Assert (Seat_First_Index <= Seat_Last_Index);
            Seat_First_Index := Seat_First_Index + 1;
         elsif E.Name = WP.Client.Output_Interface.Name then
            Image (Outputs (Output_First_Index));

            pragma Assert (Output_First_Index <= Output_Last_Index);
            Output_First_Index := Output_First_Index + 1;
         elsif E.Name = WP.Presentation_Time.Presentation_Interface.Name then
            Put_Line (L1.HT & "presentation clock id:" & Clock'Image);
         end if;
      end loop;

      Registry.Destroy;
      Display.Disconnect;
   exception
      when E : others =>
         Put_Line ("Error: " & Ada.Exceptions.Exception_Message (E));
         if Display.Is_Connected then
            Display.Disconnect;
         end if;
   end Run;

end Wayland_Ada_Info_Events;
