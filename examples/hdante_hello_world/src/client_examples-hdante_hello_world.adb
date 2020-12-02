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

with Ada.Real_Time;
with Ada.Text_IO;

with C_Binding.Linux.Files;
with C_Binding.Linux.File_Status;
with C_Binding.Linux.Memory_Maps;

with Wayland.Client.Protocol;

-- sudo apt install libwayland-dev
-- This is a wayland hello world application. It uses the wayland
-- client library and the wayland protocol to display a window.
--
-- Original code that was translated from C to Ada:
-- https://github.com/hdante/hello_wayland
-- https://hdante.wordpress.com/2014/07/08/the-hello-wayland-tutorial/
--
-- When compiled go to the .../bin directory
-- and execute the executable from there.
package body Client_Examples.Hdante_Hello_World is

   package Wayland_Client renames Wayland.Client.Protocol;

   procedure Put_Line (Value : String) renames Ada.Text_IO.Put_Line;

   subtype Unsigned_32 is Wayland.Unsigned_32;
   subtype Fixed is Wayland.Fixed;

   use type Unsigned_32;

   package Linux renames C_Binding.Linux;

   use all type Linux.Files.File_Mode;
   use all type Linux.Files.File_Permission;
   use all type Linux.Files.File;
   use all type Linux.File_Status.Status;
   use all type Linux.Memory_Maps.Memory_Map;
   use all type Linux.Memory_Maps.Memory_Protection;

   use all type Wayland_Client.Check_For_Events_Status;
   use all type Wayland_Client.Call_Result_Code;

   use all type Ada.Real_Time.Time;
   use all type Ada.Real_Time.Time_Span;

   Done : Boolean := false;

   type Data_Type is limited record
      Compositor : aliased Wayland_Client.Compositor;
      Pointer    : aliased Wayland_Client.Pointer;
      Seat       : aliased Wayland_Client.Seat;
      Shell      : aliased Wayland_Client.Shell;
      Shm        : aliased Wayland_Client.Shm;
   end record;

   type Data_Ptr is access all Data_Type;

   Data : aliased Data_Type;

   function Min (L, R : Unsigned_32) return Unsigned_32 renames
     Unsigned_32'Min;

   Exists_Mouse    : Boolean := False;
   Exists_Keyboard : Boolean := False;

   procedure Seat_Capabilities
     (Data         : not null Data_Ptr;
      Seat         : Wayland_Client.Seat;
      Capabilities : Unsigned_32) is
   begin
      if (Capabilities and Wayland_Client.Seat_Capability_Pointer) > 0 then
         Put_Line ("Display has a pointer");
         Exists_Mouse := True;
      end if;

      if (Capabilities and Wayland_Client.Seat_Capability_Keyboard) > 0 then
         Put_Line ("Display has a keyboard");
         Exists_Keyboard := True;
      end if;

      if (Capabilities and Wayland_Client.Seat_Capability_Touch) > 0 then
         Put_Line ("Display has a touch screen");
      end if;
   end Seat_Capabilities;

   procedure Seat_Name
     (Data : not null Data_Ptr;
      Seat : Wayland_Client.Seat;
      Name : String) is
   begin
      null;
   end Seat_Name;

   package Seat_Events is new Wayland_Client.Seat_Events
     (Data_Type         => Data_Type,
      Data_Ptr          => Data_Ptr,
      Seat_Capabilities => Seat_Capabilities,
      Seat_Name         => Seat_Name);

   procedure Global_Registry_Handler (Data     : not null Data_Ptr;
                                      Registry : Wayland_Client.Registry;
                                      Id       : Unsigned_32;
                                      Name     : String;
                                      Version  : Unsigned_32)
   is
      Call_Result : Wayland_Client.Call_Result_Code;
   begin
      if Name = Wayland_Client.Compositor_Interface.Name then
         Data.Compositor.Get_Proxy (Registry,
                               Id,
                               Min (Version, 4));
         Put_Line ("Got compositor proxy");
      elsif Name = Wayland_Client.Shm_Interface.Name then
         Data.Shm.Get_Proxy (Registry,
                        Id,
                        Min (Version, 1));
         Put_Line ("Got shm proxy");
      elsif Name = Wayland_Client.Shell_Interface.Name then
         Data.Shell.Get_Proxy (Registry,
                          Id,
                          Min (Version, 1));
         Put_Line ("Got shell proxy");
      elsif Name = Wayland_Client.Seat_Interface.Name then
         Data.Seat.Get_Proxy (Registry,
                         Id,
                         Min (Version, 2));

         Call_Result := Seat_Events.Subscribe (Data.Seat, Data);

         case Call_Result is
            when Success => Put_Line ("Successfully subscribed to seat events");
            when Error   => Put_Line ("Failed to subscribe to seat events");
         end case;
      end if;
   end Global_Registry_Handler;

   procedure Global_Registry_Remover (Data     : not null Data_Ptr;
                                      Registry : Wayland_Client.Registry;
                                      Id       : Unsigned_32) is
   begin
      Put_Line ("Got a registry losing event for" & Id'Image);
   end Global_Registry_Remover;

   package Registry_Events is new Wayland_Client.Registry_Events
     (Data_Type             => Data_Type,
      Data_Ptr              => Data_Ptr,
      Global_Object_Added   => Global_Registry_Handler,
      Global_Object_Removed => Global_Registry_Remover);

   procedure Shell_Surface_Ping
     (Data    : not null Data_Ptr;
      Surface : Wayland_Client.Shell_Surface;
      Serial  : Unsigned_32) is
   begin
      Surface.Pong (Serial);
   end Shell_Surface_Ping;

   procedure Shell_Surface_Configure
     (Data    : not null Data_Ptr;
      Surface : Wayland_Client.Shell_Surface;
      Edges   : Unsigned_32;
      Width   : Integer;
      Height  : Integer) is
   begin
      null;
   end Shell_Surface_Configure;

   procedure Shell_Surface_Popup_Done
     (Data    : not null Data_Ptr;
      Surface : Wayland_Client.Shell_Surface) is
   begin
      null;
   end Shell_Surface_Popup_Done;

   package Shell_Surface_Events is new Wayland_Client.Shell_Surface_Events
     (Data_Type                => Data_Type,
      Data_Ptr                 => Data_Ptr,
      Shell_Surface_Ping       => Shell_Surface_Ping,
      Shell_Surface_Configure  => Shell_Surface_Configure,
      Shell_Surface_Popup_Done => Shell_Surface_Popup_Done);

   procedure Mouse_Enter
     (Data      : not null Data_Ptr;
      Pointer   : Wayland_Client.Pointer;
      Serial    : Unsigned_32;
      Surface   : Wayland_Client.Surface;
      Surface_X : Fixed;
      Surface_Y : Fixed) is
   begin
      Put_Line ("Pointer enter");
   end Mouse_Enter;

   procedure Pointer_Leave
     (Data    : not null Data_Ptr;
      Pointer : Wayland_Client.Pointer;
      Serial  : Unsigned_32;
      Surface : Wayland_Client.Surface) is
   begin
      Put_Line ("Pointer leave");
   end Pointer_Leave;

   procedure Pointer_Motion
     (Data      : not null Data_Ptr;
      Pointer   : Wayland_Client.Pointer;
      Time      : Unsigned_32;
      Surface_X : Fixed;
      Surface_Y : Fixed) is
   begin
      Put_Line ("Pointer motion");
   end Pointer_Motion;

   procedure Pointer_Button
     (Data    : not null Data_Ptr;
      Pointer : Wayland_Client.Pointer;
      Serial  : Unsigned_32;
      Time    : Unsigned_32;
      Button  : Unsigned_32;
      State   : Unsigned_32) is
   begin
      Put_Line ("Pointer button");
      Done := True;
   end Pointer_Button;

   procedure Pointer_Axis
     (Data    : not null Data_Ptr;
      Pointer : Wayland_Client.Pointer;
      Time    : Unsigned_32;
      Axis    : Unsigned_32;
      Value   : Fixed) is
   begin
      Put_Line ("Pointer axis");
   end Pointer_Axis;

   procedure Pointer_Frame (Data    : not null Data_Ptr;
                            Pointer : Wayland_Client.Pointer) is
   begin
      Put_Line ("Pointer frame");
   end Pointer_Frame;

   procedure Pointer_Axis_Source
     (Data        : not null Data_Ptr;
      Pointer     : Wayland_Client.Pointer;
      Axis_Source : Unsigned_32) is
   begin
      Put_Line ("Pointer axis source");
   end Pointer_Axis_Source;

   procedure Pointer_Axis_Stop
     (Data    : not null Data_Ptr;
      Pointer : Wayland_Client.Pointer;
      Time    : Unsigned_32;
      Axis    : Unsigned_32) is
   begin
      Put_Line ("Pointer axis stop");
   end Pointer_Axis_Stop;

   procedure Pointer_Axis_Discrete
     (Data     : not null Data_Ptr;
      Pointer  : Wayland_Client.Pointer;
      Axis     : Unsigned_32;
      Discrete : Integer) is
   begin
      Put_Line ("Pointer axis discrete");
   end Pointer_Axis_Discrete;

   package Mouse_Events is new Wayland_Client.Pointer_Events
     (Data_Type             => Data_Type,
      Data_Ptr              => Data_Ptr,
      Pointer_Enter         => Mouse_Enter,
      Pointer_Leave         => Pointer_Leave,
      Pointer_Motion        => Pointer_Motion,
      Pointer_Button        => Pointer_Button,
      Pointer_Axis          => Pointer_Axis,
      Pointer_Frame         => Pointer_Frame,
      Pointer_Axis_Source   => Pointer_Axis_Source,
      Pointer_Axis_Stop     => Pointer_Axis_Stop,
      Pointer_Axis_Discrete => Pointer_Axis_Discrete);

   Display    : Wayland_Client.Display;
   Registry   : Wayland_Client.Registry;

   WIDTH : constant := 320;
   HEIGHT : constant := 200;
   --     CURSOR_WIDTH : constant := 100;
   --     CURSOR_HEIGHT : constant := 59;
   --     CURSOR_HOT_SPOT_X : constant := 10;
   --     CURSOR_HOT_SPOT_Y : constant := 35;

   Buffer        : Wayland_Client.Buffer;
   Pool          : Wayland_Client.Shm_Pool;
   Surface       : Wayland_Client.Surface;
   Shell_Surface : Wayland_Client.Shell_Surface;
   Image         : Linux.Files.File;
   File_Status   : Linux.File_Status.Status;

   File_Name : String := "hello_world_image.bin";

   Memory_Map : Linux.Memory_Maps.Memory_Map;

   Events_Status : Wayland_Client.Check_For_Events_Status;

   Status_Code : Integer;

   Timestamp : Ada.Real_Time.Time := Clock;

   Call_Result : Wayland_Client.Call_Result_Code;

   procedure Run is
   begin
      Display.Connect;
      if not Display.Is_Connected then
         Put_Line ("Can't connect to display");
         return;
      end if;
      Put_Line ("Connected to display");

      Display.Get_Registry (Registry);
      if not Registry.Has_Proxy then
         Put_Line ("Can't get global registry object");
         return;
      end if;

      Call_Result := Registry_Events.Subscribe (Registry, Data'Access);
      case Call_Result is
         when Success => null;
         when Error   =>
            Put_Line ("Failed to subscribe to registry events");
            Display.Disconnect;
            return;
      end case;

      Display.Dispatch;
      Display.Roundtrip;
      Registry.Destroy;

      if not Data.Compositor.Has_Proxy then
         Put_Line ("Error: no compositor");
         Display.Disconnect;
         return;
      end if;

      if not Data.Shm.Has_Proxy then
         Put_Line ("Error: no shm");
         Display.Disconnect;
         return;
      end if;

      if not Data.Shell.Has_Proxy then
         Put_Line ("Error: no shell");
         Display.Disconnect;
         return;
      end if;

      if Exists_Mouse then
         Put_Line ("Start mouse subscription");
         Data.Seat.Get_Pointer (Data.Pointer);
         Call_Result := Mouse_Events.Subscribe (Data.Pointer, Data'Access);

         case Call_Result is
            when Success =>
               Put_Line ("Successfully subscribed to mouse events");
            when Error   =>
               Put_Line ("Failed to subscribe to mouse events");
               Display.Disconnect;
               return;
         end case;
      end if;

      Linux.Files.Open
        (Image,
         File_Name,
         Mode        => Read_Write,
         Permissions => (
                         Owner_Read  => True,
                         Group_Read  => True,
                         Others_Read => True,
                         others      => False
                        ));

      if Linux.Files.Is_Closed (Image) then
         Put_Line ("Error opening surface image");
         Display.Disconnect;
         return;
      end if;

      Get_File_Status (Image, File_Status);

      if not Is_Valid (File_Status) then
         Put_Line ("File does not exist?");
         Display.Disconnect;
         return;
      end if;

      Get_Map_Memory
        (Image,
         System.Null_Address,
         Size (File_Status),
         Page_Can_Be_Read,
         Linux.Memory_Maps.MAP_SHARED, 0, Memory_Map);

      if Memory_Map.Has_Mapping then
         Data.Shm.Create_Pool (Image,
                          Integer (Size (File_Status)),
                          Pool);
      else
         Put_Line ("Failed to map file");
         Display.Disconnect;
         return;
      end if;

      if not Pool.Has_Proxy then
         Put_Line ("Failed to create pool");
         Display.Disconnect;
         return;
      end if;

      Data.Compositor.Get_Surface_Proxy (Surface);

      if not Surface.Has_Proxy then
         Put_Line ("Failed to create surface");
         Display.Disconnect;
         return;
      end if;

      Wayland_Client.Get_Shell_Surface (Data.Shell, Surface, Shell_Surface);

      if not Shell_Surface.Has_Proxy then
         Surface.Destroy;
         Put_Line ("Failed to create shell surface");
         return;
      end if;

      Call_Result := Shell_Surface_Events.Subscribe (Shell_Surface,
                                                     Data'Access);

      case Call_Result is
         when Success =>
            Put_Line ("Successfully subscribed to shell surface events");
         when Error   =>
            Put_Line ("Failed to subscribe to shell surface events");
            Display.Disconnect;
            return;
      end case;

      Shell_Surface.Set_Toplevel;

      Pool.Create_Buffer (0,
                          Integer (Width),
                          Integer (Height),
                          Integer (Width)*4,
                          Wayland_Client.Shm_Format_Argb_8888,
                          Buffer);

      if not Buffer.Has_Proxy then
         Put_Line ("Failed to create buffer");
         Display.Disconnect;
         return;
      end if;

      Surface.Attach (Buffer, 0, 0);
      Surface.Commit;

      Display.Dispatch;

      while not Done loop
         while Display.Prepare_Read /= 0 loop
            Status_Code := Display.Dispatch_Pending;
            if Status_Code = -1 then
               Put_Line ("Failed dispatch pending");
            end if;
         end loop;

         declare
            Interval : Ada.Real_Time.Time_Span
              := Ada.Real_Time.To_Time_Span (1.0);

            Timeout : constant Integer
              := Integer (To_Duration ((Timestamp + Interval) - Clock) * 1_000);
            --  The timeout to check for events in millisends
         begin
            Events_Status := Display.Check_For_Events (if Timeout > 0 then Timeout else 0);
         end;

         case Events_Status is
            when Events_Need_Processing =>
               Call_Result := Display.Read_Events;
               if Call_Result = Error then
                  Put_Line ("Failed read events");
               end if;

               Status_Code := Display.Dispatch_Pending;
               if Status_Code = -1 then
                  Put_Line ("Failed dispatch pending 2");
               end if;

            when No_Events =>
               Put_Line ("loop");
               Display.Cancel_Read;
               Timestamp := Clock;
            when Error =>
               Put_Line ("Error checking for events");
               Done := True;
               Display.Cancel_Read;
         end case;
      end loop;
   end Run;

end Client_Examples.Hdante_Hello_World;
