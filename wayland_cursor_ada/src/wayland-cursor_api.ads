--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2021 onox <denkpadje@gmail.com>
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

with Interfaces.C.Strings;

with Wayland.Protocols;

private package Wayland.Cursor_API is
   pragma Preelaborate;

   pragma Linker_Options ("-lwayland-cursor");

   type Cursor_Image is record
      Width, Height, Hotspot_X, Hotspot_Y, Delay_In_Milliseconds : aliased Unsigned_32;
   end record
     with Convention => C_Pass_By_Copy;

   type Cursor_Image_Ptr is access constant Cursor_Image;

   type Zero_Index is range 0 .. Natural'Last;

   type Cursor_Image_Ptr_Array is array (Zero_Index range <>) of aliased not null Cursor_Image_Ptr
     with Convention => C;

   type Cursor is record
      Count  : aliased Interfaces.C.unsigned;
      Images : access Cursor_Image_Ptr;
      Name   : Interfaces.C.Strings.chars_ptr;
   end record
     with Convention => C_Pass_By_Copy;

   type Cursor_Theme is limited private;

   type Cursor_Theme_Ptr is access all Cursor_Theme
     with Convention => C;

   type Cursor_Ptr is access all Cursor
     with Convention => C;

   function Cursor_Theme_Load
     (Name : Interfaces.C.Strings.chars_ptr;
      Size : Integer;
      Shm  : Wayland.Protocols.Secret_Proxy) return Cursor_Theme_Ptr
   with Import, Convention => C, External_Name => "wl_cursor_theme_load";

   procedure Cursor_Theme_Destroy
     (Object : not null Cursor_Theme_Ptr)
   with Import, Convention => C, External_Name => "wl_cursor_theme_destroy";

   function Cursor_Theme_Get_Cursor
     (Object : not null Cursor_Theme_Ptr;
      Name   : Interfaces.C.Strings.chars_ptr) return Cursor_Ptr
   with Import, Convention => C, External_Name => "wl_cursor_theme_get_cursor";

   function Cursor_Image_Get_Buffer
     (Object : not null Cursor_Image_Ptr) return Wayland.Protocols.Secret_Proxy
   with Import, Convention => C, External_Name => "wl_cursor_image_get_buffer";

   function Cursor_Frame
     (Object : not null Cursor_Ptr;
      Time   : Unsigned_32) return Zero_Index
   with Import, Convention => C, External_Name => "wl_cursor_frame";

   function Cursor_Frame_And_Duration
     (Object   : not null Cursor_Ptr;
      Time     : Unsigned_32;
      Duration : out Unsigned_32) return Zero_Index
   with Import, Convention => C, External_Name => "wl_cursor_frame_and_duration";

private

   type Cursor_Theme is limited null record;

end Wayland.Cursor_API;
