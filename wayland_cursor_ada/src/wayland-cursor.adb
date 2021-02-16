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

package body Wayland.Cursor is

   function State (Object : Cursor_Image) return Image_State is
      Image : constant Cursor_API.Cursor_Image := Object.Handle.all;
   begin
      return
        (Width       => Natural (Image.Width),
         Height      => Natural (Image.Height),
         Hotspot_X   => Natural (Image.Hotspot_X),
         Hotspot_Y   => Natural (Image.Hotspot_Y),
         Interval    => Duration (Image.Delay_In_Milliseconds) / 1e3);
   end State;

   function Get_Buffer
     (Object : in out Cursor_Image) return Wayland.Protocols.Client.Buffer'Class is
   begin
      return Wayland.Protocols.Client.Set_Proxy
        (Cursor_API.Cursor_Image_Get_Buffer (Object.Handle));
   end Get_Buffer;

   -----------------------------------------------------------------------------

   function Index_At_Elapsed_Time
     (Object : Cursor;
      Time   : Duration) return Image_Index
   is
      Milliseconds : constant Unsigned_32 := Unsigned_32 (Time * 1e3);

      Index : constant Cursor_API.Zero_Index :=
        Cursor_API.Cursor_Frame (Object.Handle, Milliseconds);
   begin
      return Image_Index (Index) + Image_Index'First;
   end Index_At_Elapsed_Time;

   function Index_At_Elapsed_Time
     (Object : Cursor;
      Time   : Duration;
      Next   : out Duration) return Image_Index
   is
      Milliseconds : constant Unsigned_32 := Unsigned_32 (Time * 1e3);

      Milliseconds_Until_Next : Unsigned_32;

      Index : constant Cursor_API.Zero_Index :=
        Cursor_API.Cursor_Frame_And_Duration
          (Object.Handle, Milliseconds, Milliseconds_Until_Next);
   begin
      Next := Duration (Milliseconds_Until_Next) / 1e3;
      return Image_Index (Index) + Image_Index'First;
   end Index_At_Elapsed_Time;

   function Length (Object : Cursor) return Positive is (Positive (Object.Handle.Count));

   function Image (Object : Cursor; Index : Image_Index) return Cursor_Image'Class is
      use type Cursor_API.Zero_Index;

      API_Index : constant Cursor_API.Zero_Index :=
        Cursor_API.Zero_Index (Index) - Cursor_API.Zero_Index (Image_Index'First);
   begin
      if Index + 1 - Image_Index'First > Image_Index (Object.Handle.Count) then
         raise Constraint_Error;
      end if;

      declare
         Images : Cursor_API.Cursor_Image_Ptr_Array
           (0 .. Cursor_API.Zero_Index (Object.Handle.Count) - 1)
         with Address => Object.Handle.Images.all'Address, Import;
      begin
         return Cursor_Image'(Handle => Images (API_Index));
      end;
   end Image;

   -----------------------------------------------------------------------------

   use type Cursor_API.Cursor_Theme_Ptr;

   package CS renames Interfaces.C.Strings;

   function Is_Initialized (Object : Cursor_Theme) return Boolean is (Object.Handle /= null);

   procedure Load_Theme
     (Object : in out Cursor_Theme;
      Name   : String;
      Size   : Positive;
      Shm    : Wayland.Protocols.Client.Shm)
   is
      C_Name : CS.chars_ptr := CS.New_String (Name);

      Handle : constant Cursor_API.Cursor_Theme_Ptr :=
        Cursor_API.Cursor_Theme_Load
          ((if Name'Length > 0 then C_Name else CS.Null_Ptr), Size, Shm.Get_Proxy);
   begin
      CS.Free (C_Name);

      if Handle = null then
         raise Constraint_Error;
      end if;

      Object.Handle := Handle;
   end Load_Theme;

   procedure Destroy (Object : in out Cursor_Theme) is
   begin
      Cursor_API.Cursor_Theme_Destroy (Object.Handle);
      Object.Handle := null;
   end Destroy;

   function Get_Cursor
     (Object : in out Cursor_Theme;
      Name   : String) return Cursor'Class
   is
      C_Name : CS.chars_ptr := CS.New_String (Name);

      Handle : constant Cursor_API.Cursor_Ptr :=
        Cursor_API.Cursor_Theme_Get_Cursor (Object.Handle, C_Name);

      use type Cursor_API.Cursor_Ptr;
   begin
      CS.Free (C_Name);

      if Handle = null then
         raise Constraint_Error;
      end if;

      return Cursor'(Handle => Handle);
   end Get_Cursor;

end Wayland.Cursor;
