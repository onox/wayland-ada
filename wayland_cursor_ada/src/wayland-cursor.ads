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

private with Wayland.Cursor_API;

with Wayland.Protocols.Client;

package Wayland.Cursor is
   pragma Preelaborate;

   subtype Image_Index is Positive;

   -----------------------------------------------------------------------------

   type Cursor_Image is tagged limited private;

   type Image_State is record
      Width, Height, Hotspot_X, Hotspot_Y : Natural;
      Interval                            : Duration;
   end record;

   function State (Object : Cursor_Image) return Image_State;

   function Get_Buffer
     (Object : in out Cursor_Image) return Wayland.Protocols.Client.Buffer'Class
   with Post => Get_Buffer'Result.Has_Proxy;
   --  Return the buffer of the image
   --
   --  The buffer can be attached to a separate surface. After the surface
   --  has been committed, it can be set via the procedure Set_Cursor of a
   --  Pointer object.
   --
   --  The buffer must not be destroyed.

   -----------------------------------------------------------------------------

   type Cursor is tagged limited private;

   function Index_At_Elapsed_Time
     (Object : Cursor;
      Time   : Duration) return Image_Index;

   function Index_At_Elapsed_Time
     (Object : Cursor;
      Time   : Duration;
      Next   : out Duration) return Image_Index;

   function Length (Object : Cursor) return Positive;

   function Image (Object : Cursor; Index : Image_Index) return Cursor_Image'Class;

   -----------------------------------------------------------------------------

   type Cursor_Theme is tagged limited private;

   function Is_Initialized (Object : Cursor_Theme) return Boolean;

   procedure Load_Theme
     (Object : in out Cursor_Theme;
      Name   : String;
      Size   : Positive;
      Shm    : Wayland.Protocols.Client.Shm)
   with Pre  => not Object.Is_Initialized and Shm.Has_Proxy,
        Post =>     Object.Is_Initialized;

   procedure Destroy (Object : in out Cursor_Theme)
     with Pre  => Object.Is_Initialized,
          Post => not Object.Is_Initialized;

   function Get_Cursor
     (Object : in out Cursor_Theme;
      Name   : String) return Cursor'Class
   with Pre => Object.Is_Initialized;

private

   type Cursor_Image is tagged limited record
      Handle : not null Cursor_API.Cursor_Image_Ptr;
   end record;

   type Cursor is tagged limited record
      Handle : not null Cursor_API.Cursor_Ptr;
   end record;

   type Cursor_Theme is tagged limited record
      Handle : Cursor_API.Cursor_Theme_Ptr;
   end record;

end Wayland.Cursor;
