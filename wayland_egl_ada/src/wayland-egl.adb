--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2020 onox <denkpadje@gmail.com>
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

package body Wayland.EGL is

   use type EGL_API.EGL_Window_Ptr;

   function Is_Initialized (Object : Window) return Boolean is (Object.Handle /= null);

   function Create_Window
     (Surface       : Protocols.Client.Surface;
      Width, Height : Integer) return Window is
   begin
      return Result : Window do
         Result.Handle := EGL_API.Window_Create (Surface.Get_Proxy, Width, Height);
      end return;
   end Create_Window;

   procedure Destroy (Object : in out Window) is
   begin
      EGL_API.Window_Destroy (Object.Handle);
      Object.Handle := null;
   end Destroy;

   procedure Resize
     (Object : Window;
      Size   : Dimension;
      X, Y   : Integer := 0) is
   begin
      EGL_API.Window_Resize (Object.Handle, Integer (Size.Width), Integer (Size.Height), X, Y);
   end Resize;

   function Attached_Size (Object : Window) return Dimension is
      Width, Height : Integer;
   begin
      EGL_API.Get_Attached_Size (Object.Handle, Width, Height);

      return (Width => Unsigned_32 (Width), Height => Unsigned_32 (Height));
   end Attached_Size;

end Wayland.EGL;