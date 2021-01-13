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

with Wayland.Protocols;

private package Wayland.EGL_API is
   pragma Preelaborate;

   pragma Linker_Options ("-lwayland-egl");

   type EGL_Window is limited private;

   type EGL_Window_Ptr is access all EGL_Window
     with Convention => C;

   function Window_Create
     (Surface       : Protocols.Secret_Proxy;
      Width, Height : Integer) return EGL_Window_Ptr
   with Import, Convention => C, External_Name => "wl_egl_window_create";

   procedure Window_Destroy (Object : EGL_Window_Ptr)
     with Import, Convention => C, External_Name => "wl_egl_window_destroy";

   procedure Window_Resize
     (Object        : EGL_Window_Ptr;
      Width, Height : Integer;
      Dx, Dy        : Integer)
   with Import, Convention => C, External_Name => "wl_egl_window_resize";

   procedure Get_Attached_Size
     (Object        : EGL_Window_Ptr;
      Width, Height : out Integer)
   with Import, Convention => C, External_Name => "wl_egl_window_get_attached_size";

private

   type EGL_Window is limited null record;

end Wayland.EGL_API;
