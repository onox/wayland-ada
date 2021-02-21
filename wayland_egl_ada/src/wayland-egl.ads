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

private with Wayland.EGL_API;

with Wayland.Protocols.Client;

package Wayland.EGL is
   pragma Preelaborate;

   type Dimension is record
      Width, Height : Unsigned_32;
   end record;

   type Window is tagged limited private;

   function Is_Initialized (Object : Window) return Boolean;

   procedure Create_Window
     (Object        : in out Window;
      Surface       : Protocols.Client.Surface;
      Width, Height : Integer)
   with Pre  => not Object.Is_Initialized and Surface.Has_Proxy,
        Post =>     Object.Is_Initialized;

   procedure Destroy (Object : in out Window)
     with Pre  => Object.Is_Initialized,
          Post => not Object.Is_Initialized;

   procedure Resize
     (Object : Window;
      Size   : Dimension;
      X, Y   : Integer := 0)
   with Pre => Object.Is_Initialized;

   function Attached_Size (Object : Window) return Dimension
     with Pre => Object.Is_Initialized;

private

   type Window is tagged limited record
      Handle : EGL_API.EGL_Window_Ptr;
   end record;

end Wayland.EGL;
