--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2018 - 2019 Joakim Strandberg <joakim@mequinox.se>
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

private with Interfaces.C.Strings;

private with Wayland.API;

package Wayland.Protocols is
   pragma Preelaborate;

   type Interface_Type is tagged limited private;

   function Name (Object : Interface_Type) return String;

   type Secret_Proxy is limited private;

private

   type Secret_Proxy is new Wayland.API.Proxy_Ptr;

   type Interface_Type is tagged limited record
      My_Interface : Wayland.API.Interface_Ptr;
   end record;

   function Name (Object : Interface_Type) return String is
     (Interfaces.C.Strings.Value (Interfaces.C.Strings.To_Chars_Ptr
        (Object.My_Interface.Name)));

end Wayland.Protocols;
