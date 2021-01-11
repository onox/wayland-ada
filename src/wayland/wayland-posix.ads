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

private package Wayland.Posix is
   pragma Preelaborate;

   type Poll_Mode is (Input, Output);

   function Poll
     (Descriptor : Integer;
      Timeout    : Duration) return Integer;
   --  Wait for data to become readable on the file descriptor

   function Poll
     (Descriptor : Integer;
      Timeout    : Duration;
      Mode       : Poll_Mode) return Integer;
   --  Wait for data to become readable or writable on the file descriptor

   function Error_Number return Integer;

end Wayland.Posix;
