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

with C_Binding.Linux.Files;

package C_Binding.Linux.Memory_Maps is

   type Memory_Map is tagged limited private;

   function Has_Mapping (This : Memory_Map) return Boolean
     with Global => null;

   function Mapping (This : Memory_Map) return Void_Ptr
     with Global => null,
          Pre    => Has_Mapping (This);

   type Error_Code is new Integer;

   function Unmap_Memory (This : in out Memory_Map) return Error_Code
     with Global => null,
          Pre    => This.Has_Mapping,
          Post   => (if Unmap_Memory'Result = 0 then not This.Has_Mapping);

   procedure Unmap_Memory (This : in out Memory_Map)
     with Global => null,
          Pre    => This.Has_Mapping,
          Post   => not This.Has_Mapping;

   function Memory_Unmap
     (Address : Void_Ptr;
      Length  : Ada.Streams.Stream_Element_Count) return Error_Code
   with Global => null;

   MAP_FAILED : constant Void_Ptr;

   MAP_SHARED : constant := 16#01#;

   type Memory_Protection is
     (Page_Can_Be_Read,
      Page_Can_Be_Read_And_Written);

   procedure Get_Map_Memory
     (File       : in C_Binding.Linux.Files.File;
      Address    : Void_Ptr;
      Length     : Ada.Streams.Stream_Element_Count;
      Protection : Memory_Protection;
      Flags      : Integer;
      Offset     : Ada.Streams.Stream_Element_Count;
      This       : in out Memory_Map)
   with Global => null,
        Pre    => not Has_Mapping (This);

private

   MS_ASYNC : constant := 1;

   MS_SYNC : constant := 4;

   MS_INVALIDATE : constant := 2;

   PROT_READ      : constant Prot_FLag := 16#1#;
   PROT_WRITE     : constant Prot_FLag := 16#2#;
   PROT_EXEC      : constant Prot_FLag := 16#4#;
   PROT_NONE      : constant Prot_FLag := 16#0#;
   PROT_GROWSDOWN : constant Prot_FLag := 16#01000000#;
   PROT_GROWSUP   : constant Prot_FLag := 16#02000000#;

   type Memory_Protection_To_Prot_Flag_Array is
     array (Memory_Protection) of Prot_FLag;

   Memory_Protection_To_Prot_Flag : constant Memory_Protection_To_Prot_Flag_Array
     := (Page_Can_Be_Read             => PROT_READ,
         Page_Can_Be_Read_And_Written => PROT_READ and PROT_WRITE);

   function Conv is new Ada.Unchecked_Conversion
     (Source => long, Target => Void_Ptr);

   MAP_FAILED_VALUE : constant long     := -1;
   MAP_FAILED       : constant Void_Ptr := Conv (MAP_FAILED_VALUE);

   type Memory_Map is tagged limited record
      My_Mapping : Void_Ptr := MAP_FAILED;
      My_Length  : Size_Type;
   end record;

   function Has_Mapping (This : Memory_Map) return Boolean is
     (This.My_Mapping /= MAP_FAILED);

   function Mapping (This : Memory_Map) return Void_Ptr is
     (This.My_Mapping);

   function C_Mmap
     (Addr   : Void_Ptr;
      Len    : Size_Type;
      Prot   : Prot_FLag;
      Flags  : int;
      Fd     : Interfaces.C.int;
      Offset : Interfaces.C.long) return Void_Ptr
   with Import, Convention => C, External_Name => "mmap";

   function C_Munmap
     (Addr   : Void_Ptr;
      Length : Size_Type) return Interfaces.C.int
   with Import, Convention => C, External_Name => "munmap";

end C_Binding.Linux.Memory_Maps;
