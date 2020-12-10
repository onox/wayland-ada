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

package body C_Binding.Linux.Memory_Maps is

   procedure Get_Map_Memory
     (File       : in C_Binding.Linux.Files.File;
      Address    : Void_Ptr;
      Length     : Ada.Streams.Stream_Element_Count;
      Protection : Memory_Protection;
      Flags      : Integer;
      Offset     : Ada.Streams.Stream_Element_Count;
      This       : in out Memory_Map)
   is
      Result : Void_Ptr;

      function Convert is new Ada.Unchecked_Conversion
        (Source => long, Target => Void_Ptr);

      MAP_FAILED : Void_Ptr := Convert (MAP_FAILED_VALUE);
   begin
      Result
        := C_Mmap (Address,
                   Size_Type (Length),
                   Memory_Protection_To_Prot_Flag (Protection),
                   Interfaces.C.int (Flags),
                   File.My_File_Descriptor,
                   Interfaces.C.long (Offset));
      if Result /= MAP_FAILED then
         pragma Assert (Result /= System.Null_Address);
         This.Mapping := Result;
         This.Length  := Size_Type (Length);
      else
         This.Mapping := System.Null_Address;
         This.Length  := 0;
      end if;
   end Get_Map_Memory;

   function Unmap_Memory (This : in out Memory_Map) return Error_Code is
      Result : Interfaces.C.int;
   begin
      Result := C_Munmap (This.Mapping, This.Length);
      if Result = 0 then
         This.Mapping := System.Null_Address;
         This.Length  := 0;
      end if;
      return Error_Code (Result);
   end Unmap_Memory;

   procedure Unmap_Memory (This : in out Memory_Map) is
      Result : constant Error_Code := This.Unmap_Memory;
   begin
      pragma Assert (Result = 0);
   end Unmap_Memory;

   function Memory_Unmap
     (Address : Void_Ptr;
      Length  : Ada.Streams.Stream_Element_Count) return Error_Code is
   begin
      return Error_Code (C_Munmap (Address, Size_Type (Length)));
   end Memory_Unmap;

end C_Binding.Linux.Memory_Maps;
