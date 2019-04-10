package body C_Binding.Linux.Memory_Maps is

   procedure Get_Map_Memory
     (File       : in C_Binding.Linux.Files.File;
      Address    : Void_Ptr;
      Length     : Ada.Streams.Stream_Element_Count;
      Protection : Memory_Protection;
      Flags      : int;
      Offset     : Ada.Streams.Stream_Element_Count;
      This       : in out Memory_Map) is
   begin
      This.My_Mapping
        := C_Mmap (Address,
                   Size_Type (Length),
                   Memory_Protection_To_Prot_Flag (Protection),
                   Flags,
                   File.My_File_Descriptor,
                   Interfaces.C.long (Offset));
      This.My_Length := Size_Type (Length);
   end Get_Map_Memory;

   function Unmap_Memory (This : in out Memory_Map) return Integer is
      R : Interfaces.C.int;
   begin
      R := C_Munmap (This.My_Mapping, This.My_Length);
      if R = 0 then
         This.My_Mapping := MAP_FAILED;
      end if;
      return Integer (R);
   end Unmap_Memory;

   function Memory_Unmap
     (Address : Void_Ptr;
      Length  : Ada.Streams.Stream_Element_Count) return Integer is
   begin
      return Integer (C_Munmap (Address, Size_Type (Length)));
   end Memory_Unmap;

end C_Binding.Linux.Memory_Maps;
