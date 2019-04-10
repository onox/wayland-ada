package body C_Binding.Linux.Memory_Maps is

   procedure Get_Map_Memory
     (File    : in C_Binding.Linux.Files.File;
      Address : Void_Ptr;
      Len     : Size_Type;
      Prot    : Prot_FLag;
      Flags   : int;
      Offset  : Linux.Offset;
      This    : in out Memory_Map) is
   begin
      This.My_Mapping
        := C_Mmap (Address,
                   Len,
                   Prot,
                   Flags,
                   File.My_File_Descriptor,
                   Offset);
      This.My_Length := Len;
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

   function Memory_Unmap (Address : Void_Ptr;
                          Length  : Size_Type) return Integer is
   begin
      return Integer (C_Munmap (Address, Length));
   end Memory_Unmap;

end C_Binding.Linux.Memory_Maps;
