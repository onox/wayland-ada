with C_Binding.Linux.Files;

package C_Binding.Linux.Memory_Maps is

   type Memory_Map is tagged limited private;

   function Has_Mapping (This : Memory_Map) return Boolean with
     Global => null;

   function Mapping (This : Memory_Map) return Void_Ptr with
     Global => null,
     Pre    => Has_Mapping (This);

   -- Returns 0 on success, otherwise -1.
   function Unmap_Memory (This : in out Memory_Map) return Integer with
     Global => null,
     Post   => (if Unmap_Memory'Result = 0 then not Has_Mapping (This));

   -- Returns 0 on success, otherwise -1.
   function Memory_Unmap (Address : Void_Ptr;
                          Length  : Size_Type) return Integer with
     Global => null;

   MAP_FAILED : constant Void_Ptr;

   -- Share changes.
   MAP_SHARED : constant := 16#01#;

   procedure Get_Map_Memory
     (File    : in C_Binding.Linux.Files.File;
      Address : Void_Ptr;
      Len     : Size_Type;
      Prot    : Prot_FLag;
      Flags   : int;
      Offset  : Linux.Offset;
      This    : in out Memory_Map) with
     Global => null,
     Pre    => not Has_Mapping (This);

private

   function Conv is new Ada.Unchecked_Conversion (Source => long,
                                                  Target => Void_Ptr);

   MAP_FAILED_VALUE : constant long     := -1;
   MAP_FAILED       : constant Void_Ptr := Conv (MAP_FAILED_VALUE);

   type Memory_Map is tagged limited record
      My_Mapping : Void_Ptr := MAP_FAILED;
      My_Length  : Size_Type;
   end record;

   function Has_Mapping
     (This : Memory_Map) return Boolean is
     (This.My_Mapping /= MAP_FAILED);

   function Mapping (This : Memory_Map) return Void_Ptr is
     (This.My_Mapping);

   function C_Mmap
     (Addr   : Void_Ptr;
      Len    : Size_Type;
      Prot   : Prot_FLag;
      Flags  : int;
      Fd     : Interfaces.C.int;
      Offset : Linux.Offset) return Void_Ptr with
     Import        => True,
     Convention    => C,
     External_Name => "mmap";

   function C_Munmap
     (Addr : Void_Ptr; Length : Size_Type) return Interfaces.C.int with
     Import        => True,
     Convention    => C,
     External_Name => "munmap";

end C_Binding.Linux.Memory_Maps;
