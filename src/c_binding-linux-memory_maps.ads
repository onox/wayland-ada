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
   function Memory_Unmap
     (Address : Void_Ptr;
      Length  : Ada.Streams.Stream_Element_Count) return Integer with
     Global => null;

   MAP_FAILED : constant Void_Ptr;

   -- Share changes.
   MAP_SHARED : constant := 16#01#;

   type Memory_Protection is
     (
      Page_Can_Be_Read,
      Page_Can_Be_Read_And_Written
     );

   procedure Get_Map_Memory
     (File       : in C_Binding.Linux.Files.File;
      Address    : Void_Ptr;
      Length     : Ada.Streams.Stream_Element_Count;
      Protection : Memory_Protection;
      Flags      : Integer;
      Offset     : Ada.Streams.Stream_Element_Count;
      This       : in out Memory_Map) with
     Global => null,
     Pre    => not Has_Mapping (This);

private

   --
   -- Flags to `msync'.
   --

   -- Sync memory asynchronously.
   MS_ASYNC : constant := 1;

   -- Synchronous memory sync.
   MS_SYNC : constant := 4;

   -- Invalidate the caches.
   MS_INVALIDATE : constant := 2;

   -- Protections are chosen from these bits, OR'd together.  The
   -- implementation does not necessarily support PROT_EXEC or PROT_WRITE
   -- without PROT_READ.  The only guarantees are that no writing will be
   -- allowed without PROT_WRITE and no access will be allowed for PROT_NONE.

   -- Page can be read.
   PROT_READ : constant Prot_FLag := 16#1#;

   -- Page can be written.
   PROT_WRITE : constant Prot_FLag := 16#2#;

   -- Page can be executed.
   PROT_EXEC : constant Prot_FLag := 16#4#;

   -- Page can not be accessed.
   PROT_NONE : constant Prot_FLag := 16#0#;

   -- Extend change to start of growsdown vma (mprotect only).
   PROT_GROWSDOWN : constant Prot_FLag := 16#01000000#;

   -- Extend change to start of growsup vma (mprotect only).
   PROT_GROWSUP : constant Prot_FLag := 16#02000000#;

   type Memory_Protection_To_Prot_Flag_Array is
     array (Memory_Protection) of Prot_FLag;

   Memory_Protection_To_Prot_Flag : constant Memory_Protection_To_Prot_Flag_Array
     := (
         Page_Can_Be_Read             => PROT_READ,
         Page_Can_Be_Read_And_Written => PROT_READ and PROT_WRITE
        );

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
      Offset : Interfaces.C.long) return Void_Ptr with
     Import        => True,
     Convention    => C,
     External_Name => "mmap";

   function C_Munmap
     (Addr   : Void_Ptr;
      Length : Size_Type) return Interfaces.C.int with
     Import        => True,
     Convention    => C,
     External_Name => "munmap";
   --  The munmap() system call deletes the mappings for the specified address
   --  range, and causes further references to addresses within the range
   --  to generate invalid memory references. The region is also automatically
   --  unmapped when the process is terminated. On the other hand, closing the
   --  file descriptor does not unmap the region.
   --
   --  The address addr must be a multiple of the page size. All pages
   --  containing a part of the indicated range are unmapped,
   --  and subsequent references to these pages will generate SIGSEGV.
   --  It is not an error if the indicated range does not contain
   --  any mapped pages.

end C_Binding.Linux.Memory_Maps;
