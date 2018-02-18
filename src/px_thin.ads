with Interfaces.C.Strings;
with System.Storage_Elements;

package Px_Thin is

   use type Interfaces.Unsigned_32;

   subtype unsigned_long is Interfaces.C.unsigned_long;
   subtype unsigned is Interfaces.C.unsigned;
   subtype int is Interfaces.C.int;
   subtype long is Interfaces.C.long;
   subtype Unsigned_32 is Interfaces.Unsigned_32;
   subtype chars_ptr is Interfaces.C.Strings.chars_ptr;

   type S_FLag_T is new Unsigned_32;
   type O_FLag_T is new Unsigned_32;

   subtype Device_Id_T is unsigned_long;

   subtype Inode_Number_T is unsigned_long;

   subtype Number_Of_Hard_Links_T is unsigned_long;

   subtype Mode_T is unsigned_long;

   subtype Owner_User_Id_T is unsigned;

   subtype Owner_Groud_Id_T is unsigned;

   subtype Size_T is unsigned_long;

   subtype SSize_T is long;

   subtype Block_Size_T is long;

   subtype Number_Of_Blocks_T is long;

   subtype Time_Sec_T is long;

   subtype Time_Nano_Sec_T is long;

   subtype Void_Ptr is System.Address;

   subtype Off_T is long;

   Nul : constant Character := Character'Val (0);

   subtype C_String is String with Dynamic_Predicate =>
     C_String'Length > 0 and then C_String (C_String'Last) = Nul;

   subtype Byte_T is System.Storage_Elements.Storage_Element;

   subtype Byte_Array_T is System.Storage_Elements.Storage_Array;

   type Time_T is record
      Sec      : aliased Time_Sec_T;
      Nano_Sec : aliased Time_Nano_Sec_T;
   end record with
     Convention => C_Pass_By_Copy;

   type File_Status_T is record
      -- ID of device containing file
      Device_Id               : aliased Device_Id_T;

      Inode_Number            : aliased Inode_Number_T;
      Number_Of_Hard_Links    : aliased Number_Of_Hard_Links_T;

      -- Protection
      Mode                    : aliased Mode_T;

      Owner_User_Id           : aliased Owner_User_Id_T;
      Owner_Groud_Id          : aliased Owner_Groud_Id_T;
      Padding_0               : aliased int;

      -- Device ID (if special file)
      Special_Device_Id       : aliased Device_Id_T;

      -- Total size, in bytes
      Size                    : aliased Size_T;

      -- Blocksize for file system I/O
      Block_Size              : aliased Block_Size_T;

      -- Number of 512B blocks allocated
      Number_Of_Blocks        : aliased Number_Of_Blocks_T;

      -- Time of last access
      Last_Access_Time        : aliased Time_T;

      -- Time of last modification
      Modification_Time       : aliased Time_T;

      -- Time of last status change
      Last_Status_Change_Time : aliased Time_T;
      Padding_1               : long;
      Padding_2               : long;
      Padding_3               : long;
   end record with
     Convention => C_Pass_By_Copy;

   function Get_File_Status (Fd     : int;
                             Status : access File_Status_T) return int with
     Import        => True,
     Convention    => C,
     External_Name => "fstat";

   -- Establishes a connection between a file and a file descriptor.
   -- The file descriptor handle (a non-negative number)
   -- is returned upon success, otherwise -1.
   --
   -- Applications shall specify exactly one of the first three flags:
   -- O_RDONLY, O_WRONLY and O_RDWR. And then any combination of O_APPEND,
   -- O_CREAT, O_DSYNC, O_EXCL, O_NOCTTY, O_NONBLOCK, O_RSYNC,
   -- O_SYNC, O_TRUNC.
   function Open (File_Name : C_String;
                  Flags     : O_FLag_T;
                  S_Flags   : S_FLag_T) return int with
     Import        => True,
     Convention    => C,
     External_Name => "open";

   procedure Close (File_Descriptor : int) with
     Import        => True,
     Convention    => C,
     External_Name => "close";

   function Write (File_Descriptor : int;
                   Buffer          : Byte_Array_T;
                   Count           : Size_T) return SSize_T with
     Import        => True,
     Convention    => C,
     External_Name => "write";

   function Read (File_Descriptor : int;
                  Buffer          : Byte_Array_T;
                  Count           : Size_T) return SSize_T with
     Import        => True,
     Convention    => C,
     External_Name => "read";

   function Mmap (Addr   : Void_Ptr;
                  Len    : Size_T;
                  Prot   : int;
                  Flags  : int;
                  Fd     : int;
                  Offset : Off_T) return Void_Ptr with
     Import        => True,
     Convention    => C,
     External_Name => "mmap";

   function Munmap (Addr   : Void_Ptr;
                    Length : Size_T) return int with
     Import        => True,
     Convention    => C,
     External_Name => "munmap";

end Px_Thin;
