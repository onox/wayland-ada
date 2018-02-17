with Interfaces.C.Strings;
with System;

package Px_Thin is

   use type Interfaces.Unsigned_32;

   subtype unsigned_long is Interfaces.C.unsigned_long;
   subtype unsigned is Interfaces.C.unsigned;
   subtype int is Interfaces.C.int;
   subtype long is Interfaces.C.long;
   subtype Unsigned_32 is Interfaces.Unsigned_32;
   subtype chars_ptr is Interfaces.C.Strings.chars_ptr;

   function Shift_Right
     (Value  : Unsigned_32;
      Amount : Natural) return Unsigned_32 renames Interfaces.Shift_Right;

   --
   -- Encoding of the file mode.
   --

   S_IFMT : constant Unsigned_32 := 0170000; --These bits determine file type.

   --
   -- File types
   --

   S_IFDIR : constant Unsigned_32 := 0040000; -- Directory.
   S_IFCHR : constant Unsigned_32 := 0020000; -- Character device.
   S_IFBLK : constant Unsigned_32 := 0060000; -- Block device.
   S_IFREG : constant Unsigned_32 := 0100000; -- Regular file.
   S_IFIFO : constant Unsigned_32 := 0010000; -- FIFO.
   S_IFLNK : constant Unsigned_32 := 0120000; -- Symbolic link.
   S_IFSOCK : constant Unsigned_32 := 0140000; -- Socket.

--  #define	__S_ISUID	04000	/* Set user ID on execution.  */
--  #define	__S_ISGID	02000	/* Set group ID on execution.  */
--  #define	__S_ISVTX	01000	/* Save swapped text after use (sticky).  */
--  #define	__S_IREAD	0400	/* Read by owner.  */
--  #define	__S_IWRITE	0200	/* Write by owner.  */
--  #define	__S_IEXEC	0100	/* Execute by owner.  */

   -- Read by owner.
   S_IRUSR : constant Unsigned_32 := 0400;

   -- Write by owner.
   S_IWUSR : constant Unsigned_32 := 0200;

   -- Execute by owner.
   S_IXUSR : constant Unsigned_32 := 0100;

   -- Read, write, and execute by owner.
   S_IRWXU : constant Unsigned_32 := S_IRUSR or S_IWUSR or S_IXUSR;

   -- Read by group.
   S_IRGRP : constant Unsigned_32 := Shift_Right (S_IRUSR, 3);

   -- Write by group.
   S_IWGRP : constant Unsigned_32 := Shift_Right (S_IWUSR, 3);

   -- Execute by group.
   S_IXGRP : constant Unsigned_32 := Shift_Right (S_IXUSR, 3);

   -- Read, write, and execute by group.
   S_IRWXG : constant Unsigned_32 := Shift_Right (S_IRWXU, 3);

   S_IROTH : constant Unsigned_32 := Shift_Right (S_IRGRP, 3); -- Read by others.
   S_IWOTH : constant Unsigned_32 := Shift_Right (S_IWGRP, 3); -- Write by others.
   S_IXOTH : constant Unsigned_32 := Shift_Right (S_IXGRP, 3); -- Execute by others.

   -- Read, write, and execute by others.
   S_IRWXO : constant Unsigned_32 := Shift_Right (S_IRWXG, 3);

   -- Open for reading only
   O_RDONLY  : constant Unsigned_32 := 16#00#;

   -- Open for write only
   O_WRONLY  : constant Unsigned_32 := 16#01#;

   -- Open for reading and writing
   O_RDWR    : constant Unsigned_32 := 16#02#;

   O_ACCMODE : constant Unsigned_32 := 16#03#;

   O_CREAT : constant Unsigned_32 := 0100;

   O_EXCL : constant Unsigned_32 := 0200;

   O_NOCTTY : constant Unsigned_32 := 0400;

   O_TRUNC : constant Unsigned_32 := 01000;

   O_APPEND : constant Unsigned_32 := 02000;

   O_NONBLOCK : constant Unsigned_32 := 04000;

   O_SYNC : constant Unsigned_32 := 04010000;

   O_ASYNC : constant Unsigned_32 := 020000;

   -- Page can be read.
   PROT_READ : constant := 16#1#;

   -- Page can be written.
   PROT_WRITE : constant := 16#2#;

   -- Page can be executed.
   PROT_EXEC : constant := 16#4#;

   -- Page can not be accessed.
   PROT_NONE : constant := 16#0#;

   -- Extend change to start of growsdown vma (mprotect only).
   PROT_GROWSDOWN : constant := 16#01000000#;

   -- Extend change to start of growsup vma (mprotect only).
   PROT_GROWSUP : constant := 16#02000000#;

   --
   -- Sharing types (must choose one and only one of these).
   --

   -- Share changes.
   MAP_SHARED  : constant := 16#01#;

   -- Changes are private.
   MAP_PRIVATE : constant := 16#02#;

   subtype Device_Id_T is unsigned_long;

   subtype Inode_Number_T is unsigned_long;

   subtype Number_Of_Hard_Links_T is unsigned_long;

   subtype Mode_T is unsigned_long;

   subtype Owner_User_Id_T is unsigned;

   subtype Owner_Groud_Id_T is unsigned;

   subtype Size_T is long;

   subtype Block_Size_T is long;

   subtype Number_Of_Blocks_T is long;

   subtype Time_Sec_T is long;

   subtype Time_Nano_Sec_T is long;

   subtype Void_Ptr is System.Address;

   subtype Off_T is long;

   type Time_T is record
      Sec      : aliased Time_Sec_T;
      Nano_Sec : aliased Time_Nano_Sec_T;
   end record with
     Convention => C_Pass_By_Copy;

   type File_Status_T is record
      Device_Id               : aliased Device_Id_T; -- ID of device containing file
      Inode_Number            : aliased Inode_Number_T;
      Number_Of_Hard_Links    : aliased Number_Of_Hard_Links_T;
      Mode                    : aliased Mode_T; -- Protection
      Owner_User_Id           : aliased Owner_User_Id_T;
      Owner_Groud_Id          : aliased Owner_Groud_Id_T;
      Padding_0               : aliased int;
      Special_Device_Id       : aliased Device_Id_T;  -- Device ID (if special file)
      Size                    : aliased Size_T;  -- Total size, in bytes
      Block_Size              : aliased Block_Size_T; -- Blocksize for file system I/O
      Number_Of_Blocks        : aliased Number_Of_Blocks_T; --Number of 512B blocks allocated
      Last_Access_Time        : aliased Time_T; -- Time of last access
      Modification_Time       : aliased Time_T; -- time of last modification
      Last_Status_Change_Time : aliased Time_T; -- time of last status change
      Padding_1               : long;
      Padding_2               : long;
      Padding_3               : long;
   end record with
     Convention => C_Pass_By_Copy;

   type File_Status_Ptr is access all File_Status_T;

   function Get_File_Status (Fd     : int;
                             Status : File_Status_Ptr) return int with
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
   function Open (File_Name : chars_ptr;
                  Flags     : Unsigned_32;
                  S_Flags   : Unsigned_32) return int with
     Import        => True,
     Convention    => C,
     External_Name => "open";

   procedure Close (Fd : int) with
     Import        => True,
     Convention    => C,
     External_Name => "close";

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
