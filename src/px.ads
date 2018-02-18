with Interfaces.C.Strings;
with System;

with Px_Thin;

package Px is

   type File_T;
   type File_Status_T;

   use type Interfaces.Unsigned_32;

   subtype unsigned_long is Px_Thin.unsigned_long;
   subtype unsigned is Px_Thin.unsigned;
   subtype int is Px_Thin.int;
   subtype long is Px_Thin.long;
   subtype Unsigned_32 is Px_Thin.Unsigned_32;
   subtype chars_ptr is Px_Thin.chars_ptr;

   subtype S_FLag_T is Px_Thin.S_FLag_T;
   subtype O_FLag_T is Px_Thin.O_FLag_T;

   function Shift_Right
     (Value  : Unsigned_32;
      Amount : Natural) return Unsigned_32 renames Interfaces.Shift_Right;

   function Shift_Right (Value  : S_FLag_T;
                         Amount : Natural) return S_FLag_T
   is
     (S_FLag_T (Shift_Right (Unsigned_32 (Value), Amount)));

   use type S_FLag_T;
   use type O_FLag_T;

   --
   -- Encoding of the file mode.
   --

   S_IFMT : constant S_FLag_T := 0170000; --These bits determine file type.

   --
   -- File types
   --

   S_IFDIR : constant S_FLag_T := 0040000; -- Directory.
   S_IFCHR : constant S_FLag_T := 0020000; -- Character device.
   S_IFBLK : constant S_FLag_T := 0060000; -- Block device.
   S_IFREG : constant S_FLag_T := 0100000; -- Regular file.
   S_IFIFO : constant S_FLag_T := 0010000; -- FIFO.
   S_IFLNK : constant S_FLag_T := 0120000; -- Symbolic link.
   S_IFSOCK : constant S_FLag_T := 0140000; -- Socket.

   --  #define  __S_ISUID   04000  /* Set user ID on execution.  */
   --  #define  __S_ISGID   02000  /* Set group ID on execution.  */
   --  #define  __S_ISVTX   01000  /* Save swapped text after use (sticky).  */
   --  #define  __S_IREAD   0400   /* Read by owner.  */
   --  #define  __S_IWRITE  0200   /* Write by owner.  */
   --  #define  __S_IEXEC   0100   /* Execute by owner.  */

   -- Read by owner.
   S_IRUSR : constant S_FLag_T := 0400;

   -- Write by owner.
   S_IWUSR : constant S_FLag_T := 0200;

   -- Execute by owner.
   S_IXUSR : constant S_FLag_T := 0100;

   -- Read, write, and execute by owner.
   S_IRWXU : constant S_FLag_T := S_IRUSR or S_IWUSR or S_IXUSR;

   -- Read by group.
   S_IRGRP : constant S_FLag_T := Shift_Right (S_IRUSR, 3);

   -- Write by group.
   S_IWGRP : constant S_FLag_T := Shift_Right (S_IWUSR, 3);

   -- Execute by group.
   S_IXGRP : constant S_FLag_T := Shift_Right (S_IXUSR, 3);

   -- Read, write, and execute by group.
   S_IRWXG : constant S_FLag_T := Shift_Right (S_IRWXU, 3);

   -- Read by others.
   S_IROTH : constant S_FLag_T := Shift_Right (S_IRGRP, 3);

   -- Write by others.
   S_IWOTH : constant S_FLag_T := Shift_Right (S_IWGRP, 3);

   -- Execute by others.
   S_IXOTH : constant S_FLag_T := Shift_Right (S_IXGRP, 3);

   -- Read, write, and execute by others.
   S_IRWXO : constant S_FLag_T := Shift_Right (S_IRWXG, 3);

   -- Open for reading only
   O_RDONLY  : constant O_FLag_T := 16#00#;

   -- Open for write only
   O_WRONLY  : constant O_FLag_T := 16#01#;

   -- Open for reading and writing
   O_RDWR    : constant O_FLag_T := 16#02#;

   O_ACCMODE : constant O_FLag_T := 16#03#;

   O_CREAT : constant O_FLag_T := 0100;

   O_EXCL : constant O_FLag_T := 0200;

   O_NOCTTY : constant O_FLag_T := 0400;

   O_TRUNC : constant O_FLag_T := 01000;

   O_APPEND : constant O_FLag_T := 02000;

   O_NONBLOCK : constant O_FLag_T := 04000;

   O_SYNC : constant O_FLag_T := 04010000;

   O_ASYNC : constant O_FLag_T := 020000;

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

   subtype Device_Id_T is Px_Thin.Device_Id_T;

   subtype Inode_Number_T is Px_Thin.Inode_Number_T;

   subtype Number_Of_Hard_Links_T is Px_Thin.Number_Of_Hard_Links_T;

   subtype Mode_T is Px_Thin.Mode_T;

   subtype Owner_User_Id_T is Px_Thin.Owner_User_Id_T;

   subtype Owner_Groud_Id_T is Px_Thin.Owner_Groud_Id_T;

   subtype Size_T is Px_Thin.Size_T;

   subtype SSize_T is Px_Thin.SSize_T;

   subtype Block_Size_T is Px_Thin.Block_Size_T;

   subtype Number_Of_Blocks_T is Px_Thin.Number_Of_Blocks_T;

   subtype Time_Sec_T is Px_Thin.Time_Sec_T;

   subtype Time_Nano_Sec_T is Px_Thin.Time_Nano_Sec_T;

   subtype Void_Ptr is Px_Thin.Void_Ptr;

   subtype Off_T is Px_Thin.Off_T;

   subtype Time_T is Px_Thin.Time_T;

   Nul : Character renames Px_Thin.Nul;

   subtype C_String is Px_Thin.C_String;

   subtype Byte_T is Px_Thin.Byte_T;

   subtype Byte_Array_T is Px_Thin.Byte_Array_T;

   type File_T is tagged limited private;

   procedure Open (File      : in out File_T;
                   File_Name : in     C_String;
                   Flags     : in     O_FLag_T;
                   S_Flags   : in     S_FLag_T) with
     Global => null,
     Pre    => File.Is_Closed;

   procedure Close (File : in out File_T) with
     Global => null,
     Pre    => File.Is_Open,
     Post   => File.Is_Closed;

   procedure Write (File  : File_T;
                    Bytes : Byte_Array_T) with
     Global => null,
     Pre    => File.Is_Open;

   function Read (File  : File_T;
                  Bytes : Byte_Array_T) return SSize_T with
     Global => null,
     Pre    => File.Is_Open;

   procedure Get_File_Status (File        : in     File_T;
                              File_Status : in out File_Status_T) with
     Global => null,
     Pre    => File.Is_Open;

   function Is_Open (File : File_T) return Boolean with
     Global => null;

   function Is_Closed (File : File_T) return Boolean with
     Global => null;

   type File_Status_T is tagged limited private;

   function Is_Valid (File_Status : File_Status_T) return Boolean with
     Global => null;

   function Device_Id (File_Status : File_Status_T) return Device_Id_T with
     Global => null,
     Pre    => File_Status.Is_Valid;

   function Inode_Number
     (
      File_Status : File_Status_T
     )
      return Inode_Number_T with
     Global => null,
     Pre    => File_Status.Is_Valid;

   function Number_Of_Hard_Links
     (
      File_Status : File_Status_T
     )
      return Number_Of_Hard_Links_T with
     Global => null,
     Pre    => File_Status.Is_Valid;

   function Mode (File_Status : File_Status_T) return Mode_T with
     Global => null,
     Pre    => File_Status.Is_Valid;

   function Owner_User_Id
     (
      File_Status : File_Status_T
     )
      return Owner_User_Id_T with
     Global => null,
     Pre    => File_Status.Is_Valid;

   function Owner_Group_Id
     (
      File_Status : File_Status_T
     )
      return Owner_Groud_Id_T with
     Global => null,
     Pre    => File_Status.Is_Valid;

   function Special_Device_Id
     (
      File_Status : File_Status_T
     )
      return Device_Id_T with
     Global => null,
     Pre    => File_Status.Is_Valid;

   function Size (File_Status : File_Status_T) return Size_T with
     Global => null,
     Pre    => File_Status.Is_Valid;

   function Block_Size (File_Status : File_Status_T) return Block_Size_T with
     Global => null,
     Pre    => File_Status.Is_Valid;

   -- Number of 512B blocks allocated
   function Number_Of_Blocks
     (
      File_Status : File_Status_T
     ) return Block_Size_T with
       Global => null,
       Pre    => File_Status.Is_Valid;

   function Last_Access_Time (File_Status : File_Status_T) return Time_T with
     Global => null,
     Pre    => File_Status.Is_Valid;

   function Modification_Time
     (
      File_Status : File_Status_T
     ) return Time_T with
       Global => null,
       Pre    => File_Status.Is_Valid;

   function Last_Status_Change_Time
     (
      File_Status : File_Status_T
     ) return Time_T with
       Global => null,
       Pre    => File_Status.Is_Valid;

private

   type File_T is tagged limited record
      My_File_Descriptor : int;
      My_Is_Open         : Boolean := False;
   end record;

   function Is_Open (File : File_T) return Boolean is (File.My_Is_Open);

   function Is_Closed (File : File_T) return Boolean is (not File.My_Is_Open);

   type File_Status_T is tagged limited record
      My_Status   : aliased Px_Thin.File_Status_T;
      My_Is_Valid : Boolean := False;
   end record;

   function Is_Valid
     (
      File_Status : File_Status_T
     )
      return Boolean
   is
     (File_Status.My_Is_Valid);

   function Device_Id
     (
      File_Status : File_Status_T
     )
      return Device_Id_T is (File_Status.My_Status.Device_Id);

   function Inode_Number
     (
      File_Status : File_Status_T
     )
      return Inode_Number_T
   is
     (File_Status.My_Status.Inode_Number);

   function Number_Of_Hard_Links
     (
      File_Status : File_Status_T
     )
      return Number_Of_Hard_Links_T
   is
     (File_Status.My_Status.Number_Of_Hard_Links);

   function Mode
     (
      File_Status : File_Status_T
     )
      return Mode_T
   is
     (File_Status.My_Status.Mode);

   function Owner_User_Id
     (
      File_Status : File_Status_T
     )
      return Owner_User_Id_T
   is
     (File_Status.My_Status.Owner_User_Id);

   function Owner_Group_Id
     (
      File_Status : File_Status_T
     )
      return Owner_Groud_Id_T
   is
     (File_Status.My_Status.Owner_Groud_Id);

   function Special_Device_Id
     (
      File_Status : File_Status_T
     )
      return Device_Id_T
   is
     (File_Status.My_Status.Special_Device_Id);

   function Size
     (
      File_Status : File_Status_T
     )
      return Size_T
   is
     (File_Status.My_Status.Size);

   function Block_Size
     (
      File_Status : File_Status_T
     )
      return Block_Size_T
   is
     (File_Status.My_Status.Block_Size);

   function Number_Of_Blocks
     (
      File_Status : File_Status_T
     )
      return Block_Size_T
   is
     (File_Status.My_Status.Number_Of_Blocks);

   function Last_Access_Time
     (
      File_Status : File_Status_T
     )
      return Time_T
   is
     (File_Status.My_Status.Last_Access_Time);

   function Modification_Time
     (
      File_Status : File_Status_T
     )
      return Time_T
   is
     (File_Status.My_Status.Modification_Time);

   function Last_Status_Change_Time
     (
      File_Status : File_Status_T
     )
      return Time_T
   is
     (File_Status.My_Status.Last_Status_Change_Time);

end Px;
