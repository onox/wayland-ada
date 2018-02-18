with Interfaces.C.Strings;
with System;

limited private with Px.Thin;

package Px is

   use type Interfaces.Unsigned_32;

   subtype unsigned_long is Interfaces.C.unsigned_long;
   subtype unsigned is Interfaces.C.unsigned;
   subtype int is Interfaces.C.int;
   subtype long is Interfaces.C.long;
   subtype Unsigned_32 is Interfaces.Unsigned_32;
   subtype chars_ptr is Interfaces.C.Strings.chars_ptr;

   type S_FLag_T is new Unsigned_32;
   type O_FLag_T is new Unsigned_32;

   function Shift_Right
     (Value  : Unsigned_32;
      Amount : Natural) return Unsigned_32 renames Interfaces.Shift_Right;

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

--  #define  __S_ISUID   04000	/* Set user ID on execution.  */
--  #define  __S_ISGID   02000	/* Set group ID on execution.  */
--  #define  __S_ISVTX   01000	/* Save swapped text after use (sticky).  */
--  #define  __S_IREAD   0400	/* Read by owner.  */
--  #define  __S_IWRITE  0200	/* Write by owner.  */
--  #define  __S_IEXEC   0100	/* Execute by owner.  */

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

   Nul : constant Character := Character'Val (0);

   subtype C_String is String with Dynamic_Predicate =>
     C_String'Length > 0 and then C_String (C_String'Last) = Nul;

   type File_T is tagged limited private;

   procedure Open (File      : in out File_T;
                   File_Name : in     C_String;
                   Flags     : in     O_FLag_T;
                   S_Flags   : in     S_FLag_T) with
     Global => null,
     Pre    => (File.Is_Closed and Flags > 0 and S_Flags > 0) and then
     (
        (((Flags and O_RDONLY) > 0) and then
             ((Flags and O_WRONLY) = 0 and ((Flags and O_RDWR) = 0))) or
        (((Flags and O_WRONLY) > 0) and then
             ((Flags and O_RDONLY) = 0 and ((Flags and O_RDWR) = 0))) or
        (((Flags and O_RDWR) > 0) and then
             ((Flags and O_WRONLY) = 0 and ((Flags and O_RDONLY) = 0)))
     );

   procedure Close (File : in out File_T) with
     Global => null,
     Pre    => File.Is_Open,
     Post   => File.Is_Closed;

   function Is_Open (File : File_T) return Boolean with
     Global => null;

   function Is_Closed (File : File_T) return Boolean with
     Global => null;

private

   type File_T is tagged limited record
      My_File_Descriptor : int;
      My_Is_Open         : Boolean := False;
   end record;

   function Is_Open (File : File_T) return Boolean is (File.My_Is_Open);

   function Is_Closed (File : File_T) return Boolean is (not File.My_Is_Open);

end Px;
