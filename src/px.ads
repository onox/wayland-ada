with Interfaces.C.Strings;
with System.Storage_Elements;

package Px is

   type File_T;
   type Status_T;

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

   subtype Device_Id_T is unsigned_long;

   subtype Inode_Number_T is unsigned_long;

   subtype Hard_Link_Count_T is unsigned_long;

   subtype Mode_T is unsigned;

   subtype User_Id_T is unsigned;

   subtype Group_Id_T is unsigned;

   subtype Size_T is unsigned_long;

   subtype SSize_T is long;

   subtype Block_Size_T is long;

   subtype Block_Count_T is long;

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
                              File_Status : in out Status_T) with
     Global => null,
     Pre    => File.Is_Open;

   function Is_Open (File : File_T) return Boolean with
     Global => null;

   function Is_Closed (File : File_T) return Boolean with
     Global => null;

   type Status_T is tagged limited private;

   function Is_Valid (Status : Status_T) return Boolean with
     Global => null;

   function Device_Id (Status : Status_T) return Device_Id_T with
     Global => null,
     Pre    => Status.Is_Valid;

   function Inode_Number (Status : Status_T) return Inode_Number_T with
     Global => null,
     Pre    => Status.Is_Valid;

   function Hard_Link_Count (Status : Status_T) return Hard_Link_Count_T with
     Global => null,
     Pre    => Status.Is_Valid;

   function Mode (Status : Status_T) return Mode_T with
     Global => null,
     Pre    => Status.Is_Valid;

   function User_Id (Status : Status_T) return User_Id_T with
     Global => null,
     Pre    => Status.Is_Valid;

   function Group_Id (Status : Status_T) return Group_Id_T with
     Global => null,
     Pre    => Status.Is_Valid;

   function Special_Device_Id (Status : Status_T) return Device_Id_T with
     Global => null,
     Pre    => Status.Is_Valid;

   function Size (Status : Status_T) return Off_T with
     Global => null,
     Pre    => Status.Is_Valid;

   function Block_Size (Status : Status_T) return Block_Size_T with
     Global => null,
     Pre    => Status.Is_Valid;

   -- Number of 512B blocks allocated
   function Block_Count (Status : Status_T) return Block_Size_T with
     Global => null,
     Pre    => Status.Is_Valid;

   function Last_Access_Time (Status : Status_T) return Time_T with
     Global => null,
     Pre    => Status.Is_Valid;

   function Modification_Time (Status : Status_T) return Time_T with
     Global => null,
     Pre    => Status.Is_Valid;

   -- Last status change time
   function Change_Time (Status : Status_T) return Time_T with
     Global => null,
     Pre    => Status.Is_Valid;

private

   package Px_Thin is

      type File_Status_T is record
         -- ID of device containing file
         Device_Id         : aliased Device_Id_T;

         Inode_Number      : aliased Inode_Number_T;
         Hard_Link_Count   : aliased Hard_Link_Count_T;

         -- Protection
         Mode              : aliased Mode_T;

         User_Id           : aliased User_Id_T;
         Group_Id          : aliased Group_Id_T;
         Padding_0         : aliased int;

         -- Device ID (if special file)
         Special_Device_Id : aliased Device_Id_T;

         -- Total size, in bytes
         Size              : aliased Off_T;

         -- Blocksize for file system I/O
         Block_Size        : aliased Block_Size_T;

         -- Number of 512B blocks allocated
         Block_Count       : aliased Block_Count_T;

         -- Time of last access
         Access_Time       : aliased Time_T;

         -- Time of last modification
         Modification_Time : aliased Time_T;

         -- Time of last status change
         Change_Time       : aliased Time_T;
         Padding_1         : long;
         Padding_2         : long;
         Padding_3         : long;
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

   type File_T is tagged limited record
      My_File_Descriptor : int;
      My_Is_Open         : Boolean := False;
   end record;

   function Is_Open (File : File_T) return Boolean is (File.My_Is_Open);

   function Is_Closed (File : File_T) return Boolean is (not File.My_Is_Open);

   type Status_T is tagged limited record
      My_Status   : aliased Px_Thin.File_Status_T;
      My_Is_Valid : Boolean := False;
   end record;

   function Is_Valid (Status : Status_T) return Boolean is (Status.My_Is_Valid);

   function Device_Id (Status : Status_T) return Device_Id_T is (Status.My_Status.Device_Id);

   function Inode_Number (Status : Status_T) return Inode_Number_T is (Status.My_Status.Inode_Number);

   function Hard_Link_Count (Status : Status_T) return Hard_Link_Count_T is (Status.My_Status.Hard_Link_Count);

   function Mode (Status : Status_T) return Mode_T is (Status.My_Status.Mode);

   function User_Id (Status : Status_T) return User_Id_T is (Status.My_Status.User_Id);

   function Group_Id (Status : Status_T) return Group_Id_T is (Status.My_Status.Group_Id);

   function Special_Device_Id (Status : Status_T) return Device_Id_T is (Status.My_Status.Special_Device_Id);

   function Size (Status : Status_T) return Off_T is (Status.My_Status.Size);

   function Block_Size (Status : Status_T) return Block_Size_T is (Status.My_Status.Block_Size);

   function Block_Count (Status : Status_T) return Block_Size_T is (Status.My_Status.Block_Count);

   function Last_Access_Time (Status : Status_T) return Time_T is (Status.My_Status.Access_Time);

   function Modification_Time (Status : Status_T) return Time_T is (Status.My_Status.Modification_Time);

   function Change_Time (Status : Status_T) return Time_T is (Status.My_Status.Change_Time);

end Px;
