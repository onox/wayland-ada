with System.Storage_Elements;
private with Ada.Unchecked_Conversion;

--  This package was originally named Posix but is nowadays Linux to avoid
--  name clash with the Ada binding to Posix named Florist.
package C_Binding.Linux is

   type File;
   type File_Status;
   type Memory_Map;

--     type unsigned       is mod 2 ** Integer'Size;
--     type unsigned_long  is mod 2 ** Long_Integer'Size;
--     type int is new Integer;
--     type long is new Long_Integer;
--     type Unsigned_32 is mod 2 ** 32;
--     for Unsigned_32'Size use 32;

   type S_FLag is new Unsigned_32;
   type O_FLag is new Unsigned_32;
   type Prot_FLag is new Unsigned_32;

   use type long;

   type Fds_Bits_Index is range 0 .. 1023;
   --  The interval is chosen 0 .. 1023 to be able to use file descriptors
   --  as index into the Fds_Bits_Array.

   type Fds_Bits_Array is array (Fds_Bits_Index) of Boolean with Pack;

   type fd_set is record
      Fds_Bits : aliased Fds_Bits_Array;
   end record with
     Convention => C_Pass_By_Copy,
     Size       => 1024;

   type timeval is record
      tv_sec  : aliased long;
      tv_usec : aliased long; -- Microseconds!
   end record with
     Convention => C_Pass_By_Copy;


   function C_Select
     (File_Descriptor : Integer;
      Readfds   : access fd_set;
      Writefds  : access fd_set;
      Exceptfds : access fd_set;
      Time      : access timeval) return int;
   pragma Import (C, C_Select, "select");
   --  On success, returns the number of file descriptors contained
   --  in the three returned descriptor sets (that is,
   --  the total number of bits that are set in readfds, writefds, exceptfds)
   --  which may be zero if the timeout expires before
   --  anything interesting happens. On error, -1 is returned,
   --  and errno is set appropriately; the sets and timeout become undefined,
   --  so do not rely on their contents after an error.

   type Poll_File_Descriptor is record
      Descriptor : aliased Integer := -1;
      Events     : aliased Unsigned_16 := 0;
      Revents    : aliased Unsigned_16 := 0;
   end record with
     Convention => C_Pass_By_Copy;

   type Poll_File_Descriptor_Array is array (Positive range <>) of
     Poll_File_Descriptor with
       Convention => C;

   --
   -- Non-primitive subprograms
   --

   -- Write to standard out. May be used instead of Ada.Text_IO.Put ().
   procedure Put (Text : String) with
     Global => null;

   -- Write to standard out. May be used instead of Ada.Text_IO.Put_Line ().
   procedure Put_Line (Text : String) with
     Global => null;

   function Get_Line return String;

   function Poll
     (File_Descriptors : Poll_File_Descriptor_Array;
      Timeout          : Integer) return Integer;

   --
   -- Encoding of the file mode.
   --

   S_IFMT : constant S_FLag := 0170000; -- These bits determine file type.

   --
   -- File types
   --

   S_IFDIR  : constant S_FLag := 0040000; -- Directory.
   S_IFCHR  : constant S_FLag := 0020000; -- Character device.
   S_IFBLK  : constant S_FLag := 0060000; -- Block device.
   S_IFREG  : constant S_FLag := 0100000; -- Regular file.
   S_IFIFO  : constant S_FLag := 0010000; -- FIFO.
   S_IFLNK  : constant S_FLag := 0120000; -- Symbolic link.
   S_IFSOCK : constant S_FLag := 0140000; -- Socket.

   POLLIN  : constant := 16#001#;
   -- There is data to read.

   POLLPRI : constant := 16#002#;
   -- There is urgent data to read.

   POLLOUT : constant := 16#004#;
   -- Writing now will not block.

   --  #define  __S_ISUID   04000  /* Set user ID on execution.  */
   --  #define  __S_ISGID   02000  /* Set group ID on execution.  */
   --  #define  __S_ISVTX   01000  /* Save swapped text after use (sticky).  */
   --  #define  __S_IREAD   0400   /* Read by owner.  */
   --  #define  __S_IWRITE  0200   /* Write by owner.  */
   --  #define  __S_IEXEC   0100   /* Execute by owner.  */

   type File_Permission is (
                            Owner_Read,  Owner_Write,  Owner_Execute,
                            Group_Read,  Group_Write,  Group_Execute,
                            Others_Read, Others_Write, Others_Execute
                           );

   type File_Permissions is array (File_Permission) of Boolean;

   type File_Mode is (
                      Read_Only, Write_Only, Read_Write
                     );

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

   --
   -- Flags to `msync'.
   --

   -- Sync memory asynchronously.
   MS_ASYNC : constant := 1;

   -- Synchronous memory sync.
   MS_SYNC : constant := 4;

   -- Invalidate the caches.
   MS_INVALIDATE : constant := 2;

   subtype Device_Id_Type is unsigned_long;

   subtype Inode_Number_Type is unsigned_long;

   subtype Hard_Link_Count_Type is unsigned_long;

   subtype Mode_Type is unsigned;

   subtype User_Id_Type is unsigned;

   subtype Group_Id_Type is unsigned;

   subtype Size_Type is unsigned_long;

   subtype SSize_Type is long;

   subtype Block_Size_Type is long;

   subtype Block_Count_Type is long;

   subtype Time_Sec is long;

   subtype Time_Nano_Sec is long;

   subtype Offset is long;

   subtype Byte is System.Storage_Elements.Storage_Element;

   subtype Byte_Array is System.Storage_Elements.Storage_Array;

   type Time is record
      Sec      : aliased Time_Sec;
      Nano_Sec : aliased Time_Nano_Sec;
   end record with
     Convention => C_Pass_By_Copy;

   type File is tagged limited private with
     Default_Initial_Condition => Is_Closed (File);
   --  Represents a file on the hard disk.

   procedure Set_File_Descriptor
     (File  : in out Linux.File;
      Value : Integer);

   procedure Open
     (File        : in out Linux.File;
      File_Name   : in     String;
      Mode        : in     File_Mode;
      Permissions : in     File_Permissions) with
     Global => null,
     Pre    => File.Is_Closed;

   procedure Close (File : in out Linux.File) with
     Global => null,
     Pre    => File.Is_Open,
     Post   => File.Is_Closed;

   procedure Write (File : Linux.File; Bytes : Byte_Array) with
     Global => null,
     Pre    => File.Is_Open;

   function Read (File : Linux.File; Bytes : in out Byte_Array) return SSize_Type with
     Global => null,
     Pre    => File.Is_Open;

   function File_Descriptor (File : Linux.File) return Integer with
     Global => null,
     Pre    => File.Is_Open;

   procedure Get_File_Status
     (File   : in     Linux.File;
      Status : in out File_Status) with
     Global => null,
     Pre    => File.Is_Open;

   procedure Map_Memory
     (File    : in Linux.File;
      Address : Void_Ptr;
      Len     : Size_Type;
      Prot    : Prot_FLag;
      Flags   : int;
      Offset  : Linux.Offset;
      Memory_Map : in out Linux.Memory_Map) with
     Global => null,
     Pre    => not Has_Mapping (Memory_Map);

   function Is_Open (File : Linux.File) return Boolean with
     Global => null;

   function Is_Closed (File : Linux.File) return Boolean with
     Global => null;

   type File_Status is tagged limited private;

   function Is_Valid (Status : File_Status) return Boolean with
     Global => null;

   function Device_Id (Status : File_Status) return Device_Id_Type with
     Global => null,
     Pre    => Status.Is_Valid;

   function Inode_Number (Status : File_Status) return Inode_Number_Type with
     Global => null,
     Pre    => Status.Is_Valid;

   function Hard_Link_Count
     (Status : File_Status) return Hard_Link_Count_Type with
     Global => null,
     Pre    => Status.Is_Valid;

   function Mode (Status : File_Status) return Mode_Type with
     Global => null,
     Pre    => Status.Is_Valid;

   function User_Id (Status : File_Status) return User_Id_Type with
     Global => null,
     Pre    => Status.Is_Valid;

   function Group_Id (Status : File_Status) return Group_Id_Type with
     Global => null,
     Pre    => Status.Is_Valid;

   function Special_Device_Id (Status : File_Status) return Device_Id_Type with
     Global => null,
     Pre    => Status.Is_Valid;

   function Size (Status : File_Status) return Offset with
     Global => null,
     Pre    => Status.Is_Valid;
   -- The file size in bytes.

   function Block_Size (Status : File_Status) return Block_Size_Type with
     Global => null,
     Pre    => Status.Is_Valid;

   -- Number of 512B blocks allocated
   function Block_Count (Status : File_Status) return Block_Size_Type with
     Global => null,
     Pre    => Status.Is_Valid;

   function Last_Access_Time (Status : File_Status) return Time with
     Global => null,
     Pre    => Status.Is_Valid;

   function Modification_Time (Status : File_Status) return Time with
     Global => null,
     Pre    => Status.Is_Valid;

   -- Last status change time
   function Change_Time (Status : File_Status) return Time with
     Global => null,
     Pre    => Status.Is_Valid;

   type Memory_Map is tagged limited private;

   function Has_Mapping (Map : Linux.Memory_Map) return Boolean with
     Global => null;

   function Mapping (Map : Linux.Memory_Map) return Void_Ptr with
     Global => null,
     Pre    => Map.Has_Mapping;

   -- Returns 0 on success, otherwise -1.
   function Unmap_Memory (Map : in out Linux.Memory_Map) return Integer with
     Global => null,
     Post   => (if Unmap_Memory'Result = 0 then not Map.Has_Mapping);

   -- Returns 0 on success, otherwise -1.
   function Memory_Unmap (Address : Void_Ptr;
                          Length  : Size_Type) return Integer with
     Global => null;

   --
   -- Standard file descriptors.
   --
   STDIN  : constant File; -- Standard input.
   STDOUT : constant File; -- Standard output.
   STDERR : constant File; -- Standard error output.

   MAP_FAILED : constant Void_Ptr;

private

   use type Void_Ptr;

   Nil : Void_Ptr renames System.Null_Address;

   --
   -- Sharing types (must choose one and only one of these).
   --

   -- Share changes.
   MAP_SHARED : constant := 16#01#;

   -- Changes are private.
   MAP_PRIVATE : constant := 16#02#;

   -- Interpret addr exactly.
   MAP_FIXED : constant :=  16#10#;

   -- Don't use a file.
   MAP_ANON : constant := 16#20#;

   MAP_HUGE_SHIFT : constant := 26;

   MAP_HUGE_MASK : constant := 16#3f#;

   -- Read by owner.
   S_IRUSR : constant S_FLag := 8#400#; -- 256 in decimal

   -- Write by owner.
   S_IWUSR : constant S_FLag := 8#200#; -- 128 in decimal

   -- Execute by owner.
   S_IXUSR : constant S_FLag := 8#100#; -- 64 in decimal

   -- Read, write, and execute by owner.
   S_IRWXU : constant S_FLag := S_IRUSR or S_IWUSR or S_IXUSR;

   -- Read by group.
   S_IRGRP : constant S_FLag := S_IRUSR / 8;

   -- Write by group.
   S_IWGRP : constant S_FLag := S_IWUSR / 8;

   -- Execute by group.
   S_IXGRP : constant S_FLag := S_IXUSR / 8;

   -- Read, write, and execute by group.
   S_IRWXG : constant S_FLag := S_IRWXU / 8;

   -- Read by others.
   S_IROTH : constant S_FLag := S_IRGRP / 8;

   -- Write by others.
   S_IWOTH : constant S_FLag := S_IWGRP / 8;

   -- Execute by others.
   S_IXOTH : constant S_FLag := S_IXGRP / 8;

   -- Read, write, and execute by others.
   S_IRWXO : constant S_FLag := S_IRWXG / 8;

   -- Open for reading only
   O_RDONLY : constant O_FLag := 16#00#;

   -- Open for write only
   O_WRONLY : constant O_FLag := 16#01#;

   -- Open for reading and writing
   O_RDWR : constant O_FLag := 16#02#;

   O_ACCMODE : constant O_FLag := 16#03#;

   O_CREAT : constant O_FLag := 8#100#;

   O_EXCL : constant O_FLag := 8#200#;

   O_NOCTTY : constant O_FLag := 8#400#;

   O_TRUNC : constant O_FLag := 8#1000#;

   O_APPEND : constant O_FLag := 8#2000#;

   O_NONBLOCK : constant O_FLag := 8#4000#;

   O_SYNC : constant O_FLag := 8#4010000#;

   O_ASYNC : constant O_FLag := 8#20000#;

   package Px_Thin is

      -- Standard file descriptors.
      STDIN_FILENO  : constant := 0; -- Standard input.
      STDOUT_FILENO : constant := 1; -- Standard output.
      STDERR_FILENO : constant := 2; -- Standard error output.

      type File_Status_T is record
         -- ID of device containing file
         Device_Id : aliased Device_Id_Type;

         Inode_Number    : aliased Inode_Number_Type;
         Hard_Link_Count : aliased Hard_Link_Count_Type;

         -- Protection
         Mode : aliased Mode_Type;

         User_Id   : aliased User_Id_Type;
         Group_Id  : aliased Group_Id_Type;
         Padding_0 : aliased int;

         -- Device ID (if special file)
         Special_Device_Id : aliased Device_Id_Type;

         -- Total size, in bytes
         Size : aliased Offset;

         -- Blocksize for file system I/O
         Block_Size : aliased Block_Size_Type;

         -- Number of 512B blocks allocated
         Block_Count : aliased Block_Count_Type;

         -- Time of last access
         Access_Time : aliased Time;

         -- Time of last modification
         Modification_Time : aliased Time;

         -- Time of last status change
         Change_Time : aliased Time;
         Padding_1   : long;
         Padding_2   : long;
         Padding_3   : long;
      end record with
        Convention => C_Pass_By_Copy;

      function Get_File_Status
        (Fd     : Integer;
         Status : access File_Status_T) return Integer with
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
      function Open
        (File_Name : C_String;
         Flags     : O_FLag;
         S_Flags   : S_FLag) return Integer with
        Import        => True,
        Convention    => C,
        External_Name => "open";

      procedure Close (File_Descriptor : Integer) with
        Import        => True,
        Convention    => C,
        External_Name => "close";

      function Write
        (File_Descriptor : Integer;
         Buffer          : Byte_Array;
         Count           : Size_Type) return SSize_Type with
        Import        => True,
        Convention    => C,
        External_Name => "write";

      function Write
        (File_Descriptor : Integer;
         Buffer          : String;
         Count           : Size_Type) return SSize_Type with
        Import        => True,
        Convention    => C,
        External_Name => "write";

      function Read
        (File_Descriptor : Integer;
         Buffer          : in out Byte_Array;
         Count           : Size_Type) return SSize_Type with
        Import        => True,
        Convention    => C,
        External_Name => "read";

      function Mmap
        (Addr   : Void_Ptr;
         Len    : Size_Type;
         Prot   : Prot_FLag;
         Flags  : int;
         Fd     : Integer;
         Offset : Linux.Offset) return Void_Ptr with
        Import        => True,
        Convention    => C,
        External_Name => "mmap";

      function Munmap (Addr : Void_Ptr; Length : Size_Type) return Integer with
        Import        => True,
        Convention    => C,
        External_Name => "munmap";

      function Poll (File_Descriptors        : Poll_File_Descriptor_Array;
                     File_Descriptors_Length : unsigned_long;
                     Timeout                 : Integer) return Integer with
        Import        => True,
        Convention    => C,
        External_Name => "poll";

   end Px_Thin;

   type File is tagged limited record
      My_File_Descriptor : Integer := -1;
   end record;

   function Is_Open (File : Linux.File) return Boolean is (File.My_File_Descriptor /= -1);

   function Is_Closed (File : Linux.File) return Boolean is (File.My_File_Descriptor = -1);

   type File_Status is tagged limited record
      My_Status   : aliased Px_Thin.File_Status_T;
      My_Is_Valid : Boolean := False;
   end record;

   function File_Descriptor (File : Linux.File) return Integer is
     (File.My_File_Descriptor);

   function Is_Valid
     (Status : File_Status) return Boolean is
     (Status.My_Is_Valid);

   function Device_Id
     (Status : File_Status) return Device_Id_Type is
     (Status.My_Status.Device_Id);

   function Inode_Number
     (Status : File_Status) return Inode_Number_Type is
     (Status.My_Status.Inode_Number);

   function Hard_Link_Count
     (Status : File_Status) return Hard_Link_Count_Type is
     (Status.My_Status.Hard_Link_Count);

   function Mode
     (Status : File_Status) return Mode_Type is (Status.My_Status.Mode);

   function User_Id
     (Status : File_Status) return User_Id_Type is
     (Status.My_Status.User_Id);

   function Group_Id
     (Status : File_Status) return Group_Id_Type is
     (Status.My_Status.Group_Id);

   function Special_Device_Id
     (Status : File_Status) return Device_Id_Type is
     (Status.My_Status.Special_Device_Id);

   function Size
     (Status : File_Status) return Offset is
     (Status.My_Status.Size);

   function Block_Size
     (Status : File_Status) return Block_Size_Type is
     (Status.My_Status.Block_Size);

   function Block_Count
     (Status : File_Status) return Block_Size_Type is
     (Status.My_Status.Block_Count);

   function Last_Access_Time
     (Status : File_Status) return Time is
     (Status.My_Status.Access_Time);

   function Modification_Time
     (Status : File_Status) return Time is
     (Status.My_Status.Modification_Time);

   function Change_Time
     (Status : File_Status) return Time is
     (Status.My_Status.Change_Time);

   function Conv is new Ada.Unchecked_Conversion (Source => long,
                                                  Target => Void_Ptr);

   MAP_FAILED_VALUE : constant long     := -1;
   MAP_FAILED       : constant Void_Ptr := Conv (MAP_FAILED_VALUE);

   type Memory_Map is tagged limited record
      My_Mapping : Void_Ptr := MAP_FAILED;
      My_Length  : Size_Type;
   end record;

   function Has_Mapping
     (Map : Linux.Memory_Map) return Boolean is (Map.My_Mapping /= MAP_FAILED);

   function Mapping (Map : Linux.Memory_Map) return Void_Ptr is (Map.My_Mapping);

   STDIN  : constant File :=
     (
      My_File_Descriptor => Px_Thin.STDIN_FILENO
     );

   STDOUT : constant File :=
     (
      My_File_Descriptor => Px_Thin.STDOUT_FILENO
     );

   STDERR : constant File :=
     (
      My_File_Descriptor => Px_Thin.STDERR_FILENO
     );

   function Poll
     (File_Descriptors : Poll_File_Descriptor_Array;
      Timeout          : Integer) return Integer is
      (Px_Thin.Poll (File_Descriptors, File_Descriptors'Length, Timeout));

end C_Binding.Linux;
