with Ada.Streams;

private with Ada.Unchecked_Conversion;

--  This package was originally named Posix but is nowadays Linux to avoid
--  name clash with the Ada binding to Posix named Florist.
package C_Binding.Linux with Preelaborate is

   subtype Success_Flag is C_Binding.Success_Flag;

   type File_Base is limited private;

   type Socket_Base is limited private;

   EPERM   : constant :=  1;
   ENOENT  : constant :=  2;
   ESRCH   : constant :=  3;
   EINTR   : constant :=  4;
   EIO     : constant :=  5;
   ENXIO   : constant :=  6;
   E2BIG   : constant :=  7;
   ENOEXEC : constant :=  8;
   EBADF   : constant :=  9;
   ECHILD  : constant := 10;
   EAGAIN  : constant := 11;
   ENOMEM  : constant := 12;
   EACCES  : constant := 13;
   EFAULT  : constant := 14;
   ENOTBLK : constant := 15;
   EBUSY   : constant := 16;
   EEXIST  : constant := 17;
   EXDEV   : constant := 18;
   ENODEV  : constant := 19;
   ENOTDIR : constant := 20;
   EISDIR  : constant := 21;
   EINVAL  : constant := 22;
   ENFILE  : constant := 23;
   EMFILE  : constant := 24;
   ENOTTY  : constant := 25;
   ETXTBSY : constant := 26;
   EFBIG   : constant := 27;
   ENOSPC  : constant := 28;
   ESPIPE  : constant := 29;
   EROFS   : constant := 30;
   EMLINK  : constant := 31;
   EPIPE   : constant := 32;
   EDOM    : constant := 33;
   ERANGE  : constant := 34;

   function Poll_File_Descriptor_Until_Timeout (Descriptor, Timeout : Integer) return Integer;

private

   use type long;
   use type Void_Ptr;
   use type Interfaces.C.int;

   subtype Size_Type is unsigned_long;

   subtype SSize_Type is long;

   type Poll_File_Descriptor is record
      Descriptor : aliased Integer := -1;
      Events     : aliased Unsigned_16 := 0;
      Revents    : aliased Unsigned_16 := 0;
   end record with
     Convention => C_Pass_By_Copy;

   type Poll_File_Descriptor_Array is
     array (Positive range <>) of Poll_File_Descriptor with
     Convention => C;

   function Poll
     (File_Descriptors : Poll_File_Descriptor_Array;
      Timeout          : Integer) return Integer;
   --  TODO: Make this function call available in public package

   POLLIN  : constant := 16#001#;
   POLLPRI : constant := 16#002#;
   POLLOUT : constant := 16#004#;

   type S_FLag is new Unsigned_32;
   type O_FLag is new Unsigned_32;
   type Prot_FLag is new Unsigned_32;

   S_IFMT : constant S_FLag := 0170000;

   S_IFDIR  : constant S_FLag := 0040000;
   S_IFCHR  : constant S_FLag := 0020000;
   S_IFBLK  : constant S_FLag := 0060000;
   S_IFREG  : constant S_FLag := 0100000;
   S_IFIFO  : constant S_FLag := 0010000;
   S_IFLNK  : constant S_FLag := 0120000;
   S_IFSOCK : constant S_FLag := 0140000;

   Nil : Void_Ptr renames System.Null_Address;

   MAP_PRIVATE : constant := 16#02#;
   MAP_FIXED : constant :=  16#10#;
   MAP_ANON : constant := 16#20#;
   MAP_HUGE_SHIFT : constant := 26;
   MAP_HUGE_MASK : constant := 16#3f#;

   S_IRUSR : constant S_FLag := 8#400#;

   S_IWUSR : constant S_FLag := 8#200#;

   S_IXUSR : constant S_FLag := 8#100#;

   S_IRWXU : constant S_FLag := S_IRUSR or S_IWUSR or S_IXUSR;

   S_IRGRP : constant S_FLag := S_IRUSR / 8;

   S_IWGRP : constant S_FLag := S_IWUSR / 8;

   S_IXGRP : constant S_FLag := S_IXUSR / 8;

   S_IRWXG : constant S_FLag := S_IRWXU / 8;

   S_IROTH : constant S_FLag := S_IRGRP / 8;

   S_IWOTH : constant S_FLag := S_IWGRP / 8;

   S_IXOTH : constant S_FLag := S_IXGRP / 8;

   S_IRWXO : constant S_FLag := S_IRWXG / 8;

   O_RDONLY : constant O_FLag := 16#00#;

   O_WRONLY : constant O_FLag := 16#01#;

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

   procedure Set_File_Descriptor_Flag_Non_Blocking
     (File_Descriptor : in out Interfaces.C.int);

   function C_Close
     (File_Descriptor : Interfaces.C.int) return Interfaces.C.int with
     Import        => True,
     Convention    => C,
     External_Name => "close";

   function C_Write
     (File_Descriptor : Interfaces.C.int;
      Buffer          : Ada.Streams.Stream_Element_Array;
      Count           : Size_Type) return SSize_Type with
     Import        => True,
     Convention    => C,
     External_Name => "write";

   function C_Read
     (File_Descriptor : Interfaces.C.int;
      Buffer          : in out Ada.Streams.Stream_Element_Array;
      Count           : Size_Type) return SSize_Type with
     Import        => True,
     Convention    => C,
     External_Name => "read";

   STDIN_FILENO  : constant := 0;
   STDOUT_FILENO : constant := 1;
   STDERR_FILENO : constant := 2;

   function C_Write
     (File_Descriptor : Integer;
      Buffer          : String;
      Count           : Size_Type) return SSize_Type with
     Import        => True,
     Convention    => C,
     External_Name => "write";

   function C_Poll (File_Descriptors        : Poll_File_Descriptor_Array;
                    File_Descriptors_Length : unsigned_long;
                    Timeout                 : Integer) return Integer with
     Import        => True,
     Convention    => C,
     External_Name => "poll";

   function Poll
     (File_Descriptors : Poll_File_Descriptor_Array;
      Timeout          : Integer) return Integer is
      (C_Poll (File_Descriptors, File_Descriptors'Length, Timeout));

   type File_Base is limited record
      My_File_Descriptor : Interfaces.C.int := -1;
   end record;

   type Socket_Base is limited record
      My_File_Descriptor : Interfaces.C.int := -1;
   end record;

end C_Binding.Linux;
