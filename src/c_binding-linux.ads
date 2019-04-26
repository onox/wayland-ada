with Ada.Streams;
private with Ada.Unchecked_Conversion;

--  This package was originally named Posix but is nowadays Linux to avoid
--  name clash with the Ada binding to Posix named Florist.
package C_Binding.Linux is

   subtype Success_Flag is C_Binding.Success_Flag;

   type File_Base is limited private;

   type Socket_Base is limited private;

   --
   -- Constants to be used with C_Binding.C_errno:
   --

   EPERM   : constant :=  1;  -- Operation not permitted
   ENOENT  : constant :=  2;  -- No such file or directory
   ESRCH   : constant :=  3;  -- No such process
   EINTR   : constant :=  4;  -- Interrupted system call
   EIO     : constant :=  5;  -- I/O error
   ENXIO   : constant :=  6;  -- No such device or address
   E2BIG   : constant :=  7;  -- Argument list too long
   ENOEXEC : constant :=  8;  -- Exec format error
   EBADF   : constant :=  9;  -- Bad file number
   ECHILD  : constant := 10;  -- No child processes
   EAGAIN  : constant := 11;  -- Try again
   ENOMEM  : constant := 12;  -- Out of memory
   EACCES  : constant := 13;  -- Permission denied
   EFAULT  : constant := 14;  -- Bad address
   ENOTBLK : constant := 15;  -- Block device required
   EBUSY   : constant := 16;  -- Device or resource busy
   EEXIST  : constant := 17;  -- File exists
   EXDEV   : constant := 18;  -- Cross-device link
   ENODEV  : constant := 19;  -- No such device
   ENOTDIR : constant := 20;  -- Not a directory
   EISDIR  : constant := 21;  -- Is a directory
   EINVAL  : constant := 22;  -- Invalid argument
   ENFILE  : constant := 23;  -- File table overflow
   EMFILE  : constant := 24;  -- Too many open files
   ENOTTY  : constant := 25;  -- Not a typewriter
   ETXTBSY : constant := 26;  -- Text file busy
   EFBIG   : constant := 27;  -- File too large
   ENOSPC  : constant := 28;  -- No space left on device
   ESPIPE  : constant := 29;  -- Illegal seek
   EROFS   : constant := 30;  -- Read-only file system
   EMLINK  : constant := 31;  -- Too many links
   EPIPE   : constant := 32;  -- Broken pipe
   EDOM    : constant := 33;  -- Math argument out of domain of func
   ERANGE  : constant := 34;  -- Math result not representable

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
   -- There is data to read.

   POLLPRI : constant := 16#002#;
   -- There is urgent data to read.

   POLLOUT : constant := 16#004#;
   -- Writing now will not block.

   type S_FLag is new Unsigned_32;
   type O_FLag is new Unsigned_32;
   type Prot_FLag is new Unsigned_32;

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

   Nil : Void_Ptr renames System.Null_Address;

   --
   -- Sharing types (must choose one and only one of these).
   --

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

   procedure Set_File_Descriptor_Flag_Non_Blocking
     (File_Descriptor : in out Interfaces.C.int);

   function C_Close
     (File_Descriptor : Interfaces.C.int) return Interfaces.C.int with
     Import        => True,
     Convention    => C,
     External_Name => "close";
   --  Closes a file descriptor, so that it no longer refers to any
   --  file and may be reused.  Any record locks (see fcntl(2)) held on the
   --  file it was associated with, and owned by the process, are removed
   --  (regardless of the file descriptor that was used to obtain the lock).
   --
   --  If fd is the last file descriptor referring to the underlying open
   --  file description (see open(2)), the resources associated with the
   --  open file description are freed; if the file descriptor was the last
   --  reference to a file which has been removed using unlink(2), the file
   --  is deleted.
   --
   --  Returns zero on success.  On error, -1 is returned, and errno
   --  is set appropriately.

   function C_Write
     (File_Descriptor : Interfaces.C.int;
      Buffer          : Ada.Streams.Stream_Element_Array;
      Count           : Size_Type) return SSize_Type with
     Import        => True,
     Convention    => C,
     External_Name => "write";
   --  Writes up to Count bytes from the buffer
   --  to the file referred to by the file descriptor.
   --  The number of bytes written may be less than count if,
   --  for example, there is insufficient space on
   --  the underlying physical medium, or the RLIMIT_FSIZE resource limit
   --  is encountered (see setrlimit(2)), or the call was interrupted
   --  by a signal handler after having written less than count bytes.
   --
   --  On success, the number of bytes written is returned (zero indicates
   --  nothing was written).
   --  On error, -1 is returned, and errno is set appropriately.
   --
   --  If count is zero and fd refers to a regular file, then write()
   --  may return a failure status if one of the errors below is detected.
   --  If no errors are detected, 0 will be returned without causing
   --  any other effect. If count is zero and fd refers to a file other than
   --  a regular file, the results are not specified.
   --
   --  A successful return from write() does not make any guarantee that data
   --  has been committed to disk. In fact, on some buggy implementations,
   --  it does not even guarantee that space has successfully been reserved
   --  for the data. The only way to be sure is to call fsync(2) after you are
   --  done writing all your data.
   --
   --  If a write() is interrupted by a signal handler before any bytes are
   --  written, then the call fails with the error EINTR;
   --  if it is interrupted after at least one byte has been written,
   --  the call succeeds, and returns the number of bytes written.

   function C_Read
     (File_Descriptor : Interfaces.C.int;
      Buffer          : in out Ada.Streams.Stream_Element_Array;
      Count           : Size_Type) return SSize_Type with
     Import        => True,
     Convention    => C,
     External_Name => "read";
   --  On success, the number of bytes read is returned (zero indicates
   --  end of file), and the file position is advanced by this number.
   --  It is not an error if this number is smaller than the number of bytes
   --  requested; this may happen for example because fewer bytes are actually
   --  available right now (maybe because we were close to end-of-file,
   --  or because we are reading from a pipe, or from a terminal),
   --  or because read() was interrupted by a signal.
   --  On error, -1 is returned, and errno is set appropriately.
   --  In this case it is left unspecified whether the file position
   --  (if any) changes.

   --
   -- Standard file descriptors.
   --
   STDIN_FILENO  : constant := 0; -- Standard input.
   STDOUT_FILENO : constant := 1; -- Standard output.
   STDERR_FILENO : constant := 2; -- Standard error output.

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
