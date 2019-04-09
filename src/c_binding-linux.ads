with Ada.Streams;
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

   type File_Permission is
     (
      Owner_Read,  Owner_Write,  Owner_Execute,
      Group_Read,  Group_Write,  Group_Execute,
      Others_Read, Others_Write, Others_Execute
     );

   type File_Permissions is array (File_Permission) of Boolean;

   type File_Mode is
     (
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

   subtype Stream_Element        is Ada.Streams.Stream_Element;
   subtype Stream_Element_Array  is Ada.Streams.Stream_Element_Array;
   subtype Stream_Element_Offset is Ada.Streams.Stream_Element_Offset;

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

   procedure Write (File : Linux.File; Bytes : Stream_Element_Array) with
     Global => null,
     Pre    => File.Is_Open;

   function Read
     (File  : Linux.File;
      Bytes : in out Stream_Element_Array) return SSize_Type with
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

   -- Share changes.
   MAP_SHARED : constant := 16#01#;

   type Socket_Base is limited private;

   type Socket is tagged limited private;

   procedure Send
     (This     : Socket;
      Elements : Ada.Streams.Stream_Element_Array;
      Last     : out Stream_Element_Offset);

   type Internet_Address is record
      Value : aliased Interfaces.C.unsigned;  --  load with inet_aton()
   end record;
   pragma Convention (C_Pass_By_Copy, Internet_Address);

   type Internet_Socket_Pad_Array is
     array (0 .. 7) of aliased Interfaces.C.unsigned_char;

   type Internet_Socket_Address is record
      Address_Family : aliased Interfaces.C.short;
      Port_Number    : aliased Interfaces.C.unsigned_short;
      Address        : aliased Internet_Address;
      Padding        : aliased Internet_Socket_Pad_Array;
   end record;
   pragma Convention (C_Pass_By_Copy, Internet_Socket_Address);

   type Epoll_Data (discr : Interfaces.C.unsigned := 0) is record
      case discr is
         when 0 =>
            ptr : System.Address;
         when 1 =>
            fd : aliased Interfaces.C.int;
         when 2 =>
            u32 : aliased Interfaces.Unsigned_32;
         when others =>
            u64 : aliased Interfaces.Unsigned_64;
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, Epoll_Data);
   pragma Unchecked_Union (Epoll_Data);

   type Epoll_Event is record
      Events : aliased Interfaces.C.unsigned;
      Data   : aliased Epoll_Data;
   end record;
   pragma Convention (C_Pass_By_Copy, Epoll_Event);

   function Is_Epoll_Error
     (Event_Flags : Interfaces.C.unsigned) return Boolean;

   function Has_Hang_Up_Happened
     (Event_Flags : Interfaces.C.unsigned) return Boolean;

   function Is_Data_Available_For_Reading
     (Event_Flags : Interfaces.C.unsigned) return Boolean;

   EPOLL_CTL_ADD : constant := 1;
   --  Add fd to the interest list and associate the settings
   --  specified in event with the internal file linked to fd.

   EPOLL_CTL_DEL : constant := 2;
   -- Remove (deregister) the target file descriptor fd from the
   -- interest list.  The event argument is ignored and can be NULL
   -- (but see BUGS below).

   EPOLL_CTL_MOD : constant := 3;
   --  Change the settings associated with fd in the interest list to
   --  the new settings specified in event.

   EPOLLIN        : constant := 1;
   --  The associated file is available for read(2) operations.

   EPOLLPRI       : constant Interfaces.C.unsigned := 2;
   --  There is an exceptional condition on the file descriptor.

   EPOLLOUT       : constant Interfaces.C.unsigned := 4;
   --  The associated file is available for write(2) operations.

   EPOLLRDNORM    : constant Interfaces.C.unsigned := 64;
   EPOLLRDBAND    : constant Interfaces.C.unsigned := 128;
   EPOLLWRNORM    : constant Interfaces.C.unsigned := 256;
   EPOLLWRBAND    : constant Interfaces.C.unsigned := 512;
   EPOLLMSG       : constant Interfaces.C.unsigned := 1024;

   EPOLLERR       : constant := 8;
   --  Error condition happened on the associated file descriptor.
   --  This event is also reported for the write end of a pipe when
   --  the read end has been closed.  epoll_wait(2) will always
   --  report for this event; it is not necessary to set it in
   --  events.

   EPOLLHUP       : constant := 16;
   --  Hang up happened on the associated file descriptor.
   --  epoll_wait(2) will always wait for this event; it is not
   --  necessary to set it in events.
   --
   --  Note that when reading from a channel such as a pipe or a
   --  stream socket, this event merely indicates that the peer
   --  closed its end of the channel.  Subsequent reads from the
   --  channel will return 0 (end of file) only after all outstanding
   --  data in the channel has been consumed.

   EPOLLRDHUP     : constant Interfaces.C.unsigned := 8192;
   --  Stream socket peer closed connection, or shut down writing
   --  half of connection.  (This flag is especially useful for writ-
   --  ing simple code to detect peer shutdown when using Edge Trig-
   --  gered monitoring.)

   EPOLLEXCLUSIVE : constant Interfaces.C.unsigned := 268435456;
   EPOLLWAKEUP    : constant Interfaces.C.unsigned := 536870912;
   EPOLLONESHOT   : constant Interfaces.C.unsigned := 1073741824;
   EPOLLET        : constant Interfaces.C.unsigned := 2147483648;

   MAX_EPOLL_EVENTS : constant := 64;

   subtype Epoll_Event_Index is Positive range 1 .. MAX_EPOLL_EVENTS;

   type Epoll_Event_Array is array (Epoll_Event_Index) of aliased Epoll_Event;
   --  This array cannot be defined as
   --     array (Positive range <>)
   --  because it will then not be possible to pass an instance of this
   --  array as a subprogram argument in an Ada binding to a C library
   --  because the type definition would be incompatible with C.

   function C_Socket
     (Domain   : Interfaces.C.int;
      Kind     : Interfaces.C.int;
      Protocol : Interfaces.C.int) return Interfaces.C.int with
     Import        => True,
     Convention    => C,
     External_Name => "socket";
   --  Creates an endpoint for communication and returns a file
   --  descriptor that refers to that endpoint.  The file descriptor
   --  returned by a successful call will be the lowest-numbered file
   --  descriptor not currently open for the process.
   --
   --  The domain argument specifies a communication domain; this selects
   --  the protocol family which will be used for communication.
   --
   --  Some socket types may not be implemented by all protocol families.
   --
   --  Since Linux 2.6.27, the type argument serves a second purpose: in
   --  addition to specifying a socket type, it may include the bitwise OR
   --  of any of the following values, to modify the behavior of socket():
   --  The protocol specifies a particular protocol to be used with the
   --  socket.  Normally only a single protocol exists to support a
   --  particular socket type within a given protocol family, in which case
   --  protocol can be specified as 0.  However, it is possible that many
   --  protocols may exist, in which case a particular protocol must be
   --  specified in this manner.  The protocol number to use is specific to
   --  the "communication domain" in which communication is to take place;
   --  see protocols(5).  See getprotoent(3) on how to map protocol name
   --  strings to protocol numbers.
   --
   --  Sockets of type SOCK_STREAM are full-duplex byte streams.  They do
   --  not preserve record boundaries.  A stream socket must be in a
   --  connected state before any data may be sent or received on it.  A
   --  connection to another socket is created with a connect(2) call.  Once
   --  connected, data may be transferred using read(2) and write(2) calls
   --  or some variant of the send(2) and recv(2) calls.  When a session has
   --  been completed a close(2) may be performed.  Out-of-band data may
   --  also be transmitted as described in send(2) and received as described
   --  in recv(2).
   --
   --  The communications protocols which implement a SOCK_STREAM ensure
   --  that data is not lost or duplicated.  If a piece of data for which
   --  the peer protocol has buffer space cannot be successfully transmitted
   --  within a reasonable length of time, then the connection is considered
   --  to be dead.  When SO_KEEPALIVE is enabled on the socket the protocol
   --  checks in a protocol-specific manner if the other end is still alive.
   --  A SIGPIPE signal is raised if a process sends or receives on a broken
   --  stream; this causes naive processes, which do not handle the signal,
   --  to exit.  SOCK_SEQPACKET sockets employ the same system calls as
   --  SOCK_STREAM sockets.  The only difference is that read(2) calls will
   --  return only the amount of data requested, and any data remaining in
   --  the arriving packet will be discarded.  Also all message boundaries
   --  in incoming datagrams are preserved.
   --
   --  SOCK_DGRAM and SOCK_RAW sockets allow sending of datagrams to
   --  correspondents named in sendto(2) calls.  Datagrams are generally
   --  received with recvfrom(2), which returns the next datagram along with
   --  the address of its sender.
   --
   --  On success, a file descriptor for the new socket is returned. On
   --  error, -1 is returned, and errno is set appropriately.

   --
   -- Address Families
   --

   AF_Unix : constant := 1;
   --  Local communication (pipes and file-domain)

   AF_INET : constant := 2;
   --  IPv4 Internet protocols

   SOCK_STREAM : constant := 1;
   -- Sequenced, reliable, connection-based byte streams.

   SOCK_DGRAM : constant := 2;
   -- Connectionless, unreliable datagrams of fixed maximum length.

   function Get_Internet_Address
     (Ipv4 : String) return Interfaces.C.unsigned with
     Import        => True,
     Convention    => C,
     External_Name => "inet_addr";
   --  Converts the Internet host address from IPv4 numbers-and-dots notation
   --  into binary data in network byte order. If the input is invalid,
   --  INADDR_NONE (usually -1) is returned. Use of this function is
   --  problematic because -1 is a valid address (255.255.255.255).
   --  Avoid its use in favor of inet_aton(), inet_pton(3), or getaddrinfo(3)
   --  which provide a cleaner way to indicate error return.

   function Host_To_Network_Short
     (Host : Interfaces.C.unsigned_short) return Interfaces.C.unsigned_short
     with
       Import        => True,
       Convention    => C,
       External_Name => "htons";
   --  Converts the unsigned short integer hostshort from host byte order to
   --  network byte order.

   function Bind
     (Socket_FD : Interfaces.C.int;
      Address   : access Linux.Internet_Socket_Address;
      Address_Length : Interfaces.C.unsigned) return Interfaces.C.int with
     Import        => True,
     Convention    => C,
     External_Name => "bind";
   --  When a socket is created with socket(2), it exists in a name space
   --  (address family) but has no address assigned to it.  bind() assigns
   --  the address specified by addr to the socket referred to by the file
   --  descriptor sockfd.  addrlen specifies the size, in bytes, of the
   --  address structure pointed to by addr.  Traditionally, this operation
   --  is called "assigning a name to a socket".

   function File_Control
     (File_Descriptor : Interfaces.C.int;
      Command         : Interfaces.C.int;
      Argument        : Interfaces.C.int) return Interfaces.C.int with
     Import        => True,
     Convention    => C,
     External_Name => "fcntl";
   --  Performs one of the operations described below on the open
   --  file descriptor fd.  The operation is determined by cmd.
   --
   --  fcntl() can take an optional third argument.  Whether or not this
   --  argument is required is determined by cmd.  The required argument
   --  type is indicated in parentheses after each cmd name (in most cases,
   --  the required type is int, and we identify the argument using the name
   --  arg), or void is specified if the argument is not required.

   F_DUPFD : constant := 0;
   --  dup

   F_GETFD : constant := 1;
   --  get close_on_exec

   F_SETFD : constant := 2;
   --  set/clear close_on_exec

   F_GETFL : constant := 3;
   --  get file->f_flags

   F_SETFL : constant := 4;
   --  set file->f_flags

   procedure Set_File_Descriptor_Flag_Non_Blocking
     (File_Descriptor : in out Interfaces.C.int);

   function Listen
     (Socket_File_Descriptor : Interfaces.C.int;
      Backlog                : Interfaces.C.int) return Interfaces.C.int with
     Import        => True,
     Convention    => C,
     External_Name => "listen";
   --  listen() marks the socket referred to by sockfd as a passive socket,
   --  that is, as a socket that will be used to accept incoming connection
   --  requests using accept(2).
   --
   --  The sockfd argument is a file descriptor that refers to a socket of
   --  type SOCK_STREAM or SOCK_SEQPACKET.
   --
   --  The backlog argument defines the maximum length to which the queue of
   --  pending connections for sockfd may grow. If a connection request arrives
   --  when the queue is full, the client may receive an error with
   --  an indication of ECONNREFUSED or, if the underlying protocol supports
   --  retransmission, the request may be ignored so that a later reattempt
   --  at connection succeeds.
   --
   --  On success, zero is returned. On error, -1 is returned,
   --  and errno is set appropriately.

   function Epoll_Create1
     (Flags : Interfaces.C.int) return Interfaces.C.int with
     Import        => True,
     Convention    => C,
     External_Name => "epoll_create1";
   --  Creates an epoll instance.
   --
   --  If flags is 0, then, other than the fact that the obsolete size
   --  argument is dropped, epoll_create1() is the same as epoll_create().
   --  The following value can be included in flags
   --  to obtain different behavior:

   function Epoll_Control
     (Epoll_Fd        : Interfaces.C.int;
      Operation       : Interfaces.C.int;
      File_Descriptor : Interfaces.C.int;
      Event           : access Epoll_Event) return Interfaces.C.int with
     Import        => True,
     Convention    => C,
     External_Name => "epoll_ctl";
   --  This system call is used to add, modify, or remove entries in the
   --  interest list of the epoll(7) instance referred to by the file
   --  descriptor epfd.  It requests that the operation op be performed for
   --  the target file descriptor, fd.
   --
   --  When successful, epoll_ctl() returns zero.  When an error occurs,
   --  epoll_ctl() returns -1 and errno is set appropriately.

   function Epoll_Wait
     (Epoll_Fd : Interfaces.C. int;
      Events   : access Epoll_Event_Array;
      Max_Events : Interfaces.C.int;
      Timeout   : Interfaces.C.int) return Interfaces.C.int with
     Import        => True,
     Convention    => C,
     External_Name => "epoll_wait";
   --  The epoll_wait() system call waits for events on the epoll(7)
   --  instance referred to by the file descriptor epfd.  The memory area
   --  pointed to by events will contain the events that will be available
   --  for the caller.  Up to maxevents are returned by epoll_wait().  The
   --  maxevents argument must be greater than zero.
   --
   --  The timeout argument specifies the number of milliseconds that
   --  epoll_wait() will block.  Time is measured against the
   --  CLOCK_MONOTONIC clock.  The call will block until either:
   --
   --    *  a file descriptor delivers an event;
   --    *  the call is interrupted by a signal handler; or
   --    *  the timeout expires.
   --
   --  Note that the timeout interval will be rounded up to the system clock
   --  granularity, and kernel scheduling delays mean that the blocking
   --  interval may overrun by a small amount.  Specifying a timeout of -1
   --  causes epoll_wait() to block indefinitely, while specifying a timeout
   --  equal to zero cause epoll_wait() to return immediately, even if no
   --  events are available.

   function Close
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

   function C_Accept
     (
      Socket_Fd      : Interfaces.C.int;
      Address        : access Internet_Socket_Address;
      Address_Length : access Interfaces.C.unsigned
     ) return Interfaces.C.int with
     Import        => True,
     Convention    => C,
       External_Name => "accept";
   --  The accept() system call is used with connection-based socket types
   --  (SOCK_STREAM, SOCK_SEQPACKET).  It extracts the first connection
   --  request on the queue of pending connections for the listening socket,
   --  sockfd, creates a new connected socket, and returns a new file
   --  descriptor referring to that socket.  The newly created socket is not
   --  in the listening state.  The original socket sockfd is unaffected by
   --  this call.

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

   function Read
     (File_Descriptor : Interfaces.C.int;
      Buffer          : System.Address;
      Count           : Size_Type) return SSize_Type with
     Import        => True,
     Convention    => C,
     External_Name => "read";

   function Shutdown
     (Socket_Fd : Interfaces.C.int;
      How : Interfaces.C.int) return Interfaces.C.int with
     Import        => True,
     Convention    => C,
     External_Name => "shutdown";
   --  The shutdown() function shall cause all or part of a full-duplex
   --  connection on the socket associated with the file descriptor socket
   --  to be shut down.
   --
   --  On successful completion, shutdown() shall return 0; otherwise, -1
   --  shall be returned and errno set to indicate the error.

   SHUT_RD : constant := 0;
   --  No more receptions.

   SHUT_WR : constant := 1;
   --  No more transmissions.

   SHUT_RDWR : constant := 2;
   --  No more receptions or transmissions.

   function Send
     (File_Descriptor : Interfaces.C.int;
      Buffer          : Stream_Element_Array;
      Count           : Size_Type) return SSize_Type with
     Import        => True,
     Convention    => C,
     External_Name => "write";

   function Set_Socket_Option
     (Socket_Fd : Interfaces.C.int;
      Level     : Interfaces.C.int;
      Option_Name : Interfaces.C.int;
      Option_Value : System.Address;
      Option_Length : Interfaces.C.unsigned) return Interfaces.C.int with
     Import        => True,
     Convention    => C,
     External_Name => "setsockopt";
   --  Manipulate options for the socket referred to by the file descriptor
   --  sockfd. Options may exist at multiple protocol levels; they are always
   --  present at the uppermost socket level.
   --
   --  When manipulating socket options, the level at which the option resides
   --  and the name of the option must be specified. To manipulate options
   --  at the sockets API level, level is specified as SOL_SOCKET.
   --  To manipulate options at any other level the protocol number
   --  of the appropriate protocol controlling the option is supplied.
   --  For example, to indicate that an option is to be interpreted
   --  by the TCP protocol, level should be set to the protocol number of TCP.
   --  The arguments optval and optlen are used to access option values
   --  for setsockopt(). For getsockopt() they identify a buffer in which
   --  the value for the requested option(s) are to be returned.
   --  For getsockopt(), optlen is a value-result argument,
   --  initially containing the size of the buffer pointed to by optval,
   --  and modified on return to indicate the actual size of the value
   --  returned. If no option value is to be supplied or returned,
   --  optval may be NULL.
   --
   --  Optname and any specified options are passed uninterpreted to the
   --  appropriate protocol module for interpretation. The include file
   --  <sys/socket.h> contains definitions for socket level options,
   --  described below. Options at other protocol levels vary in format
   --  and name; consult the appropriate entries in section 4 of the manual.
   --
   --  Most socket-level options utilize an int argument for optval.
   --  For setsockopt(), the argument should be nonzero to enable a boolean
   --  option, or zero if the option is to be disabled.
   --
   --  On success, zero is returned.
   --  On error, -1 is returned, and errno is set appropriately.

   SOL_SOCKET     : constant := 1;
   SO_DEBUG       : constant := 1;
   SO_REUSEADDR   : constant := 2;
   SO_TYPE        : constant := 3;
   SO_ERROR       : constant := 4;
   SO_DONTROUTE   : constant := 5;
   SO_BROADCAST   : constant := 6;
   SO_SNDBUF      : constant := 7;
   SO_RCVBUF      : constant := 8;
   SO_SNDBUFFORCE : constant := 32;
   SO_RCVBUFFORCE : constant := 33;
   SO_KEEPALIVE   : constant := 9;
   SO_OOBINLINE   : constant := 10;
   SO_NO_CHECK    : constant := 11;
   SO_PRIORITY    : constant := 12;
   SO_LINGER      : constant := 13;
   SO_BSDCOMPAT   : constant := 14;
   SO_REUSEPORT   : constant := 15;
   SO_PASSCRED    : constant := 16;
   SO_PEERCRED    : constant := 17;
   SO_RCVLOWAT    : constant := 18;
   SO_SNDLOWAT    : constant := 19;
   SO_RCVTIMEO    : constant := 20;
   SO_SNDTIMEO    : constant := 21;

private

   use type Void_Ptr;
   use type Interfaces.C.int;

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
         Buffer          : Stream_Element_Array;
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

      function Write
        (File_Descriptor : Integer;
         Buffer          : String;
         Count           : Size_Type) return SSize_Type with
        Import        => True,
        Convention    => C,
        External_Name => "write";

      function Read
        (File_Descriptor : Integer;
         Buffer          : in out Stream_Element_Array;
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

   type Socket_Base is limited record
      My_File_Descriptor : Interfaces.C.int := -1;
   end record;

   type Socket is tagged limited record
      My_File_Descriptor : Integer := -1;
   end record;

end C_Binding.Linux;
