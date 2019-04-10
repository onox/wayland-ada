with C_Binding.Linux.Sockets.TCP.Connection_Listeners;

package C_Binding.Linux.Event_Polls is

   MAX_EPOLL_EVENTS : constant := 64;

   subtype Epoll_Event_Index is Positive range 1 .. MAX_EPOLL_EVENTS;

   type Check_For_Events_Result_Id is
     (
      Error_Occurred,
      Timed_Out,
      Event_Registered
     );

   type Check_For_Events_Result (Id : Check_For_Events_Result_Id) is record
      case Id is
         when Error_Occurred =>
            null;
         when Timed_Out =>
            null;
         when Event_Registered =>
            Event_Count : Epoll_Event_Index;
      end case;
   end record;

   type Event_List is limited private;

   function Is_Epoll_Error
     (This  : Event_List;
      Index : Epoll_Event_Index) return Boolean;

   function Has_Hang_Up_Happened
     (This  : Event_List;
      Index : Epoll_Event_Index) return Boolean;

   function Is_Data_Available_For_Reading
     (This  : Event_List;
      Index : Epoll_Event_Index) return Boolean;

   procedure Get_Socket
     (This   : Event_List;
      Index  : Epoll_Event_Index;
      Socket : out C_Binding.Linux.Sockets.TCP.General_Socket);

   function Is_Event_Origin
     (This  : Event_List;
      Index : Epoll_Event_Index;
      Listener : C_Binding.Linux.Sockets.TCP.Connection_Listeners.Listener)
      return Boolean;

   type Read_Socket_Result_Id is
     (
      Read_Failure,
      Read_Success
     );

   type Read_Socket_Result (Id : Read_Socket_Result_Id) is record
      case Id is
         when Read_Failure => null;
         when Read_Success =>
            Element_Count : Ada.Streams.Stream_Element_Offset;
      end case;
   end record;

   function Read_Socket
     (
      This   : Event_List;
      Index  : Epoll_Event_Index;
      Buffer : access Ada.Streams.Stream_Element_Array
     ) return Read_Socket_Result;

   type Event_Poll_Watcher is limited private;

   function Initialize (This : in out Event_Poll_Watcher) return Success_Flag;

   function Add
     (This     : in out Event_Poll_Watcher;
      Listener : C_Binding.Linux.Sockets.TCP.Connection_Listeners.Listener)
      return Success_Flag;

   function Add
     (This   : in out Event_Poll_Watcher;
      Socket : C_Binding.Linux.Sockets.TCP.General_Socket)
      return Success_Flag;

   function Delete
     (This     : in out Event_Poll_Watcher;
      Listener : C_Binding.Linux.Sockets.TCP.Connection_Listeners.Listener)
      return Success_Flag;

   function Delete
     (
      This   : in out Event_Poll_Watcher;
      List   : Event_List;
      Index  : Epoll_Event_Index
     ) return Success_Flag;

   function Check_For_Events
     (This    : Event_Poll_Watcher;
      List    : in out Event_List;
      Timeout : Integer) return Check_For_Events_Result;

private

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

   type Epoll_Event_Array is array (Epoll_Event_Index) of aliased Epoll_Event;
   --  This array cannot be defined as
   --     array (Positive range <>)
   --  because it will then not be possible to pass an instance of this
   --  array as a subprogram argument in an Ada binding to a C library
   --  because the type definition would be incompatible with C.

   type Event_Poll_Watcher is limited record
      My_File_Descriptor : Interfaces.C.int := -1;
   end record;

   type Event_List is limited record
      My_Events : aliased Epoll_Event_Array;
   end record;

   function C_Epoll_Create1
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

   function C_Epoll_Control
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

   function C_Epoll_Wait
     (Epoll_Fd   : Interfaces.C. int;
      Events     : access Epoll_Event_Array;
      Max_Events : Interfaces.C.int;
      Timeout    : Interfaces.C.int) return Interfaces.C.int with
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
   --
   --  When successful, epoll_wait() returns the number of file descriptors
   --  ready for the requested I/O, or zero if no file descriptor became
   --  ready during the requested timeout milliseconds.  When an error
   --  occurs, epoll_wait() returns -1 and errno is set appropriately.

   function Is_Epoll_Error
     (Event_Flags : Interfaces.C.unsigned) return Boolean;

   function Has_Hang_Up_Happened
     (Event_Flags : Interfaces.C.unsigned) return Boolean;

   function Is_Data_Available_For_Reading
     (Event_Flags : Interfaces.C.unsigned) return Boolean;

end C_Binding.Linux.Event_Polls;
