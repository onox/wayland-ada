package C_Binding.Linux.Sockets is

private

   function C_Set_Socket_Option
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

   SHUT_RD : constant := 0;
   --  No more receptions.

   SHUT_WR : constant := 1;
   --  No more transmissions.

   SHUT_RDWR : constant := 2;
   --  No more receptions or transmissions.

   function C_Shutdown
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

end C_Binding.Linux.Sockets;
