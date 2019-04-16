package C_Binding.Linux.Sockets is

private

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

   function C_Get_Internet_Address
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

   function C_Get_Internet_Address
     (
      Address_Family : int;  --  Either AF_INET or AF_INET6
      Source         : Interfaces.C.char_array;
      Destination    : System.Address
     ) return Interfaces.C.int with
       Import        => True,
       Convention    => C,
       External_Name => "inet_pton";
   --  Convert IPv4 and IPv6 addresses from text to binary form
   --  This function converts the character string src into a network
   --  address structure in the af address family, then copies the network
   --  address structure to dst.  The af argument must be either AF_INET or
   --  AF_INET6.  dst is written in network byte order.
   --
   --  The following address families are currently supported:
   --
   --  AF_INET
   --         src points to a character string containing an IPv4 network
   --         address in dotted-decimal format, "ddd.ddd.ddd.ddd", where ddd
   --         is a decimal number of up to three digits in the range 0 to
   --         255.  The address is converted to a struct in_addr and copied
   --         to dst, which must be sizeof(struct in_addr) (4) bytes (32
   --         bits) long.
   --
   --  AF_INET6
   --         src points to a character string containing an IPv6 network
   --         address.  The address is converted to a struct in6_addr and
   --         copied to dst, which must be sizeof(struct in6_addr) (16)
   --         bytes (128 bits) long.  The allowed formats for IPv6 addresses
   --         follow these rules:
   --
   --         1. The preferred format is x:x:x:x:x:x:x:x.  This form
   --            consists of eight hexadecimal numbers, each of which
   --            expresses a 16-bit value (i.e., each x can be up to 4 hex
   --            digits).
   --
   --         2. A series of contiguous zero values in the preferred format
   --            can be abbreviated to ::.  Only one instance of :: can
   --            occur in an address.  For example, the loopback address
   --            0:0:0:0:0:0:0:1 can be abbreviated as ::1.  The wildcard
   --            address, consisting of all zeros, can be written as ::.
   --
   --         3. An alternate format is useful for expressing IPv4-mapped
   --            IPv6 addresses.  This form is written as
   --            x:x:x:x:x:x:d.d.d.d, where the six leading xs are
   --            hexadecimal values that define the six most-significant
   --            16-bit pieces of the address (i.e., 96 bits), and the ds
   --            express a value in dotted-decimal notation that defines the
   --            least significant 32 bits of the address.  An example of
   --            such an address is ::FFFF:204.152.189.116.
   --
   --         See RFC 2373 for further details on the representation of IPv6
   --         addresses.
   --
   --  inet_pton() returns 1 on success (network address was successfully
   --  converted).  0 is returned if src does not contain a character string
   --  representing a valid network address in the specified address family.
   --  If af does not contain a valid address family, -1 is returned and
   --  errno is set to EAFNOSUPPORT.

   function C_Host_To_Network_Short
     (Host : Interfaces.C.unsigned_short) return Interfaces.C.unsigned_short
     with
       Import        => True,
       Convention    => C,
       External_Name => "htons";
   --  Converts the unsigned short integer hostshort from host byte order to
   --  network byte order.

   function C_Bind
     (Socket_FD : Interfaces.C.int;
      Address   : access Internet_Socket_Address;
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

   function C_File_Control
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

   function C_Listen
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

   function C_Connect
     (Socket_Fd      : Interfaces.C.int;
      Address        : access Internet_Socket_Address;
      Address_Length : Interfaces.C.unsigned) return Interfaces.C.int with
     Import        => True,
     Convention    => C,
     External_Name => "connect";
   --  The connect() system call connects the socket referred to by the file
   --  descriptor sockfd to the address specified by addr.  The addrlen
   --  argument specifies the size of addr.  The format of the address in
   --  addr is determined by the address space of the socket sockfd; see
   --  socket(2) for further details.
   --
   --  If the socket sockfd is of type SOCK_DGRAM, then addr is the address
   --  to which datagrams are sent by default, and the only address from
   --  which datagrams are received.  If the socket is of type SOCK_STREAM
   --  or SOCK_SEQPACKET, this call attempts to make a connection to the
   --  socket that is bound to the address specified by addr.
   --
   --  Generally, connection-based protocol sockets may successfully
   --  connect() only once; connectionless protocol sockets may use
   --  connect() multiple times to change their association.  Connectionless
   --  sockets may dissolve the association by connecting to an address with
   --  the sa_family member of sockaddr set to AF_UNSPEC (supported on Linux
   --  since kernel 2.2).
   --
   --  If the connection or binding succeeds, zero is returned.
   --  On error, -1 is returned, and errno is set appropriately.

end C_Binding.Linux.Sockets;
