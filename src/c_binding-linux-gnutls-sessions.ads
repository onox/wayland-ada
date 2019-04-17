with C_Binding.Linux.Sockets.TCP_Client;

package C_Binding.Linux.GnuTLS.Sessions is

   type Session is new Session_Base;

   generic
      with procedure Handle_Success;
      with procedure Handle_Failure;
      This : access Session;
   procedure Initialize_Session;

   function Set_Server_Name
     (This : Session;
      Name : String) return Success_Flag;
   --  This should be used by clients that connect to servers
   --  that do virtual hosting.

   function Set_Default_Priority
     (This : Session) return Success_Flag;

   procedure Verify_Certificate_Using_Hostname (This : Session);

   procedure Associate_With_Client_Socket
     (This   : Session;
      Socket : Linux.Sockets.TCP_Client.Client_Socket);

   procedure Set_Default_Handshake_Timeout (This : Session);

   type Handshake_Result_Kind_Id is
     (
      Handshake_Success,
      Certificate_Verification_Failure,
      Handshake_Failure
     );

   type Handshake_Result
     (
      Kind_Id : Handshake_Result_Kind_Id;
      Length  : Natural
     )
   is record
      case Kind_Id is
         when Handshake_Success =>
            null;
         when Certificate_Verification_Failure | Handshake_Failure =>
            Error_Message : String (1 .. Length);
      end case;
   end record;

   function Perform_Handshake (This : Session) return Handshake_Result;

   function Description (This : Session) return String;
   --  Probably makes heap allocations under the hood.
   --  Therefore avoid use in performance critical code.

   type Send_Result_Kind_Id is
     (
      Send_Success,
      Send_Failure
     );

   type Send_Result (Kind_Id : Send_Result_Kind_Id) is record
      case Kind_Id is
         when Send_Success =>
            Elements_Count : Ada.Streams.Stream_Element_Offset;
         when Send_Failure =>
            null;
      end case;
   end record;

   function Send
     (This : Session;
      Data : Ada.Streams.Stream_Element_Array) return Send_Result;

   type Receive_Result_Kind_Id is
     (
      Receive_Success,
      Receive_Success_But_End_Of_File,
      Receive_Failure
     );

   type Receive_Result (Kind_Id : Receive_Result_Kind_Id) is record
      case Kind_Id is
         when Receive_Success =>
            Elements_Count : Ada.Streams.Stream_Element_Offset;
         when Receive_Success_But_End_Of_File =>
            null;
         when Receive_Failure =>
            null;
      end case;
   end record;

   function Receive
     (This : Session;
      Data : in out Ada.Streams.Stream_Element_Array) return Receive_Result;

private

   GNUTLS_E_CERTIFICATE_VERIFICATION_ERROR : constant := -348;

   function C_Init
     (
      Session : access Opaque_Session_Ptr;
      Flags   : Interfaces.C.unsigned
     ) return Interfaces.C.int with
       Import        => True,
       Convention    => C,
       External_Name => "gnutls_init";
   --  This function initializes the current session to null.
   --  Every session must be initialized before use, so internal structures
   --  can be allocated. This function allocates structures which
   --  can only be free'd by calling gnutls_deinit().
   --  Returns GNUTLS_E_SUCCESS (0) on success.
   --
   --  flags can be one of GNUTLS_CLIENT and GNUTLS_SERVER.
   --  For a DTLS entity, the flags GNUTLS_DATAGRAM and GNUTLS_NONBLOCK are
   --  also available. The latter flag will enable a non-blocking operation
   --  of the DTLS timers.
   --
   --  Note that since version 3.1.2 this function enables some common TLS
   --  extensions such as session tickets and OCSP certificate status request
   --  in client side by default. To prevent that use the
   --  GNUTLS_NO_EXTENSIONS flag.

   procedure C_Deinit (Session : Opaque_Session_Ptr) with
     Import        => True,
     Convention    => C,
     External_Name => "gnutls_deinit";
   --  This function clears all buffers associated with the session.
   --  This function will also remove session data from the session database
   --  if the session was terminated abnormally.

   type Server_Name_Kind is
     (
      GNUTLS_NAME_DNS
     );

   for Server_Name_Kind use
     (
      GNUTLS_NAME_DNS => 1
     );

   pragma Convention (C, Server_Name_Kind);

   function C_Server_Name_Set
     (
      Session     : Opaque_Session_Ptr;
      Name_Kind   : Server_Name_Kind;
      Name        : Interfaces.C.char_array;
      Name_Length : Size_Type
     ) return Interfaces.C.int with
       Import        => True,
       Convention    => C,
       External_Name => "gnutls_server_name_set";
   --  This function is to be used by clients that want to inform
   --  (via a TLS extension mechanism) the server of the name they
   --  connected to. This should be used by clients that connect to servers
   --  that do virtual hosting.
   --
   --  The value of name depends on the type type. In case of GNUTLS_NAME_DNS,
   --  an ASCII (0)-terminated domain name string, without the trailing dot,
   --  is expected. IPv4 or IPv6 addresses are not permitted.
   --
   --  Returns : On success, GNUTLS_E_SUCCESS (0) is returned,
   --  otherwise a negative error code is returned.

   function C_Set_Default_Priority
     (
      Session : Opaque_Session_Ptr
     ) return Interfaces.C.int with
       Import        => True,
       Convention    => C,
       External_Name => "gnutls_set_default_priority";
   --  Sets some default priority on the ciphers, key exchange methods,
   --  macs and compression methods.
   --
   --  This is the same as calling:
   --
   --  gnutls_priority_set_direct (session, "NORMAL", NULL);
   --
   --  This function is kept around for backwards compatibility,
   --  but because of its wide use it is still fully supported.
   --  If you wish to allow users to provide a string that specify which
   --  ciphers to use (which is recommended), you should use
   --  gnutls_priority_set_direct() or gnutls_priority_set() instead.
   --
   --  Returns : GNUTLS_E_SUCCESS on success, or an error code.

   type Certificate_Verify_Flags is mod 2 ** 32;
   for Certificate_Verify_Flags'Size use Interfaces.C.unsigned'Size;

   DISABLE_CA_SIGN             : constant Certificate_Verify_Flags := 2 ** 0;
   DO_NOT_ALLOW_SAME           : constant Certificate_Verify_Flags := 2 ** 2;
   ALLOW_ANY_X509_V1_CA_CRT    : constant Certificate_Verify_Flags := 2 ** 3;
   ALLOW_SIGN_RSA_MD2          : constant Certificate_Verify_Flags := 2 ** 4;
   ALLOW_SIGN_RSA_MD5          : constant Certificate_Verify_Flags := 2 ** 5;
   DISABLE_TIME_CHECKS         : constant Certificate_Verify_Flags := 2 ** 6;
   DISABLE_TRUSTED_TIME_CHECKS : constant Certificate_Verify_Flags := 2 ** 7;
   DO_NOT_ALLOW_X509_V1_CA_CRT : constant Certificate_Verify_Flags := 2 ** 8;
   DISABLE_CRL_CHECKS          : constant Certificate_Verify_Flags := 2 ** 9;

   ALLOW_UNSORTED_CHAIN        : constant Certificate_Verify_Flags := 2 ** 10;
   DO_NOT_ALLOW_UNSORTED_CHAIN : constant Certificate_Verify_Flags := 2 ** 11;
   DO_NOT_ALLOW_WILDCARDS      : constant Certificate_Verify_Flags := 2 ** 12;
   USE_TLS1_RSA                : constant Certificate_Verify_Flags := 2 ** 13;

   procedure C_Session_Set_Verify_Cert
     (
      Session   : Opaque_Session_Ptr;
      Host_Name : Interfaces.C.Strings.chars_ptr;
      Flags     : Interfaces.C.unsigned
     ) with
       Import        => True,
       Convention    => C,
       External_Name => "gnutls_session_set_verify_cert";
   --  This function instructs GnuTLS to verify the peer's certificate
   --  using the provided hostname. If the verification fails the handshake
   --  will also fail with GNUTLS_E_CERTIFICATE_VERIFICATION_ERROR.
   --  In that case the verification result can be obtained using
   --  gnutls_session_get_verify_cert_status().
   --
   --  The hostname pointer provided must remain valid for the lifetime
   --  of the session. More precisely it should be available during any
   --  subsequent handshakes. If no hostname is provided,
   --  no hostname verification will be performed. For a more advanced
   --  verification function check gnutls_session_set_verify_cert2().
   --
   --  If flags is provided which contain a profile, this function should
   --  be called after any session priority setting functions.
   --
   --  The gnutls_session_set_verify_cert() function is intended to be used
   --  by TLS clients to verify the server's certificate.

   procedure C_Transport_Set_Int2
     (Session             : Opaque_Session_Ptr;
      Receiving_Socket_Fd : Interfaces.C.int;
      Sending_Socket_Fd   : Interfaces.C.int) with
     Import        => True,
     Convention    => C,
     External_Name => "gnutls_transport_set_int2";
   --  This function sets the first argument of the transport functions,
   --  such as send() and recv() for the default callbacks using
   --  the system's socket API. With this function you can set two different
   --  descriptors for receiving and sending.
   --
   --  This function is equivalent to calling gnutls_transport_set_ptr2()
   --  with the descriptors, but requires no casts.

   GNUTLS_DEFAULT_HANDSHAKE_TIMEOUT : constant := -1;

   procedure C_Handshake_Set_Timeout
     (Session      : Opaque_Session_Ptr;
      Milliseconds : Interfaces.C.int) with
     Import        => True,
     Convention    => C,
     External_Name => "gnutls_handshake_set_timeout";
   --  This function sets the timeout for the TLS handshake process
   --  to the provided value. Use an ms value of zero to disable timeout,
   --  or GNUTLS_DEFAULT_HANDSHAKE_TIMEOUT for a reasonable default value.
   --  For the DTLS protocol, the more detailed
   --  gnutls_dtls_set_timeouts() is provided.
   --
   --  This function requires to set a pull timeout callback.
   --  See gnutls_transport_set_pull_timeout_function().

   function C_Handshake
     (
      Session : Opaque_Session_Ptr
     ) return Interfaces.C.int with
       Import        => True,
       Convention    => C,
       External_Name => "gnutls_handshake";
   --  This function performs the handshake of the TLS/SSL protocol,
   --  and initializes the TLS session parameters.
   --
   --  The non-fatal errors expected by this function are:
   --  GNUTLS_E_INTERRUPTED, GNUTLS_E_AGAIN, GNUTLS_E_WARNING_ALERT_RECEIVED.
   --  When this function is called for re-handshake under TLS 1.2 or earlier,
   --  the non-fatal error code GNUTLS_E_GOT_APPLICATION_DATA may
   --  also be returned.
   --
   --  The former two interrupt the handshake procedure due to
   --  the transport layer being interrupted, and the latter because of a
   --  "warning" alert that was sent by the peer (it is always a good idea
   --  to check any received alerts). On these non-fatal errors call this
   --  function again, until it returns 0; cf. gnutls_record_get_direction()
   --  and gnutls_error_is_fatal(). In DTLS sessions the non-fatal error
   --  GNUTLS_E_LARGE_PACKET is also possible, and indicates that the MTU
   --  should be adjusted.
   --
   --  When this function is called by a server after a rehandshake request
   --  under TLS 1.2 or earlier the GNUTLS_E_GOT_APPLICATION_DATA error code
   --  indicates that some data were pending prior to peer initiating
   --  the handshake. Under TLS 1.3 this function when called after
   --  a successful handshake, is a no-op and always succeeds in server side;
   --  in client side this function is equivalent to
   --  gnutls_session_key_update() with GNUTLS_KU_PEER flag.
   --
   --  This function handles both full and abbreviated TLS handshakes
   --  (resumption). For abbreviated handshakes, in client side,
   --  the gnutls_session_set_data() should be called prior to this function
   --  to set parameters from a previous session. In server side,
   --  resumption is handled by either setting a DB back-end,
   --  or setting up keys for session tickets.
   --
   --  GNUTLS_E_SUCCESS on a successful handshake,
   --  otherwise a negative error code.

   function C_Error_Is_Fatal
     (
      Error_Code : Interfaces.C.int
     ) return Interfaces.C.int with
       Import        => True,
       Convention    => C,
       External_Name => "gnutls_error_is_fatal";
   --  If a GnuTLS function returns a negative error code you may feed that
   --  value to this function to see if the error condition is fatal
   --  to a TLS session (i.e., must be terminated).
   --
   --  Note that you may also want to check the error code manually,
   --  since some non-fatal errors to the protocol (such as a warning alert
   --  or a rehandshake request) may be fatal for your program.
   --
   --  This function is only useful if you are dealing with errors
   --  from functions that relate to a TLS session (e.g., record layer or
   --  handshake layer handling functions).
   --
   --  Non-zero value on fatal errors or zero on non-fatal.

   function C_String_Error
     (
      Error_Code : Interfaces.C.int
     ) return Interfaces.C.Strings.chars_ptr with
       Import        => True,
       Convention    => C,
       External_Name => "gnutls_strerror";
   --  This function is similar to strerror.  The difference is that it
   --  accepts an error number returned by a gnutls function; In case of an
   --  unknown error a descriptive string is sent instead of NULL.
   --
   --  Error codes are always a negative error code.
   --
   --  Returns a string explaining the GnuTLS error message.

   function C_Session_Get_Description
     (
      Session : Opaque_Session_Ptr
     ) return Interfaces.C.Strings.chars_ptr with
       Import        => True,
       Convention    => C,
       External_Name => "gnutls_session_get_desc";
   --  This function returns a string describing the current session.
   --  The string is null terminated and allocated using gnutls_malloc().
   --
   --  If initial negotiation is not complete when this function is called,
   --  NULL will be returned.
   --
   --  Returns a description of the protocols and algorithms
   --  in the current session.

   function C_Session_Get_Description
     (
      Session : Opaque_Session_Ptr
     ) return System.Address with
       Import        => True,
       Convention    => C,
       External_Name => "gnutls_session_get_desc";

--     procedure C_Free (Address : System.Address) with
--       Import        => True,
--       Convention    => C,
--       External_Name => "gnutls_free";

   function C_Record_Send
     (
      Session   : Opaque_Session_Ptr;
      Data      : System.Address;
      Data_Size : Size_Type
     ) return SSize_Type with
       Import        => True,
       Convention    => C,
       External_Name => "gnutls_record_send";
   --  This function has the similar semantics with send().
   --  The only difference is that it accepts a GnuTLS session,
   --  and uses different error codes. Note that if the send buffer is full,
   --  send() will block this function. See the send() documentation for
   --  full information. You can replace the default push function
   --  by using gnutls_transport_set_ptr2() with a call to send() with
   --  a MSG_DONTWAIT flag if blocking is a problem. If the EINTR is returned
   --  by the internal push function (the default is send()) then
   --  GNUTLS_E_INTERRUPTED will be returned. If GNUTLS_E_INTERRUPTED
   --  or GNUTLS_E_AGAIN is returned, you must call this function again,
   --  with the same parameters; alternatively you could provide a NULL pointer
   --  for data, and 0 for size. cf. gnutls_record_get_direction().
   --  The errno value EMSGSIZE maps to GNUTLS_E_LARGE_PACKET.
   --
   --  Returns : The number of bytes sent, or a negative error code.
   --  The number of bytes sent might be less than data_size.
   --  The maximum number of bytes this function can send in a single call
   --  depends on the negotiated maximum record size.

   function C_Record_Receive
     (
      Session   : Opaque_Session_Ptr;
      Data      : System.Address;
      Data_Size : Size_Type
     ) return SSize_Type with
       Import        => True,
       Convention    => C,
       External_Name => "gnutls_record_recv";
   --  This function has the similar semantics with recv(). The only difference
   --  is that it accepts a GnuTLS session, and uses different error codes.
   --  In the special case that a server requests a renegotiation,
   --  the client may receive an error code of GNUTLS_E_REHANDSHAKE.
   --  This message may be simply ignored, replied with an alert
   --  GNUTLS_A_NO_RENEGOTIATION, or replied with a new handshake,
   --  depending on the client's will. If EINTR is returned by
   --  the internal push function (the default is recv()) then
   --  GNUTLS_E_INTERRUPTED will be returned. If GNUTLS_E_INTERRUPTED or
   --  GNUTLS_E_AGAIN is returned, you must call this function again to get
   --  the data. See also gnutls_record_get_direction(). A server may also
   --  receive GNUTLS_E_REHANDSHAKE when a client has initiated a handshake.
   --  In that case the server can only initiate a handshake or
   --  terminate the connection.
   --
   --  Returns : The number of bytes received and zero on EOF
   --  (for stream connections). A negative error code is returned in case
   --  of an error. The number of bytes received might be less than
   --  the requested data_size.

end C_Binding.Linux.GnuTLS.Sessions;
