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

private

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

end C_Binding.Linux.GnuTLS.Sessions;
