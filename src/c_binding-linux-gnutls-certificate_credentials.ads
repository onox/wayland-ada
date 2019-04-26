with C_Binding.Linux.GnuTLS.Sessions;

pragma Elaborate_All (C_Binding.Linux.GnuTLS.Sessions);

package C_Binding.Linux.GnuTLS.Certificate_Credentials is

   type Credentials is new Credentials_Base;

   generic
      with procedure Handle_Success;
      with procedure Handle_Failure;
      This : access Credentials;
   procedure Allocate_Credentials;

   function Set_Session_Credentials
     (This    : Credentials;
      Session : Sessions.Session) return Success_Flag;

private

   function C_Certificate_Allocate_Credentials
     (
      Credentials : access Opaque_Certificate_Credentials_Ptr
     ) return Interfaces.C.int with
       Import        => True,
       Convention    => C,
       External_Name => "gnutls_certificate_allocate_credentials";
   --   This structure is complex enough to manipulate directly thus this
   --  helper function is provided in order to allocate it.
   --
   --  Returns : GNUTLS_E_SUCCESS on success, or an error code.

   procedure C_Certificate_Free_Credentials
     (
      Credentials : Opaque_Certificate_Credentials_Ptr
     ) with
       Import        => True,
       Convention    => C,
       External_Name => "gnutls_certificate_free_credentials";
   --  This structure is complex enough to manipulate directly thus this
   --  helper function is provided in order to free (deallocate) it.
   --
   --  This function does not free any temporary parameters associated
   --  with this structure (ie RSA and DH parameters are not freed by
   --  this function).

   function C_Credentials_Set
     (
      Session     : Opaque_Session_Ptr;
      Kind        : Credentials_Kind;
      Credentials : Opaque_Certificate_Credentials_Ptr
     ) return Interfaces.C.int with
       Import        => True,
       Convention    => C,
       External_Name => "gnutls_credentials_set";
   --  Sets the needed credentials for the specified type. Eg username,
   --  password - or public and private keys etc. The cred parameter
   --  is a structure that depends on the specified type and on
   --  the current session (client or server).
   --
   --  In order to minimize memory usage, and share credentials between
   --  several threads gnutls keeps a pointer to cred, and not the whole
   --  cred structure. Thus you will have to keep the structure allocated
   --  until you call gnutls_deinit().
   --
   --  For GNUTLS_CRD_ANON, cred should be gnutls_anon_client_credentials_t
   --  in case of a client. In case of a server it should be
   --  gnutls_anon_server_credentials_t.
   --
   --  For GNUTLS_CRD_SRP, cred should be gnutls_srp_client_credentials_t
   --  in case of a client, and gnutls_srp_server_credentials_t,
   --  in case of a server.
   --
   --  For GNUTLS_CRD_CERTIFICATE, cred should be
   --  gnutls_certificate_credentials_t.
   --
   --  Returns : On success, GNUTLS_E_SUCCESS (0) is returned,
   --  otherwise a negative error code is returned.

end C_Binding.Linux.GnuTLS.Certificate_Credentials;
