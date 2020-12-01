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

   procedure C_Certificate_Free_Credentials
     (
      Credentials : Opaque_Certificate_Credentials_Ptr
     ) with
       Import        => True,
       Convention    => C,
       External_Name => "gnutls_certificate_free_credentials";

   function C_Credentials_Set
     (
      Session     : Opaque_Session_Ptr;
      Kind        : Credentials_Kind;
      Credentials : Opaque_Certificate_Credentials_Ptr
     ) return Interfaces.C.int with
       Import        => True,
       Convention    => C,
       External_Name => "gnutls_credentials_set";

end C_Binding.Linux.GnuTLS.Certificate_Credentials;
