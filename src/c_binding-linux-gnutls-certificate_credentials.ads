package C_Binding.Linux.GnuTLS.Certificate_Credentials is

   type Credentials is new Credentials_Base;

--     function Initialize
--       (This : in out Credentials) return Success_Flag;

   generic
      with procedure Handle_Success;
      with procedure Handle_Failure;
      This : access Credentials;
   procedure Allocate_Credentials;

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

end C_Binding.Linux.GnuTLS.Certificate_Credentials;
