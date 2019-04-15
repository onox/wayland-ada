with C_Binding.Linux.GnuTLS.Certificate_Credentials;

package C_Binding.Linux.GnuTLS.X509 is

   type Add_The_Systems_Default_Trusted_CAs_Result_Kind_Id is
     (
      Add_Systems_Trusted_CAs_Success,
      Add_Systems_Trusted_CAs_Failure,
      Add_Systems_Trusted_CAs_Unsupported
     );

   type Processed_Certificates_Count is new Natural;

   type Add_The_Systems_Default_Trusted_CAs_Result
     (Kind_Id : Add_The_Systems_Default_Trusted_CAs_Result_Kind_Id)
   is record
      case Kind_Id is
         when Add_Systems_Trusted_CAs_Success =>
            Certificates_Count : Processed_Certificates_Count;
         when Add_Systems_Trusted_CAs_Failure =>
            null;
         when Add_Systems_Trusted_CAs_Unsupported =>
            null;
      end case;
   end record;

   function Add_The_Systems_Default_Trusted_CAs
     (
      Credentials : Certificate_Credentials.Credentials
     ) return Add_The_Systems_Default_Trusted_CAs_Result;
   --  Add the system's default trusted certificate authorities.

   type Certificate_Format is
     (
      DER_Binary_Format,
      PEM_Text_Format
     );
   pragma Convention (C, Certificate_Format);

   for Certificate_Format use
     (
      DER_Binary_Format => 0,
      PEM_Text_Format   => 1
     );

   function Set_Key_File
     (Credentials      : Certificate_Credentials.Credentials;
      Certificate_File : String;
      Key_File         : String;
      Format           : Certificate_Format) return Success_Flag;
   --  This subprogram is useful for setting certificate/private key pair
   --  to be used for authorizing the connection when connecting to
   --  a https server in the case the https server only accepts
   --  connections to authorized clients.

private

   function C_Certificate_Set_x509_System_Trust
     (
      Credentials : Opaque_Certificate_Credentials_Ptr
     ) return Interfaces.C.int with
       Import        => True,
       Convention    => C,
       External_Name => "gnutls_certificate_set_x509_system_trust";
   --  This function adds the system's default trusted CAs in order to verify
   --  client or server certificates.
   --
   --  In the case the system is currently unsupported
   --  GNUTLS_E_UNIMPLEMENTED_FEATURE is returned.
   --
   --  Returns the number of certificates processed or
   --  a negative error code on error.

   function C_Certificate_Set_x509_Key_File
     (
      Credentials      : Opaque_Certificate_Credentials_Ptr;
      Certificate_File : Interfaces.C.char_array;
      Key_File         : Interfaces.C.char_array;
      Format           : Certificate_Format
     ) return Interfaces.C.int with
       Import        => True,
       Convention    => C,
       External_Name => "gnutls_certificate_set_x509_key_file";
   --  This function sets a certificate/private key pair in the
   --  gnutls_certificate_credentials_t structure. This function may be
   --  called more than once, in case multiple keys/certificates exist
   --- for the server. For clients that need to send more than its own
   --  end entity certificate, e.g., also an intermediate CA cert,
   --  then the certfile must contain the ordered certificate chain.
   --
   --  This function can also accept URLs at keyfile and certfile.
   --  In that case it will import the private key and certificate
   --  indicated by the URLs. Note that the supported URLs are the ones
   --  indicated by gnutls_url_is_supported().
   --
   --  Returns : GNUTLS_E_SUCCESS (0) on success, or a negative error code.

end C_Binding.Linux.GnuTLS.X509;
