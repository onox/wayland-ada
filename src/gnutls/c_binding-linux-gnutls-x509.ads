with C_Binding.Linux.GnuTLS.Certificate_Credentials;

--  At its core an X.509 certificate is a digital document that has been
--  encoded and/or digitally signed according to RFC 5280.
--  Please see the file /docs/rfc5280.txt for further details.
--
--  In fact, the term X.509 certificate usually refers to the IETF's
--  PKIX Certificate and CRL Profile of the X.509 v3 certificate standard,
--  as specified in RFC 5280, commonly referred to as PKIX
--  for Public Key Infrastructure (X.509).
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
      --  The DER extension is used for binary DER encoded certificates.
      --  These files may also bear the CER or the CRT extension.
      --  Proper English usage would be "I have a DER encoded certificate"
      --  not "I have a DER certificate".

      PEM_Text_Format
      --  The PEM extension is used for different types of X.509v3 files
      --  which contain ASCII (Base64) armored data prefixed
      --  with a "--- BEGIN ..." line.
     );
   --  A X509 cerficate can be encoded in one of two ways and this
   --  enumeration type specifies the encoding.
   --  This enumeration type is intimately connected to X509 file extensions
   --  and how certificates are encoded inside the X509 files.
   --  The first thing we have to understand is what each type of file
   --  extension is. There is a lot of confusion about what DER, PEM, CRT,
   --  and CER are and many have incorrectly said that they are all
   --  interchangeable.  While in certain cases some can be interchanged
   --  the best practice is to identify how your certificate is encoded
   --  and then label it correctly. Correctly labeled certificates will be
   --  much easier to manipulate.
   --
   --  Common file extensions:
   --
   --  .crt
   --  The CRT extension is used for certificates. The certificates may be
   --  encoded as binary DER or as ASCII PEM. The CER and CRT extensions
   --  are nearly synonymous.  Most common among *nix systems.
   --
   --  .cer
   --  A .cer file is an alternate form of .crt (Microsoft Convention).
   --  You can use MS to convert .crt to .cer (.both DER encoded .cer,
   --  or base64[PEM] encoded .cer).
   --  The .cer file extension is also recognized by IE as a command
   --  to run a MS cryptoAPI command (specifically rundll32.exe cryptext.dll,
   --  CryptExtOpenCER) which displays a dialogue for importing and/or viewing
   --  certificate contents.
   --
   --  .key
   --  The KEY extension is used both for public and private PKCS#8 keys.
   --  The keys may be encoded as binary DER or as ASCII PEM.
   --
   --  The only time CRT and CER can safely be interchanged is when
   --  the encoding type can be identical.
   --  (for example PEM encoded CRT = PEM encoded CER)

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

   type Set_Trust_CAs_File_Result_Kind_Id is
     (
      Set_Trust_CAs_File_Success,
      Set_Trust_CAs_File_Failure
     );

   type Set_Trust_CAs_File_Result
     (Kind_Id : Set_Trust_CAs_File_Result_Kind_Id)
   is record
      case Kind_Id is
         when Set_Trust_CAs_File_Success =>
            Processed_Certificates_Count : Natural;
         when Set_Trust_CAs_File_Failure =>
            null;
      end case;
   end record;

   function Set_Trust_CAs_File
     (Credentials : Certificate_Credentials.Credentials;
      Name        : String;  --  Certificate Authority File Name
      Format      : Certificate_Format) return Set_Trust_CAs_File_Result;

   type Set_CRL_File_Result_Kind_Id is
     (
      Set_CRL_File_Success,
      Set_CRL_File_Failure
     );

   type Set_CRL_File_Result
     (Kind_Id : Set_CRL_File_Result_Kind_Id)
   is record
      case Kind_Id is
         when Set_CRL_File_Success =>
            Processed_Certificates_Count : Natural;
         when Set_CRL_File_Failure =>
            null;
      end case;
   end record;

   function Set_CRL_File
     (Credentials : Certificate_Credentials.Credentials;
      Name        : String;
      Format      : Certificate_Format) return Set_CRL_File_Result;
   --  CRL is short for Certificate Revocation List.
   --
   --  Important! RFC 5280 requires the CRL to be encoded using DER, not PEM!
   --
   --  Publish the CRL at a publicly accessible location
   --  (eg, http://example.com/intermediate.crl.pem). Third-parties can
   --  fetch the CRL from this location to check whether any certificates
   --  they rely on have been revoked.
   --  Is it best-practise that a web server publicly shares a
   --  certificate revocation list?
   --  When a certificate authority signs a certificate, it will normally
   --  encode the CRL location into the certificate. Add crlDistributionPoints
   --  to the appropriate sections. In our case, add it
   --  to the [ server_cert ] section.
   --  [ server_cert ]
   --  # ... snipped ...
   --  crlDistributionPoints = URI:http://example.com/intermediate.crl.pem
   --
   --
   --  To make a private 4096-bit key (or is it public?)
   --  openssl genrsa -out ca.key 4096
   --
   --  The key is saved in clear-text. To encrypt it with a password:
   --  openssl genrsa -des3 -out ca.key 4096

private

   function C_Certificate_Set_x509_System_Trust
     (
      Credentials : access Opaque_Certificate_Credentials
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
      Credentials      : access Opaque_Certificate_Credentials;
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

   function C_Set_X509_Trust_File
     (
      Credentials : access Opaque_Certificate_Credentials;
      CA_File     : Interfaces.C.char_array;
      Format      : Certificate_Format
     ) return Interfaces.C.int with
       Import        => True,
       Convention    => C,
       External_Name => "gnutls_certificate_set_x509_trust_file";
   --  This function adds the trusted CAs in order to verify client
   --  or server certificates. In case of a client this is not required
   --  to be called if the certificates are not verified using
   --  gnutls_certificate_verify_peers2(). This function may be called
   --  multiple times.
   --
   --  In case of a server the names of the CAs set here will be sent to
   --  the client if a certificate request is sent. This can be disabled
   --  using gnutls_certificate_send_x509_rdn_sequence().
   --
   --  This function can also accept URLs. In that case it will import all
   --  certificates that are marked as trusted. Note that the supported URLs
   --  are the ones indicated by gnutls_url_is_supported().
   --
   --  Returns number of certificates processed,
   --  or a negative error code on error.

   function C_Set_X509_CRL_File
     (
      Credentials : access Opaque_Certificate_Credentials;
      CRL_File    : Interfaces.C.char_array;
      Format      : Certificate_Format
     ) return Interfaces.C.int with
       Import        => True,
       Convention    => C,
       External_Name => "gnutls_certificate_set_x509_crl_file";
   --  This function adds the trusted CRLs in order to verify client or server
   --  certificates. In case of a client this is not required to be called
   --  if the certificates are not verified using
   --  gnutls_certificate_verify_peers2(). This function may be called
   --  multiple times.
   --
   --  Returns number of CRLs processed or a negative error code on error.

end C_Binding.Linux.GnuTLS.X509;
--  Potentially good to know:
--
--  Common OpenSSL Certificate Manipulations
--
--  There are four basic types of certificate manipulations.
--  View, Transform, Combination, and Extraction
--
--  View
--
--  Even though PEM encoded certificates are ASCII they are not human readable.
--  Here are some commands that will let you output the contents
--  of a certificate in human readable form.
--
--  View PEM encoded certificate
--
--  Use the command that has the extension of your certificate replacing
--  cert.xxx with the name of your certificate
--
--  openssl x509 -in cert.pem -text -noout
--  openssl x509 -in cert.cer -text -noout
--  openssl x509 -in cert.crt -text -noout
--
--  If you get the folowing error it means that you are trying to view
--  a DER encoded certificate and need to use the commands in the
--  "View DER encoded certificate below"
--
--  unable to load certificate
--  12626:error:0906D06C:PEM routines:PEM_read_bio:no start line:
--  pem_lib.c:647:Expecting: TRUSTED CERTIFICATE
--
--  View DER encoded Certificate
--
--  openssl x509 -in certificate.der -inform der -text -noout
--
--  If you get the following error it means that you are trying to view
--  a PEM encoded certificate with a command meant for DER encoded certs.
--  Use a command in the "View PEM encoded certificate above"
--
--  unable to load certificate
--  13978:error:0D0680A8:asn1 encoding routines:ASN1_CHECK_TLEN:wrong tag:
--  tasn_dec.c:1306:
--  13978:error:0D07803A:asn1 encoding routines:ASN1_ITEM_EX_D2I:
--  nested asn1 error:tasn_dec.c:380:Type=X509
--
--  Transform
--
--  Transforms can take one type of encoded certificate to another.
--  (ie. PEM To DER conversion)
--
--  PEM to DER
--
--  openssl x509 -in cert.crt -outform der -out cert.der
--
--  DER to PEM
--
--  openssl x509 -in cert.crt -inform der -outform pem -out cert.pem
--
--  Combination
--
--  In some cases it is advantageous to combine multiple pieces of the X.509
--  infrastructure into a single file. One common example would be to combine
--  both the private key and public key into the same certificate.
--
--  The easiest way to combine certs keys and chains is to convert each
--  to a PEM encoded certificate then simple copy the contents of each file
--  into a new file. This is suitable for combining files to use
--  in applications like Apache.
--
--  Extraction
--
--  Some certs will come in a combined form. Where one file can contain
--  any one of: Certificate, Private Key, Public Key, Signed Certificate,
--  Certificate Authority (CA), and/or Authority Chain.
