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

private

   function C_Certificate_Set_x509_System_Trust
     (
      Credentials : access Opaque_Certificate_Credentials
     ) return Interfaces.C.int with
       Import        => True,
       Convention    => C,
       External_Name => "gnutls_certificate_set_x509_system_trust";

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

   function C_Set_X509_Trust_File
     (
      Credentials : access Opaque_Certificate_Credentials;
      CA_File     : Interfaces.C.char_array;
      Format      : Certificate_Format
     ) return Interfaces.C.int with
       Import        => True,
       Convention    => C,
       External_Name => "gnutls_certificate_set_x509_trust_file";

   function C_Set_X509_CRL_File
     (
      Credentials : access Opaque_Certificate_Credentials;
      CRL_File    : Interfaces.C.char_array;
      Format      : Certificate_Format
     ) return Interfaces.C.int with
       Import        => True,
       Convention    => C,
       External_Name => "gnutls_certificate_set_x509_crl_file";

end C_Binding.Linux.GnuTLS.X509;
