with Ada.Text_IO;
with GnuTLS.Certificate_Credentials;
with GnuTLS.X509;
with GnuTLS.Sessions;

package body Simple_Client is

   use all type GnuTLS.X509.Add_The_Systems_Default_Trusted_CAs_Result_Kind_Id;

   procedure Handle_Failure_Of_Initialization is
   begin
      Ada.Text_IO.Put_Line ("sdfsd error");
   end Handle_Failure_Of_Initialization;

   procedure Handle_Failure_Of_Credentials_Allocation is
   begin
      Ada.Text_IO.Put_Line ("Allocation of credentials failed");
   end Handle_Failure_Of_Credentials_Allocation;

   procedure Run is
      procedure Check_GnuTLS_Version;
      procedure Add_The_Systems_Trusted_Certificate_Authorities;

      Credentials : GnuTLS.Certificate_Credentials.Credentials;

      procedure Allocate_Credentials is
        new GnuTLS.Certificate_Credentials.Allocate_Credentials
          (Handle_Success => Add_The_Systems_Trusted_Certificate_Authorities,
           Handle_Failure => Handle_Failure_Of_Credentials_Allocation,
           This           => Credentials'Access);

      procedure Initialize_GnuTLS is new GnuTLS.Initialize_GnuTLS
        (Handle_Success => Allocate_Credentials,
         Handle_Failure => Handle_Failure_Of_Initialization);

      Session : GnuTLS.Sessions.Session;

      procedure Check_GnuTLS_Version is
         Result : GnuTLS.String_Result
           := GnuTLS.Check_Version ("3.4.6");
      begin
         if Result.Is_Success then
            Ada.Text_IO.Put_Line ("GnuTLS version supported.");
            Ada.Text_IO.Put_Line ("Current version: " & Result.Value);
            Initialize_GnuTLS;
         else
            Ada.Text_IO.Put_Line
              ("GnuTLS 3.4.6 or later is required for this example.");
            Ada.Text_IO.Put_Line ("Current version: " & GnuTLS.Get_Version);
         end if;
      end Check_GnuTLS_Version;

      procedure Add_The_Systems_Trusted_Certificate_Authorities is
         Result : constant
           GnuTLS.X509.Add_The_Systems_Default_Trusted_CAs_Result
             := GnuTLS.X509.Add_The_Systems_Default_Trusted_CAs (Credentials);
      begin
         case Result.Kind_Id is
            when Add_Systems_Trusted_CAs_Success =>
               Ada.Text_IO.Put_Line
                 ("Processed certificates count:" &
                    Result.Certificates_Count'Image);
            when Add_Systems_Trusted_CAs_Failure =>
               Ada.Text_IO.Put_Line ("Failed to add system certificates");
            when Add_Systems_Trusted_CAs_Unsupported =>
               Ada.Text_IO.Put_Line
                 ("Adding system certificates is unsupported");
         end case;
      end Add_The_Systems_Trusted_Certificate_Authorities;

   begin
      Check_GnuTLS_Version;
--          int ret, sd, ii;
--          char buffer[MAX_BUF + 1], *desc;
--          gnutls_datum_t out;
--          int type;
--          unsigned status;
--          gnutls_certificate_credentials_t xcred;
--
--          if (gnutls_check_version("3.4.6") == NULL) {
--                  fprintf(stderr, "GnuTLS 3.4.6 or later is required for this example\n");
--                  exit(1);
--          }

      null;
   end Run;

end Simple_Client;
