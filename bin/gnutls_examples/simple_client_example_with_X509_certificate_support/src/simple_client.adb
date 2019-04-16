with Ada.Text_IO;
with Aida;
with Linux.Sockets.TCP_Client;
with GnuTLS.Certificate_Credentials;
with GnuTLS.X509;
with GnuTLS.Sessions;

with Interfaces.C.Strings;

package body Simple_Client is

   use all type GnuTLS.Success_Flag;
   use all type GnuTLS.X509.Add_The_Systems_Default_Trusted_CAs_Result_Kind_Id;

   Host_Name_Asdf : aliased Interfaces.C.char_array
     := Interfaces.C.To_C ("www.google.se");

   Host_Name : aliased Interfaces.C.Strings.chars_ptr
     := Interfaces.C.Strings.To_Chars_Ptr (Host_Name_Asdf'Access);

   procedure Run is
      procedure Check_GnuTLS_Version;
      procedure Initialize_GnuTLS;
      procedure Allocate_Credentials;
      procedure Add_The_Systems_Trusted_Certificate_Authorities;
      procedure Initialize_Session;
      procedure Set_Server_Name;
      procedure Set_Default_Priority;
      procedure Set_Session_Credentials;
      procedure Prepare_For_Server_Certificate_Verification;
      procedure Connect_To_Peer;
      procedure Associate_Client_Socket_With_Session;
      procedure Perform_TLS_handshake;

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

      procedure Initialize_GnuTLS is

         procedure Handle_Failure_Of_Initialization is
         begin
            Ada.Text_IO.Put_Line ("GnuTLS initialization error");
         end Handle_Failure_Of_Initialization;

         procedure Initialize is new GnuTLS.Initialize_GnuTLS
           (Handle_Success => Allocate_Credentials,
            Handle_Failure => Handle_Failure_Of_Initialization);

      begin
         Initialize;
      end Initialize_GnuTLS;

      Credentials : GnuTLS.Certificate_Credentials.Credentials;

      procedure Allocate_Credentials is

         procedure Handle_Failure_Of_Credentials_Allocation is
         begin
            Ada.Text_IO.Put_Line ("Allocation of credentials failed");
         end Handle_Failure_Of_Credentials_Allocation;

         procedure Allocate is
           new GnuTLS.Certificate_Credentials.Allocate_Credentials
             (Handle_Success => Add_The_Systems_Trusted_Certificate_Authorities,
              Handle_Failure => Handle_Failure_Of_Credentials_Allocation,
              This           => Credentials'Access);

      begin
         Allocate;
      end Allocate_Credentials;

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
               Initialize_Session;
            when Add_Systems_Trusted_CAs_Failure =>
               Ada.Text_IO.Put_Line ("Failed to add system certificates");
            when Add_Systems_Trusted_CAs_Unsupported =>
               Ada.Text_IO.Put_Line
                 ("Adding system certificates is unsupported");
         end case;
      end Add_The_Systems_Trusted_Certificate_Authorities;

      Session : aliased GnuTLS.Sessions.Session
        (GnuTLS.Init_Client,
         Host_Name'Access);

      procedure Initialize_Session is

         procedure Handle_Failure_Of_Session_Initialization is
         begin
            Ada.Text_IO.Put_Line ("Initialization of session failed");
         end Handle_Failure_Of_Session_Initialization;

         procedure Initialize is
           new GnuTLS.Sessions.Initialize_Session
             (Handle_Success => Set_Server_Name,
              Handle_Failure => Handle_Failure_Of_Session_Initialization,
              This           => Session'Access);

      begin
         Initialize;
      end Initialize_Session;

      procedure Set_Server_Name is
         Flag : GnuTLS.Success_Flag
           := GnuTLS.Sessions.Set_Server_Name (Session, "www.google.se");
      begin
         if Flag = Success then
            Set_Default_Priority;
         else
            Ada.Text_IO.Put_Line ("Set server name failure");
         end if;
      end Set_Server_Name;

      procedure Set_Default_Priority is
         Flag : GnuTLS.Success_Flag
           := GnuTLS.Sessions.Set_Default_Priority (Session);
      begin
         if Flag = Success then
            Set_Session_Credentials;
         else
            Ada.Text_IO.Put_Line ("Set default priority failure");
         end if;
      end Set_Default_Priority;

      procedure Set_Session_Credentials is
         Flag : GnuTLS.Success_Flag
           := GnuTLS.Certificate_Credentials.Set_Session_Credentials
             (Credentials, Session);
      begin
         if Flag = Success then
            Prepare_For_Server_Certificate_Verification;
         else
            Ada.Text_IO.Put_Line ("Set session credentials failure");
         end if;
      end Set_Session_Credentials;

      procedure Prepare_For_Server_Certificate_Verification is
      begin
         GnuTLS.Sessions.Verify_Certificate_Using_Hostname (Session);
         Connect_To_Peer;
      end Prepare_For_Server_Certificate_Verification;

      Client_Socket : Linux.Sockets.TCP_Client.Client_Socket;

      procedure Connect_To_Peer is

         SERVER : aliased String := "216.58.207.195";
         --  This is the IP of www.google.se.
         --  It was determined by executing "nslookup google.se"

         Settings : Linux.Sockets.TCP_Client.Socket_Settings
           (Port    => 443,
            Address => SERVER'Access);

         Call_Result : aliased Aida.Call_Result;

         procedure Handle_Failure is
         begin
            Ada.Text_IO.Put_Line ("Connect to peer failure");
            Ada.Text_IO.Put_Line (Aida.Message (Call_Result));
         end Handle_Failure;

         procedure Connect is
           new Linux.Sockets.TCP_Client.Initialize_Client_Socket
             (Handle_Success => Associate_Client_Socket_With_Session,
              Handle_Failure => Handle_Failure,
              This           => Client_Socket'Access,
              Settings       => Settings'Access,
              Call_Result    => Call_Result'Access);

      begin
         Connect;
      end Connect_To_Peer;

      procedure Associate_Client_Socket_With_Session is
      begin
         GnuTLS.Sessions.Associate_With_Client_Socket
           (Session, Client_Socket);
         GnuTLS.Sessions.Set_Default_Handshake_Timeout (Session);
         Perform_TLS_handshake;
      end Associate_Client_Socket_With_Session;

      procedure Perform_TLS_handshake is
         Flag : GnuTLS.Success_Flag
           := GnuTLS.Sessions.Perform_Handshake (Session);
      begin
         if Flag = Success then
            Ada.Text_IO.Put_Line ("handshake success!");
         else
            Ada.Text_IO.Put_Line ("TLS handshake failure");
         end if;
      end Perform_TLS_handshake;

   begin
      Check_GnuTLS_Version;
   end Run;

end Simple_Client;
