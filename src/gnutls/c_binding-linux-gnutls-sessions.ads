--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2018 - 2019 Joakim Strandberg <joakim@mequinox.se>
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.

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
      Receive_End_Of_File,
      Receive_Premature_Termination,
      Receive_Failure
     );

   type Receive_Result (Kind_Id : Receive_Result_Kind_Id) is record
      case Kind_Id is
         when Receive_Success =>
            Elements_Count : Ada.Streams.Stream_Element_Offset;
         when Receive_Failure |
              Receive_End_Of_File |
              Receive_Premature_Termination =>
            null;
      end case;
   end record;

   function Receive
     (This : Session;
      Data : in out Ada.Streams.Stream_Element_Array) return Receive_Result;

   type How_To_Shutdown is
     (
      Read_Write_Shutdown,
      Write_Shutdown
     );

   for How_To_Shutdown use
     (
      Read_Write_Shutdown => 0,
      Write_Shutdown      => 1
     );

   pragma Convention (C, How_To_Shutdown);

   function Terminate_Connection
     (This : Session;
      How  : How_To_Shutdown) return Success_Flag;

private

   GNUTLS_E_PREMATURE_TERMINATION : constant := -110;
   GNUTLS_E_CERTIFICATE_VERIFICATION_ERROR : constant := -348;

   function C_Init
     (
      Session : access Opaque_Session_Ptr;
      Flags   : Interfaces.C.unsigned
     ) return Interfaces.C.int with
       Import        => True,
       Convention    => C,
       External_Name => "gnutls_init";

   procedure C_Deinit (Session : Opaque_Session_Ptr) with
     Import        => True,
     Convention    => C,
     External_Name => "gnutls_deinit";

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

   function C_Set_Default_Priority
     (
      Session : Opaque_Session_Ptr
     ) return Interfaces.C.int with
       Import        => True,
       Convention    => C,
       External_Name => "gnutls_set_default_priority";

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

   procedure C_Transport_Set_Int2
     (Session             : Opaque_Session_Ptr;
      Receiving_Socket_Fd : Interfaces.C.int;
      Sending_Socket_Fd   : Interfaces.C.int) with
     Import        => True,
     Convention    => C,
     External_Name => "gnutls_transport_set_int2";

   GNUTLS_DEFAULT_HANDSHAKE_TIMEOUT : constant := -1;

   procedure C_Handshake_Set_Timeout
     (Session      : Opaque_Session_Ptr;
      Milliseconds : Interfaces.C.int) with
     Import        => True,
     Convention    => C,
     External_Name => "gnutls_handshake_set_timeout";

   function C_Handshake
     (
      Session : Opaque_Session_Ptr
     ) return Interfaces.C.int with
       Import        => True,
       Convention    => C,
       External_Name => "gnutls_handshake";

   function C_Error_Is_Fatal
     (
      Error_Code : Interfaces.C.int
     ) return Interfaces.C.int with
       Import        => True,
       Convention    => C,
       External_Name => "gnutls_error_is_fatal";

   function C_String_Error
     (
      Error_Code : Interfaces.C.int
     ) return Interfaces.C.Strings.chars_ptr with
       Import        => True,
       Convention    => C,
       External_Name => "gnutls_strerror";

   function C_Session_Get_Description
     (
      Session : Opaque_Session_Ptr
     ) return Interfaces.C.Strings.chars_ptr with
       Import        => True,
       Convention    => C,
       External_Name => "gnutls_session_get_desc";

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

   function C_Record_Receive
     (
      Session   : Opaque_Session_Ptr;
      Data      : System.Address;
      Data_Size : Size_Type
     ) return SSize_Type with
       Import        => True,
       Convention    => C,
       External_Name => "gnutls_record_recv";

   function C_Bye
     (
      Session : Opaque_Session_Ptr;
      How     : How_To_Shutdown
     ) return Interfaces.C.int with
       Import        => True,
       Convention    => C,
       External_Name => "gnutls_bye";

end C_Binding.Linux.GnuTLS.Sessions;
