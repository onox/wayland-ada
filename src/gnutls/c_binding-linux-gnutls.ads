with Interfaces.C.Strings;

package C_Binding.Linux.GnuTLS is

   pragma Linker_Options ("-lgnutls");

   subtype String_Result is C_Binding.String_Result;

   subtype Success_Flag is C_Binding.Success_Flag;

   function Check_Version (Version : String) return String_Result;

   function Get_Version return String;

   generic
      with procedure Handle_Success;
      with procedure Handle_Failure;
   procedure Initialize_GnuTLS;

   type Credentials_Base is limited private;

   type Init_Flags is mod 2**6;
   for Init_Flags'Size use Interfaces.C.unsigned'Size;
   pragma Convention (C, Init_Flags);

   Init_Server : constant Init_Flags := 2**0;
   Init_Client : constant Init_Flags := 2**1;
   Init_Datagram : constant Init_Flags := 2**2;
   Init_Nonblock : constant Init_Flags := 2**3;
   Init_No_Extensions : constant Init_Flags := 2**4;
   Init_No_Replay_Protection : constant Init_Flags := 2**5;

   type Session_Base
     (
      Flags     : Init_Flags;
      Host_Name : access Interfaces.C.Strings.chars_ptr
      --  The C programming language specific type Interfaces.C.char_array
      --  can be hidden if one uses Ada 2005's extended return.
      --  The host name must exist during the whole session.
      --  Look at the "gnutls_session_set_verify_cert" for more information.
     )
   is limited private;

private

   type Opaque_Certificate_Credentials is null record;
   type Opaque_Certificate_Credentials_Ptr is
     access all Opaque_Certificate_Credentials;

   type Credentials_Base is limited record
      My_Credentials : aliased Opaque_Certificate_Credentials_Ptr;
   end record;

   type Opaque_Session is null record;
   type Opaque_Session_Ptr is access all Opaque_Session;

   type Session_Base
     (
      Flags     : Init_Flags;
      Host_Name : access Interfaces.C.Strings.chars_ptr
     ) is limited record
      My_Session : aliased Opaque_Session_Ptr;
   end record;

   type Credentials_Kind is
     (
      Certificate_Credential,
      Anonymous_Credential,
      SRP_Credential,
      PSK_Credential,
      IA_Credential
     );

   for Credentials_Kind use
     (
      Certificate_Credential => 1,
      Anonymous_Credential   => 2,
      SRP_Credential         => 3,
      PSK_Credential         => 4,
      IA_Credential          => 5
     );

   pragma Convention (C, Credentials_Kind);

   function C_Check_Version
     (
      Req_Version : Interfaces.C.char_array
     ) return Interfaces.C.Strings.chars_ptr with
       Import        => True,
       Convention    => C,
       External_Name => "gnutls_check_version";

   function C_Check_Version
     (
      Req_Version : Interfaces.C.Strings.chars_ptr
     ) return Interfaces.C.Strings.chars_ptr with
       Import        => True,
       Convention    => C,
       External_Name => "gnutls_check_version";

   function C_Global_Init return Interfaces.C.int with
     Import        => True,
     Convention    => C,
     External_Name => "gnutls_global_init";

   procedure C_Global_Deinit with
     Import        => True,
     Convention    => C,
     External_Name => "gnutls_global_deinit";

   GNUTLS_E_SUCCESS               : constant := 0;
   GNUTLS_E_UNIMPLEMENTED_FEATURE : constant := -1250;

end C_Binding.Linux.GnuTLS;
