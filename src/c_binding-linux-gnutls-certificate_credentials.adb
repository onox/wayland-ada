package body C_Binding.Linux.GnuTLS.Certificate_Credentials is

--     function Initialize
--       (This : in out Certificate_Credentials) return Success_Flag
--     is
--        Result : constant Interfaces.C.int
--          := C_Certificate_Allocate_Credentials (This.My_Credentials);
--     begin
--        if Result = GNUTLS_E_SUCCESS then
--           return Success;
--        else
--           return Failure;
--        end if;
--     end Initialize;

   procedure Allocate_Credentials is
      Result : constant Interfaces.C.int
        := C_Certificate_Allocate_Credentials (This.My_Credentials'Access);
   begin
      if Result = GNUTLS_E_SUCCESS then
         begin
            Handle_Success;
            C_Certificate_Free_Credentials (This.My_Credentials);
         exception
            when Error : others =>
               C_Certificate_Free_Credentials (This.My_Credentials);
               raise;
         end;
      else
         Handle_Failure;
      end if;
   end Allocate_Credentials;

   function Set_Session_Credentials
     (This    : Credentials;
      Session : Sessions.Session) return Success_Flag
   is
      Result : constant Interfaces.C.int
        := C_Credentials_Set
          (Session.My_Session,
           Certificate_Credential,
           This.My_Credentials);
   begin
      if Result = GNUTLS_E_SUCCESS then
         return Success;
      else
         return Failure;
      end if;
   end Set_Session_Credentials;

end C_Binding.Linux.GnuTLS.Certificate_Credentials;
