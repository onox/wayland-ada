with Ada.Unchecked_Conversion;

package body C_Binding.Linux.GnuTLS.Sessions is

   function Convert_Unchecked is new Ada.Unchecked_Conversion
     (Source => Init_Flags,
      Target => Interfaces.C.unsigned);

   function Convert_Unchecked is new Ada.Unchecked_Conversion
     (Source => Certificate_Verify_Flags,
      Target => Interfaces.C.int);

   procedure Initialize_Session is
      Result : constant Interfaces.C.int
        := C_Init
          (This.My_Session'Access, Convert_Unchecked (This.Flags));
   begin
      if Result = GNUTLS_E_SUCCESS then
         begin
            Handle_Success;
            C_Deinit (This.My_Session);
         exception
            when Error : others =>
               C_Deinit (This.My_Session);
               raise;
         end;
      else
         Handle_Failure;
      end if;
   end Initialize_Session;

   function Set_Server_Name
     (This : Session;
      Name : String) return Success_Flag
   is
      C_Name : Interfaces.C.char_array := Interfaces.C.To_C (Name);
      Result : constant Interfaces.C.int
        := C_Server_Name_Set
          (This.My_Session,
           GNUTLS_NAME_DNS,
           C_Name,
           C_Name'Length);
   begin
      if Result = GNUTLS_E_SUCCESS then
         return Success;
      else
         return Failure;
      end if;
   end Set_Server_Name;

   function Set_Default_Priority
     (This : Session) return Success_Flag
   is
      Result : constant Interfaces.C.int
        := C_Set_Default_Priority (This.My_Session);
   begin
      if Result = GNUTLS_E_SUCCESS then
         return Success;
      else
         return Failure;
      end if;
   end Set_Default_Priority;

   procedure Verify_Certificate_Using_Hostname (This : Session) is
   begin
      if This.Host_Name /= null then
         C_Session_Set_Verify_Cert
           (This.My_Session, This.Host_Name.all, 0);
      else
         C_Session_Set_Verify_Cert
           (This.My_Session, Interfaces.C.Strings.Null_Ptr, 0);
      end if;
   end Verify_Certificate_Using_Hostname;

end C_Binding.Linux.GnuTLS.Sessions;
