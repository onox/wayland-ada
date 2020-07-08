with Ada.Unchecked_Conversion;
package body C_Binding.Linux.GnuTLS.Sessions is

   use type Interfaces.C.Strings.chars_ptr;

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

   procedure Associate_With_Client_Socket
     (This   : Session;
      Socket : Linux.Sockets.TCP_Client.Client_Socket) is
   begin
      C_Transport_Set_Int2
        (This.My_Session,
         Socket.My_File_Descriptor,
         Socket.My_File_Descriptor);
   end Associate_With_Client_Socket;

   procedure Set_Default_Handshake_Timeout (This : Session) is
   begin
      C_Handshake_Set_Timeout
        (This.My_Session, GNUTLS_DEFAULT_HANDSHAKE_TIMEOUT);
   end Set_Default_Handshake_Timeout;

   function Perform_Handshake (This : Session) return Handshake_Result is
      Result : Interfaces.C.int := C_Handshake (This.My_Session);
   begin
      while Result < 0 and C_Error_Is_Fatal (Result) = 0 loop
         Result := C_Handshake (This.My_Session);
      end loop;
      if Result = GNUTLS_E_SUCCESS then
         return (Kind_Id => Handshake_Success,
                 Length  => 0);
      else
         declare
            Message : constant String
              := Interfaces.C.Strings.Value (C_String_Error (Result));
         begin
            if Result = GNUTLS_E_CERTIFICATE_VERIFICATION_ERROR then
               return (Kind_Id       => Certificate_Verification_Failure,
                       Length        => Message'Length,
                       Error_Message => Message);
            else
               return (Kind_Id       => Handshake_Failure,
                       Length        => Message'Length,
                       Error_Message => Message);
            end if;
         end;
      end if;
   end Perform_Handshake;

--     function Convert is new Ada.Unchecked_Conversion
--       (Source => Interfaces.C.Strings.chars_ptr,
--        Target => System.Address);

   function Description (This : Session) return String is
      Result : Interfaces.C.Strings.chars_ptr
        := C_Session_Get_Description (This.My_Session);
   begin
      if Result /= Interfaces.C.Strings.Null_Ptr then
         begin
            declare
               Text : constant String := Interfaces.C.Strings.Value (Result);
            begin
               Interfaces.C.Strings.Free (Result);
               return Text;
            end;
         exception
            when Error : others =>
               Interfaces.C.Strings.Free (Result);
               raise;
         end;
      else
         return "";
      end if;
   end Description;

   function Send
     (This : Session;
      Data : Ada.Streams.Stream_Element_Array) return Send_Result
   is
      Result : constant SSize_Type
        := C_Record_Send
          (Session   => This.My_Session,
           Data      => Data (Data'First)'Address,
           Data_Size => Data'Length);
   begin
      if Result < 0 then
         return (Kind_Id => Send_Failure);
      else
         return
           (Kind_Id        => Send_Success,
            Elements_Count => Ada.Streams.Stream_Element_Offset (Result));
      end if;
   end Send;

   function Receive
     (This : Session;
      Data : in out Ada.Streams.Stream_Element_Array) return Receive_Result
   is
      Result : constant SSize_Type
        := C_Record_Receive
          (Session   => This.My_Session,
           Data      => Data (Data'First)'Address,
           Data_Size => Data'Length);
   begin
      if Result < 0 then
         if Result = GNUTLS_E_PREMATURE_TERMINATION then
            return (Kind_Id => Receive_Premature_Termination);
         else
            return (Kind_Id => Receive_Failure);
         end if;
      elsif Result = 0 then
         return (Kind_Id => Receive_End_Of_File);
      else
         return
           (Kind_Id        => Receive_Success,
            Elements_Count => Ada.Streams.Stream_Element_Offset (Result));
      end if;
   end Receive;

   function Terminate_Connection
     (This : Session;
      How  : How_To_Shutdown) return Success_Flag
   is
      Result : constant Interfaces.C.int
        := C_Bye (This.My_Session, How);
   begin
      if Result = GNUTLS_E_SUCCESS then
         return Success;
      else
         return Failure;
      end if;
   end Terminate_Connection;

end C_Binding.Linux.GnuTLS.Sessions;
