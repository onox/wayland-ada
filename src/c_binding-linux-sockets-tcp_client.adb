package body C_Binding.Linux.Sockets.TCP_Client is

   use type Interfaces.C.unsigned;

   procedure Initialize_Client_Socket is
      procedure Create_Stream_Socket;
      procedure Connect_To_Socket;

      procedure Create_Stream_Socket is
      begin
         --   This file descriptor will listen
         This.My_File_Descriptor := C_Socket (AF_INET, SOCK_STREAM, 0);

         if This.My_File_Descriptor = -1 then
            Call_Result.Initialize (-0266673276, -1311936320);
            Handle_Failure;
         else
            Connect_To_Socket;
            declare
               int_Result : Interfaces.C.int;
            begin
               int_Result := C_Close (This.My_File_Descriptor);
               if int_Result = -1 then
                  Call_Result.Initialize (1659143720, 1750778895);
                  Handle_Failure;
               end if;
            end;
         end if;
      end Create_Stream_Socket;

      procedure Connect_To_Socket is
         Address : aliased Internet_Socket_Address
           := (
               Address_Family => 0,
               Port_Number    => 0,
               Address        => (Value => 0),
               Padding        => (others => 0)
              );
         Result : Interfaces.C.int;
      begin
         Address.Address_Family := AF_INET;
         Address.Address.Value
           := C_Get_Internet_Address (Settings.Address.all);
         Address.Port_Number
           := C_Host_To_Network_Short
             (Interfaces.C.unsigned_short (Settings.Port));

         Result := C_Connect
           (This.My_File_Descriptor,
            Address'Access,
            Address'Size/8);
         if Result = -1 then
            Call_Result.Initialize (-2105416615, -2051337028);
            Handle_Failure;
         else
            Handle_Success;
            declare
               int_Result : Interfaces.C.int;
            begin
               int_Result := C_Shutdown (This.My_File_Descriptor, SHUT_RDWR);
               if int_Result = -1 then
                  Call_Result.Initialize (-1550497564, 1680888681);
                  Handle_Failure;
               end if;
            end;
         end if;
      end Connect_To_Socket;

   begin
      Create_Stream_Socket;
   end Initialize_Client_Socket;

end C_Binding.Linux.Sockets.TCP_Client;
