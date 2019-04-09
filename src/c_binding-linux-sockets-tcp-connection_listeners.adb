package body C_Binding.Linux.Sockets.TCP.Connection_Listeners is

   use type Interfaces.C.unsigned;

   procedure Initialize
     (This        : in out Listener;
      Call_Result : in out Aida.Call_Result;
      Address     : String := "0.0.0.0";
      Port        : Integer := 8080)
   is
      procedure Create_Stream_Socket;
      procedure Set_Socket_Option_Reuse_Address;
      procedure Bind_Descriptor_To_Port_And_Address;
      procedure Get_Socket_Flags;
      procedure Set_Socket_Non_Blocking;
      procedure Start_Listening_For_Connections;

      procedure Create_Stream_Socket is
      begin
         --   This file descriptor will listen
         This.My_File_Descriptor
           := Linux.C_Socket (Linux.AF_INET, Linux.SOCK_STREAM, 0);

         if This.My_File_Descriptor = -1 then
            Call_Result.Initialize (0903341606, -1365917333);
         else
            Set_Socket_Option_Reuse_Address;
         end if;
      end Create_Stream_Socket;

      procedure Set_Socket_Option_Reuse_Address is
         Temp : aliased Boolean := True;
         int_Result : Interfaces.C.int;
      begin
         int_Result := Linux.Set_Socket_Option
           (This.My_File_Descriptor,
            Linux.SOL_SOCKET,
            Linux.SO_REUSEADDR,
            Temp'Address,
            Integer'Size / 8);

         if int_Result = -1 then
            Call_Result.Initialize (-1391963481, 2043132012);
         else
            Bind_Descriptor_To_Port_And_Address;
         end if;
      end Set_Socket_Option_Reuse_Address;

      procedure Bind_Descriptor_To_Port_And_Address is
         Address : aliased Linux.Internet_Socket_Address
           := (
               Address_Family => 0,
               Port_Number    => 0,
               Address        => (Value => 0),
               Padding        => (others => 0)
              );
         Result : Interfaces.C.int;
      begin
         Address.Address_Family := Linux.AF_INET;
         Address.Address.Value := Linux.Get_Internet_Address ("0.0.0.0");
         Address.Port_Number := Linux.Host_To_Network_Short (8080);

         Result := Linux.Bind
           (This.My_File_Descriptor,
            Address'Access,
            Address'Size/8);
         if Result = -1 then
            Call_Result.Initialize (-1391963481, 2043132012);
         else
            Get_Socket_Flags;
         end if;
      end Bind_Descriptor_To_Port_And_Address;

      listenfd_flags : Interfaces.C.int;

      procedure Get_Socket_Flags is
      begin
         listenfd_flags
           := Linux.File_Control (This.My_File_Descriptor, Linux.F_GETFL, 0);

         if listenfd_flags = -1 then
            Call_Result.Initialize (-0066197735, -1774369458);
         else
            Set_Socket_Non_Blocking;
         end if;
      end Get_Socket_Flags;

      procedure Set_Socket_Non_Blocking is
         int_Result : Interfaces.C.int;
      begin
         Linux.Set_File_Descriptor_Flag_Non_Blocking (listenfd_flags);

         int_Result
           := Linux.File_Control
             (This.My_File_Descriptor, Linux.F_SETFL, listenfd_flags);

         if int_Result = -1 then
            Call_Result.Initialize (1664842403, -1360419837);
         else
            Start_Listening_For_Connections;
         end if;
      end Set_Socket_Non_Blocking;

      procedure Start_Listening_For_Connections is
         int_Result : Interfaces.C.int;
      begin
         int_Result := Linux.Listen (This.My_File_Descriptor, 32);
         if int_Result = -1 then
            Call_Result.Initialize (1535334882, -1524323957);
         end if;
      end Start_Listening_For_Connections;

   begin
      Create_Stream_Socket;
   end Initialize;

   function File_Descriptor (This : Listener) return Integer is
   begin
      return Integer (This.My_File_Descriptor);
   end File_Descriptor;

   procedure Shutdown_And_Close
     (This        : in out Listener;
      Call_Result : in out Aida.Call_Result)
   is
      procedure Shutdown_Socket;
      procedure Close_Socket;

      procedure Shutdown_Socket is
         int_Result : Interfaces.C.int;
      begin
         int_Result
           := Linux.Shutdown (This.My_File_Descriptor, Linux.SHUT_RDWR);
         if int_Result = -1 then
            Call_Result.Initialize (1397807166, 2066031560);
         else
            Close_Socket;
         end if;
      end Shutdown_Socket;

      procedure Close_Socket is
         int_Result : Interfaces.C.int;
      begin
         int_Result := Linux.Close (This.My_File_Descriptor);
         if int_Result = -1 then
            Call_Result.Initialize (1417878614, -1955280056);
         end if;
      end Close_Socket;

   begin
      Shutdown_Socket;
   end Shutdown_And_Close;

end C_Binding.Linux.Sockets.TCP.Connection_Listeners;
