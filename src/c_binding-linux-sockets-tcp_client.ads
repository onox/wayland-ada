with Aida;

package C_Binding.Linux.Sockets.TCP_Client is

   type Socket_Settings
     (Port    : Positive;
      Address : access String) is null record;

   type Client_Socket is new Socket_Base;

   generic
      with procedure Handle_Success;
      with procedure Handle_Failure;
      This        : access Client_Socket;
      Settings    : access Socket_Settings;
      Call_Result : access Aida.Call_Result;
   procedure Initialize_Client_Socket;

end C_Binding.Linux.Sockets.TCP_Client;
