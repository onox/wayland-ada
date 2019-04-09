with Aida;

package C_Binding.Linux.Sockets.TCP.Connection_Listeners is

   type Listener is new Socket_Base;

   procedure Initialize
     (This        : in out Listener;
      Call_Result : in out Aida.Call_Result;
      Address     : String := "0.0.0.0";
      Port        : Integer := 8080);

   function File_Descriptor (This : Listener) return Integer;

   procedure Shutdown_And_Close
     (This        : in out Listener;
      Call_Result : in out Aida.Call_Result);

   procedure Accept_New_Connection
     (This   : Listener;
      Socket : out General_Socket;
      Flag   : out Success_Flag);

end C_Binding.Linux.Sockets.TCP.Connection_Listeners;
