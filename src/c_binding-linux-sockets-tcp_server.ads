with Aida;

package C_Binding.Linux.Sockets.TCP_Server is

   type Listener is new Socket_Base;
   --  This type of socket is a connection listener.
   --  When clients connect to this socket,
   --  new sockets are created using "accept".

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

   function "="(Left : General_Socket; Right : Listener) return Boolean;

   function "="(Left : Listener; Right : General_Socket) return Boolean;

end C_Binding.Linux.Sockets.TCP_Server;
