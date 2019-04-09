with Aida;

package C_Binding.Linux.Sockets.TCP is

   type General_Socket is new Socket_Base;

   function Close (This : in out General_Socket) return Success_Flag;

   procedure Set_Socket_Non_Blocking
     (This        : in out General_Socket;
      Call_Result : in out Aida.Call_Result);

   function Send
     (This   : General_Socket;
      Buffer : Stream_Element_Array;
      Count  : Natural) return Natural;

end C_Binding.Linux.Sockets.TCP;
