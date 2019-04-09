package body C_Binding.Linux.Sockets.TCP is

   function Close (This : in out General_Socket) return Success_Flag is
      int_Result : Interfaces.C.int;
   begin
      int_Result := C_Close (This.My_File_Descriptor);
      if int_Result = -1 then
         return Failure;
      else
         return Success;
      end if;
   end Close;

   procedure Set_Socket_Non_Blocking
     (This        : in out General_Socket;
      Call_Result : in out Aida.Call_Result)
   is
      procedure Get_Connection_Flags;
      procedure Set_Non_Blocking;

      Connection_Flags : Interfaces.C.int;

      procedure Get_Connection_Flags is
      begin
         Connection_Flags
           := Linux.File_Control (This.My_File_Descriptor, Linux.F_GETFL, 0);
         if Connection_Flags = -1 then
            Call_Result.Initialize (1415633127, -1032210731);
         else
            Set_Non_Blocking;
         end if;
      end Get_Connection_Flags;

      procedure Set_Non_Blocking is
         int_Result : Interfaces.C.int;
      begin
         Linux.Set_File_Descriptor_Flag_Non_Blocking (Connection_Flags);
         int_Result := Linux.File_Control
           (This.My_File_Descriptor, Linux.F_SETFL, Connection_Flags);
         if int_Result = -1 then
            Call_Result.Initialize (-1927227244, -0798468201);
         end if;
      end Set_Non_Blocking;

   begin
      Get_Connection_Flags;
   end Set_Socket_Non_Blocking;

   function Send
     (This   : General_Socket;
      Buffer : Stream_Element_Array;
      Count  : Natural) return Natural is
   begin
      return Natural (Linux.Send
        (This.My_File_Descriptor, Buffer, Linux.Size_Type(Count)));
   end Send;

end C_Binding.Linux.Sockets.TCP;
