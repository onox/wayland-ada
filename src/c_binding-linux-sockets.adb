package body C_Binding.Linux.Sockets is

   use type Ada.Streams.Stream_Element_Offset;

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

   function Shutdown
     (This : in out General_Socket) return Success_Flag
   is
      int_Result : Interfaces.C.int;
   begin
      int_Result := C_Shutdown (This.My_File_Descriptor, SHUT_RDWR);
      if int_Result = -1 then
         return Failure;
      else
         return Success;
      end if;
   end Shutdown;

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
           := C_File_Control (This.My_File_Descriptor, F_GETFL, 0);
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
         int_Result := C_File_Control
           (This.My_File_Descriptor, F_SETFL, Connection_Flags);
         if int_Result = -1 then
            Call_Result.Initialize (-1927227244, -0798468201);
         end if;
      end Set_Non_Blocking;

   begin
      Get_Connection_Flags;
   end Set_Socket_Non_Blocking;

   function Send
     (This   : General_Socket;
      Buffer : Ada.Streams.Stream_Element_Array;
      Count  : Natural) return Natural is
   begin
      return Natural (C_Write
        (This.My_File_Descriptor, Buffer, Linux.Size_Type(Count)));
   end Send;

   function Read
     (
      This   : General_Socket;
      Buffer : access Ada.Streams.Stream_Element_Array
     ) return Read_Result
   is
      Count : Ada.Streams.Stream_Element_Offset := 0;
      N : Interfaces.C.int;
      Shall_Continue_Reading : Boolean;
   begin
      N := Interfaces.C.int
        (C_Read
           (This.My_File_Descriptor,
            Buffer.all,
            Buffer.all'Length));

      case N is
         when Interfaces.C.int'First .. -1 =>
            return (Id => Read_Failure);
            --  Shall_Continue_Reading := False;
            --  Ada.Text_IO.Put_Line ("Might be EAGAIN");
         when 0 =>
            return (Id => Read_End_Of_File);
         when 1 .. Interfaces.C.int'Last =>
            Shall_Continue_Reading := N > 0;
            Count := Count + Ada.Streams.Stream_Element_Offset (N);
            return (Id            => Read_Success,
                    Element_Count => Count);
      end case;

--        if N >= 0 then
--           Shall_Continue_Reading := N > 0;
--           Count := Ada.Streams.Stream_Element_Offset (N);
--           while Shall_Continue_Reading loop
--              N := Interfaces.C.int
--                (C_Read (This.My_Events (Index).Data.fd,
--                 Buffer.all,
--                 Buffer.all'Length));
--              if N = -1 then
--                 return (Id => Read_Failure);
--                 --  Shall_Continue_Reading := False;
--                 --  Ada.Text_IO.Put_Line ("Might be EAGAIN");
--              else
--                 Shall_Continue_Reading := N > 0;
--                 Count := Count + Ada.Streams.Stream_Element_Offset (N);
--              end if;
--           end loop;
--           return (Id            => Read_Success,
--                   Element_Count => Count);
--        else
--           return (Id => Read_Failure);
--        end if;
   end Read;

end C_Binding.Linux.Sockets;
