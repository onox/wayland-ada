package body C_Binding.Linux.Event_Polls is

   use type Ada.Streams.Stream_Element_Offset;

   function Initialize (This : in out Event_Poll_Watcher) return Success_Flag is
      Flag : Success_Flag;
   begin
      This.My_File_Descriptor := Linux.Epoll_Create1 (0);
      if This.My_File_Descriptor = -1 then
         Flag := Failure;
      else
         Flag := Success;
      end if;
      return Flag;
   end Initialize;

   function Add
     (This            : in out Event_Poll_Watcher;
      File_Descriptor : Interfaces.C.int)
      return Success_Flag
   is
      Result : Interfaces.C.int;

      Event : aliased Linux.Epoll_Event;

      Flag : Success_Flag;
   begin
      --  add this clients descriptor to our event_poll watcher
      Event.Data.fd := File_Descriptor;
      Event.Events := Linux.EPOLLIN;
      Result
        := Linux.Epoll_Control
          (This.My_File_Descriptor,
           Linux.EPOLL_CTL_ADD,
           File_Descriptor,
           Event'Access);

      if Result = -1 then
         Flag := Failure;
      else
         Flag := Success;
      end if;
      return Flag;
   end Add;

   function Add
     (This     : in out Event_Poll_Watcher;
      Listener : C_Binding.Linux.Sockets.TCP.Connection_Listeners.Listener)
      return Success_Flag is
   begin
      return Add (This, Listener.My_File_Descriptor);
   end Add;

   function Add
     (This   : in out Event_Poll_Watcher;
      Socket : C_Binding.Linux.Sockets.TCP.General_Socket)
      return Success_Flag is
   begin
      return Add (This, Socket.My_File_Descriptor);
   end Add;

   function Delete
     (This            : in out Event_Poll_Watcher;
      File_Descriptor : Interfaces.C.int)
      return Success_Flag
   is
      Result : Interfaces.C.int;
      Flag : Success_Flag;
   begin
      Result
        := Linux.Epoll_Control
          (This.My_File_Descriptor,
           Linux.EPOLL_CTL_DEL,
           File_Descriptor,
           null);

      if Result = -1 then
         Flag := Failure;
      else
         Flag := Success;
      end if;
      return Flag;
   end Delete;

   function Delete
     (This     : in out Event_Poll_Watcher;
      Listener : C_Binding.Linux.Sockets.TCP.Connection_Listeners.Listener)
      return Success_Flag is
   begin
      return Delete (This, Listener.My_File_Descriptor);
   end Delete;

   function Delete
     (
      This   : in out Event_Poll_Watcher;
      List   : Event_List;
      Index  : Epoll_Event_Index
     ) return Success_Flag is
   begin
      return Delete (This, List.My_Events (Index).Data.fd);
   end Delete;

   function Check_For_Events
     (This    : Event_Poll_Watcher;
      List    : in out Event_List;
      Timeout : Integer) return Check_For_Events_Result
   is
      N : Interfaces.C.int;
   begin
      N := Linux.Epoll_Wait
        (This.My_File_Descriptor,
         List.My_Events'Access,
         List.My_Events'Length,
         Timeout => Interfaces.C.int (Timeout));
      case N is
         when -1 =>
            return (Id => Error_Occurred);
         when 0 =>
            return (Id => Timed_Out);
         when others =>
            return (Id          => Event_Registered,
                    Event_Count => Epoll_Event_Index (N));
      end case;
   end Check_For_Events;

   function Is_Epoll_Error
     (This  : Event_List;
      Index : Epoll_Event_Index) return Boolean is
   begin
      return Linux.Is_Epoll_Error (This.My_Events (Index).Events);
   end Is_Epoll_Error;

   function Has_Hang_Up_Happened
     (This  : Event_List;
      Index : Epoll_Event_Index) return Boolean is
   begin
      return Linux.Has_Hang_Up_Happened (This.My_Events (Index).Events);
   end Has_Hang_Up_Happened;

   function Is_Data_Available_For_Reading
     (This  : Event_List;
      Index : Epoll_Event_Index) return Boolean is
   begin
      return Linux.Is_Data_Available_For_Reading
        (This.My_Events (Index).Events);
   end Is_Data_Available_For_Reading;

   procedure Get_Socket
     (This   : Event_List;
      Index  : Epoll_Event_Index;
      Socket : out C_Binding.Linux.Sockets.TCP.General_Socket) is
   begin
      Socket.My_File_Descriptor := This.My_Events (Index).Data.fd;
   end Get_Socket;

   function Is_Event_Origin
     (This  : Event_List;
      Index : Epoll_Event_Index;
      Listener : C_Binding.Linux.Sockets.TCP.Connection_Listeners.Listener)
      return Boolean is
   begin
      return This.My_Events (Index).Data.fd = Listener.My_File_Descriptor;
   end Is_Event_Origin;

   function Read_Socket
     (
      This   : Event_List;
      Index  : Epoll_Event_Index;
      Buffer : access Ada.Streams.Stream_Element_Array
     ) return Read_Socket_Result
   is
      Count : Ada.Streams.Stream_Element_Offset := 0;
      N : Interfaces.C.int;
      Shall_Continue_Reading : Boolean;
   begin
      N := Interfaces.C.int
        (Linux.Read
           (This.My_Events (Index).Data.fd,
            Buffer.all'Address,
            Buffer.all'Length));
      if N >= 0 then
         Shall_Continue_Reading := N > 0;
         Count := Ada.Streams.Stream_Element_Offset (N);
         while Shall_Continue_Reading loop
            N := Interfaces.C.int
              (Linux.Read (This.My_Events (Index).Data.fd,
               Buffer.all'Address,
               Buffer.all'Length));
            if N = -1 then
               return (Id => Read_Failure);
               --  Shall_Continue_Reading := False;
               --  Ada.Text_IO.Put_Line ("Might be EAGAIN");
            else
               Shall_Continue_Reading := N > 0;
               Count := Count + Ada.Streams.Stream_Element_Offset (N);
            end if;
         end loop;
         return (Id            => Read_Success,
                 Element_Count => Count);
      else
         return (Id => Read_Failure);
      end if;
   end Read_Socket;

   function Shutdown_Socket
     (
      This   : Event_List;
      Index  : Epoll_Event_Index
     ) return Success_Flag
   is
      int_Result : Interfaces.C.int;
   begin
      int_Result := Linux.Shutdown
        (This.My_Events (Index).Data.fd, Linux.SHUT_RDWR);
      if int_Result = -1 then
         return Failure;
      else
         return Success;
      end if;
   end Shutdown_Socket;

end C_Binding.Linux.Event_Polls;
