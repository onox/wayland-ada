with C_Binding.Linux.Sockets.TCP.Connection_Listeners;

package C_Binding.Linux.Event_Polls is

   type Check_For_Events_Result_Id is
     (
      Error_Occurred,
      Timed_Out,
      Event_Registered
     );

   type Check_For_Events_Result (Id : Check_For_Events_Result_Id) is record
      case Id is
         when Error_Occurred =>
            null;
         when Timed_Out =>
            null;
         when Event_Registered =>
            Event_Count : Epoll_Event_Index;
      end case;
   end record;

   type Event_List is limited private;

   function Is_Epoll_Error
     (This  : Event_List;
      Index : Epoll_Event_Index) return Boolean;

   function Has_Hang_Up_Happened
     (This  : Event_List;
      Index : Epoll_Event_Index) return Boolean;

   function Is_Data_Available_For_Reading
     (This  : Event_List;
      Index : Epoll_Event_Index) return Boolean;

   procedure Get_Socket
     (This   : Event_List;
      Index  : Epoll_Event_Index;
      Socket : out C_Binding.Linux.Sockets.TCP.General_Socket);

   function Is_Event_Origin
     (This  : Event_List;
      Index : Epoll_Event_Index;
      Listener : C_Binding.Linux.Sockets.TCP.Connection_Listeners.Listener)
      return Boolean;

   type Read_Socket_Result_Id is
     (
      Read_Failure,
      Read_Success
     );

   type Read_Socket_Result (Id : Read_Socket_Result_Id) is record
      case Id is
         when Read_Failure => null;
         when Read_Success =>
            Element_Count : Ada.Streams.Stream_Element_Offset;
      end case;
   end record;

   function Read_Socket
     (
      This   : Event_List;
      Index  : Epoll_Event_Index;
      Buffer : access Ada.Streams.Stream_Element_Array
     ) return Read_Socket_Result;

   function Shutdown_Socket
     (
      This   : Event_List;
      Index  : Epoll_Event_Index
     ) return Success_Flag;

   type Event_Poll_Watcher is limited private;

   function Initialize (This : in out Event_Poll_Watcher) return Success_Flag;

   function Add
     (This     : in out Event_Poll_Watcher;
      Listener : C_Binding.Linux.Sockets.TCP.Connection_Listeners.Listener)
      return Success_Flag;

   function Add
     (This   : in out Event_Poll_Watcher;
      Socket : C_Binding.Linux.Sockets.TCP.General_Socket)
      return Success_Flag;

   function Delete
     (This     : in out Event_Poll_Watcher;
      Listener : C_Binding.Linux.Sockets.TCP.Connection_Listeners.Listener)
      return Success_Flag;

   function Delete
     (
      This   : in out Event_Poll_Watcher;
      List   : Event_List;
      Index  : Epoll_Event_Index
     ) return Success_Flag;

   function Check_For_Events
     (This    : Event_Poll_Watcher;
      List    : in out Event_List;
      Timeout : Integer) return Check_For_Events_Result;

private

   type Event_Poll_Watcher is limited record
      My_File_Descriptor : Interfaces.C.int := -1;
   end record;

   type Event_List is limited record
      My_Events : aliased Linux.Epoll_Event_Array;
   end record;

end C_Binding.Linux.Event_Polls;
