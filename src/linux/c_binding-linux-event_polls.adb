--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2018 - 2019 Joakim Strandberg <joakim@mequinox.se>
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.

package body C_Binding.Linux.Event_Polls is

   use type Ada.Streams.Stream_Element_Offset;
   use type Interfaces.Unsigned_32;

   function Convert_Unchecked is new Ada.Unchecked_Conversion
     (Source => Interfaces.C.unsigned,
      Target => Interfaces.Unsigned_32);

   function Convert_Unchecked is new Ada.Unchecked_Conversion
     (Source => Interfaces.Unsigned_32,
      Target => Interfaces.C.unsigned);

   function Is_Epoll_Error
     (Event_Flags : Interfaces.C.unsigned) return Boolean
   is
      Temp : constant Interfaces.Unsigned_32
        := Convert_Unchecked (Event_Flags);
   begin
      return (Temp and EPOLLERR) > 0;
   end Is_Epoll_Error;

   function Has_Hang_Up_Happened
     (Event_Flags : Interfaces.C.unsigned) return Boolean
   is
      Temp : constant Interfaces.Unsigned_32
        := Convert_Unchecked (Event_Flags);
   begin
      return (Temp and EPOLLHUP) > 0;
   end Has_Hang_Up_Happened;

   function Is_Data_Available_For_Reading
     (Event_Flags : Interfaces.C.unsigned) return Boolean
   is
      Temp : constant Interfaces.Unsigned_32
        := Convert_Unchecked (Event_Flags);
   begin
      return (Temp and EPOLLIN) > 0;
   end Is_Data_Available_For_Reading;

   function Initialize (This : in out Event_Poll_Watcher) return Success_Flag is
      Flag : Success_Flag;
   begin
      This.My_File_Descriptor := C_Epoll_Create1 (0);
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

      Event : aliased Epoll_Event;

      Flag : Success_Flag;
   begin
      Event.Data.fd := File_Descriptor;
      Event.Events := EPOLLIN;

      Result
        := C_Epoll_Control
          (This.My_File_Descriptor,
           EPOLL_CTL_ADD,
           File_Descriptor,
           Event'Access);

      return (if Result /= -1 then Success else Failure);
   end Add;

   function Add
     (This     : in out Event_Poll_Watcher;
      Listener : C_Binding.Linux.Sockets.TCP_Server.Listener)
      return Success_Flag is
   begin
      return Add (This, Listener.My_File_Descriptor);
   end Add;

   function Add
     (This   : in out Event_Poll_Watcher;
      Socket : C_Binding.Linux.Sockets.General_Socket)
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
        := C_Epoll_Control
          (This.My_File_Descriptor,
           EPOLL_CTL_DEL,
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
      Listener : C_Binding.Linux.Sockets.TCP_Server.Listener)
      return Success_Flag is
   begin
      return Delete (This, Listener.My_File_Descriptor);
   end Delete;

   function Delete
     (This   : in out Event_Poll_Watcher;
      List   : Event_List;
      Index  : Epoll_Event_Index) return Success_Flag is
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
      N := C_Epoll_Wait
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
      return Is_Epoll_Error (This.My_Events (Index).Events);
   end Is_Epoll_Error;

   function Has_Hang_Up_Happened
     (This  : Event_List;
      Index : Epoll_Event_Index) return Boolean is
   begin
      return Has_Hang_Up_Happened (This.My_Events (Index).Events);
   end Has_Hang_Up_Happened;

   function Is_Data_Available_For_Reading
     (This  : Event_List;
      Index : Epoll_Event_Index) return Boolean is
   begin
      return Is_Data_Available_For_Reading (This.My_Events (Index).Events);
   end Is_Data_Available_For_Reading;

   procedure Get_Socket
     (This   : Event_List;
      Index  : Epoll_Event_Index;
      Socket : out C_Binding.Linux.Sockets.General_Socket) is
   begin
      Socket.My_File_Descriptor := This.My_Events (Index).Data.fd;
   end Get_Socket;

   function Is_Event_Origin
     (This  : Event_List;
      Index : Epoll_Event_Index;
      Listener : C_Binding.Linux.Sockets.TCP_Server.Listener)
      return Boolean is
   begin
      return This.My_Events (Index).Data.fd = Listener.My_File_Descriptor;
   end Is_Event_Origin;

   function Read_Socket
     (This   : Event_List;
      Index  : Epoll_Event_Index;
      Buffer : access Ada.Streams.Stream_Element_Array) return Read_Socket_Result
   is
      Count : Ada.Streams.Stream_Element_Offset := 0;
      N : Interfaces.C.int;
      Shall_Continue_Reading : Boolean;
   begin
      N := Interfaces.C.int
        (C_Read
           (This.My_Events (Index).Data.fd,
            Buffer.all,
            Buffer.all'Length));
      if N >= 0 then
         Shall_Continue_Reading := N > 0;
         Count := Ada.Streams.Stream_Element_Offset (N);
         while Shall_Continue_Reading loop
            N := Interfaces.C.int
              (C_Read (This.My_Events (Index).Data.fd,
               Buffer.all,
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

end C_Binding.Linux.Event_Polls;
