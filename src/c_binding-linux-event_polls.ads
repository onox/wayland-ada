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

with C_Binding.Linux.Sockets.TCP_Server;

package C_Binding.Linux.Event_Polls is

   MAX_EPOLL_EVENTS : constant := 64;

   subtype Epoll_Event_Index is Positive range 1 .. MAX_EPOLL_EVENTS;

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
      Socket : out C_Binding.Linux.Sockets.General_Socket);

   function Is_Event_Origin
     (This  : Event_List;
      Index : Epoll_Event_Index;
      Listener : C_Binding.Linux.Sockets.TCP_Server.Listener)
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

   type Event_Poll_Watcher is limited private;

   function Initialize (This : in out Event_Poll_Watcher) return Success_Flag;

   function Add
     (This     : in out Event_Poll_Watcher;
      Listener : C_Binding.Linux.Sockets.TCP_Server.Listener)
      return Success_Flag;

   function Add
     (This   : in out Event_Poll_Watcher;
      Socket : C_Binding.Linux.Sockets.General_Socket)
      return Success_Flag;

   function Delete
     (This     : in out Event_Poll_Watcher;
      Listener : C_Binding.Linux.Sockets.TCP_Server.Listener)
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

   EPOLL_CTL_ADD : constant := 1;
   EPOLL_CTL_DEL : constant := 2;
   EPOLL_CTL_MOD : constant := 3;
   EPOLLIN        : constant := 1;
   EPOLLPRI       : constant Interfaces.C.unsigned := 2;
   EPOLLOUT       : constant Interfaces.C.unsigned := 4;
   EPOLLRDNORM    : constant Interfaces.C.unsigned := 64;
   EPOLLRDBAND    : constant Interfaces.C.unsigned := 128;
   EPOLLWRNORM    : constant Interfaces.C.unsigned := 256;
   EPOLLWRBAND    : constant Interfaces.C.unsigned := 512;
   EPOLLMSG       : constant Interfaces.C.unsigned := 1024;
   EPOLLERR       : constant := 8;
   EPOLLHUP       : constant := 16;
   EPOLLRDHUP     : constant Interfaces.C.unsigned := 8192;
   EPOLLEXCLUSIVE : constant Interfaces.C.unsigned := 268435456;
   EPOLLWAKEUP    : constant Interfaces.C.unsigned := 536870912;
   EPOLLONESHOT   : constant Interfaces.C.unsigned := 1073741824;
   EPOLLET        : constant Interfaces.C.unsigned := 2147483648;

   type Epoll_Data (discr : Interfaces.C.unsigned := 0) is record
      case discr is
         when 0 =>
            ptr : System.Address;
         when 1 =>
            fd : aliased Interfaces.C.int;
         when 2 =>
            u32 : aliased Interfaces.Unsigned_32;
         when others =>
            u64 : aliased Interfaces.Unsigned_64;
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, Epoll_Data);
   pragma Unchecked_Union (Epoll_Data);

   type Epoll_Event is record
      Events : aliased Interfaces.C.unsigned;
      Data   : aliased Epoll_Data;
   end record;
   pragma Convention (C_Pass_By_Copy, Epoll_Event);

   type Epoll_Event_Array is array (Epoll_Event_Index) of aliased Epoll_Event;

   type Event_Poll_Watcher is limited record
      My_File_Descriptor : Interfaces.C.int := -1;
   end record;

   type Event_List is limited record
      My_Events : aliased Epoll_Event_Array;
   end record;

   function C_Epoll_Create1
     (Flags : Interfaces.C.int) return Interfaces.C.int with
     Import        => True,
     Convention    => C,
     External_Name => "epoll_create1";

   function C_Epoll_Control
     (Epoll_Fd        : Interfaces.C.int;
      Operation       : Interfaces.C.int;
      File_Descriptor : Interfaces.C.int;
      Event           : access Epoll_Event) return Interfaces.C.int with
     Import        => True,
     Convention    => C,
     External_Name => "epoll_ctl";

   function C_Epoll_Wait
     (Epoll_Fd   : Interfaces.C. int;
      Events     : access Epoll_Event_Array;
      Max_Events : Interfaces.C.int;
      Timeout    : Interfaces.C.int) return Interfaces.C.int with
     Import        => True,
     Convention    => C,
     External_Name => "epoll_wait";

   function Is_Epoll_Error
     (Event_Flags : Interfaces.C.unsigned) return Boolean;

   function Has_Hang_Up_Happened
     (Event_Flags : Interfaces.C.unsigned) return Boolean;

   function Is_Data_Available_For_Reading
     (Event_Flags : Interfaces.C.unsigned) return Boolean;

end C_Binding.Linux.Event_Polls;
