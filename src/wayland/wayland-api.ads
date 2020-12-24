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

with Interfaces.C.Strings;

--  This package was generated with:
--
--    gcc -fdump-ada-spec /usr/include/wayland-client-core.h
--
--  and then manually modified to make it a bit higher-level.

private package Wayland.API is
   pragma Preelaborate;

   type Proxy is limited private;

   type Proxy_Ptr is access all Proxy
     with Convention => C;

   type Display_Ptr is new Proxy_Ptr;

   type Interface_T;

   type Interface_Ptr is access constant Interface_T;

   type Interface_Ptr_Array is array (Positive range <>) of aliased Interface_Ptr;

   type Message is limited record
      Name      : Interfaces.C.Strings.char_array_access;
      Signature : Interfaces.C.Strings.char_array_access;
      Types     : access Interface_Ptr;
   end record
     with Convention => C_Pass_By_Copy;

   type Message_Array is array (Natural range <>) of aliased Message
     with Convention => C;

   type Message_Array_Ptr is access constant Message_Array
     with Size => Standard'Address_Size;

   type Interface_T is limited record
      Name         : Interfaces.C.Strings.char_array_access;
      Version      : Interfaces.C.int;
      Method_Count : Interfaces.C.int;
      Methods      : Message_Array_Ptr;
      Event_Count  : Interfaces.C.int;
      Events       : Message_Array_Ptr;
   end record
     with Convention => C_Pass_By_Copy;

   -----------------------------------------------------------------------------

   --  wl_proxy_create is not task-safe
   --  See https://gitlab.freedesktop.org/wayland/wayland/-/issues/182

--   function Proxy_Create
--     (Factory   : in out Proxy;
--      Interface : in out Interface) return access Proxy
--   with Import, Convention => C, External_Name => "wl_proxy_create";

--   function Proxy_Create_wrapper (Proxy : System.Address) return System.Address
--     with Import, Convention => C, External_Name => "wl_proxy_create_wrapper";

--   procedure Proxy_Wrapper_destroy (Proxy_Wrapper : System.Address)
--     with Import, Convention => C, External_Name => "wl_proxy_wrapper_destroy";

   procedure Proxy_Marshal
     (Object : in out Proxy;
      Opcode : Unsigned_32)
   with Import, Convention => C, External_Name => "wl_proxy_marshal";

   procedure Proxy_Marshal
     (Object : in out Proxy;
      Opcode : Unsigned_32;
      Arg_1  : Integer)
   with Import, Convention => C, External_Name => "wl_proxy_marshal";

   procedure Proxy_Marshal
     (Object : in out Proxy;
      Opcode : Unsigned_32;
      Arg_1  : Unsigned_32)
   with Import, Convention => C, External_Name => "wl_proxy_marshal";

   procedure Proxy_Marshal
     (Object : in out Proxy;
      Opcode : Unsigned_32;
      Arg_1  : Unsigned_32;
      Arg_2  : Interfaces.C.Strings.chars_ptr)
   with Import, Convention => C, External_Name => "wl_proxy_marshal";

   procedure Proxy_Marshal
     (Object : in out Proxy;
      Opcode : Unsigned_32;
      Arg_1  : Unsigned_32;
      Arg_2  : Unsigned_32)
   with Import, Convention => C, External_Name => "wl_proxy_marshal";

   procedure Proxy_Marshal
     (Object : in out Proxy;
      Opcode : Unsigned_32;
      Arg_1  : Interfaces.C.Strings.chars_ptr)
   with Import, Convention => C, External_Name => "wl_proxy_marshal";

   procedure Proxy_Marshal
     (Object : in out Proxy;
      Opcode : Unsigned_32;
      Arg_1  : Interfaces.C.Strings.chars_ptr;
      Arg_2  : Integer)
   with Import, Convention => C, External_Name => "wl_proxy_marshal";

   procedure Proxy_Marshal
     (Object : in out Proxy;
      Opcode : Unsigned_32;
      Arg_1  : Integer;
      Arg_2  : Integer)
   with Import, Convention => C, External_Name => "wl_proxy_marshal";

   procedure Proxy_Marshal
     (Object : in out Proxy;
      Opcode : Unsigned_32;
      Arg_1  : Fixed;
      Arg_2  : Fixed)
   with Import, Convention => C, External_Name => "wl_proxy_marshal";

   procedure Proxy_Marshal
     (Object : in out Proxy;
      Opcode : Unsigned_32;
      Arg_1  : Proxy_Ptr)
   with Import, Convention => C, External_Name => "wl_proxy_marshal";

   procedure Proxy_Marshal
     (Object : in out Proxy;
      Opcode : Unsigned_32;
      Arg_1  : Proxy_Ptr;
      Arg_2  : Unsigned_32)
   with Import, Convention => C, External_Name => "wl_proxy_marshal";

   procedure Proxy_Marshal
     (Object : in out Proxy;
      Opcode : Unsigned_32;
      Arg_1  : Proxy_Ptr;
      Arg_2  : Unsigned_32;
      Arg_3  : Unsigned_32)
   with Import, Convention => C, External_Name => "wl_proxy_marshal";

   procedure Proxy_Marshal
     (Object : in out Proxy;
      Opcode : Unsigned_32;
      Arg_1  : Proxy_Ptr;
      Arg_2  : Integer;
      Arg_3  : Integer)
   with Import, Convention => C, External_Name => "wl_proxy_marshal";

   procedure Proxy_Marshal
     (Object : in out Proxy;
      Opcode : Unsigned_32;
      Arg_1  : Proxy_Ptr;
      Arg_2  : Unsigned_32;
      Arg_3  : Integer;
      Arg_4  : Integer)
   with Import, Convention => C, External_Name => "wl_proxy_marshal";

   procedure Proxy_Marshal
     (Object : in out Proxy;
      Opcode : Unsigned_32;
      Arg_1  : Integer;
      Arg_2  : Integer;
      Arg_3  : Integer;
      Arg_4  : Integer)
   with Import, Convention => C, External_Name => "wl_proxy_marshal";

   procedure Proxy_Marshal
     (Object : in out Proxy;
      Opcode : Unsigned_32;
      Arg_1  : Fixed;
      Arg_2  : Fixed;
      Arg_3  : Fixed;
      Arg_4  : Fixed)
   with Import, Convention => C, External_Name => "wl_proxy_marshal";

   procedure Proxy_Marshal
     (Object : in out Proxy;
      Opcode : Unsigned_32;
      Arg_1  : Proxy_Ptr;
      Arg_2  : Integer;
      Arg_3  : Integer;
      Arg_4  : Unsigned_32)
   with Import, Convention => C, External_Name => "wl_proxy_marshal";

   procedure Proxy_Marshal
     (Object : in out Proxy;
      Opcode : Unsigned_32;
      Arg_1  : Proxy_Ptr;
      Arg_2  : Proxy_Ptr;
      Arg_3  : Proxy_Ptr;
      Arg_4  : Unsigned_32)
   with Import, Convention => C, External_Name => "wl_proxy_marshal";

   procedure Proxy_Marshal
     (Object : in out Proxy;
      Opcode : Unsigned_32;
      Arg_1  : Unsigned_32;
      Arg_2  : Proxy_Ptr;
      Arg_3  : Integer;
      Arg_4  : Integer)
   with Import, Convention => C, External_Name => "wl_proxy_marshal";

   function Proxy_Marshal_Constructor
     (Object  : in out Proxy;
      Opcode  : Unsigned_32;
      Subject : Interface_Ptr;
      New_ID  : Unsigned_32) return Proxy_Ptr
   with Import, Convention => C, External_Name => "wl_proxy_marshal_constructor";

   function Proxy_Marshal_Constructor
     (Object  : in out Proxy;
      Opcode  : Unsigned_32;
      Subject : Interface_Ptr;
      New_ID  : Unsigned_32;
      Offset  : Integer;
      Width   : Integer;
      Height  : Integer;
      Stride  : Integer;
      Format  : Unsigned_32) return Proxy_Ptr
   with Import, Convention => C, External_Name => "wl_proxy_marshal_constructor";

   function Proxy_Marshal_Constructor
     (Object  : in out Proxy;
      Opcode  : Unsigned_32;
      Subject : Interface_Ptr;
      Offset  : Proxy_Ptr;
      New_ID  : Unsigned_32) return Proxy_Ptr
   with Import, Convention => C, External_Name => "wl_proxy_marshal_constructor";

   function Proxy_Marshal_Constructor
     (Object  : in out Proxy;
      Opcode  : Unsigned_32;
      Subject : Interface_Ptr;
      New_ID  : Unsigned_32;
      Offset  : Proxy_Ptr) return Proxy_Ptr
   with Import, Convention => C, External_Name => "wl_proxy_marshal_constructor";

   function Proxy_Marshal_Constructor
     (Object  : in out Proxy;
      Opcode  : Unsigned_32;
      Subject : Interface_Ptr;
      New_ID  : Unsigned_32;
      Arg_1   : Integer;
      Arg_2   : Integer) return Proxy_Ptr
   with Import, Convention => C, External_Name => "wl_proxy_marshal_constructor";

   function Proxy_Marshal_Constructor
     (Object  : in out Proxy;
      Opcode  : Unsigned_32;
      Subject : Interface_Ptr;
      New_ID  : Unsigned_32;
      Arg_1   : Proxy_Ptr;
      Arg_2   : Proxy_Ptr) return Proxy_Ptr
   with Import, Convention => C, External_Name => "wl_proxy_marshal_constructor";

   function Proxy_Marshal_Constructor
     (Object  : in out Proxy;
      Opcode  : Unsigned_32;
      Subject : Interface_Ptr;
      New_ID  : Unsigned_32;
      Arg_1   : Proxy_Ptr;
      Arg_2   : Proxy_Ptr;
      Arg_3   : Proxy_Ptr;
      Arg_4   : Unsigned_32) return Proxy_Ptr
   with Import, Convention => C, External_Name => "wl_proxy_marshal_constructor";

   function Proxy_Marshal_Constructor_Versioned
     (Object     : in out Proxy;
      Opcode     : Unsigned_32;
      Subject    : Interface_Ptr;
      New_ID_1   : Unsigned_32;
      Name       : Unsigned_32;
      Iface_Name : Interfaces.C.Strings.char_array_access;
      New_ID_2   : Unsigned_32;
      Version    : Unsigned_32) return Proxy_Ptr
   with Import, Convention => C, External_Name => "wl_proxy_marshal_constructor_versioned";

--   procedure Proxy_Marshal_Array
--     (Object : in out Proxy;
--      Opcode : Unsigned_32;
--      Args   : access wayland_util_h.wl_argument)  -- TODO Array
--   with Import, Convention => C, External_Name => "wl_proxy_marshal_array";

--   function Proxy_Marshal_Array_Constructor
--     (Object : in out Proxy;
--      Opcode : Unsigned_32;
--      Args : access wayland_util_h.wl_argument;
--      Interface : in out Interface) return access Proxy
--     with Import, Convention => C, External_Name => "wl_proxy_marshal_array_constructor";

--   function Proxy_Marshal_Array_Constructor_Versioned
--     (Object : in out Proxy;
--      Opcode : Unsigned_32;
--      Args : access wayland_util_h.wl_argument;
--      Interface : in out Interface;
--      Version : Unsigned_32) return access Proxy
--     with Import, Convention => C, External_Name => "wl_proxy_marshal_array_constructor_versioned";

   procedure Proxy_Destroy (Object : in out Proxy)
     with Import, Convention => C, External_Name => "wl_proxy_destroy";

   function Proxy_Add_Listener
     (Object  : in out Proxy;
      Subject : Void_Ptr;
      Data    : Void_Ptr) return Interfaces.C.int
   with Import, Convention => C, External_Name => "wl_proxy_add_listener";
   --  Returns 0 on success, otherwise -1 on failure

--   function Proxy_Get_Listener (Object : in out Proxy) return Listener
--     with Import, Convention => C, External_Name => "wl_proxy_get_listener";

--   function Proxy_Add_Dispatcher
--     (Object : in out Proxy;
--      Dispatcher_Func : wayland_util_h.wl_dispatcher_func_t;
--      Dispatcher_Data : System.Address;
--      Data : System.Address) return int
--     with Import, Convention => C, External_Name => "wl_proxy_add_dispatcher";

   procedure Proxy_Set_User_Data (Object : in out Proxy; User_Data : Void_Ptr)
     with Import, Convention => C, External_Name => "wl_proxy_set_user_data";

   function Proxy_Get_User_Data (Object : in out Proxy) return Void_Ptr
     with Import, Convention => C, External_Name => "wl_proxy_get_user_data";

   function Proxy_Get_Version (Object : in out Proxy) return Unsigned_32
     with Import, Convention => C, External_Name => "wl_proxy_get_version";

--   function Proxy_Get_Id (Object : in out Proxy) return Unsigned_32
--     with Import, Convention => C, External_Name => "wl_proxy_get_id";

--   function Proxy_Get_Class (Object : in out Proxy) return Interfaces.C.Strings.chars_ptr
--     with Import, Convention => C, External_Name => "wl_proxy_get_class";

   -----------------------------------------------------------------------------
   --                           Connection Handling                           --
   -----------------------------------------------------------------------------

   function Display_Connect (Name : Interfaces.C.Strings.chars_ptr) return Display_Ptr
     with Import, Convention => C, External_Name => "wl_display_connect";

--   function Display_Connect_To_FD (FD : int) return Display_Ptr
--     with Import, Convention => C, External_Name => "wl_display_connect_to_fd";

   procedure Display_Disconnect (Object : Display_Ptr)
     with Import, Convention => C, External_Name => "wl_display_disconnect";

   function Display_Get_File_Descriptor (Object : Display_Ptr) return Integer
     with Import, Convention => C, External_Name => "wl_display_get_fd";

--   function Display_Get_Error (Object : Display_Ptr) return Integer
--     with Import, Convention => C, External_Name => "wl_display_get_error";

--   function Display_Get_Protocol_Error
--     (Arg1 : System.Address;
--      Arg2 : System.Address;
--      Arg3 : access Unsigned_32) return Unsigned_32
--     with Import, Convention => C, External_Name => "wl_display_get_protocol_error";

--   procedure Log_Set_Handler_Client (Arg1 : Wayland_Util_H.WL_Log_Func_T)
--     with Import, Convention => C, External_Name => "wl_log_set_handler_client";

   function Display_Flush (Object : Display_Ptr) return Integer
     with Import, Convention => C, External_Name => "wl_display_flush";

   -----------------------------------------------------------------------------
   --                           Default Event Queue                           --
   -----------------------------------------------------------------------------

   function Display_Dispatch (Object : Display_Ptr) return Integer
     with Import, Convention => C, External_Name => "wl_display_dispatch";

   function Display_Dispatch_Pending (Object : Display_Ptr) return Integer
     with Import, Convention => C, External_Name => "wl_display_dispatch_pending";

   function Display_Roundtrip (Object : Display_Ptr) return Integer
     with Import, Convention => C, External_Name => "wl_display_roundtrip";

   function Display_Prepare_Read (Object : Display_Ptr) return Integer
     with Import, Convention => C, External_Name => "wl_display_prepare_read";
   --  Returns 0 on success or -1 if event queue was not empty

   procedure Display_Cancel_Read (Object : Display_Ptr)
     with Import, Convention => C, External_Name => "wl_display_cancel_read";

   function Display_Read_Events (Object : Display_Ptr) return Integer
     with Import, Convention => C, External_Name => "wl_display_read_events";

   -----------------------------------------------------------------------------
   --                               Event Queue                               --
   -----------------------------------------------------------------------------

--   function Display_Create_Queue (Object : Display_Ptr) return System.Address
--     with Import, Convention => C, External_Name => "wl_display_create_queue";

--   function Display_Dispatch_Queue
--     (Object : Display_Ptr;
--      Queue  : System.Address) return int
--   with Import, Convention => C, External_Name => "wl_display_dispatch_queue";

--   function Display_Dispatch_Queue_Pending
--     (Object : Display_Ptr;
--      Queue  : System.Address) return int
--   with Import, Convention => C, External_Name => "wl_display_dispatch_queue_pending";

--   function Display_Roundtrip_Queue (Object : Display_Ptr; Queue : System.Address) return int
--     with Import, Convention => C, External_Name => "wl_display_roundtrip_queue";

--   function Display_Prepare_Read_Queue (Object : Display_Ptr; Queue : System.Address) return int
--     with Import, Convention => C, External_Name => "wl_display_prepare_read_queue";

--   procedure Proxy_Set_Queue (Object : in out Proxy; Queue : access Event_Queue)
--     with Import, Convention => C, External_Name => "wl_proxy_set_queue";

--   procedure Event_Queue_Destroy (Object : in out Event_Queue)
--     with Import, Convention => C, External_Name => "wl_event_queue_destroy";

private

   type Proxy is limited null record;

end Wayland.API;
