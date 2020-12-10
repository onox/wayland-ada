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

with C_Binding.Linux.Files;
with Interfaces.C;

package C_Binding.Linux.File_Status is
   pragma Preelaborate;

   use all type C_Binding.Linux.Files.File;

   type Time_Seconds is new Interfaces.C.long;

   type Time_Nano_Seconds is new Interfaces.C.long;

   type Status_Time is record
      Seconds      : Time_Seconds;
      Nano_Seconds : Time_Nano_Seconds;
   end record;

   type Status_Device_Id is new Interfaces.C.unsigned_long;

   type Status_Inode_Number is new Interfaces.C.unsigned_long;

   type Status_Hard_Link_Count is new Interfaces.C.unsigned_long;

   type Status_Mode is new Interfaces.C.unsigned;

   type Status_User_Id is new Interfaces.C.unsigned;

   type Status_Group_Id is new Interfaces.C.unsigned;

   type Status_Block_Size is new Interfaces.C.long;

   type Status_Block_Count is new Interfaces.C.long;

   type Status is limited private;

   procedure Get_File_Status
     (File : in     C_Binding.Linux.Files.File;
      This : in out Status)
   with Global => null,
        Pre    => Is_Open (File);

   function Is_Valid (This : Status) return Boolean
     with Global => null;

   function Device_Id (This : Status) return Status_Device_Id
     with Global => null,
          Pre    => Is_Valid (This);

   function Inode_Number (This : Status) return Status_Inode_Number
     with Global => null,
          Pre    => Is_Valid (This);

   function Hard_Link_Count (This : Status) return Status_Hard_Link_Count
     with Global => null,
          Pre    => Is_Valid (This);

   function Mode (This : Status) return Status_Mode
     with Global => null,
          Pre    => Is_Valid (This);

   function User_Id (This : Status) return Status_User_Id
     with Global => null,
          Pre    => Is_Valid (This);

   function Group_Id (This : Status) return Status_Group_Id
     with Global => null,
          Pre    => Is_Valid (This);

   function Special_Device_Id (This : Status) return Status_Device_Id
     with Global => null,
          Pre    => Is_Valid (This);

   function Size (This : Status) return Ada.Streams.Stream_Element_Count
     with Global => null,
          Pre    => Is_Valid (This);

   function Block_Size (This : Status) return Status_Block_Size
     with Global => null,
          Pre    => Is_Valid (This);

   function Block_Count (This : Status) return Status_Block_Count
     with Global => null,
          Pre    => Is_Valid (This);

   function Last_Access_Time (This : Status) return Status_Time
     with Global => null,
          Pre    => Is_Valid (This);

   function Modification_Time (This : Status) return Status_Time
     with Global => null,
          Pre    => Is_Valid (This);

   function Change_Time (This : Status) return Status_Time
     with Global => null,
          Pre    => Is_Valid (This);

private

   type Status_Size is new Interfaces.C.long;

   type C_Time is record
      Sec      : aliased Time_Seconds;
      Nano_Sec : aliased Time_Nano_Seconds;
   end record
     with Convention => C_Pass_By_Copy;

   type File_Status_T is record
      Device_Id : aliased Status_Device_Id;

      Inode_Number    : aliased Status_Inode_Number;
      Hard_Link_Count : aliased Status_Hard_Link_Count;

      Mode : aliased Status_Mode;

      User_Id   : aliased Status_User_Id;
      Group_Id  : aliased Status_Group_Id;
      Padding_0 : aliased Interfaces.C.int;

      Special_Device_Id : aliased Status_Device_Id;

      Size : aliased Status_Size;

      Block_Size : aliased Status_Block_Size;

      Block_Count : aliased Status_Block_Count;

      Access_Time : aliased C_Time;

      Modification_Time : aliased C_Time;

      Change_Time : aliased C_Time;
      Padding_1   : Interfaces.C.long;
      Padding_2   : Interfaces.C.long;
      Padding_3   : Interfaces.C.long;
   end record
     with Convention => C_Pass_By_Copy;

   type Status is limited record
      My_Status   : aliased File_Status_T;
      My_Is_Valid : Boolean := False;
   end record;

   function Is_Valid
     (This : Status) return Boolean
   is (This.My_Is_Valid);

   function Device_Id
     (This : Status) return Status_Device_Id
   is (This.My_Status.Device_Id);

   function Inode_Number
     (This : Status) return Status_Inode_Number
   is (This.My_Status.Inode_Number);

   function Hard_Link_Count
     (This : Status) return Status_Hard_Link_Count
   is (This.My_Status.Hard_Link_Count);

   function Mode
     (This : Status) return Status_Mode
   is (This.My_Status.Mode);

   function User_Id
     (This : Status) return Status_User_Id
   is (This.My_Status.User_Id);

   function Group_Id
     (This : Status) return Status_Group_Id
   is (This.My_Status.Group_Id);

   function Special_Device_Id
     (This : Status) return Status_Device_Id
   is (This.My_Status.Special_Device_Id);

   function Size
     (This : Status) return Ada.Streams.Stream_Element_Count
   is (Ada.Streams.Stream_Element_Count (This.My_Status.Size));

   function Block_Size
     (This : Status) return Status_Block_Size
   is (This.My_Status.Block_Size);

   function Block_Count
     (This : Status) return Status_Block_Count
   is (This.My_Status.Block_Count);

   function Last_Access_Time
     (This : Status) return Status_Time
   is ((Seconds      => This.My_Status.Access_Time.Sec,
        Nano_Seconds => This.My_Status.Access_Time.Nano_Sec));

   function Modification_Time
     (This : Status) return Status_Time
   is ((Seconds      => This.My_Status.Modification_Time.Sec,
        Nano_Seconds => This.My_Status.Modification_Time.Nano_Sec));

   function Change_Time
     (This : Status) return Status_Time
   is ((Seconds      => This.My_Status.Change_Time.Sec,
        Nano_Seconds => This.My_Status.Change_Time.Nano_Sec));

   function C_Get_File_Status
     (Fd     : Interfaces.C.int;
      Status : access File_Status_T) return Interfaces.C.int
   with Import, Convention => C, External_Name => "fstat";

end C_Binding.Linux.File_Status;
