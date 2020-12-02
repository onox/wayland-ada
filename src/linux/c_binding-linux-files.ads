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

package C_Binding.Linux.Files with Preelaborate is

   type File_Permission is
     (
      Owner_Read,  Owner_Write,  Owner_Execute,
      Group_Read,  Group_Write,  Group_Execute,
      Others_Read, Others_Write, Others_Execute
     );

   type File_Permissions is array (File_Permission) of Boolean;

   type File_Mode is
     (
      Read_Only, Write_Only, Read_Write
     );

   type File is new File_Base;
   --  with Default_Initial_Condition => Is_Closed (File);

   procedure Set_File_Descriptor
     (This  : in out File;
      Value : Integer);

   procedure Open
     (This        : in out File;
      File_Name   : in     String;
      Mode        : in     File_Mode;
      Permissions : in     File_Permissions) with
     Global => null,
     Pre    => Is_Closed (This);

   function Close (This : in out File) return Success_Flag with
     Global => null,
     Pre    => Is_Open (This),
     Post   => Is_Closed (This);

   procedure Write
     (This : File;
      Bytes : Ada.Streams.Stream_Element_Array) with
     Global => null,
     Pre    => Is_Open (This);

   type Read_Result_Kind_Id is
     (
      Read_Success,
      End_Of_File_Reached,
      Read_Failure
     );

   type Read_Result (Kind_Id : Read_Result_Kind_Id) is record
      case Kind_Id is
         when Read_Success =>
            Element_Count : Ada.Streams.Stream_Element_Count;
         when End_Of_File_Reached =>
            null;
         when Read_Failure =>
            null;
      end case;
   end record;

   function Read
     (This  : File;
      Bytes : in out Ada.Streams.Stream_Element_Array) return Read_Result with
     Global => null,
     Pre    => Is_Open (This);

   function File_Descriptor (This : File) return Integer with
     Global => null,
     Pre    => Is_Open (This);

   function Is_Open (This : File) return Boolean with
     Global => null;

   function Is_Closed (This : File) return Boolean with
     Global => null;

private

   function File_Descriptor (This : File) return Integer is
     (Integer (This.My_File_Descriptor));

   function Is_Open (This : File) return Boolean is
     (This.My_File_Descriptor /= -1);

   function Is_Closed (This : File) return Boolean is
     (This.My_File_Descriptor = -1);

   function C_Open
     (File_Name : C_String;
      Flags     : O_FLag;
      S_Flags   : S_FLag) return Interfaces.C.int with
     Import        => True,
     Convention    => C,
     External_Name => "open";

end C_Binding.Linux.Files;
