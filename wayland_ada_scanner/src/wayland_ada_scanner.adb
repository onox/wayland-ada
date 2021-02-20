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

with Ada.Command_Line;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Aida.Deepend.XML_DOM_Parser;

with Wayland_XML;
with Xml_Parser_Utils;

procedure Wayland_Ada_Scanner is

   use type Ada.Containers.Count_Type;
   use type Wayland_XML.Event_Child;
   use type Wayland_XML.Request_Child;

   use all type Wayland_XML.Arg_Tag;
   use all type Wayland_XML.Copyright_Tag;
   use all type Wayland_XML.Description_Tag;
   use all type Wayland_XML.Entry_Tag;
   use all type Wayland_XML.Enum_Tag;
   use all type Wayland_XML.Event_Tag;
   use all type Wayland_XML.Request_Tag;
   use all type Wayland_XML.Interface_Tag;
   use all type Wayland_XML.Protocol_Tag;

   use all type Aida.Deepend.XML_DOM_Parser.Node_Kind_Id;
   use all type Aida.Deepend.XML_DOM_Parser.Attribute;
   use all type Aida.Deepend.XML_DOM_Parser.XML_Tag;
   use all type Wayland_XML.Protocol_Child_Kind_Id;
   use all type Wayland_XML.Interface_Child_Kind_Id;
   use all type Wayland_XML.Enum_Child_Kind_Id;
   use all type Wayland_XML.Event_Child_Kind_Id;
   use all type Wayland_XML.Arg_Type_Attribute;
   use all type Wayland_XML.Request_Child_Kind_Id;
   use all type Wayland_XML.Enum_Child;

   package SF renames Ada.Strings.Fixed;
   package SU renames Ada.Strings.Unbounded;

   package Unbounded_String_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => SU.Unbounded_String,
      "="          => SU."=");

   function Trim (Value : String) return String is (SF.Trim (Value, Ada.Strings.Both));

   function Is_Reserved_Keyword (Name : String) return Boolean is
     (Name in "Begin" | "End");

   procedure Put
     (File : Ada.Text_IO.File_Type;
      Item : String) renames Ada.Text_IO.Put;

   procedure Put_Line
     (File : Ada.Text_IO.File_Type;
      Item : String) renames Ada.Text_IO.Put_Line;

   procedure New_Line
     (File    : Ada.Text_IO.File_Type;
      Spacing : Ada.Text_IO.Positive_Count := 1) renames Ada.Text_IO.New_Line;

   function "+" (Value : String) return SU.Unbounded_String renames SU.To_Unbounded_String;
   function "+" (Value : SU.Unbounded_String) return String renames SU.To_String;

   XML_Exception : exception;

   procedure Create_Wayland_Spec_File (Enable_Comments : Boolean);
   procedure Create_Wayland_Body_File;

   Protocol_Tag : Wayland_XML.Protocol_Tag_Ptr;

   function Is_Deprecated (Interface_Tag : Wayland_XML.Interface_Tag) return Boolean is
     (Name (Interface_Tag) in "wl_shell" | "wl_shell_surface");
      --  wl_shell[_surface] are deprecated and replaced by the xdg-shell protocol

   function Get_Protocol_Name (Name : String) return String is
     (if Name = "wayland" then "client" else Name);

   package String_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => String,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   procedure Generate_Code_For_Arg
     (File          : Ada.Text_IO.File_Type;
      Interface_Tag : Wayland_XML.Interface_Tag;
      Arg_Tag       : Wayland_XML.Arg_Tag;
      Max_Length    : Natural;
      Is_Last       : Boolean;
      Enum_Types    : in out String_Maps.Map)
   is
      function Get_Enum_Type_Name return String is
         Result : String := Enum (Arg_Tag);
      begin
         case SF.Count (Result, ".") is
            when 0 =>
               return Name (Interface_Tag) & "_" & Result;
            when 1 =>
               Result (SF.Index (Result, ".")) := '_';
               return Result;
            when others =>
               raise XML_Exception;
         end case;
      end Get_Enum_Type_Name;

      Arg_Name      : constant String
        := Xml_Parser_Utils.Adaify_Variable_Name (Name (Arg_Tag));
      Arg_Type : constant String :=
        Xml_Parser_Utils.Arg_Type_As_String (Arg_Tag);

      Arg_Type_Name : constant String :=
        (if Exists_Enum (Arg_Tag) then
           Xml_Parser_Utils.Adaify_Name (Get_Enum_Type_Name)
         else
           Arg_Type);

      Arg_Name_Aligned : constant String := SF.Head (Arg_Name, Max_Length, ' ');
   begin
      if Arg_Type_Name /= Arg_Type then
         Enum_Types.Include (Arg_Type_Name, Arg_Type);
      end if;

      if Is_Last then
         Put (File, "      " & Arg_Name_Aligned & " : " & Arg_Type_Name & ")");
      else
         Put_Line (File, "      " & Arg_Name_Aligned & " : " & Arg_Type_Name & ";");
      end if;
   end Generate_Code_For_Arg;

   type Parameter is record
      Name, Ada_Type : SU.Unbounded_String;
   end record;

   type Parameter_Array is array (Positive range <>) of Parameter;

   type Subprogram_Kind is (Declaration, Implementation);

   Max_Line_Length : constant := 99;

   procedure Generate_Pretty_Code_For_Subprogram
     (File : Ada.Text_IO.File_Type;
      Kind : Subprogram_Kind;
      Interface_Tag  : aliased Wayland_XML.Interface_Tag;
      Procedure_Name : String;
      Return_Type    : String;
      Parameters     : Parameter_Array := (1 .. 0 => <>))
   is
      Name     : constant String
        := Xml_Parser_Utils.Adaify_Name
          (Wayland_XML.Name (Interface_Tag));
      Ptr_Name : constant String
        := Xml_Parser_Utils.Adaify_Name
          (Wayland_XML.Name (Interface_Tag) & "_Ptr");

      use Ada.Strings.Fixed;

      Subprogram_Kind : constant String := (if Return_Type'Length > 0 then "function" else "procedure");
      Return_String   : constant String := (if Return_Type'Length > 0 then " return " & Return_Type else "");

      Max_Name_Length : Natural := Name'Length;

      function Align (Value : String) return String is (SF.Head (Value, Max_Name_Length, ' '));

      Last_Char : constant String := (if Kind = Declaration then ";" else " is");
      One_Line  : Boolean;
   begin
      for Parameter of Parameters loop
         Max_Name_Length := Natural'Max (Max_Name_Length, SU.Length (Parameter.Name));
      end loop;

      declare
         Line_Length : Natural := Subprogram_Kind'Length + 8 + 2 * Name'Length + Procedure_Name'Length + Ptr_Name'Length + Return_String'Length + Last_Char'Length + 2 * Parameters'Length;
      begin
         for Parameter of Parameters loop
            Line_Length := Line_Length + SU.Length (Parameter.Name) + SU.Length (Parameter.Ada_Type) + 3;
         end loop;

         One_Line := Parameters'Length = 0 and Line_Length <= Max_Line_Length;
      end;

      if One_Line then
         Put (File, "   " & Subprogram_Kind & " " & Name & "_" & Procedure_Name & " (" & Name & " : " & Ptr_Name);
         if Parameters'Length > 0 then
            Put (File, ";");
            for Index in Parameters'Range loop
               Put (File, " " & (+Parameters (Index).Name) & " : " & (+Parameters (Index).Ada_Type) &
                 (if Index = Parameters'Last then ")" & Return_String & Last_Char else ";"));
            end loop;
            Put_Line (File, "");
         else
            Put_Line (File, ")" & Return_String & Last_Char);
         end if;
      else
         Put_Line (File, "   " & Subprogram_Kind & " " & Name & "_" & Procedure_Name);
         if Parameters'Length > 0 then
            Put_Line (File, "     (" & Align (Name) & " : " & Ptr_Name & ";");
            for Index in Parameters'Range loop
               Put_Line (File, "      " & Align (+Parameters (Index).Name) & " : " & (+Parameters (Index).Ada_Type) &
                 (if Index = Parameters'Last then ")" & Return_String & Last_Char else ";"));
            end loop;
         else
            Put_Line (File, "     (" & Name & " : " & Ptr_Name & ")" & Return_String & Last_Char);
         end if;
      end if;
   end Generate_Pretty_Code_For_Subprogram;

   procedure Iterate_Over_Interfaces
     (Process : not null access procedure (Interface_Tag : aliased Wayland_XML.Interface_Tag)) is
   begin
      for Child of Children (Protocol_Tag.all) loop
         if Child.Kind_Id = Child_Interface and then not Is_Deprecated (Child.Interface_Tag.all) then
            Process (Child.Interface_Tag.all);
         end if;
      end loop;
   end Iterate_Over_Interfaces;

   procedure Iterate_Over_Requests
     (Interface_Tag : aliased Wayland_XML.Interface_Tag;
      Process       : not null access procedure (Request_Tag : aliased Wayland_XML.Request_Tag)) is
   begin
      for Child of Children (Interface_Tag) loop
         if Child.Kind_Id = Child_Request then
            Process (Child.Request_Tag.all);
         end if;
      end loop;
   end Iterate_Over_Requests;

   procedure Iterate_Over_Events
     (Interface_Tag : aliased Wayland_XML.Interface_Tag;
      Process       : not null access procedure (Event_Tag : aliased Wayland_XML.Event_Tag)) is
   begin
      for Child of Children (Interface_Tag) loop
         if Child.Kind_Id = Child_Event then
            Process (Child.Event_Tag.all);
         end if;
      end loop;
   end Iterate_Over_Events;

   function Get_Name (Arg_Tag   : Wayland_XML.Arg_Tag) return String is
     (Xml_Parser_Utils.Adaify_Variable_Name (Name (Arg_Tag)));

   procedure Get_Max_Arg_Length
     (Request_Tag : aliased Wayland_XML.Request_Tag;
      V           : in out Wayland_XML.Request_Child_Vectors.Vector;
      Length      : in out Natural) is
   begin
      for Child of Children (Request_Tag) loop
         if Child.Kind_Id = Child_Arg then
            if Type_Attribute (Child.Arg_Tag.all) /= Type_New_Id then
               V.Append (Child);
               Length := Natural'Max (Length, Get_Name (Child.Arg_Tag.all)'Length);
            end if;
         end if;
      end loop;
   end Get_Max_Arg_Length;

   procedure Read_Wayland_XML_File
     (File_Name, Out_Folder : String;
      Enable_Comments       : Boolean)
   is
      procedure Identify_Protocol_Tag;
      procedure Identify_Protocol_Children;

      procedure Allocate_Space_For_Wayland_XML_Contents
        (File_Name     : String;
         File_Contents : out Aida.Deepend.String_Ptr)
      is
         package IO renames Ada.Streams.Stream_IO;

         Size : constant Natural := Natural (Ada.Directories.Size (File_Name));
         File : IO.File_Type;

         subtype File_String is String (1 .. Size);
      begin
         IO.Open (File, IO.In_File, File_Name);

         File_Contents := new File_String;
         File_String'Read (IO.Stream (File), File_Contents.all);

         IO.Close (File);
      end Allocate_Space_For_Wayland_XML_Contents;

      Root_Node : Aida.Deepend.XML_DOM_Parser.Node_Ptr;

      procedure Parse_Contents (File_Contents : Aida.Deepend.String_Ptr) is
         Call_Result : Aida.Call_Result;
      begin
         Aida.Deepend.XML_DOM_Parser.Parse
           (File_Contents.all, Call_Result, Root_Node);

         if Call_Result.Has_Failed then
            raise XML_Exception with Call_Result.Message;
         else
            Identify_Protocol_Tag;
            Identify_Protocol_Children;
         end if;
      end Parse_Contents;

      procedure Identify_Protocol_Tag is
      begin
         if Root_Node.Id /= Node_Kind_Tag or else Name (Root_Node.Tag) /= "protocol" then
            raise XML_Exception with "Root node is not <protocol>";
         end if;

         if
           Attributes (Root_Node.Tag).Length /= 1 or else
           Name (Attributes (Root_Node.Tag) (1).all) /= "name"
         then
            raise XML_Exception with "<protocol> node has no name attribute";
         end if;

         Protocol_Tag := new Wayland_XML.Protocol_Tag;
         Set_Name (Protocol_Tag.all, Value (Attributes (Root_Node.Tag) (1).all));
      end Identify_Protocol_Tag;

      procedure Identify_Protocol_Children is
         function Identify_Copyright
           (Node : not null Aida.Deepend.XML_DOM_Parser.Node_Ptr)
           return not null Wayland_XML.Copyright_Ptr
         is
            Copyright_Tag : constant not null Wayland_XML.Copyright_Ptr
              := new Wayland_XML.Copyright_Tag;
         begin
            if Child_Nodes (Node.Tag).Length = 1 then
               if Child_Nodes (Node.Tag) (1).Id = Node_Kind_Text then
                  Set_Text
                    (Copyright_Tag.all,
                     Trim (Child_Nodes (Node.Tag) (1).Text.all));
               else
                  raise XML_Exception;
               end if;
            else
               raise XML_Exception;
            end if;
            return Copyright_Tag;
         end Identify_Copyright;

         function Identify_Description
           (Node : not null Aida.Deepend.XML_DOM_Parser.Node_Ptr)
           return not null Wayland_XML.Description_Tag_Ptr
         is
            Description_Tag : constant not null Wayland_XML.Description_Tag_Ptr
              := new Wayland_XML.Description_Tag;

            Text : SU.Unbounded_String;
         begin
            if Attributes (Node.Tag).Length = 1 and then
              Name (Attributes (Node.Tag) (1).all) = "summary"
            then
               Set_Summary (Description_Tag.all, Value (Attributes (Node.Tag) (1).all));
            else
               raise XML_Exception with "Expected tag 'description' to have 1 attribute 'summary'";
            end if;

            for Child of Child_Nodes (Node.Tag) loop
               if Child.Id = Node_Kind_Text then
                  SU.Append (Text, Trim (Child.Text.all));
               elsif Child.Id /= Node_Kind_Comment then
                  raise XML_Exception with
                    "Tag 'description' has unexpected child " & Child.Id'Image;
               end if;
            end loop;
            Set_Text (Description_Tag.all, +Text);

            return Description_Tag;
         end Identify_Description;

         function Identify_Arg
           (Node : not null Aida.Deepend.XML_DOM_Parser.Node_Ptr)
           return not null Wayland_XML.Arg_Tag_Ptr
         is
            Arg_Tag : constant not null Wayland_XML.Arg_Tag_Ptr
              := new Wayland_XML.Arg_Tag;

            procedure Iterate
              (Tag : aliased Aida.Deepend.XML_DOM_Parser.XML_Tag) is
            begin
               for A of Attributes (Tag) loop
                  if Name (A.all) = "name" then
                     Set_Name (Arg_Tag.all, Value (A.all));
                  elsif Name (A.all) = "type" then
                     Set_Type_Attribute (Arg_Tag.all, Value (A.all));
                  elsif Name (A.all) = "summary" then
                     Set_Summary (Arg_Tag.all, Value (A.all));
                  elsif Name (A.all) = "interface" then
                     Set_Interface_Attribute (Arg_Tag.all, Value (A.all));
                  elsif Name (A.all) = "allow-null" then
                     if Value (A.all) = "true" then
                        Set_Allow_Null (Arg_Tag.all, True);
                     elsif Value (A.all) = "false" then
                        Set_Allow_Null (Arg_Tag.all, False);
                     else
                        raise XML_Exception with
                          "Attribute 'allow-null' must be 'true' or 'false'";
                     end if;
                  elsif Name (A.all) = "enum" then
                     Set_Enum (Arg_Tag.all, Value (A.all));
                  else
                     raise XML_Exception;
                  end if;
               end loop;
            end Iterate;
         begin
            Iterate (Node.Tag);

            return Arg_Tag;
         end Identify_Arg;

         function Identify_Request
           (Node : not null Aida.Deepend.XML_DOM_Parser.Node_Ptr)
           return not null Wayland_XML.Request_Tag_Ptr
         is
            Request_Tag : constant not null Wayland_XML.Request_Tag_Ptr
              := new Wayland_XML.Request_Tag;

            procedure Iterate
              (Tag : aliased Aida.Deepend.XML_DOM_Parser.XML_Tag) is
            begin
               for A of Attributes (Tag) loop
                  if Name (A.all) = "name" then
                     Set_Name (Request_Tag.all, Value (A.all));
                  elsif Name (A.all) = "type" then
                     Set_Type_Attribute (Request_Tag.all, Value (A.all));
                  elsif Name (A.all) = "since" then
                     declare
                        V          : Integer;
                        Has_Failed : Boolean;
                     begin
                        Aida.To_Integer (Value (A.all), V, Has_Failed);

                        if Has_Failed then
                           raise XML_Exception;
                        else
                           Set_Since
                             (Request_Tag.all,
                              Wayland_XML.Version_Number (V));
                        end if;
                     end;
                  else
                     raise XML_Exception;
                  end if;
               end loop;

               for Child of Child_Nodes (Tag) loop
                  if Child.Id = Node_Kind_Tag then
                     if Name (Child.Tag) = "description" then
                        Append_Child
                          (Request_Tag.all, Identify_Description (Child));
                     elsif Name (Child.Tag) = "arg" then
                        Append_Child (Request_Tag.all, Identify_Arg (Child));
                     else
                        raise XML_Exception;
                     end if;
                  else
                     raise XML_Exception;
                  end if;
               end loop;
            end Iterate;
         begin
            Iterate (Node.Tag);

            return Request_Tag;
         end Identify_Request;

         function Identify_Event
           (Node : not null Aida.Deepend.XML_DOM_Parser.Node_Ptr)
           return not null Wayland_XML.Event_Tag_Ptr
         is
            Event_Tag : constant not null Wayland_XML.Event_Tag_Ptr
              := new Wayland_XML.Event_Tag;

            procedure Iterate
              (Tag : aliased Aida.Deepend.XML_DOM_Parser.XML_Tag) is
            begin
               for A of Attributes (Tag) loop
                  if Name (A.all) = "name" then
                     Set_Name (Event_Tag.all, Value (A.all));
                  elsif Name (A.all) = "since" then
                     declare
                        V          : Integer;
                        Has_Failed : Boolean;
                     begin
                        Aida.To_Integer (Value (A.all), V, Has_Failed);

                        if Has_Failed then
                           raise XML_Exception;
                        else
                           Set_Since
                             (Event_Tag.all, Wayland_XML.Version_Number (V));
                        end if;
                     end;
                  elsif Name (A.all) = "type" and Value (A.all) = "destructor" then
                     --  FIXME Handle destructor events by destroying the proxy
                     --  See https://gitlab.freedesktop.org/wayland/wayland/-/issues/96
                     null;
                  else
                     raise XML_Exception with
                       "Tag 'event' has unexpected attribute " & Value (A.all);
                  end if;
               end loop;

               for Child of Child_Nodes (Tag) loop
                  if Child.Id = Node_Kind_Tag then
                     if Name (Child.Tag) = "description" then
                        Append_Child
                          (Event_Tag.all, Identify_Description (Child));
                     elsif Name (Child.Tag) = "arg" then
                        Append_Child (Event_Tag.all, Identify_Arg (Child));
                     else
                        raise XML_Exception;
                     end if;
                  else
                     raise XML_Exception;
                  end if;
               end loop;
            end Iterate;
         begin
            Iterate (Node.Tag);

            return Event_Tag;
         end Identify_Event;

         function Identify_Entry
           (Node : not null Aida.Deepend.XML_DOM_Parser.Node_Ptr)
           return not null Wayland_XML.Entry_Tag_Ptr
         is
            Entry_Tag : constant not null Wayland_XML.Entry_Tag_Ptr
              := new Wayland_XML.Entry_Tag;

            procedure Iterate
              (Tag : aliased Aida.Deepend.XML_DOM_Parser.XML_Tag) is
            begin
               for A of Attributes (Tag) loop
                  if Name (A.all) = "name" then
                     Set_Name (Entry_Tag.all, Value (A.all));
                  elsif Name (A.all) = "value" then
                     declare
                        V          : Integer;
                        Has_Failed : Boolean;
                     begin
                        Aida.To_Integer (Value (A.all), V, Has_Failed);

                        if Has_Failed then
                           declare
                              S : String renames Value (A.all);
                           begin
                              if
                                S (S'First .. S'First + 1) = "0x"
                              then
                                 V := Integer'Value
                                   (
                                    "16#" & S (S'First + 2 .. S'Last) & "#"
                                   );

                                 Set_Value
                                   (Entry_Tag.all,
                                    Wayland_XML.Entry_Value (V));
                              else
                                 raise XML_Exception;
                              end if;
                           end;
                        else
                           Set_Value
                             (Entry_Tag.all, Wayland_XML.Entry_Value (V));
                        end if;
                     end;
                  elsif Name (A.all) = "summary" then
                     Set_Summary (Entry_Tag.all, Value (A.all));
                  elsif Name (A.all) = "since" then
                     declare
                        The_Value  : Integer;
                        Has_Failed : Boolean;
                     begin
                        Aida.To_Integer (Value (A.all), The_Value, Has_Failed);

                        if Has_Failed then
                           raise XML_Exception;
                        else
                           Set_Since
                             (Entry_Tag.all,
                              Wayland_XML.Version_Number (The_Value));
                        end if;
                     end;
                  else
                     raise XML_Exception;
                  end if;
               end loop;

               --  Some files have <entry> nodes with a <description summary>
               --  child node instead of summary attribute
               if Child_Nodes (Tag).Length > 0 then
                  for Child of Child_Nodes (Tag) loop
                     case Child.Id is
                        when Node_Kind_Text =>
                           --  TODO Check Trim (X.Text.all) is just whitespace
                           null;
                        when Node_Kind_Tag =>
                           if Name (Child.Tag) = "description" then
                              for A of Attributes (Child.Tag) loop
                                 if Name (A.all) = "summary" then
                                    Set_Summary (Entry_Tag.all, Value (A.all));
                                 else
                                    raise XML_Exception;
                                 end if;
                              end loop;
                           else
                              raise XML_Exception;
                           end if;
                        when others =>
                           raise XML_Exception;
                     end case;
                  end loop;
               end if;
            end Iterate;
         begin
            Iterate (Node.Tag);

            return Entry_Tag;
         end Identify_Entry;

         function Identify_Enum
           (Node : not null Aida.Deepend.XML_DOM_Parser.Node_Ptr)
           return not null Wayland_XML.Enum_Tag_Ptr
         is
            Enum_Tag : constant not null Wayland_XML.Enum_Tag_Ptr
              := new Wayland_XML.Enum_Tag;

            procedure Iterate
              (Tag : aliased Aida.Deepend.XML_DOM_Parser.XML_Tag) is
            begin
               for A of Attributes (Tag) loop
                  if Name (A.all) = "name" then
                     Set_Name (Enum_Tag.all, Value (A.all));
                  elsif Name (A.all) = "bitfield" then
                     if Value (A.all) = "true" then
                        Set_Bitfield (Enum_Tag.all, True);
                     elsif Value (A.all) = "false" then
                        Set_Bitfield (Enum_Tag.all, False);
                     else
                        raise XML_Exception;
                     end if;
                  elsif Name (A.all) = "since" then
                     declare
                        The_Value  : Integer;
                        Has_Failed : Boolean;
                     begin
                        Aida.To_Integer (Value (A.all), The_Value, Has_Failed);

                        if Has_Failed then
                           raise XML_Exception;
                        else
                           Set_Since
                             (Enum_Tag.all,
                              Wayland_XML.Version_Number (The_Value));
                        end if;
                     end;
                  else
                     raise XML_Exception;
                  end if;
               end loop;

               for Child of Child_Nodes (Tag) loop
                  if Child.Id = Node_Kind_Tag then
                     if Name (Child.Tag) = "description" then
                        Append_Child
                          (Enum_Tag.all, Identify_Description (Child));
                     elsif Name (Child.Tag) = "entry" then
                        Append_Child
                          (Enum_Tag.all, Identify_Entry (Child));
                     else
                        raise XML_Exception;
                     end if;
                  elsif Child.Id = Node_Kind_Comment then
                     --  Ignore any comments inside <enum>
                     null;
                  else
                     raise XML_Exception with
                       "Tag 'enum' has unexpected child " & Child.Id'Image;
                  end if;
               end loop;

               Sort_Entries (Enum_Tag.all);
            end Iterate;
         begin
            Iterate (Node.Tag);

            return Enum_Tag;
         end Identify_Enum;

         function Identify_Interface
           (Node : not null Aida.Deepend.XML_DOM_Parser.Node_Ptr)
           return not null Wayland_XML.Interface_Tag_Ptr
         is
            Interface_Tag : constant not null Wayland_XML.Interface_Tag_Ptr
              := new Wayland_XML.Interface_Tag;

            procedure Iterate
              (Tag : aliased Aida.Deepend.XML_DOM_Parser.XML_Tag) is
            begin
               if Attributes (Tag).Length = 2 then
                  if Name (Attributes (Tag) (1).all) = "name" then
                     Set_Name
                       (Interface_Tag.all,
                        Value (Attributes (Tag) (1).all));
                  else
                     raise XML_Exception;
                  end if;

                  if Name (Attributes (Tag) (2).all) = "version" then
                     declare
                        The_Value  : Integer;
                        Has_Failed : Boolean;
                     begin
                        Aida.To_Integer
                          (Value (Attributes (Tag) (2).all),
                           The_Value,
                           Has_Failed);

                        if Has_Failed then
                           raise XML_Exception;
                        else
                           Set_Version
                             (Interface_Tag.all,
                              Wayland_XML.Version_Number (The_Value));

                           for Child of Child_Nodes (Tag) loop
                              if Child.Id = Node_Kind_Tag then
                                 if Name (Child.Tag) = "description" then
                                    Append_Child
                                      (Interface_Tag.all, Identify_Description (Child));
                                 elsif Name (Child.Tag) = "request" then
                                    Append_Child
                                      (Interface_Tag.all, Identify_Request (Child));
                                 elsif Name (Child.Tag) = "event" then
                                    Append_Child
                                      (Interface_Tag.all, Identify_Event (Child));
                                 elsif Name (Child.Tag) = "enum" then
                                    Append_Child
                                      (Interface_Tag.all, Identify_Enum (Child));
                                 else
                                    raise XML_Exception;
                                 end if;
                              elsif Child.Id = Node_Kind_Comment then
                                 null;
                              else
                                 raise XML_Exception;
                              end if;
                           end loop;
                        end if;
                     end;
                  else
                     raise XML_Exception;
                  end if;
               else
                  raise XML_Exception;
               end if;
            end Iterate;
         begin
            Iterate (Node.Tag);
            return Interface_Tag;
         end Identify_Interface;

         procedure Iterate
           (Tag : aliased Aida.Deepend.XML_DOM_Parser.XML_Tag) is
         begin
            for Child of Child_Nodes (Tag) loop
               if Child.Id = Node_Kind_Tag then
                  if Name (Child.Tag) = "interface" then
                     Append_Child
                       (Protocol_Tag.all, Identify_Interface (Child));
                  elsif Name (Child.Tag) = "copyright" then
                     Append_Child
                       (Protocol_Tag.all, Identify_Copyright (Child));
                  elsif Name (Child.Tag) = "description" then
                     Append_Child
                       (Protocol_Tag.all, Identify_Description (Child));
                  else
                     raise XML_Exception with "Unexpected tag " & Name (Child.Tag);
                  end if;
               elsif Child.Id /= Node_Kind_Comment then
                  raise XML_Exception with "Unexpected kind " & Child.Id'Image;
               end if;
            end loop;
         end Iterate;
      begin
         Iterate (Root_Node.Tag);

         Create_Wayland_Spec_File (Enable_Comments);
         Create_Wayland_Body_File;
      end Identify_Protocol_Children;

      File_Contents : Aida.Deepend.String_Ptr;

      use type Ada.Directories.File_Kind;
   begin
      if not Ada.Directories.Exists (File_Name) then
         raise Constraint_Error with File_Name & " does not exist";
      end if;

      Allocate_Space_For_Wayland_XML_Contents (File_Name, File_Contents);

      if not Ada.Directories.Exists (Out_Folder) then
         Ada.Directories.Create_Directory (Out_Folder);
      elsif Ada.Directories.Kind (Out_Folder) /= Ada.Directories.Directory then
         raise Constraint_Error with Out_Folder & " is not a directory";
      end if;

      declare
         Current_Folder : constant String := Ada.Directories.Current_Directory;
      begin
         Ada.Directories.Set_Directory (Out_Folder);
         begin
            Parse_Contents (File_Contents);
         exception
            when others =>
               Ada.Directories.Set_Directory (Current_Folder);
               raise;
         end;
         Ada.Directories.Set_Directory (Current_Folder);
      end;
   end Read_Wayland_XML_File;

   procedure Generate_Code_For_Numeric_Constants (File : Ada.Text_IO.File_Type) is
      procedure Handle_Interface
        (Interface_Tag : aliased Wayland_XML.Interface_Tag)
      is
         procedure Generate_Code_For_Opcodes is
            I : Integer := 0;

            procedure Generate_Code
              (Request_Tag : aliased Wayland_XML.Request_Tag)
            is
               Name : constant String
                 := Xml_Parser_Utils.Adaify_Name
                   (Wayland_XML.Name (Interface_Tag) & "_" &
                      Wayland_XML.Name (Request_Tag));
            begin
               if I = 0 then
                  New_Line (File);
               end if;
               Put_Line (File, "      " & Name & " : constant := " & Aida.To_String (I) & ";");

               I := I + 1;
            end Generate_Code;
         begin
            Iterate_Over_Requests (Interface_Tag, Generate_Code'Access);
         end Generate_Code_For_Opcodes;

      begin
         Generate_Code_For_Opcodes;
         --  Note: See git history for *_Since_Version constants
      end Handle_Interface;
   begin
      Iterate_Over_Interfaces (Handle_Interface'Access);
   end Generate_Code_For_Numeric_Constants;

   All_Types : Unbounded_String_Vectors.Vector;

   procedure Create_Wayland_Spec_File (Enable_Comments : Boolean) is
      File : Ada.Text_IO.File_Type;

      procedure Create_Wl_Thin_Spec_File (Protocol_Name : String);

      procedure Generate_Code_For_Type_Declarations;
      procedure Generate_Code_For_External_Proxies (Protocol_Name : String);
      procedure Generate_Code_For_The_Interface_Constants;
      procedure Generate_Code_For_Enum_Constants;
      procedure Generate_Private_Code_For_Enum_Constants;
      procedure Generate_Manually_Edited_Partial_Type_Declarations (Protocol_Name : String);
      procedure Generate_Use_Type_Declarions (Package_Name : String);
      procedure Generate_Manually_Edited_Code_For_Type_Definitions;
      procedure Generate_Private_Code_For_The_Interface_Constants;

      procedure Create_File is
         Protocol_Name : constant String := Get_Protocol_Name (Name (Protocol_Tag.all));
         Package_Name  : constant String := Xml_Parser_Utils.Adaify_Name (Protocol_Name);
      begin
         Ada.Text_IO.Create
           (File, Ada.Text_IO.Out_File,
            "wayland-protocols-" & Protocol_Name & ".ads");

         Put_Line (File, "private with Wayland.Protocols.Thin_" & Package_Name & ";");
         New_Line (File);
         Put_Line (File, "with Wayland.Enums." & Package_Name & ";");

         if Protocol_Name = "client" then
            null;
         elsif Protocol_Name = "xdg_decoration_unstable_v1" then
            New_Line (File);
            Put_Line (File, "with Wayland.Protocols.Client;");
            Put_Line (File, "with Wayland.Protocols.Xdg_Shell;");
         else
            New_Line (File);
            Put_Line (File, "with Wayland.Protocols.Client;");
         end if;

         New_Line (File);
         Put_Line (File, "package Wayland.Protocols." & Package_Name & " is");
         Put_Line (File, "   pragma Preelaborate;");
         New_Line (File);
         Put_Line (File, "   use Wayland.Enums." & Package_Name & ";");

         if Protocol_Name = "xdg_shell" then
            New_Line (File);
            Put_Line (File, "   type State_Array is array (Positive range <>) of Xdg_Toplevel_State;");
         end if;

         Generate_Code_For_Type_Declarations;
         Generate_Code_For_External_Proxies (Protocol_Name);
         Generate_Code_For_The_Interface_Constants;

         if Protocol_Name = "client" then
            Put_Line (File, "");
            Put_Line (File, "   function Bind");
            Put_Line (File, "     (Object  : Registry;");
            Put_Line (File, "      Iface   : Interface_Type;");
            Put_Line (File, "      Id      : Unsigned_32;");
            Put_Line (File, "      Version : Unsigned_32) return Secret_Proxy;");
         end if;

         Generate_Manually_Edited_Partial_Type_Declarations (Protocol_Name);

         New_Line (File);
         Put_Line (File, "private");
         New_Line (File);

         Generate_Use_Type_Declarions (Package_Name);
         Generate_Manually_Edited_Code_For_Type_Definitions;

         if Protocol_Name = "client" then
            Put_Line (File, "");
            Put_Line (File, "   function Get_Proxy (Object : Surface) return Secret_Proxy is (Secret_Proxy (Object.Proxy));");
            Put_Line (File, "   function Get_Proxy (Object : Seat) return Secret_Proxy is (Secret_Proxy (Object.Proxy));");
            Put_Line (File, "   function Get_Proxy (Object : Shm) return Secret_Proxy is (Secret_Proxy (Object.Proxy));");
            Put_Line (File, "   function Get_Proxy (Object : Output) return Secret_Proxy is (Secret_Proxy (Object.Proxy));");
            Put_Line (File, "   function Get_Proxy (Object : Pointer) return Secret_Proxy is (Secret_Proxy (Object.Proxy));");
            Put_Line (File, "   function Get_Proxy (Object : Region) return Secret_Proxy is (Secret_Proxy (Object.Proxy));");
         elsif Protocol_Name = "xdg_shell" then
            Put_Line (File, "");
            Put_Line (File, "   function Get_Proxy (Object : Xdg_Toplevel) return Secret_Proxy is (Secret_Proxy (Object.Proxy));");
         end if;

         Generate_Private_Code_For_The_Interface_Constants;

         Put_Line (File, "");
         Put_Line (File, "end Wayland.Protocols." & Package_Name & ";");

         Ada.Text_IO.Close (File);

         -----------------------------------------------------------------------

         --  TODO Do not generate file if there are no enum constants
         Ada.Text_IO.Create
           (File, Ada.Text_IO.Out_File,
            "wayland-enums-" & Protocol_Name & ".ads");

         Put_Line (File, "package Wayland.Enums." & Package_Name & " is");
         Put_Line (File, "   pragma Preelaborate;");

         Generate_Code_For_Enum_Constants;

         New_Line (File);
         Put_Line (File, "private");
         New_Line (File);

         Generate_Private_Code_For_Enum_Constants;

         Put_Line (File, "end Wayland.Enums." & Package_Name & ";");

         Ada.Text_IO.Close (File);

         Ada.Text_IO.Create
           (File, Ada.Text_IO.Out_File,
            "wayland-protocols-thin_" & Protocol_Name & ".ads");

         Put_Line (File, "with Interfaces.C.Strings;");
         Put_Line (File, "");
         Put_Line (File, "with Wayland.API;");
         Put_Line (File, "with Wayland.Enums." & Package_Name & ";");

         if Protocol_Name = "client" then
            null;
         elsif Protocol_Name = "xdg_decoration_unstable_v1" then
            Put_Line (File, "with Wayland.Protocols.Thin_Xdg_Shell;");
         else
            Put_Line (File, "with Wayland.Protocols.Thin_Client;");
         end if;
         Put_Line (File, "");
         Put_Line (File, "private package Wayland.Protocols.Thin_" & Package_Name & " is");
         Put_Line (File, "   pragma Preelaborate;");
         Put_Line (File, "");
         Put_Line (File, "   use Wayland.Enums." & Package_Name & ";");
         Put_Line (File, "");
         Put_Line (File, "   subtype chars_ptr is Interfaces.C.Strings.chars_ptr;");
         Put_Line (File, "");

         Put_Line (File, "   subtype Interface_T is Wayland.API.Interface_T;");
         Put_Line (File, "");
         Put_Line (File, "   subtype Proxy_Ptr     is Wayland.API.Proxy_Ptr;");
         Put_Line (File, "   subtype Interface_Ptr is Wayland.API.Interface_Ptr;");
         Put_Line (File, "");

         Create_Wl_Thin_Spec_File (Protocol_Name);

         Put_Line (File, "end Wayland.Protocols.Thin_" & Package_Name & ";");

         Ada.Text_IO.Close (File);
      end Create_File;

      procedure Generate_Code_For_Type_Declarations is
         procedure Handle_Interface
           (Interface_Tag : aliased Wayland_XML.Interface_Tag)
         is
            Name : constant String
              := Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Interface_Tag));
         begin
            Put_Line (File, "   type " & Name & " is tagged limited private;");
         end Handle_Interface;
      begin
         New_Line (File);
         Iterate_Over_Interfaces (Handle_Interface'Access);
      end Generate_Code_For_Type_Declarations;

      procedure Generate_Code_For_External_Proxies (Protocol_Name : String) is
      begin
         if Protocol_Name = "client" then
            Put_Line (File, "");
            Put_Line (File, "   function Get_Proxy (Object : Surface) return Secret_Proxy;");
            Put_Line (File, "   function Get_Proxy (Object : Seat) return Secret_Proxy;");
            Put_Line (File, "   function Get_Proxy (Object : Shm) return Secret_Proxy;");
            Put_Line (File, "   function Get_Proxy (Object : Output) return Secret_Proxy;");
            Put_Line (File, "   function Get_Proxy (Object : Pointer) return Secret_Proxy;");
            Put_Line (File, "   function Get_Proxy (Object : Region) return Secret_Proxy;");
            Put_Line (File, "");
            Put_Line (File, "   package Constructors is");
            Put_Line (File, "      function Set_Proxy (Proxy : Secret_Proxy) return Buffer;");
            Put_Line (File, "      function Set_Proxy (Proxy : Secret_Proxy) return Output;");
            Put_Line (File, "      function Set_Proxy (Proxy : Secret_Proxy) return Surface;");
            Put_Line (File, "   end Constructors;");
         elsif Protocol_Name = "xdg_shell" then
            Put_Line (File, "");
            Put_Line (File, "   function Get_Proxy (Object : Xdg_Toplevel) return Secret_Proxy;");
         end if;
      end Generate_Code_For_External_Proxies;

      procedure Generate_Code_For_The_Interface_Constants is
         procedure Handle_Interface
           (Interface_Tag : aliased Wayland_XML.Interface_Tag)
         is
            Name : constant String
              := Xml_Parser_Utils.Adaify_Name
                (Wayland_XML.Name (Interface_Tag) & "_Interface");
         begin
            Put_Line (File, "   " & Name & " : constant Interface_Type;");
         end Handle_Interface;
      begin
         New_Line (File);
         Iterate_Over_Interfaces (Handle_Interface'Access);
      end Generate_Code_For_The_Interface_Constants;

      function Get_Name (Entry_Tag : Wayland_XML.Entry_Tag) return String is
         --  Just because some entries in wl_output.transform start with a digit :/
         Entry_Name : constant String := Name (Entry_Tag);
      begin
         return
           (if Entry_Name in "90" | "180" | "270" then
              "Rotate_" & Entry_Name
            else
              Xml_Parser_Utils.Adaify_Name (Entry_Name));
      end Get_Name;

      function Get_Max_Child_Length (Enum_Tag : aliased Wayland_XML.Enum_Tag) return Natural is
         Result : Natural := 0;
      begin
         for Child of Wayland_XML.Entries (Enum_Tag) loop
            Result := Natural'Max (Result, Get_Name (Child.Entry_Tag.all)'Length);
         end loop;

         return Result;
      end Get_Max_Child_Length;

      procedure Generate_Code_For_Enum_Constants is
         procedure Handle_Interface
           (Interface_Tag : aliased Wayland_XML.Interface_Tag)
         is
            procedure Generate_Code (Enum_Tag : aliased Wayland_XML.Enum_Tag) is
               Enum_Type_Name : constant String := Xml_Parser_Utils.Adaify_Name
                 (Name (Interface_Tag) & "_" & Name (Enum_Tag));

               Is_Bitfield : constant Boolean :=
                  Exists_Bitfield (Enum_Tag) and then Bitfield (Enum_Tag);

               Max_Length : constant Natural := Get_Max_Child_Length (Enum_Tag);

               procedure Generate_Summary_For_Entry
                 (Entry_Tag : Wayland_XML.Entry_Tag)
               is
                  Summary_String : constant String := (if Enable_Comments and Exists_Summary (Entry_Tag) then Summary (Entry_Tag) else "");
               begin
                  if Summary_String'Length > 0 then
                     Put_Line (File, "      --  " & Summary_String);
                  end if;
               end Generate_Summary_For_Entry;

               procedure Generate_Code_For_Entry_Component
                 (Entry_Tag : Wayland_XML.Entry_Tag)
               is
                  Aligned_Name : constant String :=
                    SF.Head (Get_Name (Entry_Tag), Max_Length, ' ');

                  Value : constant Natural := Natural'Value (Value_As_String (Entry_Tag));
               begin
                  --  Ignore entry with value 0 because then all components of
                  --  the record are False
                  if Value = 0 then
                     return;
                  end if;

                  Put_Line (File, "      " & Aligned_Name & " : Boolean := False;");
                  Generate_Summary_For_Entry (Entry_Tag);
               end Generate_Code_For_Entry_Component;

               procedure Generate_Code_For_Entry
                 (Entry_Tag  : Wayland_XML.Entry_Tag;
                  Is_First   : Boolean;
                  Is_Last    : Boolean)
               is
                  Aligned_Name : constant String := Get_Name (Entry_Tag);

                  Prefix : constant String := (if Is_First then "" else "      ");
                  Suffix : constant String := (if Is_Last then ");" else ",");
               begin
                  Put_Line (File, Prefix & Aligned_Name & Suffix);
                  Generate_Summary_For_Entry (Entry_Tag);
               end Generate_Code_For_Entry;
            begin
               New_Line (File);
               if Is_Bitfield then
                  Put_Line (File, "   type " & Enum_Type_Name & " is record");
                  for Child of Wayland_XML.Entries (Enum_Tag) loop
                     Generate_Code_For_Entry_Component (Child.Entry_Tag.all);
                  end loop;
                  Put_Line (File, "   end record");
                  Put_Line (File, "     with Convention => C_Pass_By_Copy;");
               else
                  Put_Line (File, "   type " & Enum_Type_Name & " is");
                  Put (File, "     (");
                  for Child of Wayland_XML.Entries (Enum_Tag) loop
                     Generate_Code_For_Entry
                       (Child.Entry_Tag.all,
                        Wayland_XML.Entries (Enum_Tag).First_Element = Child,
                        Wayland_XML.Entries (Enum_Tag).Last_Element = Child);
                  end loop;
               end if;
            end Generate_Code;
         begin
            for Child of Children (Interface_Tag) loop
               if Child.Kind_Id = Child_Enum then
                  Generate_Code (Child.Enum_Tag.all);
               end if;
            end loop;
         end Handle_Interface;
      begin
         Iterate_Over_Interfaces (Handle_Interface'Access);
      end Generate_Code_For_Enum_Constants;

      procedure Generate_Private_Code_For_Enum_Constants is
         procedure Handle_Interface
           (Interface_Tag : aliased Wayland_XML.Interface_Tag)
         is
            procedure Generate_Code (Enum_Tag : aliased Wayland_XML.Enum_Tag) is
               Enum_Type_Name : constant String := Xml_Parser_Utils.Adaify_Name
                 (Name (Interface_Tag) & "_" & Name (Enum_Tag));

               Is_Bitfield : constant Boolean :=
                  Exists_Bitfield (Enum_Tag) and then Bitfield (Enum_Tag);

               Max_Length : constant Natural := Get_Max_Child_Length (Enum_Tag);

               procedure Generate_Code_For_Entry_Component
                 (Entry_Tag : Wayland_XML.Entry_Tag)
               is
                  Aligned_Name : constant String :=
                    SF.Head (Get_Name (Entry_Tag), Max_Length, ' ');

                  type U32 is mod 2 ** 32
                    with Size => 32;

                  Value : constant U32 := U32'Value (Value_As_String (Entry_Tag));
               begin
                  --  Ignore entry with value 0 because then all components of
                  --  the record are False
                  if Value = 0 then
                     return;
                  end if;

                  if (Value and (Value - 1)) /= 0 then
                     raise Constraint_Error with Value'Image & " is not a power of two";
                  end if;

                  declare
                     Bit : Natural := 0;
                  begin
                     while Value > 2 ** Bit loop
                        Bit := Bit + 1;
                     end loop;

                     Put_Line (File, "      " & Aligned_Name &
                       " at 0 range " & Trim (Bit'Image) & " .. " & Trim (Bit'Image) & ";");
                  end;
               end Generate_Code_For_Entry_Component;

               procedure Generate_Code_For_Entry
                 (Entry_Tag  : Wayland_XML.Entry_Tag;
                  Is_First   : Boolean;
                  Is_Last    : Boolean)
               is
                  Aligned_Name : constant String :=
                    SF.Head (Get_Name (Entry_Tag), Max_Length, ' ');

                  Prefix : constant String := (if Is_First then "" else "      ");
                  Suffix : constant String := (if Is_Last then ");" else ",");
               begin
                  Put_Line (File, Prefix & Aligned_Name & " => " &
                    Value_As_String (Entry_Tag) & Suffix);
               end Generate_Code_For_Entry;
            begin
               if Is_Bitfield then
                  Put_Line (File, "   for " & Enum_Type_Name & " use record");
                  for Child of Wayland_XML.Entries (Enum_Tag) loop
                     Generate_Code_For_Entry_Component (Child.Entry_Tag.all);
                  end loop;
                  Put_Line (File, "   end record;");
                  Put_Line (File, "   for " & Enum_Type_Name & "'Size use Unsigned_32'Size;");
               else
                  Put_Line (File, "   for " & Enum_Type_Name & " use");
                  Put (File, "     (");
                  for Child of Wayland_XML.Entries (Enum_Tag) loop
                     Generate_Code_For_Entry
                       (Child.Entry_Tag.all,
                        Wayland_XML.Entries (Enum_Tag).First_Element = Child,
                        Wayland_XML.Entries (Enum_Tag).Last_Element = Child);
                  end loop;
                  Put_Line (File, "   for " & Enum_Type_Name & "'Size use Unsigned_32'Size;");
               end if;

               New_Line (File);
            end Generate_Code;
         begin
            for Child of Children (Interface_Tag) loop
               if Child.Kind_Id = Child_Enum then
                  Generate_Code (Child.Enum_Tag.all);
               end if;
            end loop;
         end Handle_Interface;
      begin
         Iterate_Over_Interfaces (Handle_Interface'Access);
      end Generate_Private_Code_For_Enum_Constants;

      procedure Generate_Manually_Edited_Partial_Type_Declarations (Protocol_Name : String) is
         procedure Generate_Spec_Utility_Functions (Name : String) is
         begin
            Put_Line (File, "");
            Put_Line (File, "   function Get_ID (Object : " & Name & ") return Unsigned_32");
            Put_Line (File, "     with Pre => Object.Has_Proxy;");
            Put_Line (File, "");
            Put_Line (File, "   function Get_Version (Object : " & Name & ") return Unsigned_32");
            Put_Line (File, "     with Pre => Object.Has_Proxy;");
            Put_Line (File, "");
            Put_Line (File, "   function Has_Proxy (Object : " & Name & ") return Boolean;");
            Put_Line (File, "");
            Put_Line (File, "   function ""="" (Left, Right : " & Name & "'Class) return Boolean;");
         end Generate_Spec_Utility_Functions;

         procedure Generate_Spec_Destroy_Subprogram
           (Interface_Tag : aliased Wayland_XML.Interface_Tag)
         is
            Name : constant String :=
              Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Interface_Tag));

            Destructor_Name : constant String :=
              Xml_Parser_Utils.Get_Destructor (Interface_Tag);

            Request_Name : constant String :=
              Xml_Parser_Utils.Adaify_Name
                (if Destructor_Name /= "" then Destructor_Name else "Destroy");
         begin
            Put_Line (File, "");
            Put_Line (File, "   procedure " & Request_Name & " (Object : in out " & Name & ")");
            Put_Line (File, "     with Pre  => Object.Has_Proxy,");
            Put_Line (File, "          Post => not Object.Has_Proxy;");
         end Generate_Spec_Destroy_Subprogram;

         procedure Generate_Spec_User_Data_Subprogram (Name : String) is
         begin
            Put_Line (File, "");
            Put_Line (File, "   generic");
            Put_Line (File, "      type Data_Type (<>) is limited private;");
            Put_Line (File, "   package " & Name & "_User_Data is");
            Put_Line (File, "");
            Put_Line (File, "      procedure Set_Data");
            Put_Line (File, "        (Object  : " & Name & "'Class;");
            Put_Line (File, "         Subject : aliased in out Data_Type)");
            Put_Line (File, "      with Pre => Object.Has_Proxy;");
            Put_Line (File, "");
            Put_Line (File, "      function Get_Data");
            Put_Line (File, "        (Object  : " & Name & "'Class) return access Data_Type");
            Put_Line (File, "      with Pre => Object.Has_Proxy;");
            Put_Line (File, "");
            Put_Line (File, "   end " & Name & "_User_Data;");
         end Generate_Spec_User_Data_Subprogram;

         procedure Generate_Spec_Bind_Subprogram (Name : String) is
         begin
            Put_Line (File, "");
            Put_Line (File, "   procedure Bind");
            Put_Line (File, "     (Object   : in out " & Name & ";");
            Put_Line (File, "      Registry : Client.Registry'Class;");
            Put_Line (File, "      Id       : Unsigned_32;");
            Put_Line (File, "      Version  : Unsigned_32)");
            Put_Line (File, "   with Pre => not Object.Has_Proxy and Registry.Has_Proxy;");
         end Generate_Spec_Bind_Subprogram;

         procedure Generate_Prefix_Spec_Events is
         begin
            Put_Line (File, "");
            Put_Line (File, "   generic");
         end Generate_Prefix_Spec_Events;

         procedure Generate_Suffix_Spec_Events (Name : String) is
         begin
            Put_Line (File, "   package " & Name & "_Events is");
            Put_Line (File, "");
            Put_Line (File, "      procedure Subscribe");
            Put_Line (File, "        (Object : aliased in out " & Name & "'Class)");
            Put_Line (File, "      with Pre => Object.Has_Proxy;");
            Put_Line (File, "");
            Put_Line (File, "   end " & Name & "_Events;");
         end Generate_Suffix_Spec_Events;

         procedure Handle_Interface_Subprograms_Client
           (Interface_Tag : aliased Wayland_XML.Interface_Tag)
         is
            Name : constant String
              := Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Interface_Tag));
         begin
            if Name in "Compositor" | "Seat" | "Shm" | "Output" | "Data_Device_Manager" then
               Generate_Spec_Bind_Subprogram (Name);
            end if;

            if Name = "Display" then
               Put_Line (File, "");
               Put_Line (File, "   function Is_Connected (Object : Display) return Boolean renames Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Connect (Object : in out Display)");
               Put_Line (File, "     with Pre => not Object.Is_Connected;");
               Put_Line (File, "   --  Attempts connecting with the Wayland server");
               Put_Line (File, "");
               Put_Line (File, "   type Check_For_Events_Status is (Events_Need_Processing, No_Events, Error);");
               Put_Line (File, "");
               Put_Line (File, "   function Check_For_Events");
               Put_Line (File, "     (Object  : Display;");
               Put_Line (File, "      Timeout : Duration) return Check_For_Events_Status;");
               Put_Line (File, "");
               Put_Line (File, "   function Flush (Object : Display) return Optional_Result");
               Put_Line (File, "     with Pre => Object.Is_Connected;");
               Put_Line (File, "   --  Send all buffered requests to the Wayland compositor");
               Put_Line (File, "   --");
               Put_Line (File, "   --  Returns the number of bytes sent if successful.");
               Put_Line (File, "");
               Put_Line (File, "   procedure Flush (Object : Display)");
               Put_Line (File, "     with Pre => Object.Is_Connected;");
               Put_Line (File, "   --  Send all buffered requests to the Wayland compositor");
               Put_Line (File, "   --");
               Put_Line (File, "   --  Raises Program_Error if flushing failed.");
               Put_Line (File, "");
               Put_Line (File, "   function Dispatch (Object : Display) return Optional_Result");
               Put_Line (File, "     with Pre => Object.Is_Connected;");
               Put_Line (File, "   --  Process incoming events");
               Put_Line (File, "");
               Put_Line (File, "   procedure Dispatch (Object : Display)");
               Put_Line (File, "     with Pre => Object.Is_Connected;");
               Put_Line (File, "   --  Process incoming events");
               Put_Line (File, "   --");
               Put_Line (File, "   --  Raises Program_Error if dispatching failed.");
               Put_Line (File, "");
               Put_Line (File, "   function Dispatch_Pending (Object : Display) return Optional_Result");
               Put_Line (File, "     with Pre => Object.Is_Connected;");
               Put_Line (File, "   --  Dispatch events on the default queue without reading from");
               Put_Line (File, "   --  the display's file descriptor");
               Put_Line (File, "   --");
               Put_Line (File, "   --  It does not attempt to read the display's file descriptor and simply");
               Put_Line (File, "   --  returns zero if the main queue is empty, i.e., it does not block.");
               Put_Line (File, "   --");
               Put_Line (File, "   --  Returns the number of dispatched events if successful.");
               Put_Line (File, "");
               Put_Line (File, "   function Prepare_Read (Object : Display) return Call_Result_Code");
               Put_Line (File, "     with Pre => Object.Is_Connected;");
               Put_Line (File, "   --  Prepare to read events from the display's file descriptor");
               Put_Line (File, "");
               Put_Line (File, "   function Read_Events (Object : Display) return Call_Result_Code");
               Put_Line (File, "     with Pre => Object.Is_Connected;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Cancel_Read (Object : Display)");
               Put_Line (File, "     with Pre => Object.Is_Connected;");
               Put_Line (File, "   --  Cancel read intention on display's file descriptor");
               Put_Line (File, "");
               Put_Line (File, "   function Roundtrip (Object : Display) return Integer");
               Put_Line (File, "     with Pre => Object.Is_Connected;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Roundtrip (Object : Display)");
               Put_Line (File, "     with Pre => Object.Is_Connected;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Disconnect (Object : in out Display)");
               Put_Line (File, "     with Pre  => Object.Is_Connected,");
               Put_Line (File, "          Post => not Object.Is_Connected;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Get_Registry");
               Put_Line (File, "     (Object   : Display;");
               Put_Line (File, "      Registry : in out Client.Registry'Class)");
               Put_Line (File, "   with Pre => Object.Is_Connected and not Registry.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   function Sync (Object : Display) return Callback'Class");
               Put_Line (File, "     with Pre => Object.Is_Connected;");
            elsif Name = "Compositor" then
               Put_Line (File, "");
               Put_Line (File, "   procedure Create_Surface (Object  : Compositor;");
               Put_Line (File, "                             Surface : in out Client.Surface'Class)");
               Put_Line (File, "     with Pre => Object.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Create_Region (Object : Compositor;");
               Put_Line (File, "                            Region : in out Client.Region'Class)");
               Put_Line (File, "     with Pre => Object.Has_Proxy;");
            elsif Name = "Seat" then
               Put_Line (File, "");
               Put_Line (File, "   procedure Get_Pointer (Object  : Seat;");
               Put_Line (File, "                          Pointer : in out Client.Pointer'Class)");
               Put_Line (File, "     with Pre => Object.Has_Proxy and not Pointer.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Get_Keyboard (Object   : Seat;");
               Put_Line (File, "                           Keyboard : in out Client.Keyboard'Class)");
               Put_Line (File, "     with Pre => Object.Has_Proxy and not Keyboard.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Get_Touch (Object : Seat;");
               Put_Line (File, "                        Touch  : in out Client.Touch'Class)");
               Put_Line (File, "     with Pre => Object.Has_Proxy and not Touch.Has_Proxy;");
            elsif Name = "Shm_Pool" then
               Put_Line (File, "");
               Put_Line (File, "   procedure Create_Buffer (Object : Shm_Pool;");
               Put_Line (File, "                            Offset : Natural;");
               Put_Line (File, "                            Width  : Natural;");
               Put_Line (File, "                            Height : Natural;");
               Put_Line (File, "                            Stride : Natural;");
               Put_Line (File, "                            Format : Shm_Format;");
               Put_Line (File, "                            Buffer : in out Client.Buffer'Class)");
               Put_Line (File, "     with Pre => Object.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Resize (Object : Shm_Pool;");
               Put_Line (File, "                     Size   : Positive)");
               Put_Line (File, "     with Pre => Object.Has_Proxy;");
            elsif Name = "Shm" then
               Put_Line (File, "");
               Put_Line (File, "   procedure Create_Pool (Object          : Shm;");
               Put_Line (File, "                          File_Descriptor : Wayland.File_Descriptor;");
               Put_Line (File, "                          Size            : Positive;");
               Put_Line (File, "                          Pool            : in out Shm_Pool'Class);");
            elsif Name = "Data_Offer" then
               Put_Line (File, "");
               Put_Line (File, "   procedure Do_Accept (Object    : Data_Offer;");
               Put_Line (File, "                        Serial    : Unsigned_32;");
               Put_Line (File, "                        Mime_Type : String)");
               Put_Line (File, "     with Pre => Object.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Do_Not_Accept (Object : Data_Offer;");
               Put_Line (File, "                            Serial : Unsigned_32)");
               Put_Line (File, "     with Pre => Object.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Receive (Object          : Data_Offer;");
               Put_Line (File, "                      Mime_Type       : String;");
               Put_Line (File, "                      File_Descriptor : Wayland.File_Descriptor)");
               Put_Line (File, "     with Pre => Object.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Finish (Object : Data_Offer)");
               Put_Line (File, "     with Pre => Object.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_Actions (Object           : Data_Offer;");
               Put_Line (File, "                          Dnd_Actions      : Data_Device_Manager_Dnd_Action;");
               Put_Line (File, "                          Preferred_Action : Data_Device_Manager_Dnd_Action)");
               Put_Line (File, "     with Pre => Object.Has_Proxy;");
            elsif Name = "Data_Source" then
               Put_Line (File, "");
               Put_Line (File, "   procedure Offer (Object    : Data_Source;");
               Put_Line (File, "                    Mime_Type : String)");
               Put_Line (File, "     with Pre => Object.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_Actions (Object      : Data_Source;");
               Put_Line (File, "                          Dnd_Actions : Data_Device_Manager_Dnd_Action)");
               Put_Line (File, "     with Pre => Object.Has_Proxy;");
            elsif Name = "Data_Device" then
               Put_Line (File, "");
               Put_Line (File, "   procedure Start_Drag (Object : Data_Device;");
               Put_Line (File, "                         Source : Data_Source'Class;");
               Put_Line (File, "                         Origin : Surface'Class;");
               Put_Line (File, "                         Icon   : Surface'Class;");
               Put_Line (File, "                         Serial : Unsigned_32)");
               Put_Line (File, "     with Pre => Object.Has_Proxy and Source.Has_Proxy and Origin.Has_Proxy and Icon.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_Selection (Object : Data_Device;");
               Put_Line (File, "                            Source : Data_Source'Class;");
               Put_Line (File, "                            Serial : Unsigned_32)");
               Put_Line (File, "     with Pre => Object.Has_Proxy and Source.Has_Proxy;");
            elsif Name = "Data_Device_Manager" then
               Put_Line (File, "");
               Put_Line (File, "   procedure Create_Data_Source (Object : Data_Device_Manager;");
               Put_Line (File, "                                 Source : in out Data_Source'Class)");
               Put_Line (File, "     with Pre => Object.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Get_Data_Device (Object : Data_Device_Manager;");
               Put_Line (File, "                              Seat   : Client.Seat'Class;");
               Put_Line (File, "                              Device : in out Data_Device'Class)");
               Put_Line (File, "     with Pre => Object.Has_Proxy;");
            elsif Name = "Surface" then
               Put_Line (File, "");
               Put_Line (File, "   procedure Attach (Object : Surface;");
               Put_Line (File, "                     Buffer : Client.Buffer'Class;");
               Put_Line (File, "                     X, Y   : Integer)");
               Put_Line (File, "     with Pre => Object.Has_Proxy and Buffer.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Damage (Object : Surface;");
               Put_Line (File, "                     X, Y   : Integer;");
               Put_Line (File, "                     Width  : Natural;");
               Put_Line (File, "                     Height : Natural)");
               Put_Line (File, "     with Pre => Object.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   function Frame (Object : Surface) return Callback'Class");
               Put_Line (File, "     with Pre => Object.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_Opaque_Region (Object : Surface;");
               Put_Line (File, "                                Region : Client.Region'Class)");
               Put_Line (File, "     with Pre => Object.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_Input_Region (Object : Surface;");
               Put_Line (File, "                               Region : Client.Region'Class)");
               Put_Line (File, "     with Pre => Object.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Commit (Object : Surface)");
               Put_Line (File, "     with Pre => Object.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_Buffer_Transform (Object    : Surface;");
               Put_Line (File, "                                   Transform : Output_Transform)");
               Put_Line (File, "     with Pre => Object.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_Buffer_Scale (Object : Surface;");
               Put_Line (File, "                               Scale  : Positive)");
               Put_Line (File, "     with Pre => Object.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Damage_Buffer (Object : Surface;");
               Put_Line (File, "                            X, Y   : Integer;");
               Put_Line (File, "                            Width  : Natural;");
               Put_Line (File, "                            Height : Natural)");
               Put_Line (File, "     with Pre => Object.Has_Proxy;");
            elsif Name = "Pointer" then
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_Cursor (Object    : Pointer;");
               Put_Line (File, "                         Serial    : Unsigned_32;");
               Put_Line (File, "                         Surface   : Client.Surface'Class;");
               Put_Line (File, "                         Hotspot_X : Integer;");
               Put_Line (File, "                         Hotspot_Y : Integer)");
               Put_Line (File, "     with Pre => Object.Has_Proxy;");
            elsif Name = "Region" then
               Put_Line (File, "");
               Put_Line (File, "   procedure Add (Object : Region;");
               Put_Line (File, "                  X, Y   : Integer;");
               Put_Line (File, "                  Width  : Natural;");
               Put_Line (File, "                  Height : Natural)");
               Put_Line (File, "     with Pre => Object.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Subtract (Object : Region;");
               Put_Line (File, "                       X, Y   : Integer;");
               Put_Line (File, "                       Width  : Natural;");
               Put_Line (File, "                       Height : Natural)");
               Put_Line (File, "     with Pre => Object.Has_Proxy;");
            elsif Name = "Subcompositor" then
               Put_Line (File, "");
               Put_Line (File, "   procedure Get_Subsurface (Object     : Subcompositor;");
               Put_Line (File, "                             Surface    : Client.Surface'Class;");
               Put_Line (File, "                             Parent     : Client.Surface'Class;");
               Put_Line (File, "                             Subsurface : in out Client.Subsurface'Class)");
               Put_Line (File, "     with Pre => Object.Has_Proxy and Surface.Has_Proxy and Parent.Has_Proxy;");
            elsif Name = "Subsurface" then
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_Position (Object : Subsurface; X, Y : Integer)");
               Put_Line (File, "     with Pre => Object.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Place_Above (Object  : Subsurface;");
               Put_Line (File, "                          Sibling : Surface'Class)");
               Put_Line (File, "     with Pre => Object.Has_Proxy and Sibling.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Place_Below (Object  : Subsurface;");
               Put_Line (File, "                          Sibling : Surface'Class)");
               Put_Line (File, "     with Pre => Object.Has_Proxy and Sibling.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_Sync (Object : Subsurface)");
               Put_Line (File, "     with Pre => Object.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_Desync (Object : Subsurface)");
               Put_Line (File, "     with Pre => Object.Has_Proxy;");
            end if;
         end Handle_Interface_Subprograms_Client;

         procedure Handle_Interface_Events_Client
           (Interface_Tag : aliased Wayland_XML.Interface_Tag)
         is
            Name : constant String
              := Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Interface_Tag));
         begin
            if not Exists_Events (Interface_Tag) then
               return;
            end if;

            Generate_Prefix_Spec_Events;

            if Name = "Display" then
               Put_Line (File, "      with procedure Error");
               Put_Line (File, "        (Display   : in out Client.Display'Class;");
               Put_Line (File, "         Code      : Unsigned_32;");
               Put_Line (File, "         Message   : String) is null;");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Delete_Id");
               Put_Line (File, "        (Display : in out Client.Display'Class;");
               Put_Line (File, "         Id      : Unsigned_32) is null;");
            elsif Name = "Registry" then
               Put_Line (File, "      with procedure Global_Object_Added");
               Put_Line (File, "        (Registry : in out Client.Registry'Class;");
               Put_Line (File, "         Id       : Unsigned_32;");
               Put_Line (File, "         Name     : String;");
               Put_Line (File, "         Version  : Unsigned_32);");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Global_Object_Removed");
               Put_Line (File, "        (Registry : in out Client.Registry'Class;");
               Put_Line (File, "         Id       : Unsigned_32) is null;");
            elsif Name = "Callback" then
               Put_Line (File, "      with procedure Done");
               Put_Line (File, "        (Callback      : in out Client.Callback'Class;");
               Put_Line (File, "         Callback_Data : Unsigned_32);");
            elsif Name = "Shm" then
               Put_Line (File, "      with procedure Format");
               Put_Line (File, "        (Shm    : in out Client.Shm'Class;");
               Put_Line (File, "         Format : Shm_Format);");
            elsif Name = "Buffer" then
               Put_Line (File, "      with procedure Release");
               Put_Line (File, "        (Buffer : in out Client.Buffer'Class);");
            elsif Name = "Data_Offer" then
               Put_Line (File, "      with procedure Offer");
               Put_Line (File, "        (Data_Offer : in out Client.Data_Offer'Class;");
               Put_Line (File, "         Mime_Type  : String) is null;");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Source_Actions");
               Put_Line (File, "        (Data_Offer     : in out Client.Data_Offer'Class;");
               Put_Line (File, "         Source_Actions : Data_Device_Manager_Dnd_Action) is null;");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Action");
               Put_Line (File, "        (Data_Offer : in out Client.Data_Offer'Class;");
               Put_Line (File, "         Dnd_Action : Data_Device_Manager_Dnd_Action) is null;");
            elsif Name = "Data_Source" then
               Put_Line (File, "      with procedure Target");
               Put_Line (File, "        (Data_Source : in out Client.Data_Source'Class;");
               Put_Line (File, "         Mime_Type   : String) is null;");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Send");
               Put_Line (File, "        (Data_Source : in out Client.Data_Source'Class;");
               Put_Line (File, "         Mime_Type   : String;");
               Put_Line (File, "         Fd          : File_Descriptor) is null;");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Cancelled");
               Put_Line (File, "        (Data_Source : in out Client.Data_Source'Class) is null;");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Performed");
               Put_Line (File, "        (Data_Source : in out Client.Data_Source'Class) is null;");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Finished");
               Put_Line (File, "        (Data_Source : in out Client.Data_Source'Class) is null;");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Action");
               Put_Line (File, "        (Data_Source : in out Client.Data_Source'Class;");
               Put_Line (File, "         Dnd_Action  : Data_Device_Manager_Dnd_Action) is null;");
            elsif Name = "Data_Device" then
               Put_Line (File, "      with procedure Data_Offer");
               Put_Line (File, "        (Data_Device : in out Client.Data_Device'Class;");
               Put_Line (File, "         Id          : Unsigned_32) is null;");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Enter");
               Put_Line (File, "        (Data_Device : in out Client.Data_Device'Class;");
               Put_Line (File, "         Serial      : Unsigned_32;");
               Put_Line (File, "         Surface     : Client.Surface;");
               Put_Line (File, "         X, Y        : Fixed;");
               Put_Line (File, "         Id          : Client.Data_Offer) is null;");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Leave");
               Put_Line (File, "        (Data_Device : in out Client.Data_Device'Class) is null;");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Motion");
               Put_Line (File, "        (Data_Device : in out Client.Data_Device'Class;");
               Put_Line (File, "         Time        : Unsigned_32;");
               Put_Line (File, "         X, Y        : Fixed) is null;");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Drop");
               Put_Line (File, "        (Data_Device : in out Client.Data_Device'Class) is null;");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Selection");
               Put_Line (File, "        (Data_Device : in out Client.Data_Device'Class;");
               Put_Line (File, "         Id          : Client.Data_Offer) is null;");
            elsif Name = "Surface" then
               Put_Line (File, "      with procedure Enter");
               Put_Line (File, "        (Surface : in out Client.Surface'Class;");
               Put_Line (File, "         Output  : Client.Output) is null;");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Leave");
               Put_Line (File, "        (Surface : in out Client.Surface'Class;");
               Put_Line (File, "         Output  : Client.Output) is null;");
            elsif Name = "Seat" then
               Put_Line (File, "      with procedure Seat_Capabilities");
               Put_Line (File, "        (Seat         : in out Client.Seat'Class;");
               Put_Line (File, "         Capabilities : Seat_Capability) is null;");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Seat_Name");
               Put_Line (File, "        (Seat : in out Client.Seat'Class;");
               Put_Line (File, "         Name : String) is null;");
            elsif Name = "Pointer" then
               Put_Line (File, "      with procedure Pointer_Enter");
               Put_Line (File, "        (Pointer   : in out Client.Pointer'Class;");
               Put_Line (File, "         Serial    : Unsigned_32;");
               Put_Line (File, "         Surface   : Client.Surface;");
               Put_Line (File, "         Surface_X : Fixed;");
               Put_Line (File, "         Surface_Y : Fixed) is null;");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Pointer_Leave");
               Put_Line (File, "        (Pointer : in out Client.Pointer'Class;");
               Put_Line (File, "         Serial  : Unsigned_32;");
               Put_Line (File, "         Surface : Client.Surface) is null;");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Pointer_Motion");
               Put_Line (File, "        (Pointer   : in out Client.Pointer'Class;");
               Put_Line (File, "         Time      : Unsigned_32;");
               Put_Line (File, "         Surface_X : Fixed;");
               Put_Line (File, "         Surface_Y : Fixed) is null;");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Pointer_Button");
               Put_Line (File, "        (Pointer : in out Client.Pointer'Class;");
               Put_Line (File, "         Serial  : Unsigned_32;");
               Put_Line (File, "         Time    : Unsigned_32;");
               Put_Line (File, "         Button  : Unsigned_32;");
               Put_Line (File, "         State   : Pointer_Button_State) is null;");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Pointer_Scroll");
               Put_Line (File, "        (Pointer : in out Client.Pointer'Class;");
               Put_Line (File, "         Time    : Unsigned_32;");
               Put_Line (File, "         Axis    : Pointer_Axis;");
               Put_Line (File, "         Value   : Fixed) is null;");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Pointer_Frame");
               Put_Line (File, "        (Pointer : in out Client.Pointer'Class) is null;");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Pointer_Scroll_Source");
               Put_Line (File, "        (Pointer     : in out Client.Pointer'Class;");
               Put_Line (File, "         Axis_Source : Pointer_Axis_Source) is null;");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Pointer_Scroll_Stop");
               Put_Line (File, "        (Pointer : in out Client.Pointer'Class;");
               Put_Line (File, "         Time    : Unsigned_32;");
               Put_Line (File, "         Axis    : Pointer_Axis) is null;");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Pointer_Scroll_Discrete");
               Put_Line (File, "        (Pointer  : in out Client.Pointer'Class;");
               Put_Line (File, "         Axis     : Pointer_Axis;");
               Put_Line (File, "         Discrete : Integer) is null;");
            elsif Name = "Keyboard" then
               Put_Line (File, "      with procedure Keymap");
               Put_Line (File, "        (Keyboard : in out Client.Keyboard'Class;");
               Put_Line (File, "         Format   : Keyboard_Keymap_Format;");
               Put_Line (File, "         Fd       : File_Descriptor;");
               Put_Line (File, "         Size     : Unsigned_32) is null;");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Enter");
               Put_Line (File, "        (Keyboard : in out Client.Keyboard'Class;");
               Put_Line (File, "         Serial   : Unsigned_32;");
               Put_Line (File, "         Surface  : Client.Surface;");
               Put_Line (File, "         Keys     : Unsigned_32_Array) is null;");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Leave");
               Put_Line (File, "        (Keyboard : in out Client.Keyboard'Class;");
               Put_Line (File, "         Serial   : Unsigned_32;");
               Put_Line (File, "         Surface  : Client.Surface) is null;");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Key");
               Put_Line (File, "        (Keyboard : in out Client.Keyboard'Class;");
               Put_Line (File, "         Serial   : Unsigned_32;");
               Put_Line (File, "         Time     : Unsigned_32;");
               Put_Line (File, "         Key      : Unsigned_32;");
               Put_Line (File, "         State    : Keyboard_Key_State) is null;");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Modifiers");
               Put_Line (File, "        (Keyboard       : in out Client.Keyboard'Class;");
               Put_Line (File, "         Serial         : Unsigned_32;");
               Put_Line (File, "         Mods_Depressed : Unsigned_32;");
               Put_Line (File, "         Mods_Latched   : Unsigned_32;");
               Put_Line (File, "         Mods_Locked    : Unsigned_32;");
               Put_Line (File, "         Group          : Unsigned_32) is null;");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Repeat_Info");
               Put_Line (File, "        (Keyboard : in out Client.Keyboard'Class;");
               Put_Line (File, "         Rate     : Integer;");
               Put_Line (File, "         Delay_V  : Integer) is null;");
            elsif Name = "Touch" then
               Put_Line (File, "      with procedure Down");
               Put_Line (File, "        (Touch   : in out Client.Touch'Class;");
               Put_Line (File, "         Serial  : Unsigned_32;");
               Put_Line (File, "         Time    : Unsigned_32;");
               Put_Line (File, "         Surface : Client.Surface;");
               Put_Line (File, "         Id      : Integer;");
               Put_Line (File, "         X, Y    : Fixed) is null;");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Up");
               Put_Line (File, "        (Touch  : in out Client.Touch'Class;");
               Put_Line (File, "         Serial : Unsigned_32;");
               Put_Line (File, "         Time   : Unsigned_32;");
               Put_Line (File, "         Id     : Integer) is null;");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Motion");
               Put_Line (File, "        (Touch : in out Client.Touch'Class;");
               Put_Line (File, "         Time  : Unsigned_32;");
               Put_Line (File, "         Id    : Integer;");
               Put_Line (File, "         X, Y  : Fixed) is null;");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Frame");
               Put_Line (File, "        (Touch : in out Client.Touch'Class) is null;");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Cancel");
               Put_Line (File, "        (Touch : in out Client.Touch'Class) is null;");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Shape");
               Put_Line (File, "        (Touch : in out Client.Touch'Class;");
               Put_Line (File, "         Id    : Integer;");
               Put_Line (File, "         Major : Fixed;");
               Put_Line (File, "         Minor : Fixed) is null;");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Orientation");
               Put_Line (File, "        (Touch       : in out Client.Touch'Class;");
               Put_Line (File, "         Id          : Integer;");
               Put_Line (File, "         Orientation : Fixed) is null;");
            elsif Name = "Output" then
               Put_Line (File, "      with procedure Geometry");
               Put_Line (File, "        (Output          : in out Client.Output'Class;");
               Put_Line (File, "         X, Y            : Integer;");
               Put_Line (File, "         Physical_Width  : Integer;");
               Put_Line (File, "         Physical_Height : Integer;");
               Put_Line (File, "         Subpixel        : Output_Subpixel;");
               Put_Line (File, "         Make            : String;");
               Put_Line (File, "         Model           : String;");
               Put_Line (File, "         Transform       : Output_Transform) is null;");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Mode");
               Put_Line (File, "        (Output  : in out Client.Output'Class;");
               Put_Line (File, "         Flags   : Output_Mode;");
               Put_Line (File, "         Width   : Integer;");
               Put_Line (File, "         Height  : Integer;");
               Put_Line (File, "         Refresh : Integer) is null;");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Done");
               Put_Line (File, "        (Output : in out Client.Output'Class) is null;");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Scale");
               Put_Line (File, "        (Output : in out Client.Output'Class;");
               Put_Line (File, "         Factor : Integer) is null;");
            end if;

            Generate_Suffix_Spec_Events (Name);
         end Handle_Interface_Events_Client;

         procedure Handle_Interface_Subprograms_Xdg_Shell
           (Interface_Tag : aliased Wayland_XML.Interface_Tag)
         is
            Name : constant String
              := Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Interface_Tag));
         begin
            if Name in "Xdg_Wm_Base" then
               Generate_Spec_Bind_Subprogram (Name);
            end if;

            if Name = "Xdg_Wm_Base" then
               Put_Line (File, "");
               Put_Line (File, "   procedure Pong (Object : Xdg_Wm_Base; Serial : Unsigned_32)");
               Put_Line (File, "     with Pre => Object.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Create_Positioner");
               Put_Line (File, "     (Object     : Xdg_Wm_Base;");
               Put_Line (File, "      Positioner : in out Xdg_Shell.Xdg_Positioner'Class)");
               Put_Line (File, "   with Pre => Object.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Get_Surface");
               Put_Line (File, "     (Object  : Xdg_Wm_Base;");
               Put_Line (File, "      Window  : Protocols.Client.Surface'Class;");
               Put_Line (File, "      Surface : in out Xdg_Shell.Xdg_Surface'Class)");
               Put_Line (File, "   with Pre => Object.Has_Proxy and Window.Has_Proxy;");
            elsif Name = "Xdg_Positioner" then
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_Size");
               Put_Line (File, "     (Object : Xdg_Positioner;");
               Put_Line (File, "      Width  : Natural;");
               Put_Line (File, "      Height : Natural)");
               Put_Line (File, "   with Pre => Object.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_Anchor_Rect");
               Put_Line (File, "     (Object : Xdg_Positioner;");
               Put_Line (File, "      X, Y   : Integer;");
               Put_Line (File, "      Width  : Natural;");
               Put_Line (File, "      Height : Natural)");
               Put_Line (File, "   with Pre => Object.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_Anchor");
               Put_Line (File, "     (Object : Xdg_Positioner;");
               Put_Line (File, "      Anchor : Xdg_Positioner_Anchor)");
               Put_Line (File, "   with Pre => Object.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_Gravity");
               Put_Line (File, "     (Object  : Xdg_Positioner;");
               Put_Line (File, "      Gravity : Xdg_Positioner_Gravity)");
               Put_Line (File, "   with Pre => Object.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_Constraint_Adjustment");
               Put_Line (File, "     (Object     : Xdg_Positioner;");
               Put_Line (File, "      Adjustment : Unsigned_32)");
               Put_Line (File, "   with Pre => Object.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_Offset");
               Put_Line (File, "     (Object : Xdg_Positioner;");
               Put_Line (File, "      X, Y   : Integer)");
               Put_Line (File, "   with Pre => Object.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_Reactive (Object : Xdg_Positioner)");
               Put_Line (File, "     with Pre => Object.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_Parent_Size");
               Put_Line (File, "     (Object : Xdg_Positioner;");
               Put_Line (File, "      Width  : Natural;");
               Put_Line (File, "      Height : Natural)");
               Put_Line (File, "   with Pre => Object.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_Parent_Configure");
               Put_Line (File, "     (Object : Xdg_Positioner;");
               Put_Line (File, "      Serial : Unsigned_32)");
               Put_Line (File, "   with Pre => Object.Has_Proxy;");
            elsif Name = "Xdg_Surface" then
               Put_Line (File, "");
               Put_Line (File, "   procedure Get_Toplevel");
               Put_Line (File, "     (Object   : Xdg_Surface;");
               Put_Line (File, "      Toplevel : in out Xdg_Shell.Xdg_Toplevel'Class)");
               Put_Line (File, "   with Pre => Object.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Get_Popup");
               Put_Line (File, "     (Object     : Xdg_Surface;");
               Put_Line (File, "      Parent     : Xdg_Surface'Class;");
               Put_Line (File, "      Positioner : Xdg_Shell.Xdg_Positioner'Class;");
               Put_Line (File, "      Popup      : in out Xdg_Shell.Xdg_Popup'Class)");
               Put_Line (File, "   with Pre => Object.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_Window_Geometry");
               Put_Line (File, "     (Object : Xdg_Surface;");
               Put_Line (File, "      X, Y   : Integer;");
               Put_Line (File, "      Width  : Natural;");
               Put_Line (File, "      Height : Natural)");
               Put_Line (File, "   with Pre => Object.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Ack_Configure");
               Put_Line (File, "     (Object : Xdg_Surface;");
               Put_Line (File, "      Serial : Unsigned_32)");
               Put_Line (File, "   with Pre => Object.Has_Proxy;");
            elsif Name = "Xdg_Toplevel" then
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_Parent");
               Put_Line (File, "     (Object : Xdg_Toplevel;");
               Put_Line (File, "      Parent : Xdg_Toplevel'Class)");
               Put_Line (File, "   with Pre => Object.Has_Proxy and Parent.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_Title (Object : Xdg_Toplevel; Value : String)");
               Put_Line (File, "     with Pre => Object.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_App_Id (Object : Xdg_Toplevel; Value : String)");
               Put_Line (File, "     with Pre => Object.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Show_Window_Menu");
               Put_Line (File, "     (Object : Xdg_Toplevel;");
               Put_Line (File, "      Seat   : Protocols.Client.Seat'Class;");
               Put_Line (File, "      Serial : Unsigned_32;");
               Put_Line (File, "      X, Y   : Integer)");
               Put_Line (File, "   with Pre => Object.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Move");
               Put_Line (File, "     (Object : Xdg_Toplevel;");
               Put_Line (File, "      Seat   : Protocols.Client.Seat'Class;");
               Put_Line (File, "      Serial : Unsigned_32)");
               Put_Line (File, "   with Pre => Object.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Resize");
               Put_Line (File, "     (Object : Xdg_Toplevel;");
               Put_Line (File, "      Seat   : Protocols.Client.Seat'Class;");
               Put_Line (File, "      Serial : Unsigned_32;");
               Put_Line (File, "      Edges  : Xdg_Toplevel_Resize_Edge)");
               Put_Line (File, "   with Pre => Object.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_Min_Size");
               Put_Line (File, "     (Object : Xdg_Toplevel;");
               Put_Line (File, "      Width  : Natural;");
               Put_Line (File, "      Height : Natural)");
               Put_Line (File, "   with Pre => Object.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_Max_Size");
               Put_Line (File, "     (Object : Xdg_Toplevel;");
               Put_Line (File, "      Width  : Natural;");
               Put_Line (File, "      Height : Natural)");
               Put_Line (File, "   with Pre => Object.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Minimize (Object : Xdg_Toplevel)");
               Put_Line (File, "     with Pre => Object.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_Maximized (Object : Xdg_Toplevel; Enable : Boolean)");
               Put_Line (File, "     with Pre => Object.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_Fullscreen");
               Put_Line (File, "     (Object : Xdg_Toplevel;");
               Put_Line (File, "      Enable : Boolean)");
               Put_Line (File, "   with Pre => Object.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_Fullscreen");
               Put_Line (File, "     (Object : Xdg_Toplevel;");
               Put_Line (File, "      Enable : Boolean;");
               Put_Line (File, "      Output : Protocols.Client.Output'Class)");
               Put_Line (File, "   with Pre => Object.Has_Proxy and Output.Has_Proxy;");
            elsif Name = "Xdg_Popup" then
               Put_Line (File, "");
               Put_Line (File, "   procedure Grab");
               Put_Line (File, "     (Object : Xdg_Popup;");
               Put_Line (File, "      Seat   : Protocols.Client.Seat'Class;");
               Put_Line (File, "      Serial : Unsigned_32)");
               Put_Line (File, "   with Pre => Object.Has_Proxy and Seat.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Reposition");
               Put_Line (File, "     (Object     : Xdg_Popup;");
               Put_Line (File, "      Positioner : Xdg_Shell.Xdg_Positioner'Class;");
               Put_Line (File, "      Token      : Unsigned_32)");
               Put_Line (File, "   with Pre => Object.Has_Proxy and Positioner.Has_Proxy;");
            end if;
         end Handle_Interface_Subprograms_Xdg_Shell;

         procedure Handle_Interface_Events_Xdg_Shell
           (Interface_Tag : aliased Wayland_XML.Interface_Tag)
         is
            Name : constant String
              := Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Interface_Tag));
         begin
            if not Exists_Events (Interface_Tag) then
               return;
            end if;

            Generate_Prefix_Spec_Events;

            if Name = "Xdg_Wm_Base" then
               Put_Line (File, "      with procedure Ping");
               Put_Line (File, "        (Xdg_Wm_Base : in out Xdg_Shell.Xdg_Wm_Base'Class;");
               Put_Line (File, "         Serial      : Unsigned_32);");
            elsif Name = "Xdg_Surface" then
               Put_Line (File, "      with procedure Configure");
               Put_Line (File, "        (Xdg_Surface : in out Xdg_Shell.Xdg_Surface'Class;");
               Put_Line (File, "         Serial      : Unsigned_32);");
            elsif Name = "Xdg_Toplevel" then
               Put_Line (File, "      with procedure Configure");
               Put_Line (File, "        (Xdg_Toplevel : in out Xdg_Shell.Xdg_Toplevel'Class;");
               Put_Line (File, "         Width        : Natural;");
               Put_Line (File, "         Height       : Natural;");
               Put_Line (File, "         States       : State_Array);");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Close");
               Put_Line (File, "        (Xdg_Toplevel : in out Xdg_Shell.Xdg_Toplevel'Class) is null;");
            elsif Name = "Xdg_Popup" then
               Put_Line (File, "      with procedure Configure");
               Put_Line (File, "        (Xdg_Popup : in out Xdg_Shell.Xdg_Popup'Class;");
               Put_Line (File, "         X, Y      : Integer;");
               Put_Line (File, "         Width     : Natural;");
               Put_Line (File, "         Height    : Natural);");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Popup_Done");
               Put_Line (File, "        (Xdg_Popup : in out Xdg_Shell.Xdg_Popup'Class) is null;");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Repositioned");
               Put_Line (File, "        (Xdg_Popup : in out Xdg_Shell.Xdg_Popup'Class;");
               Put_Line (File, "         Token     : Unsigned_32) is null;");
            end if;

            Generate_Suffix_Spec_Events (Name);
         end Handle_Interface_Events_Xdg_Shell;

         procedure Handle_Interface_Subprograms_Presentation_Time
           (Interface_Tag : aliased Wayland_XML.Interface_Tag)
         is
            Name : constant String
              := Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Interface_Tag));
         begin
            if Name in "Presentation" then
               Generate_Spec_Bind_Subprogram (Name);
            end if;

            if Name = "Presentation" then
               Put_Line (File, "");
               Put_Line (File, "   procedure Feedback");
               Put_Line (File, "     (Object   : Presentation;");
               Put_Line (File, "      Surface  : Protocols.Client.Surface'Class;");
               Put_Line (File, "      Feedback : in out Presentation_Feedback'Class)");
               Put_Line (File, "   with Pre => Object.Has_Proxy and Surface.Has_Proxy;");
            end if;
         end Handle_Interface_Subprograms_Presentation_Time;

         procedure Handle_Interface_Events_Presentation_Time
           (Interface_Tag : aliased Wayland_XML.Interface_Tag)
         is
            Name : constant String
              := Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Interface_Tag));
         begin
            if not Exists_Events (Interface_Tag) then
               return;
            end if;

            Generate_Prefix_Spec_Events;

            if Name = "Presentation" then
               Put_Line (File, "      with procedure Clock");
               Put_Line (File, "        (Presentation : in out Presentation_Time.Presentation'Class;");
               Put_Line (File, "         Id           : Unsigned_32);");
            elsif Name = "Presentation_Feedback" then
               Put_Line (File, "      with procedure Synchronized_Output");
               Put_Line (File, "        (Presentation_Feedback : in out Presentation_Time.Presentation_Feedback'Class;");
               Put_Line (File, "         Output                : Protocols.Client.Output'Class);");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Presented");
               Put_Line (File, "        (Presentation_Feedback : in out Presentation_Time.Presentation_Feedback'Class;");
               Put_Line (File, "         Timestamp             : Duration;");
               Put_Line (File, "         Refresh               : Duration;");
               Put_Line (File, "         Counter               : Unsigned_64;");
               Put_Line (File, "         Flags                 : Enums.Presentation_Time.Presentation_Feedback_Kind);");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Discarded");
               Put_Line (File, "        (Presentation_Feedback : in out Presentation_Time.Presentation_Feedback'Class);");
            end if;

            Generate_Suffix_Spec_Events (Name);
         end Handle_Interface_Events_Presentation_Time;

         procedure Handle_Interface_Subprograms_Viewporter
           (Interface_Tag : aliased Wayland_XML.Interface_Tag)
         is
            Name : constant String
              := Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Interface_Tag));
         begin
            if Name in "Viewporter" then
               Generate_Spec_Bind_Subprogram (Name);
            end if;

            if Name = "Viewporter" then
               Put_Line (File, "");
               Put_Line (File, "   procedure Get_Viewport");
               Put_Line (File, "     (Object   : Viewporter;");
               Put_Line (File, "      Surface  : Client.Surface'Class;");
               Put_Line (File, "      Viewport : in out Protocols.Viewporter.Viewport'Class)");
               Put_Line (File, "   with Pre => Object.Has_Proxy and Surface.Has_Proxy;");
            elsif Name = "Viewport" then
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_Source");
               Put_Line (File, "     (Object              : Viewport;");
               Put_Line (File, "      X, Y, Width, Height : Fixed)");
               Put_Line (File, "   with Pre => (X = -1.0 and Y = -1.0 and Width = -1.0 and Height = -1.0)");
               Put_Line (File, "                 or (Width > 0.0 and Height > 0.0 and X >= 0.0 and Y >= 0.0);");
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_Destination");
               Put_Line (File, "     (Object        : Viewport;");
               Put_Line (File, "      Width, Height : Integer)");
               Put_Line (File, "   with Pre => (Width = -1 and Height = -1) or (Width > 0 and Height > 0);");
            end if;
         end Handle_Interface_Subprograms_Viewporter;

         procedure Handle_Interface_Subprogram_Idle_Inhibit
           (Interface_Tag : aliased Wayland_XML.Interface_Tag)
         is
            Name : constant String
              := Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Interface_Tag));
         begin
            if Name in "Idle_Inhibit_Manager_V1" then
               Generate_Spec_Bind_Subprogram (Name);
            end if;

            if Name = "Idle_Inhibit_Manager_V1" then
               Put_Line (File, "");
               Put_Line (File, "   procedure Create_Inhibitor");
               Put_Line (File, "     (Object    : Idle_Inhibit_Manager_V1;");
               Put_Line (File, "      Surface   : Client.Surface'Class;");
               Put_Line (File, "      Inhibitor : in out Idle_Inhibitor_V1'Class)");
               Put_Line (File, "   with Pre => Object.Has_Proxy and Surface.Has_Proxy;");
            elsif Name = "Idle_Inhibitor_V1" then
               null;
            end if;
         end Handle_Interface_Subprogram_Idle_Inhibit;

         procedure Handle_Interface_Subprogram_Xdg_Decoration
           (Interface_Tag : aliased Wayland_XML.Interface_Tag)
         is
            Name : constant String
              := Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Interface_Tag));
         begin
            if Name in "Decoration_Manager_V1" then
               Generate_Spec_Bind_Subprogram (Name);
            end if;

            if Name = "Decoration_Manager_V1" then
               Put_Line (File, "");
               Put_Line (File, "   procedure Get_Toplevel_Decoration");
               Put_Line (File, "     (Object     : Decoration_Manager_V1;");
               Put_Line (File, "      Toplevel   : Xdg_Shell.Xdg_Toplevel'Class;");
               Put_Line (File, "      Decoration : in out Toplevel_Decoration_V1'Class)");
               Put_Line (File, "   with Pre => Object.Has_Proxy and Toplevel.Has_Proxy;");
            elsif Name = "Toplevel_Decoration_V1" then
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_Mode");
               Put_Line (File, "     (Object : Toplevel_Decoration_V1;");
               Put_Line (File, "      Mode   : Toplevel_Decoration_V1_Mode)");
               Put_Line (File, "   with Pre => Object.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Unset_Mode (Object : Toplevel_Decoration_V1)");
               Put_Line (File, "     with Pre => Object.Has_Proxy;");
            end if;
         end Handle_Interface_Subprogram_Xdg_Decoration;

         procedure Handle_Interface_Events_Xdg_Decoration
           (Interface_Tag : aliased Wayland_XML.Interface_Tag)
         is
            Name : constant String
              := Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Interface_Tag));
         begin
            if not Exists_Events (Interface_Tag) then
               return;
            end if;

            Generate_Prefix_Spec_Events;

            if Name = "Toplevel_Decoration_V1" then
               Put_Line (File, "      with procedure Configure");
               Put_Line (File, "        (Decoration : in out Toplevel_Decoration_V1'Class;");
               Put_Line (File, "         Mode       : Toplevel_Decoration_V1_Mode);");
            end if;

            Generate_Suffix_Spec_Events (Name);
         end Handle_Interface_Events_Xdg_Decoration;

         procedure Handle_Interface_Subprogram_Relative_Pointer
           (Interface_Tag : aliased Wayland_XML.Interface_Tag)
         is
            Name : constant String
              := Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Interface_Tag));
         begin
            if Name in "Relative_Pointer_Manager_V1" then
               Generate_Spec_Bind_Subprogram (Name);
            end if;

            if Name = "Relative_Pointer_Manager_V1" then
               Put_Line (File, "");
               Put_Line (File, "   procedure Get_Relative_Pointer");
               Put_Line (File, "     (Object   : Relative_Pointer_Manager_V1;");
               Put_Line (File, "      Pointer  : Client.Pointer'Class;");
               Put_Line (File, "      Relative : in out Relative_Pointer_V1'Class)");
               Put_Line (File, "   with Pre => Object.Has_Proxy and Pointer.Has_Proxy;");
            elsif Name = "Relative_Pointer_V1" then
               null;
            end if;
         end Handle_Interface_Subprogram_Relative_Pointer;

         procedure Handle_Interface_Events_Relative_Pointer
           (Interface_Tag : aliased Wayland_XML.Interface_Tag)
         is
            Name : constant String
              := Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Interface_Tag));
         begin
            if not Exists_Events (Interface_Tag) then
               return;
            end if;

            Generate_Prefix_Spec_Events;

            if Name = "Relative_Pointer_V1" then
               Put_Line (File, "      with procedure Relative_Motion");
               Put_Line (File, "        (Pointer    : in out Relative_Pointer_V1'Class;");
               Put_Line (File, "         Timestamp  : Duration;");
               Put_Line (File, "         Dx         : Fixed;");
               Put_Line (File, "         Dy         : Fixed;");
               Put_Line (File, "         Dx_Unaccel : Fixed;");
               Put_Line (File, "         Dy_Unaccel : Fixed);");
            end if;

            Generate_Suffix_Spec_Events (Name);
         end Handle_Interface_Events_Relative_Pointer;

         procedure Handle_Interface_Subprogram_Pointer_Constraints
           (Interface_Tag : aliased Wayland_XML.Interface_Tag)
         is
            Name : constant String
              := Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Interface_Tag));
         begin
            if Name in "Pointer_Constraints_V1" then
               Generate_Spec_Bind_Subprogram (Name);
            end if;

            if Name = "Pointer_Constraints_V1" then
               Put_Line (File, "");
               Put_Line (File, "   procedure Lock_Pointer");
               Put_Line (File, "     (Object   : Pointer_Constraints_V1;");
               Put_Line (File, "      Surface  : Client.Surface'Class;");
               Put_Line (File, "      Pointer  : Client.Pointer'Class;");
               Put_Line (File, "      Region   : Client.Region'Class;");
               Put_Line (File, "      Lifetime : Pointer_Constraints_V1_Lifetime;");
               Put_Line (File, "      Locked   : in out Locked_Pointer_V1'Class)");
               Put_Line (File, "   with Pre => Object.Has_Proxy and Surface.Has_Proxy and Pointer.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Confine_Pointer");
               Put_Line (File, "     (Object   : Pointer_Constraints_V1;");
               Put_Line (File, "      Surface  : Client.Surface'Class;");
               Put_Line (File, "      Pointer  : Client.Pointer'Class;");
               Put_Line (File, "      Region   : Client.Region'Class;");
               Put_Line (File, "      Lifetime : Pointer_Constraints_V1_Lifetime;");
               Put_Line (File, "      Confined : in out Confined_Pointer_V1'Class)");
               Put_Line (File, "   with Pre => Object.Has_Proxy and Surface.Has_Proxy and Pointer.Has_Proxy;");
            elsif Name = "Locked_Pointer_V1" then
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_Cursor_Position_Hint");
               Put_Line (File, "     (Object : Locked_Pointer_V1;");
               Put_Line (File, "      X, Y   : Fixed)");
               Put_Line (File, "   with Pre => Object.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_Region");
               Put_Line (File, "     (Object : Locked_Pointer_V1;");
               Put_Line (File, "      Region : Client.Region'Class)");
               Put_Line (File, "   with Pre => Object.Has_Proxy and Region.Has_Proxy;");
            elsif Name = "Confined_Pointer_V1" then
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_Region");
               Put_Line (File, "     (Object : Confined_Pointer_V1;");
               Put_Line (File, "      Region : Client.Region'Class)");
               Put_Line (File, "   with Pre => Object.Has_Proxy and Region.Has_Proxy;");
            end if;
         end Handle_Interface_Subprogram_Pointer_Constraints;

         procedure Handle_Interface_Events_Pointer_Constraints
           (Interface_Tag : aliased Wayland_XML.Interface_Tag)
         is
            Name : constant String
              := Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Interface_Tag));
         begin
            if not Exists_Events (Interface_Tag) then
               return;
            end if;

            Generate_Prefix_Spec_Events;

            if Name = "Locked_Pointer_V1" then
               Put_Line (File, "      with procedure Locked");
               Put_Line (File, "        (Pointer : in out Locked_Pointer_V1'Class);");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Unlocked");
               Put_Line (File, "        (Pointer : in out Locked_Pointer_V1'Class);");
            elsif Name = "Confined_Pointer_V1" then
               Put_Line (File, "      with procedure Confined");
               Put_Line (File, "        (Pointer : in out Confined_Pointer_V1'Class);");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Unconfined");
               Put_Line (File, "        (Pointer : in out Confined_Pointer_V1'Class);");
            end if;

            Generate_Suffix_Spec_Events (Name);
         end Handle_Interface_Events_Pointer_Constraints;

         procedure Handle_Interface_Subprogram_Pointer_Gestures
           (Interface_Tag : aliased Wayland_XML.Interface_Tag)
         is
            Name : constant String
              := Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Interface_Tag));
         begin
            if Name in "Pointer_Gestures_V1" then
               Generate_Spec_Bind_Subprogram (Name);
            end if;

            if Name in "Pointer_Gestures_V1" then
               Put_Line (File, "");
               Put_Line (File, "   procedure Get_Swipe_Gesture");
               Put_Line (File, "     (Object  : Pointer_Gestures_V1;");
               Put_Line (File, "      Pointer : Client.Pointer'Class;");
               Put_Line (File, "      Gesture : in out Pointer_Gesture_Swipe_V1'Class)");
               Put_Line (File, "   with Pre => Object.Has_Proxy and Pointer.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Get_Pinch_Gesture");
               Put_Line (File, "     (Object  : Pointer_Gestures_V1;");
               Put_Line (File, "      Pointer : Client.Pointer'Class;");
               Put_Line (File, "      Gesture : in out Pointer_Gesture_Pinch_V1'Class)");
               Put_Line (File, "   with Pre => Object.Has_Proxy and Pointer.Has_Proxy;");
            end if;
         end Handle_Interface_Subprogram_Pointer_Gestures;

         procedure Handle_Interface_Events_Pointer_Gestures
           (Interface_Tag : aliased Wayland_XML.Interface_Tag)
         is
            Name : constant String
              := Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Interface_Tag));
         begin
            if not Exists_Events (Interface_Tag) then
               return;
            end if;

            Generate_Prefix_Spec_Events;

            if Name = "Pointer_Gesture_Swipe_V1" then
               Put_Line (File, "      with procedure Gesture_Begin");
               Put_Line (File, "        (Gesture    : in out Pointer_Gesture_Swipe_V1'Class;");
               Put_Line (File, "         Serial     : Unsigned_32;");
               Put_Line (File, "         Timestamp  : Duration;");
               Put_Line (File, "         Surface    : Client.Surface;");
               Put_Line (File, "         Fingers    : Unsigned_32);");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Gesture_Update");
               Put_Line (File, "        (Gesture   : in out Pointer_Gesture_Swipe_V1'Class;");
               Put_Line (File, "         Timestamp : Duration;");
               Put_Line (File, "         Dx, Dy    : Fixed);");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Gesture_End");
               Put_Line (File, "        (Gesture : in out Pointer_Gesture_Swipe_V1'Class;");
               Put_Line (File, "         Serial    : Unsigned_32;");
               Put_Line (File, "         Timestamp : Duration;");
               Put_Line (File, "         Cancelled : Boolean);");
            elsif Name = "Pointer_Gesture_Pinch_V1" then
               Put_Line (File, "      with procedure Gesture_Begin");
               Put_Line (File, "        (Gesture : in out Pointer_Gesture_Pinch_V1'Class;");
               Put_Line (File, "         Serial     : Unsigned_32;");
               Put_Line (File, "         Timestamp  : Duration;");
               Put_Line (File, "         Surface    : Client.Surface;");
               Put_Line (File, "         Fingers    : Unsigned_32);");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Gesture_Update");
               Put_Line (File, "        (Gesture   : in out Pointer_Gesture_Pinch_V1'Class;");
               Put_Line (File, "         Timestamp : Duration;");
               Put_Line (File, "         Dx, Dy    : Fixed;");
               Put_Line (File, "         Scale     : Fixed;");
               Put_Line (File, "         Rotation  : Fixed);");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Gesture_End");
               Put_Line (File, "        (Gesture   : in out Pointer_Gesture_Pinch_V1'Class;");
               Put_Line (File, "         Serial    : Unsigned_32;");
               Put_Line (File, "         Timestamp : Duration;");
               Put_Line (File, "         Cancelled : Boolean);");
            end if;

            Generate_Suffix_Spec_Events (Name);
         end Handle_Interface_Events_Pointer_Gestures;

         procedure Handle_Interface_Common_Subprograms
           (Interface_Tag : aliased Wayland_XML.Interface_Tag)
         is
            Name : constant String
              := Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Interface_Tag));
         begin
            if Protocol_Name /= "client" or Name /= "Display" then
               Generate_Spec_Destroy_Subprogram (Interface_Tag);
            end if;
            Generate_Spec_Utility_Functions (Name);

            if Protocol_Name /= "client" or Name /= "Display" then
               Generate_Spec_User_Data_Subprogram (Name);
            end if;
         end Handle_Interface_Common_Subprograms;

         procedure Handle_Interface_Common_Events
           (Interface_Tag : aliased Wayland_XML.Interface_Tag)
         is
            Name : constant String
              := Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Interface_Tag));
         begin
            if not Exists_Events (Interface_Tag) then
               return;
            end if;

            Generate_Prefix_Spec_Events;
            Put_Line (File, "      --  FIXME Add generic `with procedure`'s here");
            Generate_Suffix_Spec_Events (Name);
         end Handle_Interface_Common_Events;
      begin
         Iterate_Over_Interfaces (Handle_Interface_Common_Subprograms'Access);

         if Protocol_Name = "client" then
            Iterate_Over_Interfaces (Handle_Interface_Subprograms_Client'Access);
            Iterate_Over_Interfaces (Handle_Interface_Events_Client'Access);
         elsif Protocol_Name = "xdg_shell" then
            Iterate_Over_Interfaces (Handle_Interface_Subprograms_Xdg_Shell'Access);
            Iterate_Over_Interfaces (Handle_Interface_Events_Xdg_Shell'Access);
         elsif Protocol_Name = "presentation_time" then
            Iterate_Over_Interfaces (Handle_Interface_Subprograms_Presentation_Time'Access);
            Iterate_Over_Interfaces (Handle_Interface_Events_Presentation_Time'Access);
         elsif Protocol_Name = "viewporter" then
            Iterate_Over_Interfaces (Handle_Interface_Subprograms_Viewporter'Access);
         elsif Protocol_Name = "idle_inhibit_unstable_v1" then
            Iterate_Over_Interfaces (Handle_Interface_Subprogram_Idle_Inhibit'Access);
         elsif Protocol_Name = "xdg_decoration_unstable_v1" then
            Iterate_Over_Interfaces (Handle_Interface_Subprogram_Xdg_Decoration'Access);
            Iterate_Over_Interfaces (Handle_Interface_Events_Xdg_Decoration'Access);
         elsif Protocol_Name = "relative_pointer_unstable_v1" then
            Iterate_Over_Interfaces (Handle_Interface_Subprogram_Relative_Pointer'Access);
            Iterate_Over_Interfaces (Handle_Interface_Events_Relative_Pointer'Access);
         elsif Protocol_Name = "pointer_constraints_unstable_v1" then
            Iterate_Over_Interfaces (Handle_Interface_Subprogram_Pointer_Constraints'Access);
            Iterate_Over_Interfaces (Handle_Interface_Events_Pointer_Constraints'Access);
         elsif Protocol_Name = "pointer_gestures_unstable_v1" then
            Iterate_Over_Interfaces (Handle_Interface_Subprogram_Pointer_Gestures'Access);
            Iterate_Over_Interfaces (Handle_Interface_Events_Pointer_Gestures'Access);
         else
            Iterate_Over_Interfaces (Handle_Interface_Common_Events'Access);
         end if;
      end Generate_Manually_Edited_Partial_Type_Declarations;

      procedure Create_Wl_Thin_Spec_File (Protocol_Name : String) is

         procedure Generate_Code_For_Interface_Constants is
            procedure Handle_Interface (Interface_Tag : aliased Wayland_XML.Interface_Tag) is
               Name : constant String
                 := Xml_Parser_Utils.Adaify_Name
                   (Wayland_XML.Name (Interface_Tag) & "_Interface");
            begin
               if Protocol_Name = "client" then
                  Put_Line (File, "   " & Name & " : aliased constant Interface_T");
                  Put_Line (File, "     with Import, Convention => C, External_Name => """ & Wayland_XML.Name (Interface_Tag) & "_interface"";");
                  New_Line (File);
               else
                  Put_Line (File, "   " & Name & " : aliased constant Interface_T;");
               end if;
            end Handle_Interface;
         begin
            Iterate_Over_Interfaces (Handle_Interface'Access);
         end Generate_Code_For_Interface_Constants;

         procedure Generate_Private_Code_For_Interface_Constants is
            type Partial_Match (Is_Match : Boolean) is record
               case Is_Match is
                  when True  => null;
                  when False => Index : Positive;
               end case;
            end record;

            type Match (Is_Match : Boolean) is record
               case Is_Match is
                  when False => null;
                  when True  => Index : Positive;
               end case;
            end record;

            function Contains_At_Index
              (Index : Positive;
               Vector, Sub_Vector : Unbounded_String_Vectors.Vector) return Partial_Match
            is
               use type SU.Unbounded_String;
            begin
               for Sub_Index in Sub_Vector.First_Index .. Sub_Vector.Last_Index loop
                  declare
                     Vector_Index : constant Positive := Index + (Sub_Index - Sub_Vector.First_Index);
                  begin
                     if Vector_Index > Vector.Last_Index or else Sub_Vector (Sub_Index) /= Vector (Vector_Index) then
                        return (Is_Match => False, Index => Sub_Index);
                     end if;
                  end;
               end loop;
               return (Is_Match => True);
            end Contains_At_Index;

            procedure Append_To_Vector
              (Vector     : in out Unbounded_String_Vectors.Vector;
               Sub_Vector : Unbounded_String_Vectors.Vector)
            is
               Search_Index : Unbounded_String_Vectors.Extended_Index := Vector.First_Index;
               Index        : Unbounded_String_Vectors.Extended_Index;
            begin
               loop
                  Index := Vector.Find_Index (Sub_Vector.First_Element, Search_Index);
                  if Index = Unbounded_String_Vectors.No_Index then
                     Vector.Append (Sub_Vector);
                     exit;
                  else
                     declare
                        Result : constant Partial_Match := Contains_At_Index (Index, Vector, Sub_Vector);
                     begin
                        case Result.Is_Match is
                           when False =>
                              if Result.Index - Sub_Vector.First_Index + Index - 1 = Vector.Last_Index then
                                 --  Append remaining items of Sub_Vector to Vector
                                 declare
                                    Remaining_Vector : Unbounded_String_Vectors.Vector := Sub_Vector;
                                 begin
                                    Remaining_Vector.Delete_First (Count => Ada.Containers.Count_Type (Result.Index - Sub_Vector.First_Index));
                                    Vector.Append (Remaining_Vector);
                                 end;
                                 exit;
                              end if;
                           when True =>
                              exit;
                        end case;
                     end;
                  end if;
                  if Index >= Vector.Last_Index then
                     Vector.Append (Sub_Vector);
                     exit;
                  end if;
                  Search_Index := Index + 1;
               end loop;
            end Append_To_Vector;

            function Find_Match
              (Vector     : Unbounded_String_Vectors.Vector;
               Sub_Vector : Unbounded_String_Vectors.Vector) return Match
            is
               Search_Index : Unbounded_String_Vectors.Extended_Index := Vector.First_Index;
               Index        : Unbounded_String_Vectors.Extended_Index;
            begin
               loop
                  Index := Vector.Find_Index (Sub_Vector.First_Element, Search_Index);
                  if Index = Unbounded_String_Vectors.No_Index then
                     return (Is_Match => False);
                  else
                     declare
                        Result : constant Partial_Match := Contains_At_Index (Index, Vector, Sub_Vector);
                     begin
                        case Result.Is_Match is
                           when False =>
                              if Result.Index - Sub_Vector.First_Index + Index - 1 = Vector.Last_Index then
                                 return (Is_Match => False);
                              end if;
                           when True =>
                              return (Is_Match => True, Index => Index);
                        end case;
                     end;
                  end if;
                  if Index >= Vector.Last_Index then
                     return (Is_Match => False);
                  end if;
                  Search_Index := Index + 1;
               end loop;
            end Find_Match;

            function Get_Types
              (Request_Tag : aliased Wayland_XML.Request_Tag) return Unbounded_String_Vectors.Vector
            is
               Result : Unbounded_String_Vectors.Vector;
            begin
               for Child of Children (Request_Tag) loop
                  if Child.Kind_Id = Child_Arg then
                     case Type_Attribute (Child.Arg_Tag.all) is
                        when Type_New_Id | Type_Object =>
                           if not Wayland_XML.Exists_Interface_Attribute (Child.Arg_Tag.all) then
                              raise XML_Exception with
                                "Argument of type 'object' or 'new_id' has no attribute 'interface'";
                           end if;
                           Result.Append (+Wayland_XML.Interface_Attribute (Child.Arg_Tag.all));
                        when others =>
                           Result.Append (+"");
                     end case;
                  end if;
               end loop;

               return Result;
            end Get_Types;

            function Get_Types
              (Event_Tag : aliased Wayland_XML.Event_Tag) return Unbounded_String_Vectors.Vector
            is
               Result : Unbounded_String_Vectors.Vector;
            begin
               for Child of Children (Event_Tag) loop
                  if Child.Kind_Id = Child_Arg then
                     case Type_Attribute (Child.Arg_Tag.all) is
                        when Type_New_Id | Type_Object =>
                           if not Wayland_XML.Exists_Interface_Attribute (Child.Arg_Tag.all) then
                              raise XML_Exception with
                                "Argument of type 'object' or 'new_id' has no attribute 'interface'";
                           end if;
                           Result.Append (+Wayland_XML.Interface_Attribute (Child.Arg_Tag.all));
                        when others =>
                           Result.Append (+"");
                     end case;
                  end if;
               end loop;

               return Result;
            end Get_Types;

            procedure Append_Argument
              (Signature : in out SU.Unbounded_String;
               Argument  : Wayland_XML.Arg_Tag) is
            begin
               if Wayland_XML.Exists_Allow_Null (Argument)
                 and then Wayland_XML.Allow_Null (Argument)
               then
                  SU.Append (Signature, "?");
               end if;
               case Type_Attribute (Argument) is
                  when Type_Integer =>
                     SU.Append (Signature, "i");
                  when Type_Unsigned_Integer =>
                     SU.Append (Signature, "u");
                  when Type_String =>
                     SU.Append (Signature, "s");
                  when Type_FD =>
                     SU.Append (Signature, "h");
                  when Type_New_Id =>
                     SU.Append (Signature, "n");
                  when Type_Object =>
                     SU.Append (Signature, "o");
                  when Type_Fixed =>
                     SU.Append (Signature, "f");
                  when Type_Array =>
                     SU.Append (Signature, "a");
               end case;
            end Append_Argument;

            function Signature_To_String (Signature : SU.Unbounded_String) return String is
              (if SU.Length (Signature) > 0 then """" & (+Signature) & """" else "Nul");

            procedure Handle_Interface (Interface_Tag : aliased Wayland_XML.Interface_Tag) is
               Wayland_Interface_Name : constant String := Wayland_XML.Name (Interface_Tag);

               Interface_Name : constant String :=
                 Xml_Parser_Utils.Adaify_Name (Wayland_Interface_Name);

               Request_Count : Natural := 0;
               Event_Count   : Natural := 0;

               procedure Handle_Request_Name (Request_Tag : aliased Wayland_XML.Request_Tag) is
                  Wayland_Name : constant String := Wayland_XML.Name (Request_Tag);
                  Name : constant String := Xml_Parser_Utils.Adaify_Name (Wayland_Name);
               begin
                  Put_Line (File, "   " & Interface_Name & "_" & Name & "_Name : aliased char_array := " &
                    """" & Wayland_Name & """ & Nul;");
               end Handle_Request_Name;

               procedure Handle_Request_Type (Request_Tag : aliased Wayland_XML.Request_Tag) is
                  Name : constant String := Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Request_Tag));
                  Signature : SU.Unbounded_String;

                  Types : Unbounded_String_Vectors.Vector := Get_Types (Request_Tag);
               begin
                  if Wayland_XML.Exists_Since (Request_Tag) then
                     SU.Append (Signature, Aida.To_String
                       (Positive (Wayland_XML.Since (Request_Tag))));
                  end if;

                  for Child of Children (Request_Tag) loop
                     if Child.Kind_Id = Child_Arg then
                        Append_Argument (Signature, Child.Arg_Tag.all);
                     end if;
                  end loop;

                  if Types.Length = 0 then
                     Types.Append (+"");
                  end if;

                  --  Append Types to All_Types if Types is not a subset of All_Types
                  Append_To_Vector (All_Types, Types);

                  Put_Line (File, "   " & Interface_Name & "_" & Name & "_Type : aliased char_array := " &
                    Signature_To_String (Signature) & " & Nul;");
               end Handle_Request_Type;

               procedure Handle_Event_Name (Event_Tag : aliased Wayland_XML.Event_Tag) is
                  Wayland_Name : constant String := Wayland_XML.Name (Event_Tag);
                  Name : constant String := Xml_Parser_Utils.Adaify_Name (Wayland_Name);
               begin
                  Put_Line (File, "   " & Interface_Name & "_" & Name & "_Name : aliased char_array := " &
                    """" & Wayland_Name & """ & Nul;");
               end Handle_Event_Name;

               procedure Handle_Event_Type (Event_Tag : aliased Wayland_XML.Event_Tag) is
                  Name : constant String := Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Event_Tag));
                  Signature : SU.Unbounded_String;

                  Types : Unbounded_String_Vectors.Vector := Get_Types (Event_Tag);
               begin
                  if Wayland_XML.Exists_Since (Event_Tag) then
                     SU.Append (Signature, Aida.To_String
                       (Positive (Wayland_XML.Since (Event_Tag))));
                  end if;

                  for Child of Children (Event_Tag) loop
                     if Child.Kind_Id = Child_Arg then
                        Append_Argument (Signature, Child.Arg_Tag.all);
                     end if;
                  end loop;

                  if Types.Length = 0 then
                     Types.Append (+"");
                  end if;

                  --  Append Types to All_Types if Types is not a subset of All_Types
                  Append_To_Vector (All_Types, Types);

                  Put_Line (File, "   " & Interface_Name & "_" & Name & "_Type : aliased char_array := " & Signature_To_String (Signature) & " & Nul;");
               end Handle_Event_Type;
            begin
               for Child of Children (Interface_Tag) loop
                  if Child.Kind_Id = Child_Request then
                     Request_Count := Request_Count + 1;
                  end if;
               end loop;

               for Child of Children (Interface_Tag) loop
                  if Child.Kind_Id = Child_Event then
                     Event_Count := Event_Count + 1;
                  end if;
               end loop;

               if Request_Count > 0 then
                  Put_Line (File, "");
                  Iterate_Over_Requests (Interface_Tag, Handle_Request_Name'Access);
                  Put_Line (File, "");
                  Iterate_Over_Requests (Interface_Tag, Handle_Request_Type'Access);
               end if;

               if Event_Count > 0 then
                  Put_Line (File, "");
                  Iterate_Over_Events (Interface_Tag, Handle_Event_Name'Access);
                  Put_Line (File, "");
                  Iterate_Over_Events (Interface_Tag, Handle_Event_Type'Access);
               end if;
            end Handle_Interface;

            procedure Handle_Interface_2 (Interface_Tag : aliased Wayland_XML.Interface_Tag) is
               Version : constant String :=
                 (if Wayland_XML.Exists_Version (Interface_Tag) then
                    Aida.To_String (Positive (Wayland_XML.Version (Interface_Tag)))
                  else
                    "1");

               Wayland_Interface_Name : constant String := Wayland_XML.Name (Interface_Tag);

               Interface_Name : constant String :=
                 Xml_Parser_Utils.Adaify_Name (Wayland_Interface_Name);

               Name : constant String
                 := Xml_Parser_Utils.Adaify_Name
                   (Wayland_Interface_Name & "_Interface");

               Request_Count : Natural := 0;
               Event_Count   : Natural := 0;

               Request_Index : Positive := 1;
               Event_Index   : Positive := 1;

               procedure Handle_Request_Tuple (Request_Tag : aliased Wayland_XML.Request_Tag) is
                  Prefix : constant String :=
                    (if Request_Count = 1 then "(1 => " elsif Request_Index = 1 then "(" else " ");
                  Suffix : constant String :=
                    (if Request_Index = Request_Count then ");" else ",");

                  Name : constant String :=
                    Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Request_Tag));

                  Name_String : constant String := Interface_Name & "_" & Name & "_Name";
                  Type_String : constant String := Interface_Name & "_" & Name & "_Type";

                  Types : Unbounded_String_Vectors.Vector := Get_Types (Request_Tag);
               begin
                  if Types.Length = 0 then
                     Types.Append (+"");
                  end if;

                  declare
                     Index_Match : constant Match := Find_Match (All_Types, Types);
                     pragma Assert (Index_Match.Is_Match);
                  begin
                     Put_Line (File, "     " & Prefix & "(" & Name_String & "'Access,");
                     Put_Line (File, "     " & "  " & Type_String & "'Access,");
                     Put_Line (File, "     " & "  " & "Interface_Pointers (" & Trim (Index_Match.Index'Image) & ")'Access)" & Suffix);
                  end;
                  Request_Index := Request_Index + 1;
               end Handle_Request_Tuple;

               procedure Handle_Event_Tuple (Event_Tag : aliased Wayland_XML.Event_Tag) is
                  Prefix : constant String :=
                    (if Event_Count = 1 then "(1 => " elsif Event_Index = 1 then "(" else " ");
                  Suffix : constant String :=
                    (if Event_Index = Event_Count then ");" else ",");

                  Name : constant String :=
                    Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Event_Tag));

                  Name_String : constant String := Interface_Name & "_" & Name & "_Name";
                  Type_String : constant String := Interface_Name & "_" & Name & "_Type";

                  Types : Unbounded_String_Vectors.Vector := Get_Types (Event_Tag);
               begin
                  if Types.Length = 0 then
                     Types.Append (+"");
                  end if;

                  declare
                     Index_Match : constant Match := Find_Match (All_Types, Types);
                     pragma Assert (Index_Match.Is_Match);
                  begin
                     Put_Line (File, "     " & Prefix & "(" & Name_String & "'Access,");
                     Put_Line (File, "     " & "  " & Type_String & "'Access,");
                     Put_Line (File, "     " & "  " & "Interface_Pointers (" & Trim (Index_Match.Index'Image) & ")'Access)" & Suffix);
                  end;
                  Event_Index := Event_Index + 1;
               end Handle_Event_Tuple;
            begin
               for Child of Children (Interface_Tag) loop
                  if Child.Kind_Id = Child_Request then
                     Request_Count := Request_Count + 1;
                  end if;
               end loop;

               for Child of Children (Interface_Tag) loop
                  if Child.Kind_Id = Child_Event then
                     Event_Count := Event_Count + 1;
                  end if;
               end loop;

               if Request_Count > 0 then
                  Put_Line (File, "");
                  Put_Line (File, "   " & Interface_Name & "_Requests : aliased constant Wayland.API.Message_Array :=");
                  Iterate_Over_Requests (Interface_Tag, Handle_Request_Tuple'Access);
               end if;

               if Event_Count > 0 then
                  Put_Line (File, "");
                  Put_Line (File, "   " & Interface_Name & "_Events : aliased constant Wayland.API.Message_Array :=");
                  Iterate_Over_Events (Interface_Tag, Handle_Event_Tuple'Access);
               end if;

               Put_Line (File, "");
               Put_Line (File, "   " & Interface_Name & "_Interface_Name : aliased char_array := """ & Wayland_Interface_Name &  """ & Nul;");
               Put_Line (File, "");

               Put_Line (File, "   " & Name & " : aliased constant Interface_T :=");
               Put_Line (File, "     (Name         => " & Interface_Name & "_Interface_Name'Access,");
               Put_Line (File, "      Version      => " & Version & ",");
               Put_Line (File, "      Method_Count => " & Aida.To_String (Request_Count) & ",");
               Put_Line (File, "      Methods      => " & (if Request_Count > 0 then Interface_Name & "_Requests'Access," else "null,"));
               Put_Line (File, "      Event_Count  => " & Aida.To_String (Event_Count) & ",");
               Put_Line (File, "      Events       => " & (if Event_Count > 0 then Interface_Name & "_Events'Access);" else "null);"));
            end Handle_Interface_2;
         begin
            Put_Line (File, "   use Interfaces.C;");
            Put_Line (File, "");
            Put_Line (File, "   Nul : char renames Interfaces.C.nul;");
            Iterate_Over_Interfaces (Handle_Interface'Access);

            Put_Line (File, "");
            Put_Line (File, "   Interface_Pointers : Wayland.API.Interface_Ptr_Array (1 .." & All_Types.Length'Image & ");");

            Iterate_Over_Interfaces (Handle_Interface_2'Access);
         end Generate_Private_Code_For_Interface_Constants;

         procedure Generate_Code_For_Interface_Ptrs is
            procedure Handle_Interface (Interface_Tag : aliased Wayland_XML.Interface_Tag) is
               Interface_Ptr_Name : constant String :=
                 Xml_Parser_Utils.Adaify_Name (Name (Interface_Tag)) & "_Ptr";
            begin
               if Interface_Ptr_Name /= "Display_Ptr" then
                  Put_Line (File, "   type " & Interface_Ptr_Name & " is new Proxy_Ptr;");
                  New_Line (File);
               end if;
            end Handle_Interface;
         begin
            Iterate_Over_Interfaces (Handle_Interface'Access);
         end Generate_Code_For_Interface_Ptrs;

         procedure Generate_Code_For_Each_Interface is

            procedure Handle_Interface
              (Interface_Tag : aliased Wayland_XML.Interface_Tag)
            is
               procedure Generate_Code_For_Subprogram_Ptrs is

                  procedure Generate_Code_For_Subprogram
                    (Event_Tag : aliased Wayland_XML.Event_Tag)
                  is
                     V : Wayland_XML.Event_Child_Vectors.Vector;

                     Subprogram_Name : constant String
                       := Xml_Parser_Utils.Adaify_Name
                         (Name (Interface_Tag) & "_" & Name (Event_Tag) & "_Subprogram_Ptr");
                     Interface_Name  : constant String
                       := Xml_Parser_Utils.Adaify_Name (Name (Interface_Tag));
                  begin
                     for Child of Wayland_XML.Children (Event_Tag) loop
                        if Child.Kind_Id = Child_Arg then
                           V.Append (Child);
                        end if;
                     end loop;

                     declare
                        Max_Name_Length : Natural := Interface_Name'Length;
                        Dont_Care : String_Maps.Map;
                     begin
                        for Child of V loop
                           declare
                              Arg_Name      : constant String
                                := Xml_Parser_Utils.Adaify_Variable_Name (Name (Child.Arg_Tag.all));
                           begin
                              Max_Name_Length := Natural'Max (Max_Name_Length, Arg_Name'Length);
                           end;
                        end loop;

                        Put_Line (File, "   type " & Subprogram_Name & " is access procedure");

                        declare
                           Interface_Name_Aligned : constant String := SF.Head (Interface_Name, Max_Name_Length, ' ');
                        begin
                           Put_Line (File, "     (" & SF.Head ("Data", Max_Name_Length, ' ') & " : Void_Ptr;");
                           Put (File, "      " & Interface_Name_Aligned & " : " & Interface_Name & "_Ptr");
                        end;

                        if V.Length = 0 then
                           Put (File, ")");
                        else
                           Put_Line (File, ";");
                        end if;

                        for Child of V loop
                           if Child.Kind_Id = Child_Arg then
                              Generate_Code_For_Arg
                                (File,
                                 Interface_Tag,
                                 Child.Arg_Tag.all,
                                 Max_Name_Length,
                                 Children (Event_Tag).Last_Element = Child,
                                 Dont_Care);
                           end if;
                        end loop;
                     end;

                     New_Line (File);
                     Put_Line (File, "   with Convention => C;");
                     New_Line (File);
                  end Generate_Code_For_Subprogram;
               begin
                  Iterate_Over_Events (Interface_Tag, Generate_Code_For_Subprogram'Access);
               end Generate_Code_For_Subprogram_Ptrs;

               procedure Generate_Code_For_Listener_Type_Definition is
                  function Get_Name (Event_Tag   : Wayland_XML.Event_Tag) return String is
                    (Xml_Parser_Utils.Adaify_Name (Name (Event_Tag)));

                  Name_Length : Natural := 0;

                  procedure Generate_Code_For_Record_Component
                    (Event_Tag : aliased Wayland_XML.Event_Tag)
                  is
                     Component_Name      : constant String := Get_Name (Event_Tag);

                     Component_Type_Name : constant String
                       := Xml_Parser_Utils.Adaify_Name
                         (Name (Interface_Tag) & "_" &
                              Name (Event_Tag) & "_Subprogram_Ptr");

                     Component_Name_Aligned : constant String := SF.Head (Component_Name & (if Is_Reserved_Keyword (Component_Name) then "_F" else ""), Name_Length, ' ');
                  begin
                     Put_Line (File, "      " & Component_Name_Aligned & " : " & Component_Type_Name & ";");
                  end Generate_Code_For_Record_Component;

                  Name     : constant String
                    := Xml_Parser_Utils.Adaify_Name
                      (Wayland_XML.Name (Interface_Tag) & "_Listener_T");
                  Ptr_Name : constant String
                    := Xml_Parser_Utils.Adaify_Name
                      (Wayland_XML.Name (Interface_Tag) & "_Listener_Ptr");
               begin
                  Put_Line (File, "   type " & Name & " is record");

                  for Child of Children (Interface_Tag) loop
                     if Child.Kind_Id = Child_Event then
                        declare
                           Arg_Name : constant String := Get_Name (Child.Event_Tag.all);
                        begin
                           Name_Length := Natural'Max (Name_Length, Arg_Name'Length + (if Is_Reserved_Keyword (Arg_Name) then 2 else 0));
                        end;
                     end if;
                  end loop;

                  Iterate_Over_Events (Interface_Tag, Generate_Code_For_Record_Component'Access);
                  Put_Line (File, "   end record");
                  Put_Line (File, "     with Convention => C_Pass_By_Copy;");
                  Put_Line (File, "");
                  Put_Line (File, "   type " & Ptr_Name & " is access all " & Name & ";");
               end Generate_Code_For_Listener_Type_Definition;

               procedure Generate_Code_For_Add_Listener_Subprogram_Declaration is
                  Ptr_Listener_Name : constant String
                    := Xml_Parser_Utils.Adaify_Name
                      (Wayland_XML.Name (Interface_Tag) & "_Listener_Ptr");
               begin
                  Generate_Pretty_Code_For_Subprogram
                    (File, Declaration, Interface_Tag, "Add_Listener", "Interfaces.C.int",
                     ((+"Listener", +Ptr_Listener_Name),
                      (+"Data", +"Void_Ptr")));
               end Generate_Code_For_Add_Listener_Subprogram_Declaration;

               procedure Generate_Code_For_Set_User_Data_Subprogram_Declaration is
               begin
                  Generate_Pretty_Code_For_Subprogram
                    (File, Declaration, Interface_Tag, "Set_User_Data", "",
                     (1 => (+"Data", +"Void_Ptr")));
               end Generate_Code_For_Set_User_Data_Subprogram_Declaration;

               procedure Generate_Code_For_Get_User_Data_Subprogram_Declaration is
               begin
                  Generate_Pretty_Code_For_Subprogram
                    (File, Declaration, Interface_Tag, "Get_User_Data", "Void_Ptr");
               end Generate_Code_For_Get_User_Data_Subprogram_Declaration;

               procedure Generate_Code_For_Get_ID_Subprogram_Declaration is
               begin
                  Generate_Pretty_Code_For_Subprogram
                    (File, Declaration, Interface_Tag, "Get_ID", "Unsigned_32");
               end Generate_Code_For_Get_ID_Subprogram_Declaration;

               procedure Generate_Code_For_Get_Version_Subprogram_Declaration is
               begin
                  Generate_Pretty_Code_For_Subprogram
                    (File, Declaration, Interface_Tag, "Get_Version", "Unsigned_32");
               end Generate_Code_For_Get_Version_Subprogram_Declaration;

               procedure Generate_Code_For_Destroy_Subprogram_Declaration is
               begin
                  Generate_Pretty_Code_For_Subprogram
                    (File, Declaration, Interface_Tag, "Destroy", "");
               end Generate_Code_For_Destroy_Subprogram_Declaration;

               procedure Generate_Code_For_Requests is

                  procedure Generate_Code_For_Subprogram_Declaration
                    (Request_Tag : aliased Wayland_XML.Request_Tag)
                  is
                     procedure Generate_Comment (Text : String) is
                        Interval_Identifier : constant Xml_Parser_Utils.Interval_Identifier := Xml_Parser_Utils.Make (Text);
                     begin
                        for Interval of Interval_Identifier.Intervals loop
                           declare
                              Comment : constant String := Trim (Text (Interval.First .. Interval.Last));
                           begin
                              if Comment'Length > 0 then
                                 Put_Line (File, "   --  " & Comment);
                              else
                                 Put_Line (File, "   --");
                              end if;
                           end;
                        end loop;
                     end Generate_Comment;

                     Request_Name : constant String
                       := Xml_Parser_Utils.Adaify_Name (Name (Request_Tag));
                     Subprogram_Name : constant String
                       := Xml_Parser_Utils.Adaify_Name
                         (Name (Interface_Tag) & "_" & Request_Name);
                     Name            : constant String
                       := Xml_Parser_Utils.Adaify_Name
                         (Wayland_XML.Name (Interface_Tag));
                     Ptr_Name        : constant String
                       := Xml_Parser_Utils.Adaify_Name
                         (Wayland_XML.Name (Interface_Tag) & "_Ptr");

                     procedure Generate_Pretty_Function_Code
                       (Subprogram_Kind : String;
                        Max_Name_Length : in out Natural;
                        Have_Last       : Boolean := True)
                     is
                        V : Wayland_XML.Request_Child_Vectors.Vector;
                        Dont_Care : String_Maps.Map;

                        function Align (Value : String) return String is (SF.Head (Value, Max_Name_Length, ' '));
                     begin
                        Get_Max_Arg_Length (Request_Tag, V, Max_Name_Length);

                        Put_Line (File, "   " & Subprogram_Kind & " " & Subprogram_Name);
                        Put_Line (File, "     (" & Align (Name) & " : " & Ptr_Name & ";");

                        for Child of V loop
                           if Child.Kind_Id = Child_Arg then
                              Generate_Code_For_Arg
                                (File,
                                 Interface_Tag,
                                 Child.Arg_Tag.all,
                                 Max_Name_Length,
                                 Have_Last and Child = V.Last_Element,
                                 Dont_Care);
                           end if;
                        end loop;
                     end Generate_Pretty_Function_Code;
                  begin
                     Put_Line (File, "");
                     if Xml_Parser_Utils.Is_New_Id_Argument_Present (Request_Tag) then
                        if Enable_Comments and Exists_Description (Request_Tag) then
                           Generate_Comment
                             (Xml_Parser_Utils.Remove_Tabs (Description (Request_Tag)));
                        end if;

                        if Xml_Parser_Utils.Is_Interface_Specified (Request_Tag) then
                           declare
                              Return_Type : constant String := Xml_Parser_Utils.Adaify_Name (Xml_Parser_Utils.Find_Specified_Interface (Request_Tag) & "_Ptr");
                           begin
                              if Xml_Parser_Utils.Number_Of_Args (Request_Tag) > 1 then
                                 declare
                                    Max_Name_Length : Natural := Name'Length;
                                 begin
                                    Generate_Pretty_Function_Code ("function", Max_Name_Length);
                                 end;
                                 Put_Line (File, " return " & Return_Type & ";");
                              else
                                 if 27 + Subprogram_Name'Length + Name'Length + Ptr_Name'Length + Return_Type'Length <= Max_Line_Length then
                                    Put_Line (File, "   function " & Subprogram_Name & " (" & Name & " : " & Ptr_Name & ") return " & Return_Type & ";");
                                 else
                                    Put_Line (File, "   function " & Subprogram_Name);
                                    Put_Line (File, "     (" & Name & " : " & Ptr_Name & ") return " & Return_Type & ";");
                                 end if;
                              end if;
                           end;
                        else
                           if Xml_Parser_Utils.Number_Of_Args (Request_Tag) > 1 then
                              declare
                                 Max_Name_Length : Natural := Natural'Max (Name'Length, 11);

                                 --  TODO Use parameter list in Generate_Pretty_Function_Code
                                 function Align (Value : String) return String is (SF.Head (Value, Max_Name_Length, ' '));
                              begin
                                 Generate_Pretty_Function_Code ("function", Max_Name_Length, False);
                                 Put_Line (File, "      " & Align ("Interface_V") & " : Interface_Ptr;");
                                 Put_Line (File, "      " & Align ("New_Id") & " : Unsigned_32) return Proxy_Ptr;");
                              end;
                           else
                              Put_Line (File, "   function " & Subprogram_Name & " (" & Name & " : " & Ptr_Name & ") return Proxy_Ptr;");
                           end if;
                        end if;
                     elsif Xml_Parser_Utils.Is_Request_Destructor (Request_Tag) then
                        Generate_Pretty_Code_For_Subprogram
                          (File, Declaration, Interface_Tag, Request_Name, "");
                     else
                        if Enable_Comments and Exists_Description (Request_Tag) then
                           Generate_Comment
                             (Xml_Parser_Utils.Remove_Tabs (Description (Request_Tag)));
                        end if;
                        if Xml_Parser_Utils.Number_Of_Args (Request_Tag) > 0 then
                           declare
                              Max_Name_Length : Natural := Name'Length;
                           begin
                              Generate_Pretty_Function_Code ("procedure", Max_Name_Length);
                              Put_Line (File, ";");
                           end;
                        else
                           Put_Line (File, "   procedure " & Subprogram_Name & " (" & Name & " : " & Ptr_Name & ");");
                        end if;
                     end if;
                  end Generate_Code_For_Subprogram_Declaration;
               begin
                  Iterate_Over_Requests (Interface_Tag, Generate_Code_For_Subprogram_Declaration'Access);
               end Generate_Code_For_Requests;

            begin
               Generate_Code_For_Subprogram_Ptrs;

               if Xml_Parser_Utils.Exists_Any_Event_Tag (Interface_Tag) then
                  Generate_Code_For_Listener_Type_Definition;
                  Put_Line (File, "");
                  Generate_Code_For_Add_Listener_Subprogram_Declaration;
                  Put_Line (File, "");
               end if;

               Generate_Code_For_Set_User_Data_Subprogram_Declaration;
               Put_Line (File, "");
               Generate_Code_For_Get_User_Data_Subprogram_Declaration;
               Put_Line (File, "");
               Generate_Code_For_Get_ID_Subprogram_Declaration;
               Put_Line (File, "");
               Generate_Code_For_Get_Version_Subprogram_Declaration;

               if Wayland_XML.Name (Interface_Tag) /= "wl_display"
                 and then Xml_Parser_Utils.Get_Destructor (Interface_Tag) = ""
               then
                  Put_Line (File, "");
                  Generate_Code_For_Destroy_Subprogram_Declaration;
               end if;

               Generate_Code_For_Requests;
               New_Line (File);
            end Handle_Interface;

         begin
            Iterate_Over_Interfaces (Handle_Interface'Access);
         end Generate_Code_For_Each_Interface;

      begin
         if Protocol_Name = "client" then
            Put_Line (File, "   subtype Display_Ptr   is Wayland.API.Display_Ptr;");
            Put_Line (File, "");
            Put_Line (File, "   function Display_Connect return Display_Ptr;");
            Put_Line (File, "");
            Put_Line (File, "   procedure Display_Disconnect (This : in out Display_Ptr);");
         elsif Protocol_Name = "xdg_decoration_unstable_v1" then
            Put_Line (File, "   use Wayland.Protocols.Thin_Xdg_Shell;");
         else
            Put_Line (File, "   use Wayland.Protocols.Thin_Client;");
         end if;
         Put_Line (File, "");
         Generate_Code_For_Interface_Constants;
         if Protocol_Name /= "client" then
            New_Line (File);
            Put_Line (File, "   function Initialize return Boolean;");
            New_Line (File);
         end if;

         Generate_Code_For_Interface_Ptrs;
         Generate_Code_For_Each_Interface;

         if Protocol_Name /= "client" then
            Put_Line (File, "private");
            New_Line (File);

            Generate_Private_Code_For_Interface_Constants;
            New_Line (File);
         end if;
      end Create_Wl_Thin_Spec_File;

      procedure Generate_Use_Type_Declarions (Package_Name : String) is
         procedure Handle_Interface (Interface_Tag : aliased Wayland_XML.Interface_Tag) is
            Name : constant String
              := Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Interface_Tag));
         begin
            Put_Line (File, "   use type Thin." & Name & "_Ptr;");
         end Handle_Interface;
      begin
         Put_Line (File, "   package Thin renames Wayland.Protocols.Thin_" & Package_Name & ";");
         Put_Line (File, "");
         Iterate_Over_Interfaces (Handle_Interface'Access);
      end Generate_Use_Type_Declarions;

      procedure Generate_Manually_Edited_Code_For_Type_Definitions is
         procedure Handle_Interface (Interface_Tag : aliased Wayland_XML.Interface_Tag) is
            Name : constant String
              := Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Interface_Tag));
         begin
            Put_Line (File, "");
            Put_Line (File, "   type " & Name & " is tagged limited record");
            Put_Line (File, "      Proxy : Thin." & Name & "_Ptr;");
            Put_Line (File, "   end record;");
            --  FIXME add Initialized : Boolean := Thin.Initialize; for Xdg_Wm_Base and Wp_Presentation
         end Handle_Interface;
      begin
         Iterate_Over_Interfaces (Handle_Interface'Access);
      end Generate_Manually_Edited_Code_For_Type_Definitions;

      procedure Generate_Private_Code_For_The_Interface_Constants is
         procedure Handle_Interface (Interface_Tag : aliased Wayland_XML.Interface_Tag) is
            Name : constant String
              := Xml_Parser_Utils.Adaify_Name
                (Wayland_XML.Name (Interface_Tag) & "_Interface");
         begin
            Put_Line (File, "");
            Put_Line (File, "   " & Name & " : constant Interface_Type :=");
            Put_Line (File, "     (My_Interface => Thin." & Name & "'Access);");
         end Handle_Interface;
      begin
         Iterate_Over_Interfaces (Handle_Interface'Access);
      end Generate_Private_Code_For_The_Interface_Constants;

   begin
      Create_File;
   end Create_Wayland_Spec_File;

   procedure Create_Wayland_Body_File is
      File : Ada.Text_IO.File_Type;

      procedure Create_Wl_Thin_Body_File;

      procedure Generate_Manually_Edited_Code (Protocol_Name : String);

      procedure Create_File is
         Protocol_Name : constant String := Get_Protocol_Name (Name (Protocol_Tag.all));
         Package_Name  : constant String := Xml_Parser_Utils.Adaify_Name (Protocol_Name);
      begin
         Ada.Text_IO.Create
           (File, Ada.Text_IO.Out_File,
            "wayland-protocols-" & Protocol_Name & ".adb");

         --  TODO Do not import System.Address_To_Access_Conversions if
         --  there are no generic *_Events packages
         Put_Line (File, "with System.Address_To_Access_Conversions;");
         New_Line (File);
         Put_Line (File, "with Interfaces.C.Strings;");
         New_Line (File);

         if Protocol_Name = "client" then
            Put_Line (File, "with Wayland.Posix;");
         elsif Protocol_Name in "presentation_time" | "relative_pointer_unstable_v1" then
            Put_Line (File, "with Ada.Unchecked_Conversion;");
            Put_Line (File, "");
            Put_Line (File, "with Wayland.Protocols.Thin_Client;");
         elsif Protocol_Name = "xdg_decoration_unstable_v1" then
            Put_Line (File, "with Wayland.Protocols.Thin_Xdg_Shell;");
         else
            Put_Line (File, "with Wayland.Protocols.Thin_Client;");
         end if;

         New_Line (File);
         Put_Line (File, "package body Wayland.Protocols." & Package_Name & " is");
         New_Line (File);

         if Protocol_Name = "client" then
            Put_Line (File, "   package body Constructors is");
            Put_Line (File, "      function Set_Proxy (Proxy : Secret_Proxy) return Buffer is");
            Put_Line (File, "         (Proxy => Thin.Buffer_Ptr (Proxy));");
            Put_Line (File, "");
            Put_Line (File, "      function Set_Proxy (Proxy : Secret_Proxy) return Output is");
            Put_Line (File, "         (Proxy => Thin.Output_Ptr (Proxy));");
            Put_Line (File, "");
            Put_Line (File, "      function Set_Proxy (Proxy : Secret_Proxy) return Surface is");
            Put_Line (File, "         (Proxy => Thin.Surface_Ptr (Proxy));");
            Put_Line (File, "   end Constructors;");
            Put_Line (File, "");
         end if;

         Generate_Manually_Edited_Code (Protocol_Name);

         Put_Line (File, "end Wayland.Protocols." & Package_Name & ";");

         Ada.Text_IO.Close (File);

         -----------------------------------------------------------------------

         Ada.Text_IO.Create
           (File, Ada.Text_IO.Out_File,
            "wayland-protocols-thin_" & Protocol_Name & ".adb");

         Put_Line (File, "with Ada.Unchecked_Conversion;");
         Put_Line (File, "");
         Put_Line (File, "package body Wayland.Protocols.Thin_" & Package_Name & " is");
         Put_Line (File, "");

         if Protocol_Name /= "client" then
            Put_Line (File, "   function Initialize return Boolean is");
            Put_Line (File, "   begin");

            Put_Line (File, "      Interface_Pointers :=");
            for Index in All_Types.First_Index .. All_Types.Last_Index loop
               declare
                  Value : constant String := +All_Types (Index);

                  Prefix : constant String := (if Index = All_Types.First_Index then "(" else " ");
                  Suffix : constant String := (if Index = All_Types.Last_Index then ");" else ",");
               begin
                  Put_Line (File, "        " & Prefix & (if Value'Length > 0 then Xml_Parser_Utils.Adaify_Name (Value) & "_Interface'Access" else "null") & Suffix);
               end;
            end loop;

            Put_Line (File, "      return True;");
            Put_Line (File, "   end Initialize;");
            Put_Line (File, "");
         end if;

         Put (File, "   package Constants is");

         Generate_Code_For_Numeric_Constants (File);

         Put_Line (File, "   end Constants;");
         Put_Line (File, "");
         Put_Line (File, "   use type Proxy_Ptr;");

         if Protocol_Name = "client" then
            Put_Line (File, "");
            Put_Line (File, "   function Display_Connect return Display_Ptr is");
            Put_Line (File, "   begin");
            Put_Line (File, "      return Wayland.API.Display_Connect (Interfaces.C.Strings.Null_Ptr);");
            Put_Line (File, "   end Display_Connect;");
            Put_Line (File, "");
            Put_Line (File, "   procedure Display_Disconnect (This : in out Display_Ptr) is");
            Put_Line (File, "      use type Wayland.API.Display_Ptr;");
            Put_Line (File, "   begin");
            Put_Line (File, "      if This /= null then");
            Put_Line (File, "         Wayland.API.Display_Disconnect (This);");
            Put_Line (File, "         This := null;");
            Put_Line (File, "      end if;");
            Put_Line (File, "   end Display_Disconnect;");
         end if;

         Create_Wl_Thin_Body_File;

         Put_Line (File, "");
         Put_Line (File, "end Wayland.Protocols.Thin_" & Package_Name & ";");

         Ada.Text_IO.Close (File);
      end Create_File;

      procedure Create_Wl_Thin_Body_File is

         procedure Generate_Code_For_Protocol_Tag_Children is
            procedure Handle_Interface
              (Interface_Tag : aliased Wayland_XML.Interface_Tag)
            is
               procedure Generate_Code_For_Add_Listener_Subprogram_Implementations (Name : String) is
                  Ptr_Listener_Name : constant String := Name & "_Listener_Ptr";
               begin
                  Generate_Pretty_Code_For_Subprogram
                    (File, Implementation, Interface_Tag, "Add_Listener", "Interfaces.C.int",
                      ((+"Listener", +Ptr_Listener_Name),
                       (+"Data", +"Void_Ptr")));
                  Put_Line (File, "   begin");
                  Put_Line (File, "      return Wayland.API.Proxy_Add_Listener");
                  Put_Line (File, "        (" & Name & ".all, Listener.all'Address, Data);");
                  Put_Line (File, "   end " & Name & "_Add_Listener;");
               end Generate_Code_For_Add_Listener_Subprogram_Implementations;

               procedure Generate_Code_For_Set_User_Data_Subprogram_Implementations (Name : String) is
               begin
                  Generate_Pretty_Code_For_Subprogram
                    (File, Implementation, Interface_Tag, "Set_User_Data", "",
                      (1 => (+"Data", +"Void_Ptr")));
                  Put_Line (File, "   begin");
                  Put_Line (File, "      Wayland.API.Proxy_Set_User_Data (" & Name & ".all, Data);");
                  Put_Line (File, "   end " & Name & "_Set_User_Data;");
               end Generate_Code_For_Set_User_Data_Subprogram_Implementations;

               procedure Generate_Code_For_Get_User_Data_Subprogram_Implementations (Name : String) is
               begin
                  Generate_Pretty_Code_For_Subprogram
                    (File, Implementation, Interface_Tag, "Get_User_Data", "Void_Ptr");
                  Put_Line (File, "   begin");
                  Put_Line (File, "      return Wayland.API.Proxy_Get_User_Data (" & Name & ".all);");
                  Put_Line (File, "   end " & Name & "_Get_User_Data;");
               end Generate_Code_For_Get_User_Data_Subprogram_Implementations;

               procedure Generate_Code_For_Get_ID_Subprogram_Implementations (Name : String) is
               begin
                  Generate_Pretty_Code_For_Subprogram
                    (File, Implementation, Interface_Tag, "Get_ID", "Unsigned_32");
                  Put_Line (File, "   begin");
                  Put_Line (File, "      return Wayland.API.Proxy_Get_Id (" & Name & ".all);");
                  Put_Line (File, "   end " & Name & "_Get_ID;");
               end Generate_Code_For_Get_ID_Subprogram_Implementations;

               procedure Generate_Code_For_Get_Version_Subprogram_Implementations (Name : String) is
               begin
                  Generate_Pretty_Code_For_Subprogram
                    (File, Implementation, Interface_Tag, "Get_Version", "Unsigned_32");
                  Put_Line (File, "   begin");
                  Put_Line (File, "      return Wayland.API.Proxy_Get_Version (" & Name & ".all);");
                  Put_Line (File, "   end " & Name & "_Get_Version;");
               end Generate_Code_For_Get_Version_Subprogram_Implementations;

               procedure Generate_Code_For_Destroy_Subprogram_Implementations (Name : String) is
               begin
                  Generate_Pretty_Code_For_Subprogram
                    (File, Implementation, Interface_Tag, "Destroy", "");
                  Put_Line (File, "   begin");
                  Put_Line (File, "      Wayland.API.Proxy_Destroy (" & Name & ".all);");
                  Put_Line (File, "   end " & Name & "_Destroy;");
               end Generate_Code_For_Destroy_Subprogram_Implementations;

               procedure Generate_Code_For_Requests is

                  procedure Generate_Code_For_Subprogram_Implementation
                    (Request_Tag : aliased Wayland_XML.Request_Tag)
                  is
                     Request_Name : constant String :=
                       Xml_Parser_Utils.Adaify_Name (Name (Request_Tag));

                     Subprogram_Name : constant String
                       := Xml_Parser_Utils.Adaify_Name
                            (Name (Interface_Tag) & "_" & Request_Name);
                     Opcode : constant String := "Constants." & Subprogram_Name;
                     Name            : constant String
                       := Xml_Parser_Utils.Adaify_Name
                         (Wayland_XML.Name (Interface_Tag));
                     Ptr_Name        : constant String
                       := Xml_Parser_Utils.Adaify_Name
                         (Wayland_XML.Name (Interface_Tag) & "_Ptr");

                     procedure Generate_Arguments (Spaces : Natural; Null_Id : Boolean) is
                        use SF;

                        function Get_Value (Child : Wayland_XML.Arg_Tag; Value : String) return String is
                          (if Exists_Enum (Child) then "Convert (" & Value & ")" else Value);

                        First_Element : Boolean := True;
                     begin
                        for Child of Children (Request_Tag) loop
                           if Child.Kind_Id = Child_Arg then
                              if not First_Element then
                                 Put_Line (File, ",");
                              end if;
                              First_Element := False;
                              if Type_Attribute (Child.Arg_Tag.all) /= Type_Object then
                                 if Type_Attribute (Child.Arg_Tag.all) = Type_New_Id then
                                    if Null_Id then
                                       Put (File, Spaces * " " & "0");
                                    end if;
                                 else
                                    Put (File, Spaces * " " & Get_Value (Child.Arg_Tag.all, Xml_Parser_Utils.Adaify_Variable_Name (Wayland_XML.Name (Child.Arg_Tag.all))));
                                 end if;
                              else
                                 Put (File, Spaces * " " & "Proxy_Ptr (" & Get_Value (Child.Arg_Tag.all, Xml_Parser_Utils.Adaify_Variable_Name (Wayland_XML.Name (Child.Arg_Tag.all))) & ")");
                              end if;
                           end if;
                        end loop;
                     end Generate_Arguments;

                     procedure Generate_Code_Before_Arguments is
                        Interface_Name : constant String :=
                          Xml_Parser_Utils.Adaify_Name (Xml_Parser_Utils.Find_Specified_Interface (Request_Tag)) & "_Interface";
                     begin
                        Put_Line (File, "      P : constant Proxy_Ptr :=");
                        Put_Line (File, "        Wayland.API.Proxy_Marshal_Constructor");
                        Put_Line (File, "          (" & Name & ".all,");
                        Put_Line (File, "           " & Opcode & ",");
                        Put_Line (File, "           " & Interface_Name & "'Access,");
                     end Generate_Code_Before_Arguments;

                     procedure Generate_Code_After_Arguments is
                     begin
                        Put_Line (File, ");");
                        Put_Line (File, "   begin");
                        Put_Line (File, "      return (if P /= null then P.all'Access else null);");
                        Put_Line (File, "   end " & Subprogram_Name & ";");
                     end Generate_Code_After_Arguments;

                     Enum_Types : String_Maps.Map;

                     procedure Generate_Conversion_Code_For_Args is
                        procedure Generate_Convert (Cursor : String_Maps.Cursor) is
                           From : constant String := String_Maps.Key (Cursor);
                           To   : constant String := String_Maps.Element (Cursor);
                        begin
                           Put_Line (File, "      function Convert is new Ada.Unchecked_Conversion");
                           Put_Line (File, "        (" & From & ", " & To & ");");
                        end Generate_Convert;
                     begin
                        Enum_Types.Iterate (Generate_Convert'Access);
                     end Generate_Conversion_Code_For_Args;

                     V : Wayland_XML.Request_Child_Vectors.Vector;

                     Max_Name_Length : Natural := Name'Length;

                     function Align (Value : String) return String is (SF.Head (Value, Max_Name_Length, ' '));
                  begin
                     Put_Line (File, "");
                     if Xml_Parser_Utils.Is_New_Id_Argument_Present (Request_Tag) then
                        if Xml_Parser_Utils.Is_Interface_Specified (Request_Tag) then
                           declare
                              Return_Type : constant String := Xml_Parser_Utils.Adaify_Name (Xml_Parser_Utils.Find_Specified_Interface (Request_Tag) & "_Ptr");
                           begin
                              Get_Max_Arg_Length (Request_Tag, V, Max_Name_Length);

                              if Xml_Parser_Utils.Number_Of_Args (Request_Tag) > 1 then
                                 Put_Line (File, "   function " & Subprogram_Name);
                                 Put_Line (File, "     (" & Align (Name) & " : " & Ptr_Name & ";");

                                 for Child of V loop
                                    if Child.Kind_Id = Child_Arg then
                                       Generate_Code_For_Arg
                                         (File,
                                          Interface_Tag,
                                          Child.Arg_Tag.all,
                                          Max_Name_Length,
                                          Child = V.Last_Element,
                                          Enum_Types);
                                    end if;
                                 end loop;

                                 Put_Line (File, " return " & Return_Type);
                                 Put_Line (File, "   is");
                                 Generate_Conversion_Code_For_Args;
                                 if not Enum_Types.Is_Empty then
                                    Put_Line (File, "");
                                 end if;
                              else
                                 if 27 + Subprogram_Name'Length + Name'Length + Ptr_Name'Length + Return_Type'Length <= Max_Line_Length then
                                    Put_Line (File, "   function " & Subprogram_Name & " (" & Name & " : " & Ptr_Name & ") return " & Return_Type & " is");
                                 else
                                    Put_Line (File, "   function " & Subprogram_Name);
                                    Put_Line (File, "     (" & Name & " : " & Ptr_Name & ") return " & Return_Type);
                                    Put_Line (File, "   is");
                                 end if;
                              end if;
                              Generate_Code_Before_Arguments;
                              Generate_Arguments (11, True);
                              Generate_Code_After_Arguments;
                           end;
                        else
                           if Xml_Parser_Utils.Number_Of_Args (Request_Tag) > 1 then
                              Max_Name_Length := Natural'Max (11, Name'Length);
                              Get_Max_Arg_Length (Request_Tag, V, Max_Name_Length);

                              Put_Line (File, "   function " & Subprogram_Name);
                              Put_Line (File, "     (" & Align (Name) & " : " & Ptr_Name & ";");

                              for Child of V loop
                                 if Child.Kind_Id = Child_Arg then
                                    Generate_Code_For_Arg
                                      (File,
                                       Interface_Tag,
                                       Child.Arg_Tag.all,
                                       Max_Name_Length,
                                       False,
                                       Enum_Types);
                                 end if;
                              end loop;

                              Put_Line (File, "      " & Align ("Interface_V") & " : Interface_Ptr;");
                              Put (File, "      " & Align ("New_Id") & " : Unsigned_32) return Proxy_Ptr");
                              if Enum_Types.Is_Empty then
                                 Put_Line (File, " is");
                              else
                                 Put_Line (File, "");
                                 Put_Line (File, "is");
                                 Generate_Conversion_Code_For_Args;
                              end if;
                              Put_Line (File, "   begin");
                              Put_Line (File, "      return Wayland.API.Proxy_Marshal_Constructor_Versioned");
                              Put_Line (File, "        (" & Name & ".all,");
                              Put_Line (File, "         " & Opcode & ",");
                              Put_Line (File, "         Interface_V,");
                              Put_Line (File, "         New_Id,");

                              Generate_Arguments (9, False);

                              Put_Line (File, "         Interface_V.Name,");
                              Put_Line (File, "         New_Id,");
                              Put_Line (File, "         0);");
                              Put_Line (File, "   end " & Subprogram_Name & ";");
                           else
                              Put_Line (File, "   function " & Subprogram_Name & " (" & Name & " : " & Ptr_Name & ") return Proxy_Ptr is");
                              Generate_Code_Before_Arguments;
                              Put (File, "           0");
                              Generate_Code_After_Arguments;
                           end if;
                        end if;
                     else
                        if Xml_Parser_Utils.Number_Of_Args (Request_Tag) > 0 then
                           Get_Max_Arg_Length (Request_Tag, V, Max_Name_Length);

                           Put_Line (File, "   procedure " & Subprogram_Name);
                           Put_Line (File, "     (" & Align (Name) & " : " & Ptr_Name & ";");

                           for Child of V loop
                              if Child.Kind_Id = Child_Arg then
                                 Generate_Code_For_Arg
                                   (File,
                                    Interface_Tag,
                                    Child.Arg_Tag.all,
                                    Max_Name_Length,
                                    Child = Children (Request_Tag).Last_Element,
                                    Enum_Types);
                              end if;
                           end loop;

                           if Enum_Types.Is_Empty then
                              Put_Line (File, " is");
                           else
                              Put_Line (File, "");
                              Put_Line (File, "   is");
                              Generate_Conversion_Code_For_Args;
                           end if;

                           Put_Line (File, "   begin");
                           Put_Line (File, "      Wayland.API.Proxy_Marshal");
                           Put_Line (File, "        (" & Name & ".all,");
                           Put_Line (File, "         " & Opcode & ",");

                           Generate_Arguments (9, False);

                           Put_Line (File, ");");

                           if Xml_Parser_Utils.Is_Request_Destructor (Request_Tag) then
                              Put_Line (File, "");
                              Put_Line (File, "      Wayland.API.Proxy_Destroy (" & Name & ".all);");
                           end if;
                           Put_Line (File, "   end " & Subprogram_Name & ";");
                        else
                           Put_Line (File, "   procedure " & Subprogram_Name & " (" & Name & " : " & Ptr_Name & ") is");
                           Put_Line (File, "   begin");
                           Put_Line (File, "      Wayland.API.Proxy_Marshal (" & Name & ".all, " & Opcode & ");");
                           if Xml_Parser_Utils.Is_Request_Destructor (Request_Tag) then
                              Put_Line (File, "");
                              Put_Line (File, "      Wayland.API.Proxy_Destroy (" & Name & ".all);");
                           end if;
                           Put_Line (File, "   end " & Subprogram_Name & ";");
                        end if;
                     end if;
                  end Generate_Code_For_Subprogram_Implementation;

               begin
                  Iterate_Over_Requests (Interface_Tag, Generate_Code_For_Subprogram_Implementation'Access);
               end Generate_Code_For_Requests;

               Name : constant String :=
                 Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Interface_Tag));
            begin
               if Xml_Parser_Utils.Exists_Any_Event_Tag (Interface_Tag) then
                  Put_Line (File, "");
                  Generate_Code_For_Add_Listener_Subprogram_Implementations (Name);
               end if;

               Put_Line (File, "");
               Generate_Code_For_Set_User_Data_Subprogram_Implementations (Name);
               Put_Line (File, "");
               Generate_Code_For_Get_User_Data_Subprogram_Implementations (Name);
               Put_Line (File, "");
               Generate_Code_For_Get_ID_Subprogram_Implementations (Name);
               Put_Line (File, "");
               Generate_Code_For_Get_Version_Subprogram_Implementations (Name);

               if Wayland_XML.Name (Interface_Tag) /= "wl_display"
                 and then Xml_Parser_Utils.Get_Destructor (Interface_Tag) = ""
               then
                  Put_Line (File, "");
                  Generate_Code_For_Destroy_Subprogram_Implementations (Name);
               end if;

               Generate_Code_For_Requests;
            end Handle_Interface;

         begin
            Iterate_Over_Interfaces (Handle_Interface'Access);
         end Generate_Code_For_Protocol_Tag_Children;
      begin
         Generate_Code_For_Protocol_Tag_Children;
      end Create_Wl_Thin_Body_File;

      procedure Generate_Manually_Edited_Code (Protocol_Name : String) is
         procedure Generate_Body_Utility_Functions (Name : String) is
         begin
            Put_Line (File, "");
            Put_Line (File, "   function Get_ID (Object : " & Name & ") return Unsigned_32 is");
            Put_Line (File, "     (Thin." & Name & "_Get_ID (Object.Proxy));");
            Put_Line (File, "");
            Put_Line (File, "   function Get_Version (Object : " & Name & ") return Unsigned_32 is");
            Put_Line (File, "     (Thin." & Name & "_Get_Version (Object.Proxy));");
            Put_Line (File, "");
            Put_Line (File, "   function Has_Proxy (Object : " & Name & ") return Boolean is");
            Put_Line (File, "     (Object.Proxy /= null);");
            Put_Line (File, "");
            Put_Line (File, "   function ""="" (Left, Right : " & Name & "'Class) return Boolean is");
            Put_Line (File, "     (Left.Proxy = Right.Proxy);");
         end Generate_Body_Utility_Functions;

         procedure Generate_Body_Destroy_Subprogram
           (Interface_Tag : aliased Wayland_XML.Interface_Tag)
         is
            Name : constant String :=
              Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Interface_Tag));

            Destructor_Name : constant String :=
              Xml_Parser_Utils.Get_Destructor (Interface_Tag);

            Request_Name : constant String :=
              Xml_Parser_Utils.Adaify_Name
                (if Destructor_Name /= "" then Destructor_Name else "Destroy");
         begin
            Put_Line (File, "");
            Put_Line (File, "   procedure " & Request_Name & " (Object : in out " & Name & ") is");
            Put_Line (File, "   begin");
            Put_Line (File, "      if Object.Proxy /= null then");
            Put_Line (File, "         Thin." & Name & "_" & Request_Name & " (Object.Proxy);");
            Put_Line (File, "         Object.Proxy := null;");
            Put_Line (File, "      end if;");
            Put_Line (File, "   end " & Request_Name & ";");
         end Generate_Body_Destroy_Subprogram;

         procedure Generate_Body_User_Data_Subprogram (Name : String) is
         begin
            Put_Line (File, "");
            Put_Line (File, "   package body " & Name & "_User_Data is");
            Put_Line (File, "");
            Put_Line (File, "      package Conversion is new System.Address_To_Access_Conversions (Data_Type);");
            Put_Line (File, "");
            Put_Line (File, "      function Set_Data");
            Put_Line (File, "        (Object  : " & Name & "'Class;");
            Put_Line (File, "         Subject : aliased in out Data_Type) return Call_Result_Code is");
            Put_Line (File, "      begin");
            Put_Line (File, "         Thin." & Name & "_Set_User_Data");
            Put_Line (File, "           (Object.Proxy, Conversion.To_Address (Subject'Access));");
            Put_Line (File, "         return Success;");
            Put_Line (File, "      end Set_Data;");
            Put_Line (File, "");
            Put_Line (File, "      procedure Set_Data");
            Put_Line (File, "        (Object  : " & Name & "'Class;");
            Put_Line (File, "         Subject : aliased in out Data_Type)");
            Put_Line (File, "      is");
            Put_Line (File, "         Result : constant Call_Result_Code := Set_Data (Object, Subject);");
            Put_Line (File, "      begin");
            Put_Line (File, "         pragma Assert (Result = Success);");
            Put_Line (File, "      end Set_Data;");
            Put_Line (File, "");
            Put_Line (File, "      function Get_Data");
            Put_Line (File, "        (Object  : " & Name & "'Class) return access Data_Type");
            Put_Line (File, "      is");
            Put_Line (File, "         Subject_Address : constant Void_Ptr :=");
            Put_Line (File, "           Thin." & Name & "_Get_User_Data (Object.Proxy);");
            Put_Line (File, "");
            Put_Line (File, "         use type Void_Ptr;");
            Put_Line (File, "      begin");
            Put_Line (File, "         if Subject_Address /= System.Null_Address then");
            Put_Line (File, "            return Conversion.To_Pointer (Subject_Address).all'Access;");
            Put_Line (File, "         else");
            Put_Line (File, "            return null;");
            Put_Line (File, "         end if;");
            Put_Line (File, "      end Get_Data;");
            Put_Line (File, "");
            Put_Line (File, "   end " & Name & "_User_Data;");
         end Generate_Body_User_Data_Subprogram;

         procedure Generate_Body_Bind_Subprogram (Name : String) is
         begin
            Put_Line (File, "");
            Put_Line (File, "   procedure Bind");
            Put_Line (File, "     (Object   : in out " & Name & ";");
            Put_Line (File, "      Registry : Client.Registry'Class;");
            Put_Line (File, "      Id       : Unsigned_32;");
            Put_Line (File, "      Version  : Unsigned_32)");
            Put_Line (File, "   is");
            Put_Line (File, "      Proxy : constant Thin.Proxy_Ptr :=");
            Put_Line (File, "        Thin.Proxy_Ptr (Registry.Bind (" & Name & "_Interface, Id, Version));");
            Put_Line (File, "   begin");
            Put_Line (File, "      if Proxy /= null then");
            Put_Line (File, "         Object.Proxy := Proxy.all'Access;");
            Put_Line (File, "      end if;");
            Put_Line (File, "   end Bind;");
         end Generate_Body_Bind_Subprogram;

         procedure Generate_Prefix_Body_Events (Name : String) is
         begin
            Put_Line (File, "");
            Put_Line (File, "   package body " & Name & "_Events is");
            Put_Line (File, "");
            Put_Line (File, "      package Conversion is new System.Address_To_Access_Conversions");
            Put_Line (File, "        (" & Name & "'Class);");
            Put_Line (File, "");
         end Generate_Prefix_Body_Events;

         procedure Generate_Suffix_Body_Events (Name : String) is
            function Align (Value : String) return String is (SF.Head (Value, Natural'Max (8, Name'Length), ' '));
         begin
            Put_Line (File, "      function Subscribe");
            Put_Line (File, "        (Object : aliased in out " & Name & "'Class) return Call_Result_Code");
            Put_Line (File, "      is");
            Put_Line (File, "         I : int;");
            Put_Line (File, "      begin");
            Put_Line (File, "         I := Thin." & Name & "_Add_Listener");
            Put_Line (File, "           (" & Align (Name)       & " => Object.Proxy,");
            Put_Line (File, "            " & Align ("Listener") & " => Listener'Access,");
            Put_Line (File, "            " & Align ("Data")     & " => Conversion.To_Address (Object'Access));");
            Put_Line (File, "         return (if I = 0 then Success else Error);");
            Put_Line (File, "      end Subscribe;");
            Put_Line (File, "");
            Put_Line (File, "      procedure Subscribe");
            Put_Line (File, "        (Object : aliased in out " & Name & "'Class)");
            Put_Line (File, "      is");
            Put_Line (File, "         Result : constant Call_Result_Code := Subscribe (Object);");
            Put_Line (File, "      begin");
            Put_Line (File, "         pragma Assert (Result = Success);");
            Put_Line (File, "      end Subscribe;");
            Put_Line (File, "   end " & Name & "_Events;");
         end Generate_Suffix_Body_Events;

         procedure Handle_Interface_Client_Prefix is
         begin
            Put_Line (File, "");
            Put_Line (File, "   function Bind");
            Put_Line (File, "     (Object  : Registry;");
            Put_Line (File, "      Iface   : Interface_Type;");
            Put_Line (File, "      Id      : Unsigned_32;");
            Put_Line (File, "      Version : Unsigned_32) return Secret_Proxy");
            Put_Line (File, "   is");
            Put_Line (File, "      Proxy : constant Thin.Proxy_Ptr :=");
            Put_Line (File, "        Thin.Registry_Bind");
            Put_Line (File, "          (Registry    => Object.Proxy,");
            Put_Line (File, "           Name        => Id,");
            Put_Line (File, "           Interface_V => Iface.My_Interface,");
            Put_Line (File, "           New_Id      => Version);");
            Put_Line (File, "   begin");
            Put_Line (File, "      return Secret_Proxy (Proxy);");
            Put_Line (File, "   end Bind;");
         end Handle_Interface_Client_Prefix;

         procedure Handle_Interface_Client_Suffix is
         begin
            Put_Line (File, "");
            Put_Line (File, "   procedure Connect (Object : in out Display) is");
            Put_Line (File, "   begin");
            Put_Line (File, "      Object.Proxy := Thin.Display_Connect;");
            Put_Line (File, "");
            Put_Line (File, "      if Object.Proxy /= null");
            Put_Line (File, "        and then Wayland.API.Display_Get_File_Descriptor (Object.Proxy) = -1");
            Put_Line (File, "      then");
            Put_Line (File, "         raise Program_Error;");
            Put_Line (File, "      end if;");
            Put_Line (File, "   end Connect;");
            Put_Line (File, "");
            Put_Line (File, "   procedure Disconnect (Object : in out Display) is");
            Put_Line (File, "   begin");
            Put_Line (File, "      if Object.Proxy /= null then");
            Put_Line (File, "         Thin.Display_Disconnect (Object.Proxy);");
            Put_Line (File, "      end if;");
            Put_Line (File, "   end Disconnect;");
            Put_Line (File, "");
            Put_Line (File, "   function Check_For_Events");
            Put_Line (File, "     (Object  : Display;");
            Put_Line (File, "      Timeout : Duration) return Check_For_Events_Status");
            Put_Line (File, "   is");
            Put_Line (File, "      Descriptor : constant Integer :=");
            Put_Line (File, "        Integer (Wayland.API.Display_Get_File_Descriptor (Object.Proxy));");
            Put_Line (File, "      Result : constant Integer := Wayland.Posix.Poll (Descriptor, Timeout);");
            Put_Line (File, "   begin");
            Put_Line (File, "      case Result is");
            Put_Line (File, "         when 1 .. Integer'Last   => return Events_Need_Processing;");
            Put_Line (File, "         when 0                   => return No_Events;");
            Put_Line (File, "         when Integer'First .. -1 => return Error;");
            Put_Line (File, "      end case;");
            Put_Line (File, "   end Check_For_Events;");
            Put_Line (File, "");
            Put_Line (File, "   procedure Get_Registry (Object   : Display;");
            Put_Line (File, "                           Registry : in out Client.Registry'Class) is");
            Put_Line (File, "   begin");
            Put_Line (File, "      Registry.Proxy := Thin.Display_Get_Registry (Object.Proxy);");
            Put_Line (File, "   end Get_Registry;");
            Put_Line (File, "");
            Put_Line (File, "   function Flush (Object : Display) return Optional_Result is");
            Put_Line (File, "      Descriptor : constant Integer :=");
            Put_Line (File, "        Integer (Wayland.API.Display_Get_File_Descriptor (Object.Proxy));");
            Put_Line (File, "   begin");
            Put_Line (File, "      while True loop");
            Put_Line (File, "         declare");
            Put_Line (File, "            Result : constant Integer := Wayland.API.Display_Flush (Object.Proxy);");
            Put_Line (File, "            Error_Again : constant := 11;");
            Put_Line (File, "         begin");
            Put_Line (File, "            if Result /= -1 or else Wayland.Posix.Error_Number /= Error_Again then");
            Put_Line (File, "               if Result /= -1 then");
            Put_Line (File, "                  return (Is_Success => True, Count => Result);");
            Put_Line (File, "               else");
            Put_Line (File, "                  return (Is_Success => False);");
            Put_Line (File, "               end if;");
            Put_Line (File, "            end if;");
            Put_Line (File, "");
            Put_Line (File, "            if Wayland.Posix.Poll (Descriptor, Duration'Last, Wayland.Posix.Output) = -1 then");
            Put_Line (File, "               return (Is_Success => False);");
            Put_Line (File, "            end if;");
            Put_Line (File, "         end;");
            Put_Line (File, "      end loop;");
            Put_Line (File, "      raise Program_Error;");
            Put_Line (File, "   end Flush;");
            Put_Line (File, "");
            Put_Line (File, "   procedure Flush (Object : Display) is");
            Put_Line (File, "      Result : constant Optional_Result := Object.Flush;");
            Put_Line (File, "   begin");
            Put_Line (File, "      if not Result.Is_Success then");
            Put_Line (File, "         raise Program_Error;");
            Put_Line (File, "      end if;");
            Put_Line (File, "   end Flush;");
            Put_Line (File, "");
            Put_Line (File, "   function Dispatch (Object : Display) return Optional_Result is");
            Put_Line (File, "      Result : constant Integer := Wayland.API.Display_Dispatch (Object.Proxy);");
            Put_Line (File, "   begin");
            Put_Line (File, "      if Result /= -1 then");
            Put_Line (File, "         return (Is_Success => True, Count => Result);");
            Put_Line (File, "      else");
            Put_Line (File, "         return (Is_Success => False);");
            Put_Line (File, "      end if;");
            Put_Line (File, "   end Dispatch;");
            Put_Line (File, "");
            Put_Line (File, "   procedure Dispatch (Object : Display) is");
            Put_Line (File, "      Result : constant Optional_Result := Object.Dispatch;");
            Put_Line (File, "   begin");
            Put_Line (File, "      if not Result.Is_Success then");
            Put_Line (File, "         raise Program_Error;");
            Put_Line (File, "      end if;");
            Put_Line (File, "   end Dispatch;");
            Put_Line (File, "");
            Put_Line (File, "   function Dispatch_Pending (Object : Display) return Optional_Result is");
            Put_Line (File, "      Result : constant Integer := Wayland.API.Display_Dispatch_Pending (Object.Proxy);");
            Put_Line (File, "   begin");
            Put_Line (File, "      if Result /= -1 then");
            Put_Line (File, "         return (Is_Success => True, Count => Result);");
            Put_Line (File, "      else");
            Put_Line (File, "         return (Is_Success => False);");
            Put_Line (File, "      end if;");
            Put_Line (File, "   end Dispatch_Pending;");
            Put_Line (File, "");
            Put_Line (File, "   function Prepare_Read (Object : Display) return Call_Result_Code is");
            Put_Line (File, "      Result : constant Integer := Wayland.API.Display_Prepare_Read (Object.Proxy);");
            Put_Line (File, "   begin");
            Put_Line (File, "      return (if Result = 0 then Success else Error);");
            Put_Line (File, "   end Prepare_Read;");
            Put_Line (File, "");
            Put_Line (File, "   procedure Cancel_Read (Object : Display) is");
            Put_Line (File, "   begin");
            Put_Line (File, "      Wayland.API.Display_Cancel_Read (Object.Proxy);");
            Put_Line (File, "   end Cancel_Read;");
            Put_Line (File, "");
            Put_Line (File, "   function Read_Events (Object : Display) return Call_Result_Code is");
            Put_Line (File, "      Result : constant Integer");
            Put_Line (File, "        := Wayland.API.Display_Read_Events (Object.Proxy);");
            Put_Line (File, "   begin");
            Put_Line (File, "      return (if Result = 0 then Success else Error);");
            Put_Line (File, "   end Read_Events;");
            Put_Line (File, "");
            Put_Line (File, "   function Roundtrip (Object : Display) return Integer is");
            Put_Line (File, "   begin");
            Put_Line (File, "      return Wayland.API.Display_Roundtrip (Object.Proxy);");
            Put_Line (File, "   end Roundtrip;");
            Put_Line (File, "");
            Put_Line (File, "   procedure Roundtrip (Object : Display) is");
            Put_Line (File, "      I : Integer;");
            Put_Line (File, "   begin");
            Put_Line (File, "      I := Object.Roundtrip;");
            Put_Line (File, "      pragma Assert (I /= -1);");
            Put_Line (File, "   end Roundtrip;");
            Put_Line (File, "");
            Put_Line (File, "   procedure Create_Surface (Object  : Compositor;");
            Put_Line (File, "                             Surface : in out Client.Surface'Class) is");
            Put_Line (File, "   begin");
            Put_Line (File, "      Surface.Proxy := Thin.Compositor_Create_Surface (Object.Proxy);");
            Put_Line (File, "   end Create_Surface;");
            Put_Line (File, "");
            Put_Line (File, "   procedure Create_Region (Object : Compositor;");
            Put_Line (File, "                            Region : in out Client.Region'Class) is");
            Put_Line (File, "   begin");
            Put_Line (File, "      Region.Proxy := Thin.Compositor_Create_Region (Object.Proxy);");
            Put_Line (File, "   end Create_Region;");
            Put_Line (File, "");
            Put_Line (File, "   procedure Get_Pointer (Object  : Seat;");
            Put_Line (File, "                          Pointer : in out Client.Pointer'Class) is");
            Put_Line (File, "   begin");
            Put_Line (File, "      Pointer.Proxy := Thin.Seat_Get_Pointer (Object.Proxy);");
            Put_Line (File, "   end Get_Pointer;");
            Put_Line (File, "");
            Put_Line (File, "   procedure Get_Keyboard (Object   : Seat;");
            Put_Line (File, "                           Keyboard : in out Client.Keyboard'Class) is");
            Put_Line (File, "   begin");
            Put_Line (File, "      Keyboard.Proxy := Thin.Seat_Get_Keyboard (Object.Proxy);");
            Put_Line (File, "   end Get_Keyboard;");
            Put_Line (File, "");
            Put_Line (File, "   procedure Get_Touch (Object : Seat;");
            Put_Line (File, "                        Touch  : in out Client.Touch'Class) is");
            Put_Line (File, "   begin");
            Put_Line (File, "      Touch.Proxy := Thin.Seat_Get_Touch (Object.Proxy);");
            Put_Line (File, "   end Get_Touch;");
            Put_Line (File, "");
            Put_Line (File, "   procedure Create_Pool");
            Put_Line (File, "     (Object          : Shm;");
            Put_Line (File, "      File_Descriptor : Wayland.File_Descriptor;");
            Put_Line (File, "      Size            : Positive;");
            Put_Line (File, "      Pool            : in out Client.Shm_Pool'Class) is");
            Put_Line (File, "   begin");
            Put_Line (File, "      Pool.Proxy := Thin.Shm_Create_Pool");
            Put_Line (File, "        (Object.Proxy,");
            Put_Line (File, "         File_Descriptor,");
            Put_Line (File, "         Size);");
            Put_Line (File, "   end Create_Pool;");
            Put_Line (File, "");
            Put_Line (File, "   procedure Create_Buffer (Object : Shm_Pool;");
            Put_Line (File, "                            Offset : Natural;");
            Put_Line (File, "                            Width  : Natural;");
            Put_Line (File, "                            Height : Natural;");
            Put_Line (File, "                            Stride : Natural;");
            Put_Line (File, "                            Format : Shm_Format;");
            Put_Line (File, "                            Buffer : in out Client.Buffer'Class) is");
            Put_Line (File, "   begin");
            Put_Line (File, "      Buffer.Proxy := Thin.Shm_Pool_Create_Buffer");
            Put_Line (File, "        (Object.Proxy, Offset, Width, Height, Stride, Format);");
            Put_Line (File, "   end Create_Buffer;");
            Put_Line (File, "");
            Put_Line (File, "   procedure Resize (Object : Shm_Pool;");
            Put_Line (File, "                     Size   : Positive) is");
            Put_Line (File, "   begin");
            Put_Line (File, "      Thin.Shm_Pool_Resize (Object.Proxy, Size);");
            Put_Line (File, "   end Resize;");
            Put_Line (File, "");
            Put_Line (File, "   procedure Do_Accept (Object    : Data_Offer;");
            Put_Line (File, "                        Serial    : Unsigned_32;");
            Put_Line (File, "                        Mime_Type : String)");
            Put_Line (File, "   is");
            Put_Line (File, "      MT : Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.New_String (Mime_Type);");
            Put_Line (File, "   begin");
            Put_Line (File, "      Thin.Data_Offer_Accept (Object.Proxy, Serial, MT);");
            Put_Line (File, "      Interfaces.C.Strings.Free (MT);");
            Put_Line (File, "   end Do_Accept;");
            Put_Line (File, "");
            Put_Line (File, "   procedure Do_Not_Accept (Object : Data_Offer;");
            Put_Line (File, "                            Serial : Unsigned_32) is");
            Put_Line (File, "   begin");
            Put_Line (File, "      Thin.Data_Offer_Accept (Object.Proxy, Serial, Interfaces.C.Strings.Null_Ptr);");
            Put_Line (File, "   end Do_Not_Accept;");
            Put_Line (File, "");
            Put_Line (File, "   procedure Receive (Object          : Data_Offer;");
            Put_Line (File, "                      Mime_Type       : String;");
            Put_Line (File, "                      File_Descriptor : Wayland.File_Descriptor)");
            Put_Line (File, "   is");
            Put_Line (File, "      MT : Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.New_String (Mime_Type);");
            Put_Line (File, "   begin");
            Put_Line (File, "      Thin.Data_Offer_Receive (Object.Proxy, MT, File_Descriptor);");
            Put_Line (File, "      Interfaces.C.Strings.Free (MT);");
            Put_Line (File, "   end Receive;");
            Put_Line (File, "");
            Put_Line (File, "   procedure Finish (Object : Data_Offer) is");
            Put_Line (File, "   begin");
            Put_Line (File, "      Thin.Data_Offer_Finish (Object.Proxy);");
            Put_Line (File, "   end Finish;");
            Put_Line (File, "");
            Put_Line (File, "   procedure Set_Actions (Object           : Data_Offer;");
            Put_Line (File, "                          Dnd_Actions      : Data_Device_Manager_Dnd_Action;");
            Put_Line (File, "                          Preferred_Action : Data_Device_Manager_Dnd_Action) is");
            Put_Line (File, "   begin");
            Put_Line (File, "      Thin.Data_Offer_Set_Actions");
            Put_Line (File, "        (Object.Proxy, Dnd_Actions, Preferred_Action);");
            Put_Line (File, "   end Set_Actions;");
            Put_Line (File, "");
            Put_Line (File, "   procedure Attach (Object : Surface;");
            Put_Line (File, "                     Buffer : Client.Buffer'Class;");
            Put_Line (File, "                     X, Y   : Integer) is");
            Put_Line (File, "   begin");
            Put_Line (File, "      Thin.Surface_Attach (Object.Proxy, Buffer.Proxy, X, Y);");
            Put_Line (File, "   end Attach;");
            Put_Line (File, "");
            Put_Line (File, "   procedure Damage (Object : Surface;");
            Put_Line (File, "                     X, Y   : Integer;");
            Put_Line (File, "                     Width  : Natural;");
            Put_Line (File, "                     Height : Natural) is");
            Put_Line (File, "   begin");
            Put_Line (File, "      Thin.Surface_Damage (Object.Proxy, X, Y, Width, Height);");
            Put_Line (File, "   end Damage;");
            Put_Line (File, "");
            Put_Line (File, "   function Frame (Object : Surface) return Callback'Class is");
            Put_Line (File, "   begin");
            Put_Line (File, "      return Result : Callback do");
            Put_Line (File, "         Result.Proxy := Thin.Surface_Frame (Object.Proxy);");
            Put_Line (File, "      end return;");
            Put_Line (File, "   end Frame;");
            Put_Line (File, "");
            Put_Line (File, "   procedure Set_Opaque_Region (Object : Surface;");
            Put_Line (File, "                                Region : Client.Region'Class) is");
            Put_Line (File, "   begin");
            Put_Line (File, "      Thin.Surface_Set_Opaque_Region (Object.Proxy, Region.Proxy);");
            Put_Line (File, "   end Set_Opaque_Region;");
            Put_Line (File, "");
            Put_Line (File, "   procedure Set_Input_Region (Object : Surface;");
            Put_Line (File, "                               Region : Client.Region'Class) is");
            Put_Line (File, "   begin");
            Put_Line (File, "      Thin.Surface_Set_Input_Region (Object.Proxy, Region.Proxy);");
            Put_Line (File, "");
            Put_Line (File, "   end Set_Input_Region;");
            Put_Line (File, "");
            Put_Line (File, "   procedure Commit (Object : Surface) is");
            Put_Line (File, "   begin");
            Put_Line (File, "      Thin.Surface_Commit (Object.Proxy);");
            Put_Line (File, "   end Commit;");
            Put_Line (File, "");
            Put_Line (File, "   procedure Set_Buffer_Transform (Object    : Surface;");
            Put_Line (File, "                                   Transform : Output_Transform) is");
            Put_Line (File, "   begin");
            Put_Line (File, "      Thin.Surface_Set_Buffer_Transform (Object.Proxy, Transform);");
            Put_Line (File, "   end Set_Buffer_Transform;");
            Put_Line (File, "");
            Put_Line (File, "   procedure Set_Buffer_Scale (Object : Surface;");
            Put_Line (File, "                               Scale  : Positive) is");
            Put_Line (File, "   begin");
            Put_Line (File, "      Thin.Surface_Set_Buffer_Scale (Object.Proxy, Scale);");
            Put_Line (File, "   end Set_Buffer_Scale;");
            Put_Line (File, "");
            Put_Line (File, "   procedure Damage_Buffer (Object : Surface;");
            Put_Line (File, "                            X, Y   : Integer;");
            Put_Line (File, "                            Width  : Natural;");
            Put_Line (File, "                            Height : Natural) is");
            Put_Line (File, "   begin");
            Put_Line (File, "      Thin.Surface_Damage_Buffer");
            Put_Line (File, "        (Object.Proxy, X, Y, Width, Height);");
            Put_Line (File, "   end Damage_Buffer;");
            Put_Line (File, "");
            Put_Line (File, "   function Sync (Object : Display) return Callback'Class is");
            Put_Line (File, "   begin");
            Put_Line (File, "      return Callback : Client.Callback do");
            Put_Line (File, "         Callback.Proxy := Thin.Display_Sync (Object.Proxy);");
            Put_Line (File, "      end return;");
            Put_Line (File, "   end Sync;");
            Put_Line (File, "");
            Put_Line (File, "   procedure Set_Cursor (Object    : Pointer;");
            Put_Line (File, "                         Serial    : Unsigned_32;");
            Put_Line (File, "                         Surface   : Client.Surface'Class;");
            Put_Line (File, "                         Hotspot_X : Integer;");
            Put_Line (File, "                         Hotspot_Y : Integer) is");
            Put_Line (File, "   begin");
            Put_Line (File, "      Thin.Pointer_Set_Cursor (Object.Proxy,");
            Put_Line (File, "                                  Serial,");
            Put_Line (File, "                                  Surface.Proxy,");
            Put_Line (File, "                                  Hotspot_X,");
            Put_Line (File, "                                  Hotspot_Y);");
            Put_Line (File, "   end Set_Cursor;");
            Put_Line (File, "");
            Put_Line (File, "   procedure Add (Object : Region;");
            Put_Line (File, "                  X, Y   : Integer;");
            Put_Line (File, "                  Width  : Natural;");
            Put_Line (File, "                  Height : Natural) is");
            Put_Line (File, "   begin");
            Put_Line (File, "      Thin.Region_Add (Object.Proxy, X, Y, Width, Height);");
            Put_Line (File, "   end Add;");
            Put_Line (File, "");
            Put_Line (File, "   procedure Subtract (Object : Region;");
            Put_Line (File, "                       X, Y   : Integer;");
            Put_Line (File, "                       Width  : Natural;");
            Put_Line (File, "                       Height : Natural) is");
            Put_Line (File, "   begin");
            Put_Line (File, "      Thin.Region_Subtract (Object.Proxy, X, Y, Width, Height);");
            Put_Line (File, "   end Subtract;");
            Put_Line (File, "");
            Put_Line (File, "   procedure Get_Subsurface");
            Put_Line (File, "     (Object     : Subcompositor;");
            Put_Line (File, "      Surface    : Client.Surface'Class;");
            Put_Line (File, "      Parent     : Client.Surface'Class;");
            Put_Line (File, "      Subsurface : in out Client.Subsurface'Class) is");
            Put_Line (File, "   begin");
            Put_Line (File, "      Subsurface.Proxy :=");
            Put_Line (File, "        Thin.Subcompositor_Get_Subsurface");
            Put_Line (File, "          (Object.Proxy,");
            Put_Line (File, "           Surface.Proxy,");
            Put_Line (File, "           Parent.Proxy);");
            Put_Line (File, "   end Get_Subsurface;");
            Put_Line (File, "");
            Put_Line (File, "   procedure Set_Position (Object : Subsurface;");
            Put_Line (File, "                           X, Y   : Integer) is");
            Put_Line (File, "   begin");
            Put_Line (File, "      Thin.Subsurface_Set_Position (Object.Proxy, X, Y);");
            Put_Line (File, "   end Set_Position;");
            Put_Line (File, "");
            Put_Line (File, "   procedure Place_Above (Object  : Subsurface;");
            Put_Line (File, "                          Sibling : Surface'Class) is");
            Put_Line (File, "   begin");
            Put_Line (File, "      Thin.Subsurface_Place_Above (Object.Proxy, Sibling.Proxy);");
            Put_Line (File, "   end Place_Above;");
            Put_Line (File, "");
            Put_Line (File, "   procedure Place_Below (Object  : Subsurface;");
            Put_Line (File, "                          Sibling : Surface'Class) is");
            Put_Line (File, "   begin");
            Put_Line (File, "      Thin.Subsurface_Place_Below (Object.Proxy, Sibling.Proxy);");
            Put_Line (File, "   end Place_Below;");
            Put_Line (File, "");
            Put_Line (File, "   procedure Set_Sync (Object : Subsurface) is");
            Put_Line (File, "   begin");
            Put_Line (File, "      Thin.Subsurface_Set_Sync (Object.Proxy);");
            Put_Line (File, "   end Set_Sync;");
            Put_Line (File, "");
            Put_Line (File, "   procedure Set_Desync (Object : Subsurface) is");
            Put_Line (File, "   begin");
            Put_Line (File, "      Thin.Subsurface_Set_Desync (Object.Proxy);");
            Put_Line (File, "   end Set_Desync;");
            Put_Line (File, "");
            Put_Line (File, "   procedure Offer (Object    : Data_Source;");
            Put_Line (File, "                    Mime_Type : String)");
            Put_Line (File, "   is");
            Put_Line (File, "      MT : Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.New_String (Mime_Type);");
            Put_Line (File, "   begin");
            Put_Line (File, "      Thin.Data_Source_Offer (Object.Proxy, MT);");
            Put_Line (File, "      Interfaces.C.Strings.Free (MT);");
            Put_Line (File, "   end Offer;");
            Put_Line (File, "");
            Put_Line (File, "   procedure Set_Actions (Object      : Data_Source;");
            Put_Line (File, "                          Dnd_Actions : Data_Device_Manager_Dnd_Action) is");
            Put_Line (File, "   begin");
            Put_Line (File, "      Thin.Data_Source_Set_Actions (Object.Proxy, Dnd_Actions);");
            Put_Line (File, "   end Set_Actions;");
            Put_Line (File, "");
            Put_Line (File, "   procedure Start_Drag (Object : Data_Device;");
            Put_Line (File, "                         Source : Data_Source'class;");
            Put_Line (File, "                         Origin : Surface'class;");
            Put_Line (File, "                         Icon   : Surface'class;");
            Put_Line (File, "                         Serial : Unsigned_32) is");
            Put_Line (File, "   begin");
            Put_Line (File, "      Thin.Data_Device_Start_Drag (Object.Proxy,");
            Put_Line (File, "                                      Source.Proxy,");
            Put_Line (File, "                                      Origin.Proxy,");
            Put_Line (File, "                                      Icon.Proxy,");
            Put_Line (File, "                                      Serial);");
            Put_Line (File, "   end Start_Drag;");
            Put_Line (File, "");
            Put_Line (File, "   procedure Set_Selection (Object : Data_Device;");
            Put_Line (File, "                            Source : Data_Source'Class;");
            Put_Line (File, "                            Serial : Unsigned_32) is");
            Put_Line (File, "   begin");
            Put_Line (File, "      Thin.Data_Device_Set_Selection (Object.Proxy, Source.Proxy, Serial);");
            Put_Line (File, "   end Set_Selection;");
            Put_Line (File, "");
            Put_Line (File, "   procedure Create_Data_Source (Object : Data_Device_Manager;");
            Put_Line (File, "                                 Source : in out Data_Source'Class) is");
            Put_Line (File, "   begin");
            Put_Line (File, "      Source.Proxy := Thin.Data_Device_Manager_Create_Data_Source");
            Put_Line (File, "        (Object.Proxy);");
            Put_Line (File, "   end Create_Data_Source;");
            Put_Line (File, "");
            Put_Line (File, "   procedure Get_Data_Device (Object : Data_Device_Manager;");
            Put_Line (File, "                              Seat   : Client.Seat'Class;");
            Put_Line (File, "                              Device : in out Data_Device'Class) is");
            Put_Line (File, "   begin");
            Put_Line (File, "      Device.Proxy := Thin.Data_Device_Manager_Get_Data_Device");
            Put_Line (File, "        (Object.Proxy, Seat.Proxy);");
            Put_Line (File, "   end Get_Data_Device;");
         end Handle_Interface_Client_Suffix;

         procedure Handle_Interface_Client
           (Interface_Tag : aliased Wayland_XML.Interface_Tag)
         is
            Name : constant String
              := Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Interface_Tag));
         begin
            if Name in "Compositor" | "Seat" | "Shm" | "Output" | "Data_Device_Manager" then
               Generate_Body_Bind_Subprogram (Name);
            end if;
         end Handle_Interface_Client;

         procedure Handle_Interface_Events_Client
           (Interface_Tag : aliased Wayland_XML.Interface_Tag)
         is
            Name : constant String
              := Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Interface_Tag));
         begin
            if not Exists_Events (Interface_Tag) then
               return;
            end if;

            Generate_Prefix_Body_Events (Name);

            if Name = "Display" then
               Put_Line (File, "      procedure Internal_Error");
               Put_Line (File, "        (Data      : Void_Ptr;");
               Put_Line (File, "         Display   : Thin.Display_Ptr;");
               Put_Line (File, "         Object_Id : Void_Ptr;");
               Put_Line (File, "         Code      : Unsigned_32;");
               Put_Line (File, "         Message   : chars_ptr)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Delete_Id");
               Put_Line (File, "        (Data    : Void_Ptr;");
               Put_Line (File, "         Display : Thin.Display_Ptr;");
               Put_Line (File, "         Id      : Unsigned_32)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Error");
               Put_Line (File, "        (Data      : Void_Ptr;");
               Put_Line (File, "         Display   : Thin.Display_Ptr;");
               Put_Line (File, "         Object_Id : Void_Ptr;");
               Put_Line (File, "         Code      : Unsigned_32;");
               Put_Line (File, "         Message   : chars_ptr)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = Display);");
               Put_Line (File, "");
               Put_Line (File, "         M : constant String := Interfaces.C.Strings.Value (Message);");
               Put_Line (File, "      begin");
               Put_Line (File, "         Error (Conversion.To_Pointer (Data).all, Code, M);");
               Put_Line (File, "      end Internal_Error;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Delete_Id");
               Put_Line (File, "        (Data    : Void_Ptr;");
               Put_Line (File, "         Display : Thin.Display_Ptr;");
               Put_Line (File, "         Id      : Unsigned_32)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = Display);");
               Put_Line (File, "      begin");
               Put_Line (File, "         Delete_Id (Conversion.To_Pointer (Data).all, Id);");
               Put_Line (File, "      end Internal_Delete_Id;");
               Put_Line (File, "");
               Put_Line (File, "      Listener : aliased Thin.Display_Listener_T :=");
               Put_Line (File, "        (Error     => Internal_Error'Unrestricted_Access,");
               Put_Line (File, "         Delete_Id => Internal_Delete_Id'Unrestricted_Access);");
               Put_Line (File, "");
            elsif Name = "Registry" then
               Put_Line (File, "      procedure Internal_Object_Added");
               Put_Line (File, "        (Data        : Void_Ptr;");
               Put_Line (File, "         Registry    : Thin.Registry_Ptr;");
               Put_Line (File, "         Id          : Unsigned_32;");
               Put_Line (File, "         Interface_V : chars_ptr;");
               Put_Line (File, "         Version     : Unsigned_32)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Object_Removed");
               Put_Line (File, "        (Data     : Void_Ptr;");
               Put_Line (File, "         Registry : Thin.Registry_Ptr;");
               Put_Line (File, "         Id       : Unsigned_32)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Object_Added");
               Put_Line (File, "        (Data        : Void_Ptr;");
               Put_Line (File, "         Registry    : Thin.Registry_Ptr;");
               Put_Line (File, "         Id          : Unsigned_32;");
               Put_Line (File, "         Interface_V : chars_ptr;");
               Put_Line (File, "         Version     : Unsigned_32)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = Registry);");
               Put_Line (File, "      begin");
               Put_Line (File, "         Global_Object_Added");
               Put_Line (File, "           (Conversion.To_Pointer (Data).all, Id, Value (Interface_V), Version);");
               Put_Line (File, "      end Internal_Object_Added;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Object_Removed");
               Put_Line (File, "        (Data     : Void_Ptr;");
               Put_Line (File, "         Registry : Thin.Registry_Ptr;");
               Put_Line (File, "         Id       : Unsigned_32)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = Registry);");
               Put_Line (File, "      begin");
               Put_Line (File, "         Global_Object_Removed (Conversion.To_Pointer (Data).all, Id);");
               Put_Line (File, "      end Internal_Object_Removed;");
               Put_Line (File, "");
               Put_Line (File, "      Listener : aliased Thin.Registry_Listener_T :=");
               Put_Line (File, "        (Global        => Internal_Object_Added'Unrestricted_Access,");
               Put_Line (File, "         Global_Remove => Internal_Object_Removed'Unrestricted_Access);");
               Put_Line (File, "      --  Note: It should be safe to use Unrestricted_Access here since");
               Put_Line (File, "      --  this generic can only be instantiated at library level");
               Put_Line (File, "");
            elsif Name = "Callback" then
               Put_Line (File, "      procedure Internal_Done");
               Put_Line (File, "        (Data          : Void_Ptr;");
               Put_Line (File, "         Callback      : Thin.Callback_Ptr;");
               Put_Line (File, "         Callback_Data : Unsigned_32)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Done");
               Put_Line (File, "        (Data          : Void_Ptr;");
               Put_Line (File, "         Callback      : Thin.Callback_Ptr;");
               Put_Line (File, "         Callback_Data : Unsigned_32)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = Callback);");
               Put_Line (File, "      begin");
               Put_Line (File, "         Done (Conversion.To_Pointer (Data).all, Callback_Data);");
               Put_Line (File, "      end Internal_Done;");
               Put_Line (File, "");
               Put_Line (File, "      Listener : aliased Thin.Callback_Listener_T :=");
               Put_Line (File, "        (Done => Internal_Done'Unrestricted_Access);");
               Put_Line (File, "");
            elsif Name = "Shm" then
               Put_Line (File, "      procedure Internal_Format");
               Put_Line (File, "        (Data   : Void_Ptr;");
               Put_Line (File, "         Shm    : Thin.Shm_Ptr;");
               Put_Line (File, "         Format : Shm_Format)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Format");
               Put_Line (File, "        (Data   : Void_Ptr;");
               Put_Line (File, "         Shm    : Thin.Shm_Ptr;");
               Put_Line (File, "         Format : Shm_Format)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = Shm);");
               Put_Line (File, "      begin");
               Put_Line (File, "         Shm_Events.Format");
               Put_Line (File, "           (Conversion.To_Pointer (Data).all, Format);");
               Put_Line (File, "      end Internal_Format;");
               Put_Line (File, "");
               Put_Line (File, "      Listener : aliased Thin.Shm_Listener_T :=");
               Put_Line (File, "        (Format => Internal_Format'Unrestricted_Access);");
               Put_Line (File, "");
            elsif Name = "Buffer" then
               Put_Line (File, "      procedure Internal_Release");
               Put_Line (File, "        (Data   : Void_Ptr;");
               Put_Line (File, "         Buffer : Thin.Buffer_Ptr)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Release");
               Put_Line (File, "        (Data   : Void_Ptr;");
               Put_Line (File, "         Buffer : Thin.Buffer_Ptr)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = Buffer);");
               Put_Line (File, "      begin");
               Put_Line (File, "         Release (Conversion.To_Pointer (Data).all);");
               Put_Line (File, "      end Internal_Release;");
               Put_Line (File, "");
               Put_Line (File, "      Listener : aliased Thin.Buffer_Listener_T :=");
               Put_Line (File, "        (Release => Internal_Release'Unrestricted_Access);");
               Put_Line (File, "");
            elsif Name = "Data_Offer" then
               Put_Line (File, "      procedure Internal_Offer");
               Put_Line (File, "        (Data       : Void_Ptr;");
               Put_Line (File, "         Data_Offer : Thin.Data_Offer_Ptr;");
               Put_Line (File, "         Mime_Type  : chars_ptr)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Source_Actions");
               Put_Line (File, "        (Data           : Void_Ptr;");
               Put_Line (File, "         Data_Offer     : Thin.Data_Offer_Ptr;");
               Put_Line (File, "         Source_Actions : Data_Device_Manager_Dnd_Action)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Action");
               Put_Line (File, "        (Data       : Void_Ptr;");
               Put_Line (File, "         Data_Offer : Thin.Data_Offer_Ptr;");
               Put_Line (File, "         Dnd_Action : Data_Device_Manager_Dnd_Action)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Offer");
               Put_Line (File, "        (Data       : Void_Ptr;");
               Put_Line (File, "         Data_Offer : Thin.Data_Offer_Ptr;");
               Put_Line (File, "         Mime_Type  : chars_ptr)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = Data_Offer);");
               Put_Line (File, "");
               Put_Line (File, "         M : constant String := Interfaces.C.Strings.Value (Mime_Type);");
               Put_Line (File, "      begin");
               Put_Line (File, "         Offer (Conversion.To_Pointer (Data).all, M);");
               Put_Line (File, "      end Internal_Offer;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Source_Actions");
               Put_Line (File, "        (Data           : Void_Ptr;");
               Put_Line (File, "         Data_Offer     : Thin.Data_Offer_Ptr;");
               Put_Line (File, "         Source_Actions : Data_Device_Manager_Dnd_Action)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = Data_Offer);");
               Put_Line (File, "      begin");
               Put_Line (File, "         Data_Offer_Events.Source_Actions");
               Put_Line (File, "           (Conversion.To_Pointer (Data).all, Source_Actions);");
               Put_Line (File, "      end Internal_Source_Actions;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Action");
               Put_Line (File, "        (Data       : Void_Ptr;");
               Put_Line (File, "         Data_Offer : Thin.Data_Offer_Ptr;");
               Put_Line (File, "         Dnd_Action : Data_Device_Manager_Dnd_Action)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = Data_Offer);");
               Put_Line (File, "      begin");
               Put_Line (File, "         Action (Conversion.To_Pointer (Data).all, Dnd_Action);");
               Put_Line (File, "      end Internal_Action;");
               Put_Line (File, "");
               Put_Line (File, "      Listener : aliased Thin.Data_Offer_Listener_T :=");
               Put_Line (File, "        (Offer          => Internal_Offer'Unrestricted_Access,");
               Put_Line (File, "         Source_Actions => Internal_Source_Actions'Unrestricted_Access,");
               Put_Line (File, "         Action         => Internal_Action'Unrestricted_Access);");
               Put_Line (File, "");
            elsif Name = "Data_Source" then
               Put_Line (File, "      procedure Internal_Target");
               Put_Line (File, "        (Data        : Void_Ptr;");
               Put_Line (File, "         Data_Source : Thin.Data_Source_Ptr;");
               Put_Line (File, "         Mime_Type   : chars_ptr)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Send");
               Put_Line (File, "        (Data        : Void_Ptr;");
               Put_Line (File, "         Data_Source : Thin.Data_Source_Ptr;");
               Put_Line (File, "         Mime_Type   : chars_ptr;");
               Put_Line (File, "         Fd          : File_Descriptor)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Cancelled");
               Put_Line (File, "        (Data        : Void_Ptr;");
               Put_Line (File, "         Data_Source : Thin.Data_Source_Ptr)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Performed");
               Put_Line (File, "        (Data        : Void_Ptr;");
               Put_Line (File, "         Data_Source : Thin.Data_Source_Ptr)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Finished");
               Put_Line (File, "        (Data        : Void_Ptr;");
               Put_Line (File, "         Data_Source : Thin.Data_Source_Ptr)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Action");
               Put_Line (File, "        (Data        : Void_Ptr;");
               Put_Line (File, "         Data_Source : Thin.Data_Source_Ptr;");
               Put_Line (File, "         Dnd_Action  : Data_Device_Manager_Dnd_Action)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Target");
               Put_Line (File, "        (Data        : Void_Ptr;");
               Put_Line (File, "         Data_Source : Thin.Data_Source_Ptr;");
               Put_Line (File, "         Mime_Type   : chars_ptr)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = Data_Source);");
               Put_Line (File, "");
               Put_Line (File, "         M : constant String := Interfaces.C.Strings.Value (Mime_Type);");
               Put_Line (File, "      begin");
               Put_Line (File, "         Target (Conversion.To_Pointer (Data).all, M);");
               Put_Line (File, "      end Internal_Target;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Send");
               Put_Line (File, "        (Data        : Void_Ptr;");
               Put_Line (File, "         Data_Source : Thin.Data_Source_Ptr;");
               Put_Line (File, "         Mime_Type   : chars_ptr;");
               Put_Line (File, "         Fd          : File_Descriptor)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = Data_Source);");
               Put_Line (File, "");
               Put_Line (File, "         M : constant String := Interfaces.C.Strings.Value (Mime_Type);");
               Put_Line (File, "      begin");
               Put_Line (File, "         Send (Conversion.To_Pointer (Data).all, M, Fd);");
               Put_Line (File, "      end Internal_Send;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Cancelled");
               Put_Line (File, "        (Data        : Void_Ptr;");
               Put_Line (File, "         Data_Source : Thin.Data_Source_Ptr)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = Data_Source);");
               Put_Line (File, "      begin");
               Put_Line (File, "         Cancelled (Conversion.To_Pointer (Data).all);");
               Put_Line (File, "      end Internal_Cancelled;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Performed");
               Put_Line (File, "        (Data        : Void_Ptr;");
               Put_Line (File, "         Data_Source : Thin.Data_Source_Ptr)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = Data_Source);");
               Put_Line (File, "      begin");
               Put_Line (File, "         Performed (Conversion.To_Pointer (Data).all);");
               Put_Line (File, "      end Internal_Performed;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Finished");
               Put_Line (File, "        (Data        : Void_Ptr;");
               Put_Line (File, "         Data_Source : Thin.Data_Source_Ptr)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = Data_Source);");
               Put_Line (File, "      begin");
               Put_Line (File, "         Finished (Conversion.To_Pointer (Data).all);");
               Put_Line (File, "      end Internal_Finished;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Action");
               Put_Line (File, "        (Data        : Void_Ptr;");
               Put_Line (File, "         Data_Source : Thin.Data_Source_Ptr;");
               Put_Line (File, "         Dnd_Action  : Data_Device_Manager_Dnd_Action)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = Data_Source);");
               Put_Line (File, "      begin");
               Put_Line (File, "         Action (Conversion.To_Pointer (Data).all, Dnd_Action);");
               Put_Line (File, "      end Internal_Action;");
               Put_Line (File, "");
               Put_Line (File, "      Listener : aliased Thin.Data_Source_Listener_T :=");
               Put_Line (File, "        (Target             => Internal_Target'Unrestricted_Access,");
               Put_Line (File, "         Send               => Internal_Send'Unrestricted_Access,");
               Put_Line (File, "         Cancelled          => Internal_Cancelled'Unrestricted_Access,");
               Put_Line (File, "         Dnd_Drop_Performed => Internal_Performed'Unrestricted_Access,");
               Put_Line (File, "         Dnd_Finished       => Internal_Finished'Unrestricted_Access,");
               Put_Line (File, "         Action             => Internal_Action'Unrestricted_Access);");
               Put_Line (File, "");
            elsif Name = "Data_Device" then
               Put_Line (File, "      procedure Internal_Data_Offer");
               Put_Line (File, "        (Data        : Void_Ptr;");
               Put_Line (File, "         Data_Device : Thin.Data_Device_Ptr;");
               Put_Line (File, "         Id          : Unsigned_32)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Enter");
               Put_Line (File, "        (Data        : Void_Ptr;");
               Put_Line (File, "         Data_Device : Thin.Data_Device_Ptr;");
               Put_Line (File, "         Serial      : Unsigned_32;");
               Put_Line (File, "         Surface     : Thin.Surface_Ptr;");
               Put_Line (File, "         X, Y        : Fixed;");
               Put_Line (File, "         Id          : Thin.Data_Offer_Ptr)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Leave");
               Put_Line (File, "        (Data        : Void_Ptr;");
               Put_Line (File, "         Data_Device : Thin.Data_Device_Ptr)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Motion");
               Put_Line (File, "        (Data        : Void_Ptr;");
               Put_Line (File, "         Data_Device : Thin.Data_Device_Ptr;");
               Put_Line (File, "         Time        : Unsigned_32;");
               Put_Line (File, "         X, Y        : Fixed)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Drop");
               Put_Line (File, "        (Data        : Void_Ptr;");
               Put_Line (File, "         Data_Device : Thin.Data_Device_Ptr)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Selection");
               Put_Line (File, "        (Data        : Void_Ptr;");
               Put_Line (File, "         Data_Device : Thin.Data_Device_Ptr;");
               Put_Line (File, "         Id          : Thin.Data_Offer_Ptr)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Data_Offer");
               Put_Line (File, "        (Data        : Void_Ptr;");
               Put_Line (File, "         Data_Device : Thin.Data_Device_Ptr;");
               Put_Line (File, "         Id          : Unsigned_32)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = Data_Device);");
               Put_Line (File, "      begin");
               Put_Line (File, "         Data_Offer (Conversion.To_Pointer (Data).all, Id);");
               Put_Line (File, "      end Internal_Data_Offer;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Enter");
               Put_Line (File, "        (Data        : Void_Ptr;");
               Put_Line (File, "         Data_Device : Thin.Data_Device_Ptr;");
               Put_Line (File, "         Serial      : Unsigned_32;");
               Put_Line (File, "         Surface     : Thin.Surface_Ptr;");
               Put_Line (File, "         X, Y        : Fixed;");
               Put_Line (File, "         Id          : Thin.Data_Offer_Ptr)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = Data_Device);");
               Put_Line (File, "");
               Put_Line (File, "         S : constant Client.Surface     := (Proxy => Surface);");
               Put_Line (File, "         Offer : constant Client.Data_Offer := (Proxy => Id);");
               Put_Line (File, "      begin");
               Put_Line (File, "         Enter (Conversion.To_Pointer (Data).all, Serial, S, X, Y, Offer);");
               Put_Line (File, "      end Internal_Enter;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Leave");
               Put_Line (File, "        (Data        : Void_Ptr;");
               Put_Line (File, "         Data_Device : Thin.Data_Device_Ptr)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = Data_Device);");
               Put_Line (File, "      begin");
               Put_Line (File, "         Leave (Conversion.To_Pointer (Data).all);");
               Put_Line (File, "      end Internal_Leave;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Motion");
               Put_Line (File, "        (Data        : Void_Ptr;");
               Put_Line (File, "         Data_Device : Thin.Data_Device_Ptr;");
               Put_Line (File, "         Time        : Unsigned_32;");
               Put_Line (File, "         X, Y        : Fixed)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = Data_Device);");
               Put_Line (File, "      begin");
               Put_Line (File, "         Motion (Conversion.To_Pointer (Data).all, Time, X, Y);");
               Put_Line (File, "      end Internal_Motion;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Drop");
               Put_Line (File, "        (Data        : Void_Ptr;");
               Put_Line (File, "         Data_Device : Thin.Data_Device_Ptr)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = Data_Device);");
               Put_Line (File, "      begin");
               Put_Line (File, "         Drop (Conversion.To_Pointer (Data).all);");
               Put_Line (File, "      end Internal_Drop;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Selection");
               Put_Line (File, "        (Data        : Void_Ptr;");
               Put_Line (File, "         Data_Device : Thin.Data_Device_Ptr;");
               Put_Line (File, "         Id          : Thin.Data_Offer_Ptr)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = Data_Device);");
               Put_Line (File, "");
               Put_Line (File, "         Offer : constant Client.Data_Offer := (Proxy => Id);");
               Put_Line (File, "      begin");
               Put_Line (File, "         Selection (Conversion.To_Pointer (Data).all, Offer);");
               Put_Line (File, "      end Internal_Selection;");
               Put_Line (File, "");
               Put_Line (File, "      Listener : aliased Thin.Data_Device_Listener_T");
               Put_Line (File, "        := (Data_Offer => Internal_Data_Offer'Unrestricted_Access,");
               Put_Line (File, "            Enter      => Internal_Enter'Unrestricted_Access,");
               Put_Line (File, "            Leave      => Internal_Leave'Unrestricted_Access,");
               Put_Line (File, "            Motion     => Internal_Motion'Unrestricted_Access,");
               Put_Line (File, "            Drop       => Internal_Drop'Unrestricted_Access,");
               Put_Line (File, "            Selection  => Internal_Selection'Unrestricted_Access);");
               Put_Line (File, "");
            elsif Name = "Surface" then
               Put_Line (File, "      procedure Internal_Enter");
               Put_Line (File, "        (Data    : Void_Ptr;");
               Put_Line (File, "         Surface : Thin.Surface_Ptr;");
               Put_Line (File, "         Output  : Thin.Output_Ptr)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Leave");
               Put_Line (File, "        (Data    : Void_Ptr;");
               Put_Line (File, "         Surface : Thin.Surface_Ptr;");
               Put_Line (File, "         Output  : Thin.Output_Ptr)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Enter");
               Put_Line (File, "        (Data    : Void_Ptr;");
               Put_Line (File, "         Surface : Thin.Surface_Ptr;");
               Put_Line (File, "         Output  : Thin.Output_Ptr)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = Surface);");
               Put_Line (File, "");
               Put_Line (File, "         O : constant Client.Output := (Proxy => Output);");
               Put_Line (File, "      begin");
               Put_Line (File, "         Enter (Conversion.To_Pointer (Data).all, O);");
               Put_Line (File, "      end Internal_Enter;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Leave");
               Put_Line (File, "        (Data    : Void_Ptr;");
               Put_Line (File, "         Surface : Thin.Surface_Ptr;");
               Put_Line (File, "         Output  : Thin.Output_Ptr)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = Surface);");
               Put_Line (File, "");
               Put_Line (File, "         O : constant Client.Output := (Proxy => Output);");
               Put_Line (File, "      begin");
               Put_Line (File, "         Leave (Conversion.To_Pointer (Data).all, O);");
               Put_Line (File, "      end Internal_Leave;");
               Put_Line (File, "");
               Put_Line (File, "      Listener : aliased Thin.Surface_Listener_T");
               Put_Line (File, "        := (Enter => Internal_Enter'Unrestricted_Access,");
               Put_Line (File, "            Leave => Internal_Leave'Unrestricted_Access);");
               Put_Line (File, "");
            elsif Name = "Seat" then
               Put_Line (File, "      procedure Internal_Seat_Capabilities");
               Put_Line (File, "        (Data         : Void_Ptr;");
               Put_Line (File, "         Seat         : Thin.Seat_Ptr;");
               Put_Line (File, "         Capabilities : Seat_Capability)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Seat_Name");
               Put_Line (File, "        (Data : Void_Ptr;");
               Put_Line (File, "         Seat : Thin.Seat_Ptr;");
               Put_Line (File, "         Name : Interfaces.C.Strings.chars_ptr)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Seat_Capabilities");
               Put_Line (File, "        (Data         : Void_Ptr;");
               Put_Line (File, "         Seat         : Thin.Seat_Ptr;");
               Put_Line (File, "         Capabilities : Seat_Capability)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = Seat);");
               Put_Line (File, "      begin");
               Put_Line (File, "         Seat_Capabilities (Conversion.To_Pointer (Data).all, Capabilities);");
               Put_Line (File, "      end Internal_Seat_Capabilities;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Seat_Name");
               Put_Line (File, "        (Data : Void_Ptr;");
               Put_Line (File, "         Seat : Thin.Seat_Ptr;");
               Put_Line (File, "         Name : Interfaces.C.Strings.chars_ptr)");
               Put_Line (File, "      is");
               Put_Line (File, "         N : constant String := Interfaces.C.Strings.Value (Name);");
               Put_Line (File, "");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = Seat);");
               Put_Line (File, "      begin");
               Put_Line (File, "         Seat_Name (Conversion.To_Pointer (Data).all, N);");
               Put_Line (File, "      end Internal_Seat_Name;");
               Put_Line (File, "");
               Put_Line (File, "      Listener : aliased Thin.Seat_Listener_T :=");
               Put_Line (File, "        (Capabilities => Internal_Seat_Capabilities'Unrestricted_Access,");
               Put_Line (File, "         Name         => Internal_Seat_Name'Unrestricted_Access);");
               Put_Line (File, "");
            elsif Name = "Pointer" then
               Put_Line (File, "      procedure Internal_Pointer_Enter");
               Put_Line (File, "        (Data      : Void_Ptr;");
               Put_Line (File, "         Pointer   : Thin.Pointer_Ptr;");
               Put_Line (File, "         Serial    : Unsigned_32;");
               Put_Line (File, "         Surface   : Thin.Surface_Ptr;");
               Put_Line (File, "         Surface_X : Fixed;");
               Put_Line (File, "         Surface_Y : Fixed)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Pointer_Leave");
               Put_Line (File, "        (Data    : Void_Ptr;");
               Put_Line (File, "         Pointer : Thin.Pointer_Ptr;");
               Put_Line (File, "         Serial  : Unsigned_32;");
               Put_Line (File, "         Surface : Thin.Surface_Ptr)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Pointer_Motion");
               Put_Line (File, "        (Data      : Void_Ptr;");
               Put_Line (File, "         Pointer   : Thin.Pointer_Ptr;");
               Put_Line (File, "         Time      : Unsigned_32;");
               Put_Line (File, "         Surface_X : Fixed;");
               Put_Line (File, "         Surface_Y : Fixed)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Pointer_Button");
               Put_Line (File, "        (Data    : Void_Ptr;");
               Put_Line (File, "         Pointer : Thin.Pointer_Ptr;");
               Put_Line (File, "         Serial  : Unsigned_32;");
               Put_Line (File, "         Time    : Unsigned_32;");
               Put_Line (File, "         Button  : Unsigned_32;");
               Put_Line (File, "         State   : Pointer_Button_State)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Pointer_Axis");
               Put_Line (File, "        (Data    : Void_Ptr;");
               Put_Line (File, "         Pointer : Thin.Pointer_Ptr;");
               Put_Line (File, "         Time    : Unsigned_32;");
               Put_Line (File, "         Axis    : Pointer_Axis;");
               Put_Line (File, "         Value   : Fixed)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Pointer_Frame");
               Put_Line (File, "        (Data    : Void_Ptr;");
               Put_Line (File, "         Pointer : Thin.Pointer_Ptr)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Pointer_Axis_Source");
               Put_Line (File, "        (Data        : Void_Ptr;");
               Put_Line (File, "         Pointer     : Thin.Pointer_Ptr;");
               Put_Line (File, "         Axis_Source : Pointer_Axis_Source)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Pointer_Axis_Stop");
               Put_Line (File, "        (Data    : Void_Ptr;");
               Put_Line (File, "         Pointer : Thin.Pointer_Ptr;");
               Put_Line (File, "         Time    : Unsigned_32;");
               Put_Line (File, "         Axis    : Pointer_Axis)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Pointer_Axis_Discrete");
               Put_Line (File, "        (Data     : Void_Ptr;");
               Put_Line (File, "         Pointer  : Thin.Pointer_Ptr;");
               Put_Line (File, "         Axis     : Pointer_Axis;");
               Put_Line (File, "         Discrete : Integer)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Pointer_Enter");
               Put_Line (File, "        (Data      : Void_Ptr;");
               Put_Line (File, "         Pointer   : Thin.Pointer_Ptr;");
               Put_Line (File, "         Serial    : Unsigned_32;");
               Put_Line (File, "         Surface   : Thin.Surface_Ptr;");
               Put_Line (File, "         Surface_X : Fixed;");
               Put_Line (File, "         Surface_Y : Fixed)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = Pointer);");
               Put_Line (File, "");
               Put_Line (File, "         S : constant Client.Surface := (Proxy => Surface);");
               Put_Line (File, "      begin");
               Put_Line (File, "         Pointer_Enter (Conversion.To_Pointer (Data).all, Serial, S, Surface_X, Surface_Y);");
               Put_Line (File, "      end Internal_Pointer_Enter;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Pointer_Leave");
               Put_Line (File, "        (Data    : Void_Ptr;");
               Put_Line (File, "         Pointer : Thin.Pointer_Ptr;");
               Put_Line (File, "         Serial  : Unsigned_32;");
               Put_Line (File, "         Surface : Thin.Surface_Ptr)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = Pointer);");
               Put_Line (File, "");
               Put_Line (File, "         S : constant Client.Surface := (Proxy => Surface);");
               Put_Line (File, "      begin");
               Put_Line (File, "         Pointer_Leave (Conversion.To_Pointer (Data).all, Serial, S);");
               Put_Line (File, "      end Internal_Pointer_Leave;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Pointer_Motion");
               Put_Line (File, "        (Data      : Void_Ptr;");
               Put_Line (File, "         Pointer   : Thin.Pointer_Ptr;");
               Put_Line (File, "         Time      : Unsigned_32;");
               Put_Line (File, "         Surface_X : Fixed;");
               Put_Line (File, "         Surface_Y : Fixed)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = Pointer);");
               Put_Line (File, "      begin");
               Put_Line (File, "         Pointer_Motion (Conversion.To_Pointer (Data).all, Time, Surface_X, Surface_Y);");
               Put_Line (File, "      end Internal_Pointer_Motion;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Pointer_Button");
               Put_Line (File, "        (Data    : Void_Ptr;");
               Put_Line (File, "         Pointer : Thin.Pointer_Ptr;");
               Put_Line (File, "         Serial  : Unsigned_32;");
               Put_Line (File, "         Time    : Unsigned_32;");
               Put_Line (File, "         Button  : Unsigned_32;");
               Put_Line (File, "         State   : Pointer_Button_State)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = Pointer);");
               Put_Line (File, "      begin");
               Put_Line (File, "         Pointer_Button (Conversion.To_Pointer (Data).all, Serial, Time, Button, State);");
               Put_Line (File, "      end Internal_Pointer_Button;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Pointer_Axis");
               Put_Line (File, "        (Data    : Void_Ptr;");
               Put_Line (File, "         Pointer : Thin.Pointer_Ptr;");
               Put_Line (File, "         Time    : Unsigned_32;");
               Put_Line (File, "         Axis    : Pointer_Axis;");
               Put_Line (File, "         Value   : Fixed)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = Pointer);");
               Put_Line (File, "      begin");
               Put_Line (File, "         Pointer_Scroll (Conversion.To_Pointer (Data).all, Time, Axis, Value);");
               Put_Line (File, "      end Internal_Pointer_Axis;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Pointer_Frame (Data    : Void_Ptr;");
               Put_Line (File, "                                        Pointer : Thin.Pointer_Ptr)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = Pointer);");
               Put_Line (File, "      begin");
               Put_Line (File, "         Pointer_Frame (Conversion.To_Pointer (Data).all);");
               Put_Line (File, "      end Internal_Pointer_Frame;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Pointer_Axis_Source");
               Put_Line (File, "        (Data        : Void_Ptr;");
               Put_Line (File, "         Pointer     : Thin.Pointer_Ptr;");
               Put_Line (File, "         Axis_Source : Pointer_Axis_Source)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = Pointer);");
               Put_Line (File, "      begin");
               Put_Line (File, "         Pointer_Scroll_Source (Conversion.To_Pointer (Data).all, Axis_Source);");
               Put_Line (File, "      end Internal_Pointer_Axis_Source;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Pointer_Axis_Stop");
               Put_Line (File, "        (Data    : Void_Ptr;");
               Put_Line (File, "         Pointer : Thin.Pointer_Ptr;");
               Put_Line (File, "         Time    : Unsigned_32;");
               Put_Line (File, "         Axis    : Pointer_Axis)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = Pointer);");
               Put_Line (File, "      begin");
               Put_Line (File, "         Pointer_Scroll_Stop (Conversion.To_Pointer (Data).all, Time, Axis);");
               Put_Line (File, "      end Internal_Pointer_Axis_Stop;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Pointer_Axis_Discrete");
               Put_Line (File, "        (Data     : Void_Ptr;");
               Put_Line (File, "         Pointer  : Thin.Pointer_Ptr;");
               Put_Line (File, "         Axis     : Pointer_Axis;");
               Put_Line (File, "         Discrete : Integer)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = Pointer);");
               Put_Line (File, "      begin");
               Put_Line (File, "         Pointer_Scroll_Discrete (Conversion.To_Pointer (Data).all, Axis, Discrete);");
               Put_Line (File, "      end Internal_Pointer_Axis_Discrete;");
               Put_Line (File, "");
               Put_Line (File, "      Listener : aliased Thin.Pointer_Listener_T :=");
               Put_Line (File, "        (Enter         => Internal_Pointer_Enter'Unrestricted_Access,");
               Put_Line (File, "         Leave         => Internal_Pointer_Leave'Unrestricted_Access,");
               Put_Line (File, "         Motion        => Internal_Pointer_Motion'Unrestricted_Access,");
               Put_Line (File, "         Button        => Internal_Pointer_Button'Unrestricted_Access,");
               Put_Line (File, "         Axis          => Internal_Pointer_Axis'Unrestricted_Access,");
               Put_Line (File, "         Frame         => Internal_Pointer_Frame'Unrestricted_Access,");
               Put_Line (File, "         Axis_Source   => Internal_Pointer_Axis_Source'Unrestricted_Access,");
               Put_Line (File, "         Axis_Stop     => Internal_Pointer_Axis_Stop'Unrestricted_Access,");
               Put_Line (File, "         Axis_Discrete => Internal_Pointer_Axis_Discrete'Unrestricted_Access);");
               Put_Line (File, "");
            elsif Name = "Keyboard" then
               Put_Line (File, "      procedure Internal_Keymap");
               Put_Line (File, "        (Data     : Void_Ptr;");
               Put_Line (File, "         Keyboard : Thin.Keyboard_Ptr;");
               Put_Line (File, "         Format   : Keyboard_Keymap_Format;");
               Put_Line (File, "         Fd       : File_Descriptor;");
               Put_Line (File, "         Size     : Unsigned_32)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Enter");
               Put_Line (File, "        (Data     : Void_Ptr;");
               Put_Line (File, "         Keyboard : Thin.Keyboard_Ptr;");
               Put_Line (File, "         Serial   : Unsigned_32;");
               Put_Line (File, "         Surface  : Thin.Surface_Ptr;");
               Put_Line (File, "         Keys     : Wayland_Array)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Leave");
               Put_Line (File, "        (Data     : Void_Ptr;");
               Put_Line (File, "         Keyboard : Thin.Keyboard_Ptr;");
               Put_Line (File, "         Serial   : Unsigned_32;");
               Put_Line (File, "         Surface  : Thin.Surface_Ptr)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Key");
               Put_Line (File, "        (Data     : Void_Ptr;");
               Put_Line (File, "         Keyboard : Thin.Keyboard_Ptr;");
               Put_Line (File, "         Serial   : Unsigned_32;");
               Put_Line (File, "         Time     : Unsigned_32;");
               Put_Line (File, "         Key      : Unsigned_32;");
               Put_Line (File, "         State    : Keyboard_Key_State)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Modifiers");
               Put_Line (File, "        (Data           : Void_Ptr;");
               Put_Line (File, "         Keyboard       : Thin.Keyboard_Ptr;");
               Put_Line (File, "         Serial         : Unsigned_32;");
               Put_Line (File, "         Mods_Depressed : Unsigned_32;");
               Put_Line (File, "         Mods_Latched   : Unsigned_32;");
               Put_Line (File, "         Mods_Locked    : Unsigned_32;");
               Put_Line (File, "         Group          : Unsigned_32)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Repeat_Info");
               Put_Line (File, "        (Data     : Void_Ptr;");
               Put_Line (File, "         Keyboard : Thin.Keyboard_Ptr;");
               Put_Line (File, "         Rate     : Integer;");
               Put_Line (File, "         Delay_V  : Integer)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Keymap");
               Put_Line (File, "        (Data     : Void_Ptr;");
               Put_Line (File, "         Keyboard : Thin.Keyboard_Ptr;");
               Put_Line (File, "         Format   : Keyboard_Keymap_Format;");
               Put_Line (File, "         Fd       : File_Descriptor;");
               Put_Line (File, "         Size     : Unsigned_32)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = Keyboard);");
               Put_Line (File, "      begin");
               Put_Line (File, "         Keymap (Conversion.To_Pointer (Data).all, Format, Fd, Size);");
               Put_Line (File, "      end Internal_Keymap;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Enter");
               Put_Line (File, "        (Data     : Void_Ptr;");
               Put_Line (File, "         Keyboard : Thin.Keyboard_Ptr;");
               Put_Line (File, "         Serial   : Unsigned_32;");
               Put_Line (File, "         Surface  : Thin.Surface_Ptr;");
               Put_Line (File, "         Keys     : Wayland_Array)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = Keyboard);");
               Put_Line (File, "");
               Put_Line (File, "         S : constant Client.Surface  := (Proxy => Surface);");
               Put_Line (File, "");
               Put_Line (File, "         Key_Size : constant := Unsigned_32'Size / System.Storage_Unit;");
               Put_Line (File, "");
               Put_Line (File, "         Pressed_Keys : Unsigned_32_Array (1 .. Natural (Keys.Size) / Key_Size)");
               Put_Line (File, "           with Address => Keys.Data;");
               Put_Line (File, "      begin");
               Put_Line (File, "         Enter (Conversion.To_Pointer (Data).all, Serial, S, Pressed_Keys);");
               Put_Line (File, "      end Internal_Enter;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Leave");
               Put_Line (File, "        (Data     : Void_Ptr;");
               Put_Line (File, "         Keyboard : Thin.Keyboard_Ptr;");
               Put_Line (File, "         Serial   : Unsigned_32;");
               Put_Line (File, "         Surface  : Thin.Surface_Ptr)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = Keyboard);");
               Put_Line (File, "");
               Put_Line (File, "         S : constant Client.Surface  := (Proxy => Surface);");
               Put_Line (File, "      begin");
               Put_Line (File, "         Leave (Conversion.To_Pointer (Data).all, Serial, S);");
               Put_Line (File, "      end Internal_Leave;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Key");
               Put_Line (File, "        (Data     : Void_Ptr;");
               Put_Line (File, "         Keyboard : Thin.Keyboard_Ptr;");
               Put_Line (File, "         Serial   : Unsigned_32;");
               Put_Line (File, "         Time     : Unsigned_32;");
               Put_Line (File, "         Key      : Unsigned_32;");
               Put_Line (File, "         State    : Keyboard_Key_State)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = Keyboard);");
               Put_Line (File, "      begin");
               Put_Line (File, "         Keyboard_Events.Key (Conversion.To_Pointer (Data).all, Serial, Time, Key, State);");
               Put_Line (File, "      end Internal_Key;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Modifiers");
               Put_Line (File, "        (Data           : Void_Ptr;");
               Put_Line (File, "         Keyboard       : Thin.Keyboard_Ptr;");
               Put_Line (File, "         Serial         : Unsigned_32;");
               Put_Line (File, "         Mods_Depressed : Unsigned_32;");
               Put_Line (File, "         Mods_Latched   : Unsigned_32;");
               Put_Line (File, "         Mods_Locked    : Unsigned_32;");
               Put_Line (File, "         Group          : Unsigned_32)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = Keyboard);");
               Put_Line (File, "      begin");
               Put_Line (File, "         Modifiers");
               Put_Line (File, "           (Conversion.To_Pointer (Data).all,");
               Put_Line (File, "            Serial,");
               Put_Line (File, "            Mods_Depressed,");
               Put_Line (File, "            Mods_Latched,");
               Put_Line (File, "            Mods_Locked,");
               Put_Line (File, "            Group);");
               Put_Line (File, "      end Internal_Modifiers;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Repeat_Info");
               Put_Line (File, "        (Data     : Void_Ptr;");
               Put_Line (File, "         Keyboard : Thin.Keyboard_Ptr;");
               Put_Line (File, "         Rate     : Integer;");
               Put_Line (File, "         Delay_V  : Integer)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = Keyboard);");
               Put_Line (File, "      begin");
               Put_Line (File, "         Repeat_Info (Conversion.To_Pointer (Data).all, Rate, Delay_V);");
               Put_Line (File, "      end Internal_Repeat_Info;");
               Put_Line (File, "");
               Put_Line (File, "      Listener : aliased Thin.Keyboard_Listener_T :=");
               Put_Line (File, "        (Keymap      => Internal_Keymap'Unrestricted_Access,");
               Put_Line (File, "         Enter       => Internal_Enter'Unrestricted_Access,");
               Put_Line (File, "         Leave       => Internal_Leave'Unrestricted_Access,");
               Put_Line (File, "         Key         => Internal_Key'Unrestricted_Access,");
               Put_Line (File, "         Modifiers   => Internal_Modifiers'Unrestricted_Access,");
               Put_Line (File, "         Repeat_Info => Internal_Repeat_Info'Unrestricted_Access);");
               Put_Line (File, "");
            elsif Name = "Touch" then
               Put_Line (File, "      procedure Internal_Down");
               Put_Line (File, "        (Data    : Void_Ptr;");
               Put_Line (File, "         Touch   : Thin.Touch_Ptr;");
               Put_Line (File, "         Serial  : Unsigned_32;");
               Put_Line (File, "         Time    : Unsigned_32;");
               Put_Line (File, "         Surface : Thin.Surface_Ptr;");
               Put_Line (File, "         Id      : Integer;");
               Put_Line (File, "         X, Y    : Fixed)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Up");
               Put_Line (File, "        (Data   : Void_Ptr;");
               Put_Line (File, "         Touch  : Thin.Touch_Ptr;");
               Put_Line (File, "         Serial : Unsigned_32;");
               Put_Line (File, "         Time   : Unsigned_32;");
               Put_Line (File, "         Id     : Integer)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Motion");
               Put_Line (File, "        (Data  : Void_Ptr;");
               Put_Line (File, "         Touch : Thin.Touch_Ptr;");
               Put_Line (File, "         Time  : Unsigned_32;");
               Put_Line (File, "         Id    : Integer;");
               Put_Line (File, "         X, Y  : Fixed)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Frame");
               Put_Line (File, "        (Data  : Void_Ptr;");
               Put_Line (File, "         Touch : Thin.Touch_Ptr)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Cancel");
               Put_Line (File, "        (Data  : Void_Ptr;");
               Put_Line (File, "         Touch : Thin.Touch_Ptr)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Shape");
               Put_Line (File, "        (Data  : Void_Ptr;");
               Put_Line (File, "         Touch : Thin.Touch_Ptr;");
               Put_Line (File, "         Id    : Integer;");
               Put_Line (File, "         Major : Fixed;");
               Put_Line (File, "         Minor : Fixed)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Orientation");
               Put_Line (File, "        (Data        : Void_Ptr;");
               Put_Line (File, "         Touch       : Thin.Touch_Ptr;");
               Put_Line (File, "         Id          : Integer;");
               Put_Line (File, "         Orientation : Fixed)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Down");
               Put_Line (File, "        (Data    : Void_Ptr;");
               Put_Line (File, "         Touch   : Thin.Touch_Ptr;");
               Put_Line (File, "         Serial  : Unsigned_32;");
               Put_Line (File, "         Time    : Unsigned_32;");
               Put_Line (File, "         Surface : Thin.Surface_Ptr;");
               Put_Line (File, "         Id      : Integer;");
               Put_Line (File, "         X, Y    : Fixed)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = Touch);");
               Put_Line (File, "");
               Put_Line (File, "         S : constant Client.Surface := (Proxy => Surface);");
               Put_Line (File, "      begin");
               Put_Line (File, "         Down (Conversion.To_Pointer (Data).all, Serial, Time, S, Id, X, Y);");
               Put_Line (File, "      end Internal_Down;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Up");
               Put_Line (File, "        (Data   : Void_Ptr;");
               Put_Line (File, "         Touch  : Thin.Touch_Ptr;");
               Put_Line (File, "         Serial : Unsigned_32;");
               Put_Line (File, "         Time   : Unsigned_32;");
               Put_Line (File, "         Id     : Integer)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = Touch);");
               Put_Line (File, "      begin");
               Put_Line (File, "         Up (Conversion.To_Pointer (Data).all, Serial, Time, Id);");
               Put_Line (File, "      end Internal_Up;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Motion");
               Put_Line (File, "        (Data  : Void_Ptr;");
               Put_Line (File, "         Touch : Thin.Touch_Ptr;");
               Put_Line (File, "         Time  : Unsigned_32;");
               Put_Line (File, "         Id    : Integer;");
               Put_Line (File, "         X, Y  : Fixed)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = Touch);");
               Put_Line (File, "      begin");
               Put_Line (File, "         Motion (Conversion.To_Pointer (Data).all, Time, Id, X, Y);");
               Put_Line (File, "      end Internal_Motion;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Frame");
               Put_Line (File, "        (Data  : Void_Ptr;");
               Put_Line (File, "         Touch : Thin.Touch_Ptr)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = Touch);");
               Put_Line (File, "      begin");
               Put_Line (File, "         Frame (Conversion.To_Pointer (Data).all);");
               Put_Line (File, "      end Internal_Frame;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Cancel");
               Put_Line (File, "        (Data  : Void_Ptr;");
               Put_Line (File, "         Touch : Thin.Touch_Ptr)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = Touch);");
               Put_Line (File, "      begin");
               Put_Line (File, "         Cancel (Conversion.To_Pointer (Data).all);");
               Put_Line (File, "      end Internal_Cancel;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Shape");
               Put_Line (File, "        (Data  : Void_Ptr;");
               Put_Line (File, "         Touch : Thin.Touch_Ptr;");
               Put_Line (File, "         Id    : Integer;");
               Put_Line (File, "         Major : Fixed;");
               Put_Line (File, "         Minor : Fixed)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = Touch);");
               Put_Line (File, "      begin");
               Put_Line (File, "         Shape (Conversion.To_Pointer (Data).all, Id, Major, Minor);");
               Put_Line (File, "      end Internal_Shape;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Orientation");
               Put_Line (File, "        (Data        : Void_Ptr;");
               Put_Line (File, "         Touch       : Thin.Touch_Ptr;");
               Put_Line (File, "         Id          : Integer;");
               Put_Line (File, "         Orientation : Fixed)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = Touch);");
               Put_Line (File, "      begin");
               Put_Line (File, "         Touch_Events.Orientation (Conversion.To_Pointer (Data).all, Id, Orientation);");
               Put_Line (File, "      end Internal_Orientation;");
               Put_Line (File, "");
               Put_Line (File, "      Listener : aliased Thin.Touch_Listener_T :=");
               Put_Line (File, "        (Down        => Internal_Down'Unrestricted_Access,");
               Put_Line (File, "         Up          => Internal_Up'Unrestricted_Access,");
               Put_Line (File, "         Motion      => Internal_Motion'Unrestricted_Access,");
               Put_Line (File, "         Frame       => Internal_Frame'Unrestricted_Access,");
               Put_Line (File, "         Cancel      => Internal_Cancel'Unrestricted_Access,");
               Put_Line (File, "         Shape       => Internal_Shape'Unrestricted_Access,");
               Put_Line (File, "         Orientation => Internal_Orientation'Unrestricted_Access);");
               Put_Line (File, "");
            elsif Name = "Output" then
               Put_Line (File, "      procedure Internal_Geometry");
               Put_Line (File, "        (Data            : Void_Ptr;");
               Put_Line (File, "         Output          : Thin.Output_Ptr;");
               Put_Line (File, "         X, Y            : Integer;");
               Put_Line (File, "         Physical_Width  : Integer;");
               Put_Line (File, "         Physical_Height : Integer;");
               Put_Line (File, "         Subpixel        : Output_Subpixel;");
               Put_Line (File, "         Make            : chars_ptr;");
               Put_Line (File, "         Model           : chars_ptr;");
               Put_Line (File, "         Transform       : Output_Transform)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Mode");
               Put_Line (File, "        (Data    : Void_Ptr;");
               Put_Line (File, "         Output  : Thin.Output_Ptr;");
               Put_Line (File, "         Flags   : Output_Mode;");
               Put_Line (File, "         Width   : Integer;");
               Put_Line (File, "         Height  : Integer;");
               Put_Line (File, "         Refresh : Integer)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Done");
               Put_Line (File, "        (Data   : Void_Ptr;");
               Put_Line (File, "         Output : Thin.Output_Ptr)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Scale");
               Put_Line (File, "        (Data   : Void_Ptr;");
               Put_Line (File, "         Output : Thin.Output_Ptr;");
               Put_Line (File, "         Factor : Integer)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Geometry");
               Put_Line (File, "        (Data            : Void_Ptr;");
               Put_Line (File, "         Output          : Thin.Output_Ptr;");
               Put_Line (File, "         X, Y            : Integer;");
               Put_Line (File, "         Physical_Width  : Integer;");
               Put_Line (File, "         Physical_Height : Integer;");
               Put_Line (File, "         Subpixel        : Output_Subpixel;");
               Put_Line (File, "         Make            : chars_ptr;");
               Put_Line (File, "         Model           : chars_ptr;");
               Put_Line (File, "         Transform       : Output_Transform)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = Output);");
               Put_Line (File, "");
               Put_Line (File, "         Ma : constant String := Interfaces.C.Strings.Value (Make);");
               Put_Line (File, "         Mo : constant String := Interfaces.C.Strings.Value (Model);");
               Put_Line (File, "      begin");
               Put_Line (File, "         Geometry");
               Put_Line (File, "           (Conversion.To_Pointer (Data).all,");
               Put_Line (File, "            X,");
               Put_Line (File, "            Y,");
               Put_Line (File, "            Physical_Width,");
               Put_Line (File, "            Physical_Height,");
               Put_Line (File, "            Subpixel,");
               Put_Line (File, "            Ma,");
               Put_Line (File, "            Mo,");
               Put_Line (File, "            Transform);");
               Put_Line (File, "      end Internal_Geometry;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Mode");
               Put_Line (File, "        (Data    : Void_Ptr;");
               Put_Line (File, "         Output  : Thin.Output_Ptr;");
               Put_Line (File, "         Flags   : Output_Mode;");
               Put_Line (File, "         Width   : Integer;");
               Put_Line (File, "         Height  : Integer;");
               Put_Line (File, "         Refresh : Integer)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = Output);");
               Put_Line (File, "      begin");
               Put_Line (File, "         Mode");
               Put_Line (File, "           (Conversion.To_Pointer (Data).all,");
               Put_Line (File, "            Flags,");
               Put_Line (File, "            Width,");
               Put_Line (File, "            Height,");
               Put_Line (File, "            Refresh);");
               Put_Line (File, "      end Internal_Mode;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Done");
               Put_Line (File, "        (Data   : Void_Ptr;");
               Put_Line (File, "         Output : Thin.Output_Ptr)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = Output);");
               Put_Line (File, "      begin");
               Put_Line (File, "         Done (Conversion.To_Pointer (Data).all);");
               Put_Line (File, "      end Internal_Done;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Scale");
               Put_Line (File, "        (Data   : Void_Ptr;");
               Put_Line (File, "         Output : Thin.Output_Ptr;");
               Put_Line (File, "         Factor : Integer)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = Output);");
               Put_Line (File, "      begin");
               Put_Line (File, "         Scale (Conversion.To_Pointer (Data).all, Factor);");
               Put_Line (File, "      end Internal_Scale;");
               Put_Line (File, "");
               Put_Line (File, "      Listener : aliased Thin.Output_Listener_T");
               Put_Line (File, "        := (Geometry => Internal_Geometry'Unrestricted_Access,");
               Put_Line (File, "            Mode     => Internal_Mode'Unrestricted_Access,");
               Put_Line (File, "            Done     => Internal_Done'Unrestricted_Access,");
               Put_Line (File, "            Scale    => Internal_Scale'Unrestricted_Access);");
               Put_Line (File, "");
            end if;

            Generate_Suffix_Body_Events (Name);
         end Handle_Interface_Events_Client;

         procedure Handle_Interface_Xdg_Shell
           (Interface_Tag : aliased Wayland_XML.Interface_Tag)
         is
            Name : constant String
              := Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Interface_Tag));
         begin
            if Name in "Xdg_Wm_Base" then
               Generate_Body_Bind_Subprogram (Name);
            end if;

            if Name in "Xdg_Wm_Base" then
               Put_Line (File, "");
               Put_Line (File, "   procedure Pong (Object : Xdg_Wm_Base; Serial : Unsigned_32) is");
               Put_Line (File, "   begin");
               Put_Line (File, "      Thin.Xdg_Wm_Base_Pong (Object.Proxy, Serial);");
               Put_Line (File, "   end Pong;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Create_Positioner");
               Put_Line (File, "     (Object     : Xdg_Wm_Base;");
               Put_Line (File, "      Positioner : in out Xdg_Shell.Xdg_Positioner'Class) is");
               Put_Line (File, "   begin");
               Put_Line (File, "      Positioner.Proxy := Thin.Xdg_Wm_Base_Create_Positioner (Object.Proxy);");
               Put_Line (File, "   end Create_Positioner;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Get_Surface");
               Put_Line (File, "     (Object  : Xdg_Wm_Base;");
               Put_Line (File, "      Window  : Protocols.Client.Surface'Class;");
               Put_Line (File, "      Surface : in out Xdg_Shell.Xdg_Surface'Class) is");
               Put_Line (File, "   begin");
               Put_Line (File, "      Surface.Proxy := Thin.Xdg_Wm_Base_Get_Xdg_Surface");
               Put_Line (File, "        (Object.Proxy, Thin_Client.Surface_Ptr (Window.Get_Proxy));");
               Put_Line (File, "   end Get_Surface;");
            elsif Name in "Xdg_Positioner" then
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_Size");
               Put_Line (File, "     (Object : Xdg_Positioner;");
               Put_Line (File, "      Width  : Natural;");
               Put_Line (File, "      Height : Natural) is");
               Put_Line (File, "   begin");
               Put_Line (File, "      Thin.Xdg_Positioner_Set_Size (Object.Proxy, Width, Height);");
               Put_Line (File, "   end Set_Size;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_Anchor_Rect");
               Put_Line (File, "     (Object : Xdg_Positioner;");
               Put_Line (File, "      X, Y   : Integer;");
               Put_Line (File, "      Width  : Natural;");
               Put_Line (File, "      Height : Natural) is");
               Put_Line (File, "   begin");
               Put_Line (File, "      Thin.Xdg_Positioner_Set_Anchor_Rect (Object.Proxy, X, Y, Width, Height);");
               Put_Line (File, "   end Set_Anchor_Rect;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_Anchor");
               Put_Line (File, "     (Object : Xdg_Positioner;");
               Put_Line (File, "      Anchor : Xdg_Positioner_Anchor) is");
               Put_Line (File, "   begin");
               Put_Line (File, "      Thin.Xdg_Positioner_Set_Anchor (Object.Proxy, Anchor);");
               Put_Line (File, "   end Set_Anchor;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_Gravity");
               Put_Line (File, "     (Object  : Xdg_Positioner;");
               Put_Line (File, "      Gravity : Xdg_Positioner_Gravity) is");
               Put_Line (File, "   begin");
               Put_Line (File, "      Thin.Xdg_Positioner_Set_Gravity (Object.Proxy, Gravity);");
               Put_Line (File, "   end Set_Gravity;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_Constraint_Adjustment");
               Put_Line (File, "     (Object     : Xdg_Positioner;");
               Put_Line (File, "      Adjustment : Unsigned_32) is");
               Put_Line (File, "   begin");
               Put_Line (File, "      Thin.Xdg_Positioner_Set_Constraint_Adjustment (Object.Proxy, Adjustment);");
               Put_Line (File, "   end Set_Constraint_Adjustment;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_Offset");
               Put_Line (File, "     (Object : Xdg_Positioner;");
               Put_Line (File, "      X, Y   : Integer) is");
               Put_Line (File, "   begin");
               Put_Line (File, "      Thin.Xdg_Positioner_Set_Offset (Object.Proxy, X, Y);");
               Put_Line (File, "   end Set_Offset;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_Reactive (Object : Xdg_Positioner) is");
               Put_Line (File, "   begin");
               Put_Line (File, "      Thin.Xdg_Positioner_Set_Reactive (Object.Proxy);");
               Put_Line (File, "   end Set_Reactive;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_Parent_Size");
               Put_Line (File, "     (Object : Xdg_Positioner;");
               Put_Line (File, "      Width  : Natural;");
               Put_Line (File, "      Height : Natural) is");
               Put_Line (File, "   begin");
               Put_Line (File, "      Thin.Xdg_Positioner_Set_Parent_Size (Object.Proxy, Width, Height);");
               Put_Line (File, "   end Set_Parent_Size;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_Parent_Configure");
               Put_Line (File, "     (Object : Xdg_Positioner;");
               Put_Line (File, "      Serial : Unsigned_32) is");
               Put_Line (File, "   begin");
               Put_Line (File, "      Thin.Xdg_Positioner_Set_Parent_Configure (Object.Proxy, Serial);");
               Put_Line (File, "   end Set_Parent_Configure;");
            elsif Name in "Xdg_Surface" then
               Put_Line (File, "");
               Put_Line (File, "   procedure Get_Toplevel");
               Put_Line (File, "     (Object   : Xdg_Surface;");
               Put_Line (File, "      Toplevel : in out Xdg_Shell.Xdg_Toplevel'Class) is");
               Put_Line (File, "   begin");
               Put_Line (File, "      Toplevel.Proxy := Thin.Xdg_Surface_Get_Toplevel (Object.Proxy);");
               Put_Line (File, "   end Get_Toplevel;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Get_Popup");
               Put_Line (File, "     (Object     : Xdg_Surface;");
               Put_Line (File, "      Parent     : Xdg_Surface'Class;");
               Put_Line (File, "      Positioner : Xdg_Shell.Xdg_Positioner'Class;");
               Put_Line (File, "      Popup      : in out Xdg_Shell.Xdg_Popup'Class) is");
               Put_Line (File, "   begin");
               Put_Line (File, "      Popup.Proxy := Thin.Xdg_Surface_Get_Popup (Object.Proxy, Parent.Proxy, Positioner.Proxy);");
               Put_Line (File, "   end Get_Popup;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_Window_Geometry");
               Put_Line (File, "     (Object : Xdg_Surface;");
               Put_Line (File, "      X, Y   : Integer;");
               Put_Line (File, "      Width  : Natural;");
               Put_Line (File, "      Height : Natural) is");
               Put_Line (File, "   begin");
               Put_Line (File, "      Thin.Xdg_Surface_Set_Window_Geometry (Object.Proxy, X, Y, Width, Height);");
               Put_Line (File, "   end Set_Window_Geometry;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Ack_Configure");
               Put_Line (File, "     (Object : Xdg_Surface;");
               Put_Line (File, "      Serial : Unsigned_32) is");
               Put_Line (File, "   begin");
               Put_Line (File, "      Thin.Xdg_Surface_Ack_Configure (Object.Proxy, Serial);");
               Put_Line (File, "   end Ack_Configure;");
            elsif Name in "Xdg_Toplevel" then
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_Parent");
               Put_Line (File, "     (Object : Xdg_Toplevel;");
               Put_Line (File, "      Parent : Xdg_Toplevel'Class) is");
               Put_Line (File, "   begin");
               Put_Line (File, "      Thin.Xdg_Toplevel_Set_Parent (Object.Proxy, Parent.Proxy);");
               Put_Line (File, "   end Set_Parent;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_Title (Object : Xdg_Toplevel; Value : String) is");
               Put_Line (File, "      Value_Ptr : Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.New_String (Value);");
               Put_Line (File, "   begin");
               Put_Line (File, "      Thin.Xdg_Toplevel_Set_Title (Object.Proxy, Value_Ptr);");
               Put_Line (File, "      Interfaces.C.Strings.Free (Value_Ptr);");
               Put_Line (File, "   end Set_Title;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_App_Id (Object : Xdg_Toplevel; Value : String) is");
               Put_Line (File, "      Value_Ptr : Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.New_String (Value);");
               Put_Line (File, "   begin");
               Put_Line (File, "      Thin.Xdg_Toplevel_Set_App_Id (Object.Proxy, Value_Ptr);");
               Put_Line (File, "      Interfaces.C.Strings.Free (Value_Ptr);");
               Put_Line (File, "   end Set_App_Id;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Show_Window_Menu");
               Put_Line (File, "     (Object : Xdg_Toplevel;");
               Put_Line (File, "      Seat   : Protocols.Client.Seat'Class;");
               Put_Line (File, "      Serial : Unsigned_32;");
               Put_Line (File, "      X, Y   : Integer) is");
               Put_Line (File, "   begin");
               Put_Line (File, "      Thin.Xdg_Toplevel_Show_Window_Menu");
               Put_Line (File, "        (Object.Proxy, Thin_Client.Seat_Ptr (Seat.Get_Proxy), Serial, X, Y);");
               Put_Line (File, "   end Show_Window_Menu;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Move");
               Put_Line (File, "     (Object : Xdg_Toplevel;");
               Put_Line (File, "      Seat   : Protocols.Client.Seat'Class;");
               Put_Line (File, "      Serial : Unsigned_32) is");
               Put_Line (File, "   begin");
               Put_Line (File, "      Thin.Xdg_Toplevel_Move");
               Put_Line (File, "        (Object.Proxy, Thin_Client.Seat_Ptr (Seat.Get_Proxy), Serial);");
               Put_Line (File, "   end Move;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Resize");
               Put_Line (File, "     (Object : Xdg_Toplevel;");
               Put_Line (File, "      Seat   : Protocols.Client.Seat'Class;");
               Put_Line (File, "      Serial : Unsigned_32;");
               Put_Line (File, "      Edges  : Xdg_Toplevel_Resize_Edge) is");
               Put_Line (File, "   begin");
               Put_Line (File, "      Thin.Xdg_Toplevel_Resize");
               Put_Line (File, "        (Object.Proxy, Thin_Client.Seat_Ptr (Seat.Get_Proxy), Serial, Edges);");
               Put_Line (File, "   end Resize;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_Min_Size");
               Put_Line (File, "     (Object : Xdg_Toplevel;");
               Put_Line (File, "      Width  : Natural;");
               Put_Line (File, "      Height : Natural) is");
               Put_Line (File, "   begin");
               Put_Line (File, "      Thin.Xdg_Toplevel_Set_Min_Size (Object.Proxy, Width, Height);");
               Put_Line (File, "   end Set_Min_Size;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_Max_Size");
               Put_Line (File, "     (Object : Xdg_Toplevel;");
               Put_Line (File, "      Width  : Natural;");
               Put_Line (File, "      Height : Natural) is");
               Put_Line (File, "   begin");
               Put_Line (File, "      Thin.Xdg_Toplevel_Set_Max_Size (Object.Proxy, Width, Height);");
               Put_Line (File, "   end Set_Max_Size;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Minimize (Object : Xdg_Toplevel) is");
               Put_Line (File, "   begin");
               Put_Line (File, "      Thin.Xdg_Toplevel_Set_Minimized (Object.Proxy);");
               Put_Line (File, "   end Minimize;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_Maximized (Object : Xdg_Toplevel; Enable : Boolean) is");
               Put_Line (File, "   begin");
               Put_Line (File, "      if Enable then");
               Put_Line (File, "         Thin.Xdg_Toplevel_Set_Maximized (Object.Proxy);");
               Put_Line (File, "      else");
               Put_Line (File, "         Thin.Xdg_Toplevel_Unset_Maximized (Object.Proxy);");
               Put_Line (File, "      end if;");
               Put_Line (File, "   end Set_Maximized;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_Fullscreen");
               Put_Line (File, "     (Object : Xdg_Toplevel;");
               Put_Line (File, "      Enable : Boolean) is");
               Put_Line (File, "   begin");
               Put_Line (File, "      if Enable then");
               Put_Line (File, "         Thin.Xdg_Toplevel_Set_Fullscreen (Object.Proxy, null);");
               Put_Line (File, "      else");
               Put_Line (File, "         Thin.Xdg_Toplevel_Unset_Fullscreen (Object.Proxy);");
               Put_Line (File, "      end if;");
               Put_Line (File, "   end Set_Fullscreen;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_Fullscreen");
               Put_Line (File, "     (Object : Xdg_Toplevel;");
               Put_Line (File, "      Enable : Boolean;");
               Put_Line (File, "      Output : Protocols.Client.Output'Class) is");
               Put_Line (File, "   begin");
               Put_Line (File, "      if Enable then");
               Put_Line (File, "         Thin.Xdg_Toplevel_Set_Fullscreen (Object.Proxy, Thin_Client.Output_Ptr (Output.Get_Proxy));");
               Put_Line (File, "      else");
               Put_Line (File, "         Thin.Xdg_Toplevel_Unset_Fullscreen (Object.Proxy);");
               Put_Line (File, "      end if;");
               Put_Line (File, "   end Set_Fullscreen;");
            elsif Name in "Xdg_Popup" then
               Put_Line (File, "");
               Put_Line (File, "   procedure Grab");
               Put_Line (File, "     (Object : Xdg_Popup;");
               Put_Line (File, "      Seat   : Protocols.Client.Seat'Class;");
               Put_Line (File, "      Serial : Unsigned_32) is");
               Put_Line (File, "   begin");
               Put_Line (File, "      Thin.Xdg_Popup_Grab (Object.Proxy, Thin_Client.Seat_Ptr (Seat.Get_Proxy), Serial);");
               Put_Line (File, "   end Grab;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Reposition");
               Put_Line (File, "     (Object     : Xdg_Popup;");
               Put_Line (File, "      Positioner : Xdg_Shell.Xdg_Positioner'Class;");
               Put_Line (File, "      Token      : Unsigned_32) is");
               Put_Line (File, "   begin");
               Put_Line (File, "      Thin.Xdg_Popup_Reposition (Object.Proxy, Positioner.Proxy, Token);");
               Put_Line (File, "   end Reposition;");
            end if;
         end Handle_Interface_Xdg_Shell;

         procedure Handle_Interface_Events_Xdg_Shell
           (Interface_Tag : aliased Wayland_XML.Interface_Tag)
         is
            Name : constant String
              := Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Interface_Tag));
         begin
            if not Exists_Events (Interface_Tag) then
               return;
            end if;

            Generate_Prefix_Body_Events (Name);

            if Name = "Xdg_Wm_Base" then
               Put_Line (File, "      procedure Internal_Ping");
               Put_Line (File, "        (Data        : Void_Ptr;");
               Put_Line (File, "         Xdg_Wm_Base : Thin.Xdg_Wm_Base_Ptr;");
               Put_Line (File, "         Serial      : Unsigned_32)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Ping");
               Put_Line (File, "        (Data        : Void_Ptr;");
               Put_Line (File, "         Xdg_Wm_Base : Thin.Xdg_Wm_Base_Ptr;");
               Put_Line (File, "         Serial      : Unsigned_32)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = " & Name & ");");
               Put_Line (File, "      begin");
               Put_Line (File, "         Ping (Conversion.To_Pointer (Data).all, Serial);");
               Put_Line (File, "      end Internal_Ping;");
               Put_Line (File, "");
               Put_Line (File, "      Listener : aliased Thin." & Name & "_Listener_T :=");
               Put_Line (File, "        (Ping => Internal_Ping'Unrestricted_Access);");
               Put_Line (File, "");
            elsif Name = "Xdg_Surface" then
               Put_Line (File, "      procedure Internal_Configure");
               Put_Line (File, "        (Data        : Void_Ptr;");
               Put_Line (File, "         Xdg_Surface : Thin.Xdg_Surface_Ptr;");
               Put_Line (File, "         Serial      : Unsigned_32)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Configure");
               Put_Line (File, "        (Data        : Void_Ptr;");
               Put_Line (File, "         Xdg_Surface : Thin.Xdg_Surface_Ptr;");
               Put_Line (File, "         Serial      : Unsigned_32)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = " & Name & ");");
               Put_Line (File, "      begin");
               Put_Line (File, "         Configure (Conversion.To_Pointer (Data).all, Serial);");
               Put_Line (File, "      end Internal_Configure;");
               Put_Line (File, "");
               Put_Line (File, "      Listener : aliased Thin." & Name & "_Listener_T :=");
               Put_Line (File, "        (Configure => Internal_Configure'Unrestricted_Access);");
               Put_Line (File, "");
            elsif Name = "Xdg_Toplevel" then
               Put_Line (File, "      procedure Internal_Configure");
               Put_Line (File, "        (Data         : Void_Ptr;");
               Put_Line (File, "         Xdg_Toplevel : Thin.Xdg_Toplevel_Ptr;");
               Put_Line (File, "         Width        : Integer;");
               Put_Line (File, "         Height       : Integer;");
               Put_Line (File, "         States       : Wayland_Array)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Close");
               Put_Line (File, "        (Data         : Void_Ptr;");
               Put_Line (File, "         Xdg_Toplevel : Thin.Xdg_Toplevel_Ptr)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Configure");
               Put_Line (File, "        (Data         : Void_Ptr;");
               Put_Line (File, "         Xdg_Toplevel : Thin.Xdg_Toplevel_Ptr;");
               Put_Line (File, "         Width        : Integer;");
               Put_Line (File, "         Height       : Integer;");
               Put_Line (File, "         States       : Wayland_Array)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = " & Name & ");");
               Put_Line (File, "");
               Put_Line (File, "         State_Size : constant := Unsigned_32'Size / System.Storage_Unit;");
               Put_Line (File, "");
               Put_Line (File, "         Current_States : State_Array (1 .. Natural (States.Size) / State_Size)");
               Put_Line (File, "           with Address => States.Data;");
               Put_Line (File, "      begin");
               Put_Line (File, "         Configure (Conversion.To_Pointer (Data).all, Width, Height, Current_States);");
               Put_Line (File, "      end Internal_Configure;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Close");
               Put_Line (File, "        (Data         : Void_Ptr;");
               Put_Line (File, "         Xdg_Toplevel : Thin.Xdg_Toplevel_Ptr)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = " & Name & ");");
               Put_Line (File, "      begin");
               Put_Line (File, "         Close (Conversion.To_Pointer (Data).all);");
               Put_Line (File, "      end Internal_Close;");
               Put_Line (File, "");
               Put_Line (File, "      Listener : aliased Thin." & Name & "_Listener_T :=");
               Put_Line (File, "        (Configure => Internal_Configure'Unrestricted_Access,");
               Put_Line (File, "         Close     => Internal_Close'Unrestricted_Access);");
               Put_Line (File, "");
            elsif Name = "Xdg_Popup" then
               Put_Line (File, "      procedure Internal_Configure");
               Put_Line (File, "        (Data      : Void_Ptr;");
               Put_Line (File, "         Xdg_Popup : Thin.Xdg_Popup_Ptr;");
               Put_Line (File, "         X, Y      : Integer;");
               Put_Line (File, "         Width     : Integer;");
               Put_Line (File, "         Height    : Integer)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Popup_Done");
               Put_Line (File, "        (Data      : Void_Ptr;");
               Put_Line (File, "         Xdg_Popup : Thin.Xdg_Popup_Ptr)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Repositioned");
               Put_Line (File, "        (Data      : Void_Ptr;");
               Put_Line (File, "         Xdg_Popup : Thin.Xdg_Popup_Ptr;");
               Put_Line (File, "         Token     : Unsigned_32)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Configure");
               Put_Line (File, "        (Data      : Void_Ptr;");
               Put_Line (File, "         Xdg_Popup : Thin.Xdg_Popup_Ptr;");
               Put_Line (File, "         X, Y      : Integer;");
               Put_Line (File, "         Width     : Integer;");
               Put_Line (File, "         Height    : Integer)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = " & Name & ");");
               Put_Line (File, "      begin");
               Put_Line (File, "         Configure (Conversion.To_Pointer (Data).all, X, Y, Width, Height);");
               Put_Line (File, "      end Internal_Configure;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Popup_Done");
               Put_Line (File, "        (Data      : Void_Ptr;");
               Put_Line (File, "         Xdg_Popup : Thin.Xdg_Popup_Ptr)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = " & Name & ");");
               Put_Line (File, "      begin");
               Put_Line (File, "         Popup_Done (Conversion.To_Pointer (Data).all);");
               Put_Line (File, "      end Internal_Popup_Done;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Repositioned");
               Put_Line (File, "        (Data      : Void_Ptr;");
               Put_Line (File, "         Xdg_Popup : Thin.Xdg_Popup_Ptr;");
               Put_Line (File, "         Token     : Unsigned_32)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = " & Name & ");");
               Put_Line (File, "      begin");
               Put_Line (File, "         Repositioned (Conversion.To_Pointer (Data).all, Token);");
               Put_Line (File, "      end Internal_Repositioned;");
               Put_Line (File, "");
               Put_Line (File, "      Listener : aliased Thin." & Name & "_Listener_T :=");
               Put_Line (File, "        (Configure    => Internal_Configure'Unrestricted_Access,");
               Put_Line (File, "         Popup_Done   => Internal_Popup_Done'Unrestricted_Access,");
               Put_Line (File, "         Repositioned => Internal_Repositioned'Unrestricted_Access);");
               Put_Line (File, "");
            end if;

            Generate_Suffix_Body_Events (Name);
         end Handle_Interface_Events_Xdg_Shell;

         procedure Handle_Interface_Presentation_Time
           (Interface_Tag : aliased Wayland_XML.Interface_Tag)
         is
            Name : constant String
              := Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Interface_Tag));
         begin
            if Name in "Presentation" then
               Generate_Body_Bind_Subprogram (Name);
            end if;

            if Name = "Presentation" then
               Put_Line (File, "");
               Put_Line (File, "   procedure Feedback");
               Put_Line (File, "     (Object   : Presentation;");
               Put_Line (File, "      Surface  : Protocols.Client.Surface'Class;");
               Put_Line (File, "      Feedback : in out Presentation_Feedback'Class) is");
               Put_Line (File, "   begin");
               Put_Line (File, "      Feedback.Proxy := Thin.Presentation_Feedback (Object.Proxy, Thin_Client.Surface_Ptr (Surface.Get_Proxy));");
               Put_Line (File, "   end Feedback;");
            end if;
         end Handle_Interface_Presentation_Time;

         procedure Handle_Interface_Events_Presentation_Time
           (Interface_Tag : aliased Wayland_XML.Interface_Tag)
         is
            Name : constant String
              := Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Interface_Tag));
         begin
            if not Exists_Events (Interface_Tag) then
               return;
            end if;

            Generate_Prefix_Body_Events (Name);

            if Name = "Presentation" then
               Put_Line (File, "      procedure Internal_Clock_Id");
               Put_Line (File, "        (Data         : Void_Ptr;");
               Put_Line (File, "         Presentation : Thin.Presentation_Ptr;");
               Put_Line (File, "         Clk_Id       : Unsigned_32)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Clock_Id");
               Put_Line (File, "        (Data         : Void_Ptr;");
               Put_Line (File, "         Presentation : Thin.Presentation_Ptr;");
               Put_Line (File, "         Clk_Id       : Unsigned_32)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = " & Name & ");");
               Put_Line (File, "      begin");
               Put_Line (File, "         Clock (Conversion.To_Pointer (Data).all, Clk_Id);");
               Put_Line (File, "      end Internal_Clock_Id;");
               Put_Line (File, "");
               Put_Line (File, "      Listener : aliased Thin." & Name & "_Listener_T :=");
               Put_Line (File, "        (Clock_Id => Internal_Clock_Id'Unrestricted_Access);");
               Put_Line (File, "");
            elsif Name = "Presentation_Feedback" then
               Put_Line (File, "      procedure Internal_Sync_Output");
               Put_Line (File, "        (Data                  : Void_Ptr;");
               Put_Line (File, "         Presentation_Feedback : Thin.Presentation_Feedback_Ptr;");
               Put_Line (File, "         Output                : Protocols.Thin_Client.Output_Ptr)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Presented");
               Put_Line (File, "        (Data                  : Void_Ptr;");
               Put_Line (File, "         Presentation_Feedback : Thin.Presentation_Feedback_Ptr;");
               Put_Line (File, "         Tv_Sec_Hi             : Unsigned_32;");
               Put_Line (File, "         Tv_Sec_Lo             : Unsigned_32;");
               Put_Line (File, "         Tv_Nsec               : Unsigned_32;");
               Put_Line (File, "         Refresh               : Unsigned_32;");
               Put_Line (File, "         Seq_Hi                : Unsigned_32;");
               Put_Line (File, "         Seq_Lo                : Unsigned_32;");
               Put_Line (File, "         Flags                 : Enums.Presentation_Time.Presentation_Feedback_Kind)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Discarded");
               Put_Line (File, "        (Data                  : Void_Ptr;");
               Put_Line (File, "         Presentation_Feedback : Thin.Presentation_Feedback_Ptr)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Sync_Output");
               Put_Line (File, "        (Data                  : Void_Ptr;");
               Put_Line (File, "         Presentation_Feedback : Thin.Presentation_Feedback_Ptr;");
               Put_Line (File, "         Output                : Protocols.Thin_Client.Output_Ptr)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = " & Name & ");");
               Put_Line (File, "");
               Put_Line (File, "         O : constant Protocols.Client.Output :=");
               Put_Line (File, "           Protocols.Client.Constructors.Set_Proxy (Proxy => Secret_Proxy (Output));");
               Put_Line (File, "      begin");
               Put_Line (File, "         Synchronized_Output (Conversion.To_Pointer (Data).all, O);");
               Put_Line (File, "      end Internal_Sync_Output;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Presented");
               Put_Line (File, "        (Data                  : Void_Ptr;");
               Put_Line (File, "         Presentation_Feedback : Thin.Presentation_Feedback_Ptr;");
               Put_Line (File, "         Tv_Sec_Hi             : Unsigned_32;");
               Put_Line (File, "         Tv_Sec_Lo             : Unsigned_32;");
               Put_Line (File, "         Tv_Nsec               : Unsigned_32;");
               Put_Line (File, "         Refresh               : Unsigned_32;");
               Put_Line (File, "         Seq_Hi                : Unsigned_32;");
               Put_Line (File, "         Seq_Lo                : Unsigned_32;");
               Put_Line (File, "         Flags                 : Enums.Presentation_Time.Presentation_Feedback_Kind)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = " & Name & ");");
               Put_Line (File, "");
               Put_Line (File, "         type Unsigned_64_Group is record");
               Put_Line (File, "            Low, High : Unsigned_32;");
               Put_Line (File, "         end record;");
               Put_Line (File, "");
               Put_Line (File, "         for Unsigned_64_Group use record");
               Put_Line (File, "            Low  at 0 range 0 .. 31;");
               Put_Line (File, "            High at 0 range 32 .. 63;");
               Put_Line (File, "         end record;");
               Put_Line (File, "");
               Put_Line (File, "         function Convert is new Ada.Unchecked_Conversion");
               Put_Line (File, "           (Source => Unsigned_64_Group, Target => Unsigned_64);");
               Put_Line (File, "");
               Put_Line (File, "         Seconds   : constant Unsigned_64 := Convert ((Low => Tv_Sec_Lo, High => Tv_Sec_Hi));");
               Put_Line (File, "         Counter   : constant Unsigned_64 := Convert ((Low => Seq_Lo, High => Seq_Hi));");
               Put_Line (File, "");
               Put_Line (File, "         Timestamp : constant Duration := Duration (Seconds) + Duration (Tv_Nsec) / 1e9;");
               Put_Line (File, "      begin");
               Put_Line (File, "         Presented (Conversion.To_Pointer (Data).all, Timestamp, Duration (Refresh) / 1e9, Counter, Flags);");
               Put_Line (File, "      end Internal_Presented;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Discarded");
               Put_Line (File, "        (Data                  : Void_Ptr;");
               Put_Line (File, "         Presentation_Feedback : Thin.Presentation_Feedback_Ptr)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = " & Name & ");");
               Put_Line (File, "      begin");
               Put_Line (File, "         Discarded (Conversion.To_Pointer (Data).all);");
               Put_Line (File, "      end Internal_Discarded;");
               Put_Line (File, "");
               Put_Line (File, "      Listener : aliased Thin." & Name & "_Listener_T :=");
               Put_Line (File, "        (Sync_Output => Internal_Sync_Output'Unrestricted_Access,");
               Put_Line (File, "         Presented   => Internal_Presented'Unrestricted_Access,");
               Put_Line (File, "         Discarded   => Internal_Discarded'Unrestricted_Access);");
               Put_Line (File, "");
            end if;

            Generate_Suffix_Body_Events (Name);
         end Handle_Interface_Events_Presentation_Time;

         procedure Handle_Interface_Viewporter
           (Interface_Tag : aliased Wayland_XML.Interface_Tag)
         is
            Name : constant String
              := Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Interface_Tag));
         begin
            if Name in "Viewporter" then
               Generate_Body_Bind_Subprogram (Name);
            end if;

            if Name = "Viewporter" then
               Put_Line (File, "");
               Put_Line (File, "   procedure Get_Viewport");
               Put_Line (File, "     (Object   : Viewporter;");
               Put_Line (File, "      Surface  : Client.Surface'Class;");
               Put_Line (File, "      Viewport : in out Protocols.Viewporter.Viewport'Class) is");
               Put_Line (File, "   begin");
               Put_Line (File, "      Viewport.Proxy := Thin.Viewporter_Get_Viewport (Object.Proxy, Thin_Client.Surface_Ptr (Surface.Get_Proxy));");
               Put_Line (File, "   end Get_Viewport;");
            elsif Name = "Viewport" then
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_Source");
               Put_Line (File, "     (Object              : Viewport;");
               Put_Line (File, "      X, Y, Width, Height : Fixed) is");
               Put_Line (File, "   begin");
               Put_Line (File, "      Thin.Viewport_Set_Source (Object.Proxy, X, Y, Width, Height);");
               Put_Line (File, "   end Set_Source;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_Destination");
               Put_Line (File, "     (Object        : Viewport;");
               Put_Line (File, "      Width, Height : Integer) is");
               Put_Line (File, "   begin");
               Put_Line (File, "      Thin.Viewport_Set_Destination (Object.Proxy, Width, Height);");
               Put_Line (File, "   end Set_Destination;");
            end if;
         end Handle_Interface_Viewporter;

         procedure Handle_Interface_Idle_Inhibit
           (Interface_Tag : aliased Wayland_XML.Interface_Tag)
         is
            Name : constant String
              := Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Interface_Tag));
         begin
            if Name in "Idle_Inhibit_Manager_V1" then
               Generate_Body_Bind_Subprogram (Name);
            end if;

            if Name = "Idle_Inhibit_Manager_V1" then
               Put_Line (File, "");
               Put_Line (File, "   procedure Create_Inhibitor");
               Put_Line (File, "     (Object    : Idle_Inhibit_Manager_V1;");
               Put_Line (File, "      Surface   : Client.Surface'Class;");
               Put_Line (File, "      Inhibitor : in out Idle_Inhibitor_V1'Class) is");
               Put_Line (File, "   begin");
               Put_Line (File, "      Inhibitor.Proxy := Thin.Idle_Inhibit_Manager_V1_Create_Inhibitor");
               Put_Line (File, "        (Object.Proxy, Thin_Client.Surface_Ptr (Surface.Get_Proxy));");
               Put_Line (File, "   end Create_Inhibitor;");
            elsif Name = "Idle_Inhibitor_V1" then
               null;
            end if;
         end Handle_Interface_Idle_Inhibit;

         procedure Handle_Interface_Xdg_Decoration
           (Interface_Tag : aliased Wayland_XML.Interface_Tag)
         is
            Name : constant String
              := Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Interface_Tag));
         begin
            if Name in "Decoration_Manager_V1" then
               Generate_Body_Bind_Subprogram (Name);
            end if;

            if Name = "Decoration_Manager_V1" then
               Put_Line (File, "");
               Put_Line (File, "   procedure Get_Toplevel_Decoration");
               Put_Line (File, "     (Object     : Decoration_Manager_V1;");
               Put_Line (File, "      Toplevel   : Xdg_Shell.Xdg_Toplevel'Class;");
               Put_Line (File, "      Decoration : in out Toplevel_Decoration_V1'Class) is");
               Put_Line (File, "   begin");
               Put_Line (File, "      Decoration.Proxy := Thin.Decoration_Manager_V1_Get_Toplevel_Decoration");
               Put_Line (File, "        (Object.Proxy, Thin_Xdg_Shell.Xdg_Toplevel_Ptr (Toplevel.Get_Proxy));");
               Put_Line (File, "   end Get_Toplevel_Decoration;");
            elsif Name = "Toplevel_Decoration_V1" then
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_Mode");
               Put_Line (File, "     (Object : Toplevel_Decoration_V1;");
               Put_Line (File, "      Mode   : Toplevel_Decoration_V1_Mode) is");
               Put_Line (File, "   begin");
               Put_Line (File, "      Thin.Toplevel_Decoration_V1_Set_Mode (Object.Proxy, Mode);");
               Put_Line (File, "   end Set_Mode;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Unset_Mode (Object : Toplevel_Decoration_V1) is");
               Put_Line (File, "   begin");
               Put_Line (File, "      Thin.Toplevel_Decoration_V1_Unset_Mode (Object.Proxy);");
               Put_Line (File, "   end Unset_Mode;");
            end if;
         end Handle_Interface_Xdg_Decoration;

         procedure Handle_Interface_Events_Xdg_Decoration
           (Interface_Tag : aliased Wayland_XML.Interface_Tag)
         is
            Name : constant String
              := Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Interface_Tag));
         begin
            if not Exists_Events (Interface_Tag) then
               return;
            end if;

            Generate_Prefix_Body_Events (Name);

            if Name = "Toplevel_Decoration_V1" then
               Put_Line (File, "      procedure Internal_Configure");
               Put_Line (File, "        (Data                   : Void_Ptr;");
               Put_Line (File, "         Toplevel_Decoration_V1 : Thin.Toplevel_Decoration_V1_Ptr;");
               Put_Line (File, "         Mode                   : Toplevel_Decoration_V1_Mode)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Configure");
               Put_Line (File, "        (Data                   : Void_Ptr;");
               Put_Line (File, "         Toplevel_Decoration_V1 : Thin.Toplevel_Decoration_V1_Ptr;");
               Put_Line (File, "         Mode                   : Toplevel_Decoration_V1_Mode)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = " & Name & ");");
               Put_Line (File, "      begin");
               Put_Line (File, "         Configure (Conversion.To_Pointer (Data).all, Mode);");
               Put_Line (File, "      end Internal_Configure;");
               Put_Line (File, "");
               Put_Line (File, "      Listener : aliased Thin." & Name & "_Listener_T :=");
               Put_Line (File, "        (Configure => Internal_Configure'Unrestricted_Access);");
               Put_Line (File, "");
            end if;

            Generate_Suffix_Body_Events (Name);
         end Handle_Interface_Events_Xdg_Decoration;

         procedure Handle_Interface_Relative_Pointer
           (Interface_Tag : aliased Wayland_XML.Interface_Tag)
         is
            Name : constant String
              := Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Interface_Tag));
         begin
            if Name in "Relative_Pointer_Manager_V1" then
               Generate_Body_Bind_Subprogram (Name);
            end if;

            if Name = "Relative_Pointer_Manager_V1" then
               Put_Line (File, "");
               Put_Line (File, "   procedure Get_Relative_Pointer");
               Put_Line (File, "     (Object   : Relative_Pointer_Manager_V1;");
               Put_Line (File, "      Pointer  : Client.Pointer'Class;");
               Put_Line (File, "      Relative : in out Relative_Pointer_V1'Class) is");
               Put_Line (File, "   begin");
               Put_Line (File, "      Relative.Proxy := Thin.Relative_Pointer_Manager_V1_Get_Relative_Pointer (Object.Proxy, Thin_Client.Pointer_Ptr (Pointer.Get_Proxy));");
               Put_Line (File, "   end Get_Relative_Pointer;");
            elsif Name = "Toplevel_Decoration_V1" then
               null;
            end if;
         end Handle_Interface_Relative_Pointer;

         procedure Handle_Interface_Events_Relative_Pointer
           (Interface_Tag : aliased Wayland_XML.Interface_Tag)
         is
            Name : constant String
              := Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Interface_Tag));
         begin
            if not Exists_Events (Interface_Tag) then
               return;
            end if;

            Generate_Prefix_Body_Events (Name);

            if Name = "Relative_Pointer_V1" then
               Put_Line (File, "      procedure Internal_Relative_Motion");
               Put_Line (File, "        (Data                : Void_Ptr;");
               Put_Line (File, "         Relative_Pointer_V1 : Thin.Relative_Pointer_V1_Ptr;");
               Put_Line (File, "         Utime_Hi            : Unsigned_32;");
               Put_Line (File, "         Utime_Lo            : Unsigned_32;");
               Put_Line (File, "         Dx                  : Fixed;");
               Put_Line (File, "         Dy                  : Fixed;");
               Put_Line (File, "         Dx_Unaccel          : Fixed;");
               Put_Line (File, "         Dy_Unaccel          : Fixed)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Relative_Motion");
               Put_Line (File, "        (Data                : Void_Ptr;");
               Put_Line (File, "         Relative_Pointer_V1 : Thin.Relative_Pointer_V1_Ptr;");
               Put_Line (File, "         Utime_Hi            : Unsigned_32;");
               Put_Line (File, "         Utime_Lo            : Unsigned_32;");
               Put_Line (File, "         Dx                  : Fixed;");
               Put_Line (File, "         Dy                  : Fixed;");
               Put_Line (File, "         Dx_Unaccel          : Fixed;");
               Put_Line (File, "         Dy_Unaccel          : Fixed)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = " & Name & ");");
               Put_Line (File, "");
               Put_Line (File, "         type Unsigned_64_Group is record");
               Put_Line (File, "            Low, High : Unsigned_32;");
               Put_Line (File, "         end record;");
               Put_Line (File, "");
               Put_Line (File, "         for Unsigned_64_Group use record");
               Put_Line (File, "            Low  at 0 range 0 .. 31;");
               Put_Line (File, "            High at 0 range 32 .. 63;");
               Put_Line (File, "         end record;");
               Put_Line (File, "");
               Put_Line (File, "         function Convert is new Ada.Unchecked_Conversion");
               Put_Line (File, "           (Source => Unsigned_64_Group, Target => Unsigned_64);");
               Put_Line (File, "");
               Put_Line (File, "         Microseconds   : constant Unsigned_64 := Convert ((Low => Utime_Lo, High => Utime_Hi));");
               Put_Line (File, "");
               Put_Line (File, "         Timestamp : constant Duration := Duration (Microseconds / 1e6) + Duration (Microseconds mod 1e6) / 1e6;");
               Put_Line (File, "      begin");
               Put_Line (File, "         Relative_Motion (Conversion.To_Pointer (Data).all, Timestamp, Dx, Dy, Dx_Unaccel, Dy_Unaccel);");
               Put_Line (File, "      end Internal_Relative_Motion;");
               Put_Line (File, "");
               Put_Line (File, "      Listener : aliased Thin." & Name & "_Listener_T :=");
               Put_Line (File, "        (Relative_Motion => Internal_Relative_Motion'Unrestricted_Access);");
               Put_Line (File, "");
            end if;

            Generate_Suffix_Body_Events (Name);
         end Handle_Interface_Events_Relative_Pointer;

         procedure Handle_Interface_Pointer_Constraints
           (Interface_Tag : aliased Wayland_XML.Interface_Tag)
         is
            Name : constant String
              := Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Interface_Tag));
         begin
            if Name in "Pointer_Constraints_V1" then
               Generate_Body_Bind_Subprogram (Name);
            end if;

            if Name = "Pointer_Constraints_V1" then
               Put_Line (File, "");
               Put_Line (File, "   procedure Lock_Pointer");
               Put_Line (File, "     (Object   : Pointer_Constraints_V1;");
               Put_Line (File, "      Surface  : Client.Surface'Class;");
               Put_Line (File, "      Pointer  : Client.Pointer'Class;");
               Put_Line (File, "      Region   : Client.Region'Class;");
               Put_Line (File, "      Lifetime : Pointer_Constraints_V1_Lifetime;");
               Put_Line (File, "      Locked   : in out Locked_Pointer_V1'Class) is");
               Put_Line (File, "   begin");
               Put_Line (File, "      Locked.Proxy := Thin.Pointer_Constraints_V1_Lock_Pointer");
               Put_Line (File, "        (Object.Proxy,");
               Put_Line (File, "         Thin_Client.Surface_Ptr (Surface.Get_Proxy),");
               Put_Line (File, "         Thin_Client.Pointer_Ptr (Pointer.Get_Proxy),");
               Put_Line (File, "         Thin_Client.Region_Ptr (Region.Get_Proxy),");
               Put_Line (File, "         Lifetime);");
               Put_Line (File, "   end Lock_Pointer;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Confine_Pointer");
               Put_Line (File, "     (Object   : Pointer_Constraints_V1;");
               Put_Line (File, "      Surface  : Client.Surface'Class;");
               Put_Line (File, "      Pointer  : Client.Pointer'Class;");
               Put_Line (File, "      Region   : Client.Region'Class;");
               Put_Line (File, "      Lifetime : Pointer_Constraints_V1_Lifetime;");
               Put_Line (File, "      Confined : in out Confined_Pointer_V1'Class) is");
               Put_Line (File, "   begin");
               Put_Line (File, "      Confined.Proxy := Thin.Pointer_Constraints_V1_Confine_Pointer");
               Put_Line (File, "        (Object.Proxy,");
               Put_Line (File, "         Thin_Client.Surface_Ptr (Surface.Get_Proxy),");
               Put_Line (File, "         Thin_Client.Pointer_Ptr (Pointer.Get_Proxy),");
               Put_Line (File, "         Thin_Client.Region_Ptr (Region.Get_Proxy),");
               Put_Line (File, "         Lifetime);");
               Put_Line (File, "   end Confine_Pointer;");
            elsif Name = "Locked_Pointer_V1" then
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_Cursor_Position_Hint");
               Put_Line (File, "     (Object : Locked_Pointer_V1;");
               Put_Line (File, "      X, Y   : Fixed) is");
               Put_Line (File, "   begin");
               Put_Line (File, "      Thin.Locked_Pointer_V1_Set_Cursor_Position_Hint (Object.Proxy, X, Y);");
               Put_Line (File, "   end Set_Cursor_Position_Hint;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_Region");
               Put_Line (File, "     (Object : Locked_Pointer_V1;");
               Put_Line (File, "      Region : Client.Region'Class) is");
               Put_Line (File, "   begin");
               Put_Line (File, "      Thin.Locked_Pointer_V1_Set_Region (Object.Proxy, Thin_Client.Region_Ptr (Region.Get_Proxy));");
               Put_Line (File, "   end Set_Region;");
            elsif Name = "Confined_Pointer_V1" then
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_Region");
               Put_Line (File, "     (Object : Confined_Pointer_V1;");
               Put_Line (File, "      Region : Client.Region'Class) is");
               Put_Line (File, "   begin");
               Put_Line (File, "      Thin.Confined_Pointer_V1_Set_Region (Object.Proxy, Thin_Client.Region_Ptr (Region.Get_Proxy));");
               Put_Line (File, "   end Set_Region;");
            end if;
         end Handle_Interface_Pointer_Constraints;

         procedure Handle_Interface_Events_Pointer_Constraints
           (Interface_Tag : aliased Wayland_XML.Interface_Tag)
         is
            Name : constant String
              := Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Interface_Tag));
         begin
            if not Exists_Events (Interface_Tag) then
               return;
            end if;

            Generate_Prefix_Body_Events (Name);

            if Name = "Locked_Pointer_V1" then
               Put_Line (File, "      procedure Internal_Locked");
               Put_Line (File, "        (Data              : Void_Ptr;");
               Put_Line (File, "         Locked_Pointer_V1 : Thin.Locked_Pointer_V1_Ptr)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Unlocked");
               Put_Line (File, "        (Data              : Void_Ptr;");
               Put_Line (File, "         Locked_Pointer_V1 : Thin.Locked_Pointer_V1_Ptr)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Locked");
               Put_Line (File, "        (Data              : Void_Ptr;");
               Put_Line (File, "         Locked_Pointer_V1 : Thin.Locked_Pointer_V1_Ptr)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = " & Name & ");");
               Put_Line (File, "      begin");
               Put_Line (File, "         Locked (Conversion.To_Pointer (Data).all);");
               Put_Line (File, "      end Internal_Locked;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Unlocked");
               Put_Line (File, "        (Data              : Void_Ptr;");
               Put_Line (File, "         Locked_Pointer_V1 : Thin.Locked_Pointer_V1_Ptr)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = " & Name & ");");
               Put_Line (File, "      begin");
               Put_Line (File, "         Unlocked (Conversion.To_Pointer (Data).all);");
               Put_Line (File, "      end Internal_Unlocked;");
               Put_Line (File, "");
               Put_Line (File, "      Listener : aliased Thin." & Name & "_Listener_T :=");
               Put_Line (File, "        (Locked   => Internal_Locked'Unrestricted_Access,");
               Put_Line (File, "         Unlocked => Internal_Unlocked'Unrestricted_Access);");
               Put_Line (File, "");
            elsif Name = "Confined_Pointer_V1" then
               Put_Line (File, "      procedure Internal_Confined");
               Put_Line (File, "        (Data                : Void_Ptr;");
               Put_Line (File, "         Confined_Pointer_V1 : Thin.Confined_Pointer_V1_Ptr)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Unconfined");
               Put_Line (File, "        (Data                : Void_Ptr;");
               Put_Line (File, "         Confined_Pointer_V1 : Thin.Confined_Pointer_V1_Ptr)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Confined");
               Put_Line (File, "        (Data                : Void_Ptr;");
               Put_Line (File, "         Confined_Pointer_V1 : Thin.Confined_Pointer_V1_Ptr)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = " & Name & ");");
               Put_Line (File, "      begin");
               Put_Line (File, "         Confined (Conversion.To_Pointer (Data).all);");
               Put_Line (File, "      end Internal_Confined;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Unconfined");
               Put_Line (File, "        (Data                : Void_Ptr;");
               Put_Line (File, "         Confined_Pointer_V1 : Thin.Confined_Pointer_V1_Ptr)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = " & Name & ");");
               Put_Line (File, "      begin");
               Put_Line (File, "         Unconfined (Conversion.To_Pointer (Data).all);");
               Put_Line (File, "      end Internal_Unconfined;");
               Put_Line (File, "");
               Put_Line (File, "      Listener : aliased Thin." & Name & "_Listener_T :=");
               Put_Line (File, "        (Confined   => Internal_Confined'Unrestricted_Access,");
               Put_Line (File, "         Unconfined => Internal_Unconfined'Unrestricted_Access);");
               Put_Line (File, "");
            end if;

            Generate_Suffix_Body_Events (Name);
         end Handle_Interface_Events_Pointer_Constraints;

         procedure Handle_Interface_Pointer_Gestures
           (Interface_Tag : aliased Wayland_XML.Interface_Tag)
         is
            Name : constant String
              := Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Interface_Tag));
         begin
            if Name in "Pointer_Gestures_V1" then
               Generate_Body_Bind_Subprogram (Name);
            end if;

            if Name in "Pointer_Gestures_V1" then
               Put_Line (File, "");
               Put_Line (File, "   procedure Get_Swipe_Gesture");
               Put_Line (File, "     (Object  : Pointer_Gestures_V1;");
               Put_Line (File, "      Pointer : Client.Pointer'Class;");
               Put_Line (File, "      Gesture : in out Pointer_Gesture_Swipe_V1'Class) is");
               Put_Line (File, "   begin");
               Put_Line (File, "      Gesture.Proxy := Thin.Pointer_Gestures_V1_Get_Swipe_Gesture");
               Put_Line (File, "        (Object.Proxy, Thin_Client.Pointer_Ptr (Pointer.Get_Proxy));");
               Put_Line (File, "   end Get_Swipe_Gesture;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Get_Pinch_Gesture");
               Put_Line (File, "     (Object  : Pointer_Gestures_V1;");
               Put_Line (File, "      Pointer : Client.Pointer'Class;");
               Put_Line (File, "      Gesture : in out Pointer_Gesture_Pinch_V1'Class) is");
               Put_Line (File, "   begin");
               Put_Line (File, "      Gesture.Proxy := Thin.Pointer_Gestures_V1_Get_Pinch_Gesture");
               Put_Line (File, "        (Object.Proxy, Thin_Client.Pointer_Ptr (Pointer.Get_Proxy));");
               Put_Line (File, "   end Get_Pinch_Gesture;");
            end if;
         end Handle_Interface_Pointer_Gestures;

         procedure Handle_Interface_Events_Pointer_Gestures
           (Interface_Tag : aliased Wayland_XML.Interface_Tag)
         is
            Name : constant String
              := Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Interface_Tag));
         begin
            if not Exists_Events (Interface_Tag) then
               return;
            end if;

            Generate_Prefix_Body_Events (Name);

            if Name = "Pointer_Gesture_Swipe_V1" then
               Put_Line (File, "      procedure Internal_Begin");
               Put_Line (File, "        (Data                     : Void_Ptr;");
               Put_Line (File, "         Pointer_Gesture_Swipe_V1 : Thin.Pointer_Gesture_Swipe_V1_Ptr;");
               Put_Line (File, "         Serial                   : Unsigned_32;");
               Put_Line (File, "         Time                     : Unsigned_32;");
               Put_Line (File, "         Surface                  : Thin_Client.Surface_Ptr;");
               Put_Line (File, "         Fingers                  : Unsigned_32)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Update");
               Put_Line (File, "        (Data                     : Void_Ptr;");
               Put_Line (File, "         Pointer_Gesture_Swipe_V1 : Thin.Pointer_Gesture_Swipe_V1_Ptr;");
               Put_Line (File, "         Time                     : Unsigned_32;");
               Put_Line (File, "         Dx                       : Fixed;");
               Put_Line (File, "         Dy                       : Fixed)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_End");
               Put_Line (File, "        (Data                     : Void_Ptr;");
               Put_Line (File, "         Pointer_Gesture_Swipe_V1 : Thin.Pointer_Gesture_Swipe_V1_Ptr;");
               Put_Line (File, "         Serial                   : Unsigned_32;");
               Put_Line (File, "         Time                     : Unsigned_32;");
               Put_Line (File, "         Cancelled                : Integer)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Begin");
               Put_Line (File, "        (Data                     : Void_Ptr;");
               Put_Line (File, "         Pointer_Gesture_Swipe_V1 : Thin.Pointer_Gesture_Swipe_V1_Ptr;");
               Put_Line (File, "         Serial                   : Unsigned_32;");
               Put_Line (File, "         Time                     : Unsigned_32;");
               Put_Line (File, "         Surface                  : Thin_Client.Surface_Ptr;");
               Put_Line (File, "         Fingers                  : Unsigned_32)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = " & Name & ");");
               Put_Line (File, "");
               Put_Line (File, "         Timestamp : constant Duration := Duration (Time) / 1e3;");
               Put_Line (File, "");
               Put_Line (File, "         S : constant Protocols.Client.Surface :=");
               Put_Line (File, "           Protocols.Client.Constructors.Set_Proxy (Proxy => Secret_Proxy (Surface));");
               Put_Line (File, "      begin");
               Put_Line (File, "         Gesture_Begin (Conversion.To_Pointer (Data).all, Serial, Timestamp, S, Fingers);");
               Put_Line (File, "      end Internal_Begin;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Update");
               Put_Line (File, "        (Data                     : Void_Ptr;");
               Put_Line (File, "         Pointer_Gesture_Swipe_V1 : Thin.Pointer_Gesture_Swipe_V1_Ptr;");
               Put_Line (File, "         Time                     : Unsigned_32;");
               Put_Line (File, "         Dx                       : Fixed;");
               Put_Line (File, "         Dy                       : Fixed)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = " & Name & ");");
               Put_Line (File, "");
               Put_Line (File, "         Timestamp : constant Duration := Duration (Time) / 1e3;");
               Put_Line (File, "      begin");
               Put_Line (File, "         Gesture_Update (Conversion.To_Pointer (Data).all, Timestamp, Dx, Dy);");
               Put_Line (File, "      end Internal_Update;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_End");
               Put_Line (File, "        (Data                     : Void_Ptr;");
               Put_Line (File, "         Pointer_Gesture_Swipe_V1 : Thin.Pointer_Gesture_Swipe_V1_Ptr;");
               Put_Line (File, "         Serial                   : Unsigned_32;");
               Put_Line (File, "         Time                     : Unsigned_32;");
               Put_Line (File, "         Cancelled                : Integer)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = " & Name & ");");
               Put_Line (File, "");
               Put_Line (File, "         Timestamp : constant Duration := Duration (Time) / 1e3;");
               Put_Line (File, "      begin");
               Put_Line (File, "         Gesture_End (Conversion.To_Pointer (Data).all, Serial, Timestamp, Cancelled = 1);");
               Put_Line (File, "      end Internal_End;");
               Put_Line (File, "");
               Put_Line (File, "      Listener : aliased Thin." & Name & "_Listener_T :=");
               Put_Line (File, "        (Begin_F => Internal_Begin'Unrestricted_Access,");
               Put_Line (File, "         Update  => Internal_Update'Unrestricted_Access,");
               Put_Line (File, "         End_F   => Internal_End'Unrestricted_Access);");
               Put_Line (File, "");
            elsif Name = "Pointer_Gesture_Pinch_V1" then
               Put_Line (File, "      procedure Internal_Begin");
               Put_Line (File, "        (Data                     : Void_Ptr;");
               Put_Line (File, "         Pointer_Gesture_Pinch_V1 : Thin.Pointer_Gesture_Pinch_V1_Ptr;");
               Put_Line (File, "         Serial                   : Unsigned_32;");
               Put_Line (File, "         Time                     : Unsigned_32;");
               Put_Line (File, "         Surface                  : Thin_Client.Surface_Ptr;");
               Put_Line (File, "         Fingers                  : Unsigned_32)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Update");
               Put_Line (File, "        (Data                     : Void_Ptr;");
               Put_Line (File, "         Pointer_Gesture_Pinch_V1 : Thin.Pointer_Gesture_Pinch_V1_Ptr;");
               Put_Line (File, "         Time                     : Unsigned_32;");
               Put_Line (File, "         Dx                       : Fixed;");
               Put_Line (File, "         Dy                       : Fixed;");
               Put_Line (File, "         Scale                    : Fixed;");
               Put_Line (File, "         Rotation                 : Fixed)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_End");
               Put_Line (File, "        (Data                     : Void_Ptr;");
               Put_Line (File, "         Pointer_Gesture_Pinch_V1 : Thin.Pointer_Gesture_Pinch_V1_Ptr;");
               Put_Line (File, "         Serial                   : Unsigned_32;");
               Put_Line (File, "         Time                     : Unsigned_32;");
               Put_Line (File, "         Cancelled                : Integer)");
               Put_Line (File, "      with Convention => C;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Begin");
               Put_Line (File, "        (Data                     : Void_Ptr;");
               Put_Line (File, "         Pointer_Gesture_Pinch_V1 : Thin.Pointer_Gesture_Pinch_V1_Ptr;");
               Put_Line (File, "         Serial                   : Unsigned_32;");
               Put_Line (File, "         Time                     : Unsigned_32;");
               Put_Line (File, "         Surface                  : Thin_Client.Surface_Ptr;");
               Put_Line (File, "         Fingers                  : Unsigned_32)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = " & Name & ");");
               Put_Line (File, "");
               Put_Line (File, "         Timestamp : constant Duration := Duration (Time) / 1e3;");
               Put_Line (File, "");
               Put_Line (File, "         S : constant Protocols.Client.Surface :=");
               Put_Line (File, "           Protocols.Client.Constructors.Set_Proxy (Proxy => Secret_Proxy (Surface));");
               Put_Line (File, "      begin");
               Put_Line (File, "         Gesture_Begin (Conversion.To_Pointer (Data).all, Serial, Timestamp, S, Fingers);");
               Put_Line (File, "      end Internal_Begin;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_Update");
               Put_Line (File, "        (Data                     : Void_Ptr;");
               Put_Line (File, "         Pointer_Gesture_Pinch_V1 : Thin.Pointer_Gesture_Pinch_V1_Ptr;");
               Put_Line (File, "         Time                     : Unsigned_32;");
               Put_Line (File, "         Dx                       : Fixed;");
               Put_Line (File, "         Dy                       : Fixed;");
               Put_Line (File, "         Scale                    : Fixed;");
               Put_Line (File, "         Rotation                 : Fixed)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = " & Name & ");");
               Put_Line (File, "");
               Put_Line (File, "         Timestamp : constant Duration := Duration (Time) / 1e3;");
               Put_Line (File, "      begin");
               Put_Line (File, "         Gesture_Update (Conversion.To_Pointer (Data).all, Timestamp, Dx, Dy, Scale, Rotation);");
               Put_Line (File, "      end Internal_Update;");
               Put_Line (File, "");
               Put_Line (File, "      procedure Internal_End");
               Put_Line (File, "        (Data                     : Void_Ptr;");
               Put_Line (File, "         Pointer_Gesture_Pinch_V1 : Thin.Pointer_Gesture_Pinch_V1_Ptr;");
               Put_Line (File, "         Serial                   : Unsigned_32;");
               Put_Line (File, "         Time                     : Unsigned_32;");
               Put_Line (File, "         Cancelled                : Integer)");
               Put_Line (File, "      is");
               Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = " & Name & ");");
               Put_Line (File, "");
               Put_Line (File, "         Timestamp : constant Duration := Duration (Time) / 1e3;");
               Put_Line (File, "      begin");
               Put_Line (File, "         Gesture_End (Conversion.To_Pointer (Data).all, Serial, Timestamp, Cancelled = 1);");
               Put_Line (File, "      end Internal_End;");
               Put_Line (File, "");
               Put_Line (File, "      Listener : aliased Thin." & Name & "_Listener_T :=");
               Put_Line (File, "        (Begin_F => Internal_Begin'Unrestricted_Access,");
               Put_Line (File, "         Update  => Internal_Update'Unrestricted_Access,");
               Put_Line (File, "         End_F   => Internal_End'Unrestricted_Access);");
               Put_Line (File, "");
            end if;

            Generate_Suffix_Body_Events (Name);
         end Handle_Interface_Events_Pointer_Gestures;

         procedure Handle_Interface_Common_Subprograms
           (Interface_Tag : aliased Wayland_XML.Interface_Tag)
         is
            Name : constant String
              := Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Interface_Tag));
         begin
            if Protocol_Name /= "client" or Name /= "Display" then
               Generate_Body_Destroy_Subprogram (Interface_Tag);
            end if;
            Generate_Body_Utility_Functions (Name);

            if Protocol_Name /= "client" or Name /= "Display" then
               Generate_Body_User_Data_Subprogram (Name);
            end if;
         end Handle_Interface_Common_Subprograms;

         procedure Handle_Interface_Common_Events
           (Interface_Tag : aliased Wayland_XML.Interface_Tag)
         is
            Name : constant String
              := Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Interface_Tag));
         begin
            if not Exists_Events (Interface_Tag) then
               return;
            end if;

            Generate_Prefix_Body_Events (Name);
            Put_Line (File, "      --  FIXME Add internal procedures here");
            Put_Line (File, "");
            Put_Line (File, "      Listener : aliased Thin." & Name & "_Listener_T :=");
            Put_Line (File, "        (FIXME => Internal_FIXME'Unrestricted_Access);");
            Put_Line (File, "");
            Generate_Suffix_Body_Events (Name);
         end Handle_Interface_Common_Events;
      begin
         Put_Line (File, "   subtype int is Interfaces.C.int;");
         Put_Line (File, "   subtype chars_ptr is Interfaces.C.Strings.chars_ptr;");
         Put_Line (File, "");
         Put_Line (File, "   use type int;");
         Put_Line (File, "   use all type chars_ptr;");
         Put_Line (File, "");
         Put_Line (File, "   use type Thin.Proxy_Ptr;");

         Iterate_Over_Interfaces (Handle_Interface_Common_Subprograms'Access);

         if Protocol_Name = "client" then
            Handle_Interface_Client_Prefix;
            Iterate_Over_Interfaces (Handle_Interface_Events_Client'Access);
            Iterate_Over_Interfaces (Handle_Interface_Client'Access);
            Handle_Interface_Client_Suffix;
         elsif Protocol_Name = "xdg_shell" then
            Iterate_Over_Interfaces (Handle_Interface_Events_Xdg_Shell'Access);
            Iterate_Over_Interfaces (Handle_Interface_Xdg_Shell'Access);
         elsif Protocol_Name = "presentation_time" then
            Iterate_Over_Interfaces (Handle_Interface_Events_Presentation_Time'Access);
            Iterate_Over_Interfaces (Handle_Interface_Presentation_Time'Access);
         elsif Protocol_Name = "viewporter" then
            Iterate_Over_Interfaces (Handle_Interface_Viewporter'Access);
         elsif Protocol_Name = "idle_inhibit_unstable_v1" then
            Iterate_Over_Interfaces (Handle_Interface_Idle_Inhibit'Access);
         elsif Protocol_Name = "xdg_decoration_unstable_v1" then
            Iterate_Over_Interfaces (Handle_Interface_Events_Xdg_Decoration'Access);
            Iterate_Over_Interfaces (Handle_Interface_Xdg_Decoration'Access);
         elsif Protocol_Name = "relative_pointer_unstable_v1" then
            Iterate_Over_Interfaces (Handle_Interface_Events_Relative_Pointer'Access);
            Iterate_Over_Interfaces (Handle_Interface_Relative_Pointer'Access);
         elsif Protocol_Name = "pointer_constraints_unstable_v1" then
            Iterate_Over_Interfaces (Handle_Interface_Events_Pointer_Constraints'Access);
            Iterate_Over_Interfaces (Handle_Interface_Pointer_Constraints'Access);
         elsif Protocol_Name = "pointer_gestures_unstable_v1" then
            Iterate_Over_Interfaces (Handle_Interface_Events_Pointer_Gestures'Access);
            Iterate_Over_Interfaces (Handle_Interface_Pointer_Gestures'Access);
         else
            Iterate_Over_Interfaces (Handle_Interface_Common_Events'Access);
         end if;
         Put_Line (File, "");
      end Generate_Manually_Edited_Code;
   begin
      Create_File;
   end Create_Wayland_Body_File;

begin
   Read_Wayland_XML_File
     (File_Name       => Ada.Command_Line.Argument (1),
      Out_Folder      => Ada.Command_Line.Argument (2),
      Enable_Comments => False);
end Wayland_Ada_Scanner;
