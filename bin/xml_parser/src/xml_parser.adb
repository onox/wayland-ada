with Ada.Command_Line;
with Ada.Containers;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Sequential_IO;
with Ada.Streams;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Aida.Deepend.XML_DOM_Parser;

with Wayland_XML;
with Xml_Parser_Utils;

with Standard_Extensions; use Standard_Extensions;

-- chars_ptr should be replaced with C_String in the thin Ada binding.
-- The enumeration subprogram arguments should
-- be used when applicable instead of Unsigned_32
-- Pretty print this file with "gnatpp -M140"
procedure XML_Parser is

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

   package SF renames Ada.Strings.Fixed;
   package SU renames Ada.Strings.Unbounded;

   function "+" (Value : String) return SU.Unbounded_String renames SU.To_Unbounded_String;
   function "+" (Value : SU.Unbounded_String) return String renames SU.To_String;

   XML_Exception : exception;

   procedure Read_Wayland_XML_File (File_Name : String);
   procedure Create_Wayland_Spec_File (File_Name : String);
   procedure Create_Wayland_Body_File (File_Name : String);

   Protocol_Tag : Wayland_XML.Protocol_Tag_Ptr;

   function Get_Protocol_Name (Name : String) return String is
     (if Name = "wayland" then "client" else Name);

   procedure Generate_Code_For_Arg
     (File : Ada.Text_IO.File_Type;
      Arg_Tag    : Wayland_XML.Arg_Tag;
      Max_Length : Natural;
      Is_Last    : Boolean) is
   begin
      declare
         Arg_Name      : constant String
           := Xml_Parser_Utils.Adaify_Variable_Name
             (Name (Arg_Tag));
         Arg_Type_Name : constant String :=
           Xml_Parser_Utils.Arg_Type_As_String (Arg_Tag);

         Arg_Name_Aligned : constant String := SF.Head (Arg_Name, Max_Length, ' ');
      begin
         if Is_Last then
            Put (File, "      " & Arg_Name_Aligned & " : " & Arg_Type_Name & ")");
         else
            Put_Line (File, "      " & Arg_Name_Aligned & " : " & Arg_Type_Name & ";");
         end if;
      end;
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

   procedure Read_Wayland_XML_File (File_Name : String) is

      procedure Check_Wayland_XML_File_Exists;
      procedure Allocate_Space_For_Wayland_XML_Contents;
      procedure Read_Contents_Of_Wayland_XML;
      procedure Parse_Contents (File_Name : String);
      procedure Identify_Protocol_Tag;
      procedure Identify_Protocol_Children (File_Name : String);

      procedure Check_Wayland_XML_File_Exists is
      begin
         if Ada.Directories.Exists (File_Name) then
            Allocate_Space_For_Wayland_XML_Contents;
         else
            Put_Line ("Could not find " & File_Name & "!");
         end if;
      end Check_Wayland_XML_File_Exists;

      File_Size : Natural;

      File_Contents : Aida.Deepend.String_Ptr;

      procedure Allocate_Space_For_Wayland_XML_Contents is
      begin
         File_Size := Natural (Ada.Directories.Size (File_Name));

         if File_Size > 4 then
            File_Contents := new String (1 .. File_Size);
            Read_Contents_Of_Wayland_XML;
         else
            Put_Line ("File " & File_Name & " is too small!");
         end if;
      end Allocate_Space_For_Wayland_XML_Contents;

      pragma Unmodified (File_Size);
      pragma Unmodified (File_Contents);

      procedure Read_Contents_Of_Wayland_XML is
         package IO is new Ada.Sequential_IO (Ada.Streams.Stream_Element);

         File : IO.File_Type;
         SE   : Ada.Streams.Stream_Element;
      begin
         IO.Open (File, IO.In_File, File_Name);

         for I in File_Contents.all'First .. File_Contents.all'Last loop
            IO.Read (File, SE);
            File_Contents (I) := Character'Val (SE);
         end loop;

         IO.Close (File);

         Parse_Contents (File_Name);
      end Read_Contents_Of_Wayland_XML;

      Root_Node : Aida.Deepend.XML_DOM_Parser.Node_Ptr;

      procedure Parse_Contents (File_Name : String) is
         Call_Result : Aida.Call_Result;
      begin
         Aida.Deepend.XML_DOM_Parser.Parse
           (File_Contents.all, Call_Result, Root_Node);

         if Call_Result.Has_Failed then
            Put_Line (Call_Result.Message);
         else
            Identify_Protocol_Tag;
            Identify_Protocol_Children (File_Name);
         end if;
      end Parse_Contents;

      pragma Unmodified (Root_Node);

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

      procedure Identify_Protocol_Children (File_Name : String) is
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
         begin
            if Attributes (Node.Tag).Length = 1 then
               if Name (Attributes (Node.Tag) (1).all) = "summary" then
                  Set_Summary
                    (Description_Tag.all,
                     Value (Attributes (Node.Tag) (1).all));
               else
                  raise XML_Exception;
               end if;
            else
               raise XML_Exception;
            end if;

            if Child_Nodes (Node.Tag).Length = 1 then
               if Child_Nodes (Node.Tag) (1).Id = Node_Kind_Text then
                  Set_Text
                    (Description_Tag.all,
                     Trim (Child_Nodes (Node.Tag) (1).Text.all));
               else
                  raise XML_Exception;
               end if;
            elsif Child_Nodes (Node.Tag).Length > 1 then
               raise XML_Exception;
            end if;

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
                        raise XML_Exception;
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
                           Set_Since_Attribute
                             (Event_Tag.all, Wayland_XML.Version_Number (V));
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
                  else
                     raise XML_Exception;
                  end if;
               end loop;
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
                  else
                     raise XML_Exception;
                  end if;
               else
                  raise XML_Exception;
               end if;
            end loop;
         end Iterate;
      begin
         Iterate (Root_Node.Tag);

         Create_Wayland_Spec_File (File_Name);
         Create_Wayland_Body_File (File_Name);
      end Identify_Protocol_Children;

   begin
      Check_Wayland_XML_File_Exists;
   end Read_Wayland_XML_File;

   pragma Unmodified (Protocol_Tag);

   procedure Generate_Code_For_Numeric_Constants (File : Ada.Text_IO.File_Type) is

      procedure Handle_Interface
        (Interface_Tag : aliased Wayland_XML.Interface_Tag)
      is
         procedure Generate_Code_For_Opcodes is
            I : Integer := 0;

            procedure Generate_Code
              (Request_Tag : Wayland_XML.Request_Tag)
            is
               Name : constant String
                 := Xml_Parser_Utils.Adaify_Name
                   (Wayland_XML.Name (Interface_Tag) & "_" &
                      Wayland_XML.Name (Request_Tag));
            begin
               Put (File, "   " & Name & " : constant := " & Aida.To_String (I) & ";");
               Put_Line (File, "");

               I := I + 1;
            end Generate_Code;
         begin
            for Child of Children (Interface_Tag) loop
               if Child.Kind_Id = Child_Request then
                  Generate_Code (Child.Request_Tag.all);
               end if;
            end loop;
         end Generate_Code_For_Opcodes;

         procedure Generate_Code_For_Event_Since_Version is

            procedure Generate_Code (Event_Tag : Wayland_XML.Event_Tag) is
               Name : constant String
                 := Xml_Parser_Utils.Adaify_Name
                   (Wayland_XML.Name (Interface_Tag) & "_" &
                      Wayland_XML.Name (Event_Tag) & "_SINCE_VERSION");
            begin
               if Exists_Since_Attribute (Event_Tag) then
                  Put (File, "   " & Name & " : constant := " & Aida.To_String (Since_Attribute_As_Pos32 (Event_Tag)) & ";");
               else
                  Put_Line (File, "   " & Name & " : constant := 1;");
               end if;
               Put_Line (File, "");
            end Generate_Code;

         begin
            for Child of Children (Interface_Tag) loop
               if Child.Kind_Id = Child_Event then
                  Generate_Code (Child.Event_Tag.all);
               end if;
            end loop;
         end Generate_Code_For_Event_Since_Version;

         procedure Generate_Code_For_Opcodes_Since_Version is

            procedure Generate_Code (Request_Tag : Wayland_XML.Request_Tag) is
               Name : constant String
                 := Xml_Parser_Utils.Adaify_Name
                   (Wayland_XML.Name (Interface_Tag) & "_" &
                      Wayland_XML.Name (Request_Tag) & "_SINCE_VERSION");
            begin
               if Exists_Since (Request_Tag) then
                  Put (File, "   " & Name & " : constant := " & Aida.To_String (Since_As_Pos32 (Request_Tag)) & ";");
               else
                  Put_Line (File, "   " & Name & " : constant := 1;");
               end if;
               Put_Line (File, "");
            end Generate_Code;

         begin
            for Child of Children (Interface_Tag) loop
               if Child.Kind_Id = Child_Request then
                  Generate_Code (Child.Request_Tag.all);
               end if;
            end loop;
         end Generate_Code_For_Opcodes_Since_Version;

      begin
         Generate_Code_For_Opcodes;
         Generate_Code_For_Event_Since_Version;
         Generate_Code_For_Opcodes_Since_Version;
      end Handle_Interface;

   begin
      for Child of Children (Protocol_Tag.all) loop
         if Child.Kind_Id = Child_Interface then
            Handle_Interface (Child.Interface_Tag.all);
         end if;
      end loop;
   end Generate_Code_For_Numeric_Constants;

   procedure Create_Wayland_Spec_File (File_Name : String) is
      File : Ada.Text_IO.File_Type;

      procedure Create_Wl_Thin_Spec_File;

      procedure Generate_Code_For_Type_Declarations;
      procedure Generate_Code_For_The_Interface_Type;
      procedure Generate_Code_For_The_Interface_Constants;
      procedure Generate_Code_For_Enum_Constants;
      procedure Generate_Manually_Edited_Partial_Type_Declarations;
      procedure Generate_Code_For_The_Private_Part;
      procedure Generate_Use_Type_Declarions;
      procedure Generate_Manually_Edited_Code_For_Type_Definitions;
      procedure Generate_Private_Code_For_The_Interface_Constants;

      procedure Create_File is
         Protocol_Name : constant String := Get_Protocol_Name (Name (Protocol_Tag.all));
         Package_Name  : constant String := Xml_Parser_Utils.Adaify_Name (Protocol_Name);
      begin
         if Protocol_Name = "client" then
            Ada.Text_IO.Create
              (File, Ada.Text_IO.Out_File, "wayland-" & Protocol_Name & "-protocol.ads");

            Put_Line (File, "private with Interfaces.C.Strings;");
            Put_Line (File, "private with Wayland." & Package_Name & ".Thin;");
            New_Line (File);
            Put_Line (File, "with C_Binding.Linux.Files;");
            New_Line (File);
            Put_Line (File, "package Wayland." & Package_Name & ".Protocol is");
            Put_Line (File, "   pragma Preelaborate;");
            New_Line (File);

            Generate_Code_For_Type_Declarations;
            Generate_Code_For_The_Interface_Type;
            Generate_Code_For_The_Interface_Constants;
            Generate_Code_For_Enum_Constants;
            Generate_Manually_Edited_Partial_Type_Declarations;
            Generate_Code_For_The_Private_Part;
            Generate_Use_Type_Declarions;
            Generate_Manually_Edited_Code_For_Type_Definitions;
            Generate_Private_Code_For_The_Interface_Constants;

            Put_Line (File, "end Wayland." & Package_Name & ".Protocol;");

            Ada.Text_IO.Close (File);
         end if;

         -----------------------------------------------------------------------

         Ada.Text_IO.Create
           (File, Ada.Text_IO.Out_File, "wayland-" & Protocol_Name & "-constants.ads");

         Put_Line (File, "private package Wayland." & Package_Name & ".Constants is");
         Put_Line (File, "   pragma Pure;");
         New_Line (File);

         Generate_Code_For_Numeric_Constants (File);

         Put_Line (File, "end " & Package_Name & ".Constants;");

         Ada.Text_IO.Close (File);

         -----------------------------------------------------------------------

         Ada.Text_IO.Create
           (File, Ada.Text_IO.Out_File, "wayland-" & Protocol_Name & "-thin.ads");

         Put_Line (File, "with Interfaces.C.Strings;");
         Put_Line (File, "");
         Put_Line (File, "with Wayland.API;");
         Put_Line (File, "");
         Put_Line (File, "--  Mostly auto generated from " & File_Name);
         Put_Line (File, "private package Wayland." & Package_Name & ".Thin is");
         Put_Line (File, "   pragma Preelaborate;");
         Put_Line (File, "");
         Put_Line (File, "   subtype chars_ptr is Interfaces.C.Strings.chars_ptr;");
         Put_Line (File, "");
         Put_Line (File, "   --  Begin core parts");
         Put_Line (File, "");
         Put_Line (File, "   subtype Interface_T is Wayland.API.Interface_T;");
         Put_Line (File, "");
         Put_Line (File, "   subtype Proxy_Ptr     is Wayland.API.Proxy_Ptr;");
         Put_Line (File, "   subtype Display_Ptr   is Wayland.API.Display_Ptr;");
         Put_Line (File, "   subtype Interface_Ptr is Wayland.API.Interface_Ptr;");
         Put_Line (File, "");
         Put_Line (File, "   function Display_Connect (Name : C_String) return Display_Ptr;");
         Put_Line (File, "");
         Put_Line (File, "   procedure Display_Disconnect (This : in out Display_Ptr);");
         Put_Line (File, "");
         Put_Line (File, "   --  End core parts");
         Put_Line (File, "");

         Create_Wl_Thin_Spec_File;

         Put_Line (File, "");
         Put_Line (File, "end Wayland." & Package_Name & ".Thin;");

         Ada.Text_IO.Close (File);
      end Create_File;

      pragma Unmodified (File);

      procedure Generate_Code_For_Type_Declarations is
         procedure Handle_Interface
           (Interface_Tag : Wayland_XML.Interface_Tag)
         is
            Name : constant String
              := Xml_Parser_Utils.Adaify_Name
                (Wayland_XML.Name (Interface_Tag));
         begin
            Put_Line (File, "   type " & Name & ";");
         end Handle_Interface;
      begin
         for Child of Children (Protocol_Tag.all) loop
            if Child.Kind_Id = Child_Interface then
               Handle_Interface (Child.Interface_Tag.all);
            end if;
         end loop;

         New_Line (File);

         Put_Line (File, "   pragma Linker_Options (""-lwayland-client"");");
         Put_Line (File, "   --  Added this linker option here to avoid adding it");
         Put_Line (File, "   --  to each gpr file that with's this Wayland Ada binding.");
         New_Line (File);
      end Generate_Code_For_Type_Declarations;

      procedure Generate_Code_For_The_Interface_Type is
      begin
         Put_Line (File, "   type Interface_Type is tagged limited private;");
         Put_Line (File, "   --  This type name ends with _Type because 'interface'");
         Put_Line (File, "   --  is a reserved keyword in the Ada programming language.");
         New_Line (File);
         Put_Line (File, "   function Name (I : Interface_Type) return String");
         Put_Line (File, "     with Global => null;");
         New_Line (File);
      end Generate_Code_For_The_Interface_Type;

      procedure Generate_Code_For_The_Interface_Constants is
         procedure Handle_Interface
           (Interface_Tag : Wayland_XML.Interface_Tag)
         is
            Name : constant String
              := Xml_Parser_Utils.Adaify_Name
                (Wayland_XML.Name (Interface_Tag) & "_Interface");
         begin
            Ada.Text_IO.Put (File, "   " & Name);
            Put_Line (File, " : constant Interface_Type;");
            New_Line (File);
         end Handle_Interface;
      begin
         for Child of Children (Protocol_Tag.all) loop
            if Child.Kind_Id = Child_Interface then
               Handle_Interface (Child.Interface_Tag.all);
            end if;
         end loop;

         Put_Line (File, "   Default_Display_Name : constant String := ""wayland-0"";");
         New_Line (File);
      end Generate_Code_For_The_Interface_Constants;

      procedure Generate_Code_For_Enum_Constants is
         procedure Handle_Interface
           (Interface_Tag : aliased Wayland_XML.Interface_Tag)
         is
            procedure Generate_Code (Enum_Tag : aliased Wayland_XML.Enum_Tag) is
               Enum_Type_Name : constant String := Xml_Parser_Utils.Adaify_Name
                 (Name (Interface_Tag) & "_" & Name (Enum_Tag));

               Is_Enum_Used : constant Boolean :=
                 Xml_Parser_Utils.Exists_Reference_To_Enum
                   (Protocol_Tag.all,
                    Name (Interface_Tag),
                    Name (Enum_Tag));

               procedure Generate_Code_For_Enum_Value
                 (Entry_Tag : Wayland_XML.Entry_Tag)
               is
                  Name : constant String := Xml_Parser_Utils.Adaify_Name
                    (Wayland_XML.Name (Interface_Tag) & "_" &
                       Wayland_XML.Name (Enum_Tag) & "_" &
                       Wayland_XML.Name (Entry_Tag));
               begin
                  if Is_Enum_Used then
                     Put (File, "   " & Name & " : constant " & Enum_Type_Name);
                  else
                     Put (File, "   " & Name & " : constant Unsigned_32");
                  end if;
                  Put_Line (File, " := " & Value_As_String (Entry_Tag) & ";");
                  declare
                     Summary_String : constant String := Summary (Entry_Tag);
                  begin
                     if Summary_String'Length > 0 then
                        Put_Line (File, "   --  " & Summary_String);
                     else
                        Put_Line (File, "   --");
                     end if;
                  end;
                  New_Line (File);
               end Generate_Code_For_Enum_Value;

            begin
               if Is_Enum_Used then
                  Put_Line (File, "   type " & Enum_Type_Name & " is new Unsigned_32;");
                  New_Line (File);
               end if;

               for Child of Wayland_XML.Children (Enum_Tag) loop
                  if Child.Kind_Id = Child_Entry then
                     Generate_Code_For_Enum_Value (Child.Entry_Tag.all);
                  end if;
               end loop;
            end Generate_Code;

         begin
            for Child of Children (Interface_Tag) loop
               if Child.Kind_Id = Child_Enum then
                  Generate_Code (Child.Enum_Tag.all);
               end if;
            end loop;
         end Handle_Interface;
      begin
         for Child of Children (Protocol_Tag.all) loop
            if Child.Kind_Id = Child_Interface then
               Handle_Interface (Child.Interface_Tag.all);
            end if;
         end loop;
      end Generate_Code_For_Enum_Constants;

      procedure Generate_Manually_Edited_Partial_Type_Declarations is
      begin
         Put_Line (File, "   type Compositor is tagged limited private;");
         Put_Line (File, "");
         Put_Line (File, "   function Has_Proxy (Compositor : Protocol.Compositor) return Boolean");
         Put_Line (File, "     with Global => null;");
         Put_Line (File, "");
         Put_Line (File, "   procedure Bind (Compositor  : in out Protocol.Compositor;");
         Put_Line (File, "                   Registry    : Protocol.Registry;");
         Put_Line (File, "                   Id          : Unsigned_32;");
         Put_Line (File, "                   Version     : Unsigned_32) with");
         Put_Line (File, "     Global => null,");
         Put_Line (File, "     Pre    => Has_Registry_Object (Registry);");
         Put_Line (File, "");
         Put_Line (File, "   procedure Create_Surface (Compositor : Protocol.Compositor;");
         Put_Line (File, "                             Surface    : in out Protocol.Surface) with");
         Put_Line (File, "     Global => null;");
         Put_Line (File, "");
         Put_Line (File, "   type Seat is tagged limited private;");
         Put_Line (File, "");
         Put_Line (File, "   function Has_Proxy (Seat : Protocol.Seat) return Boolean");
         Put_Line (File, "     with Global => null;");
         Put_Line (File, "");
         Put_Line (File, "   procedure Bind (Seat     : in out Protocol.Seat;");
         Put_Line (File, "                   Registry : Protocol.Registry;");
         Put_Line (File, "                   Id       : Unsigned_32;");
         Put_Line (File, "                   Version  : Unsigned_32) with");
         Put_Line (File, "     Global => null,");
         Put_Line (File, "     Pre    => Has_Registry_Object (Registry);");
         Put_Line (File, "");
         Put_Line (File, "   procedure Get_Pointer (Seat    : Protocol.Seat;");
         Put_Line (File, "                          Pointer : in out Protocol.Pointer) with");
         Put_Line (File, "     Global => null;");
         Put_Line (File, "");
         Put_Line (File, "   type Pointer is tagged limited private;");
         Put_Line (File, "");
         Put_Line (File, "   type Shell is tagged limited private;");
         Put_Line (File, "");
         Put_Line (File, "   function Has_Proxy (Shell : Protocol.Shell) return Boolean");
         Put_Line (File, "     with Global => null;");
         Put_Line (File, "");
         Put_Line (File, "   procedure Bind (Shell    : in out Protocol.Shell;");
         Put_Line (File, "                   Registry : Protocol.Registry;");
         Put_Line (File, "                   Id       : Unsigned_32;");
         Put_Line (File, "                   Version  : Unsigned_32) with");
         Put_Line (File, "     Global => null,");
         Put_Line (File, "     Pre    => Has_Registry_Object (Registry);");
         Put_Line (File, "");
         Put_Line (File, "   procedure Get_Shell_Surface (Shell         : Protocol.Shell;");
         Put_Line (File, "                                Surface       : Protocol.Surface;");
         Put_Line (File, "                                Shell_Surface : in out Protocol.Shell_Surface) with");
         Put_Line (File, "     Global => null;");
         Put_Line (File, "");
         Put_Line (File, "   type Shm is tagged limited private;");
         Put_Line (File, "");
         Put_Line (File, "   function Has_Proxy (Shm : Protocol.Shm) return Boolean");
         Put_Line (File, "     with Global => null;");
         Put_Line (File, "");
         Put_Line (File, "   procedure Bind (Shm      : in out Protocol.Shm;");
         Put_Line (File, "                   Registry : Protocol.Registry;");
         Put_Line (File, "                   Id       : Unsigned_32;");
         Put_Line (File, "                   Version  : Unsigned_32) with");
         Put_Line (File, "     Global => null,");
         Put_Line (File, "     Pre    => Has_Registry_Object (Registry);");
         Put_Line (File, "");
         Put_Line (File, "   procedure Create_Pool (Shm             : Protocol.Shm;");
         Put_Line (File, "                          File_Descriptor : Integer;");
         Put_Line (File, "                          Size            : Integer;");
         Put_Line (File, "                          Pool            : in out Shm_Pool);");
         Put_Line (File, "");
         Put_Line (File, "   type Shm_Pool is tagged limited private;");
         Put_Line (File, "");
         Put_Line (File, "   function Has_Proxy (Pool : Protocol.Shm_Pool) return Boolean");
         Put_Line (File, "     with Global => null;");
         Put_Line (File, "");
         Put_Line (File, "   procedure Create_Buffer (Pool     : Protocol.Shm_Pool;");
         Put_Line (File, "                            Offset   : Integer;");
         Put_Line (File, "                            Width    : Integer;");
         Put_Line (File, "                            Height   : Integer;");
         Put_Line (File, "                            Stride   : Integer;");
         Put_Line (File, "                            Format   : Unsigned_32;");
         Put_Line (File, "                            Buffer : in out Protocol.Buffer) with");
         Put_Line (File, "     Global => null;");
         Put_Line (File, "");
         Put_Line (File, "   type Surface is tagged limited private;");
         Put_Line (File, "");
         Put_Line (File, "   function Has_Proxy (Surface : Wayland.Client.Surface) return Boolean");
         Put_Line (File, "     with Global => null;");
         Put_Line (File, "");
         Put_Line (File, "   procedure Attach (Surface : Wayland.Client.Surface;");
         Put_Line (File, "                     Buffer  : Wayland.Client.Buffer;");
         Put_Line (File, "                     X       : Integer;");
         Put_Line (File, "                     Y       : Integer) with");
         Put_Line (File, "     Global => null;");
         Put_Line (File, "");
         Put_Line (File, "   procedure Commit (Surface : Wayland.Client.Surface) with");
         Put_Line (File, "     Global => null;");
         Put_Line (File, "");
         Put_Line (File, "   procedure Destroy (Surface : in out Wayland.Client.Surface) with");
         Put_Line (File, "     Global => null,");
         Put_Line (File, "     Pre    => Surface.Exists,");
         Put_Line (File, "     Post   => not Surface.Exists;");
         Put_Line (File, "");
         Put_Line (File, "   type Buffer is tagged limited private;");
         Put_Line (File, "");
         Put_Line (File, "   function Has_Proxy (Buffer : Wayland.Client.Buffer) return Boolean");
         Put_Line (File, "     with Global => null;");
         Put_Line (File, "");
         Put_Line (File, "   type Shell_Surface is tagged limited private;");
         Put_Line (File, "");
         Put_Line (File, "   function Has_Proxy (Surface : Shell_Surface) return Boolean");
         Put_Line (File, "     with Global => null;");
         Put_Line (File, "");
         Put_Line (File, "   procedure Set_Toplevel (Surface : Shell_Surface) with");
         Put_Line (File, "     Global => null;");
         Put_Line (File, "");
         Put_Line (File, "   procedure Pong (Surface : Shell_Surface;");
         Put_Line (File, "                   Serial  : Unsigned_32) with");
         Put_Line (File, "     Global => null;");
         Put_Line (File, "");
         Put_Line (File, "   type Display is tagged limited private with");
         Put_Line (File, "     Default_Initial_Condition => not Display.Is_Connected;");
         Put_Line (File, "");
         Put_Line (File, "   function Is_Connected (Display : Wayland.Client.Display) return Boolean with");
         Put_Line (File, "     Global => null;");
         Put_Line (File, "");
         Put_Line (File, "   procedure Connect (Display : in out Wayland.Client.Display;");
         Put_Line (File, "                      Name    : Px.C_String) with");
         Put_Line (File, "     Global => null,");
         Put_Line (File, "     Pre    => not Display.Is_Connected;");
         Put_Line (File, "   -- Attempts connecting with the Wayland server.");
         Put_Line (File, "");
         Put_Line (File, "   function Dispatch (Display : Wayland.Client.Display) return Int with");
         Put_Line (File, "     Global => null,");
         Put_Line (File, "     Pre    => Display.Is_Connected;");
         Put_Line (File, "");
         Put_Line (File, "   procedure Dispatch (Display : Wayland.Client.Display) with");
         Put_Line (File, "     Global => null,");
         Put_Line (File, "     Pre    => Display.Is_Connected;");
         Put_Line (File, "");
         Put_Line (File, "   function Roundtrip (Display : Wayland.Client.Display) return Int with");
         Put_Line (File, "     Global => null,");
         Put_Line (File, "     Pre    => Display.Is_Connected;");
         Put_Line (File, "");
         Put_Line (File, "   procedure Roundtrip (Display : Wayland.Client.Display) with");
         Put_Line (File, "     Global => null,");
         Put_Line (File, "     Pre    => Display.Is_Connected;");
         Put_Line (File, "");
         Put_Line (File, "   procedure Disconnect (Display : in out Wayland.Client.Display) with");
         Put_Line (File, "     Global => null,");
         Put_Line (File, "     Pre    => Display.Is_Connected,");
         Put_Line (File, "     Post   => not Display.Is_Connected;");
         Put_Line (File, "");
         Put_Line (File, "   procedure Get_Registry (Display  : Wayland.Client.Display;");
         Put_Line (File, "                           Registry : in out Wayland.Client.Registry)");
         Put_Line (File, "     with Global => null,");
         Put_Line (File, "          Pre    => Display.Is_Connected and not Registry.Has_Proxy;");
         Put_Line (File, "");
         Put_Line (File, "   type Registry is tagged limited private;");
         Put_Line (File, "");
         Put_Line (File, "   function Has_Proxy (Registry : Wayland.Client.Registry) return Boolean");
         Put_Line (File, "     with Global => null;");
         Put_Line (File, "");
         Put_Line (File, "   function Has_Started_Subscription (Registry : Wayland.Client.Registry) return Boolean");
         Put_Line (File, "     with Global => null;");
         Put_Line (File, "");
         Put_Line (File, "   procedure Destroy (Registry : in out Wayland.Client.Registry) with");
         Put_Line (File, "     Global => null,");
         Put_Line (File, "     Pre    => Registry.Has_Proxy,");
         Put_Line (File, "     Post   => not Registry.Has_Proxy;");
         Put_Line (File, "");
         Put_Line (File, "   type Callback is tagged limited private;");
         Put_Line (File, "");
         Put_Line (File, "   type Data_Offer is tagged limited private;");
         Put_Line (File, "");
         Put_Line (File, "   type Data_Source is tagged limited private;");
         Put_Line (File, "");
         Put_Line (File, "   type Data_Device is tagged limited private;");
         Put_Line (File, "");
         Put_Line (File, "   type Data_Device_Manager is tagged limited private;");
         Put_Line (File, "");
         Put_Line (File, "   type Keyboard is tagged limited private;");
         Put_Line (File, "");
         Put_Line (File, "   type Touch is tagged limited private;");
         Put_Line (File, "");
         Put_Line (File, "   type Output is tagged limited private;");
         Put_Line (File, "");
         Put_Line (File, "   type Region is tagged limited private;");
         Put_Line (File, "");
         Put_Line (File, "   type Subcompositor is tagged limited private;");
         Put_Line (File, "");
         Put_Line (File, "   type Subsurface is tagged limited private;");
         Put_Line (File, "");
         Put_Line (File, "   generic");
         Put_Line (File, "      type Data_T is private;");
         Put_Line (File, "      Data : Data_T;");
         Put_Line (File, "      with procedure Global_Object_Added (Data     : Data_T;");
         Put_Line (File, "                                          Registry : Protocol.Registry;");
         Put_Line (File, "                                          Id       : Unsigned_32;");
         Put_Line (File, "                                          Name     : String;");
         Put_Line (File, "                                          Version  : Unsigned_32);");
         Put_Line (File, "");
         Put_Line (File, "      with procedure Global_Object_Removed (Data     : Data_T;");
         Put_Line (File, "                                            Registry : Protocol.Registry;");
         Put_Line (File, "                                            Id       : Unsigned_32);");
         Put_Line (File, "   package Registry_Objects_Subscriber is");
         Put_Line (File, "");
         Put_Line (File, "      -- Starts subcription to global objects addded and removed events.");
         Put_Line (File, "      -- To stop subscription, call Registry.Destroy.");
         Put_Line (File, "      procedure Start_Subscription (Registry : in out Protocol.Registry);");
         Put_Line (File, "");
         Put_Line (File, "   end Registry_Objects_Subscriber;");
         Put_Line (File, "");
         Put_Line (File, "   generic");
         Put_Line (File, "      type Data_Type is private;");
         Put_Line (File, "      Data : Data_Type;");
         Put_Line (File, "");
         Put_Line (File, "      with procedure Shell_Surface_Ping");
         Put_Line (File, "        (Data    : Data_Type;");
         Put_Line (File, "         Surface : Shell_Surface;");
         Put_Line (File, "         Serial  : Unsigned_32);");
         Put_Line (File, "");
         Put_Line (File, "      with procedure Shell_Surface_Configure");
         Put_Line (File, "        (Data    : Data_Type;");
         Put_Line (File, "         Surface : Shell_Surface;");
         Put_Line (File, "         Edges   : Unsigned_32;");
         Put_Line (File, "         Width   : Integer;");
         Put_Line (File, "         Height  : Integer);");
         Put_Line (File, "");
         Put_Line (File, "      with procedure Shell_Surface_Popup_Done");
         Put_Line (File, "        (Data    : Data_Type;");
         Put_Line (File, "         Surface : Shell_Surface);");
         Put_Line (File, "");
         Put_Line (File, "   package Shell_Surface_Subscriber is");
         Put_Line (File, "");
         Put_Line (File, "      procedure Start_Subscription (Surface : in out Shell_Surface);");
         Put_Line (File, "");
         Put_Line (File, "   end Shell_Surface_Subscriber;");
         Put_Line (File, "");
         Put_Line (File, "   generic");
         Put_Line (File, "      type Data_Type is private;");
         Put_Line (File, "      Data : Data_Type;");
         Put_Line (File, "");
         Put_Line (File, "      with procedure Seat_Capabilities");
         Put_Line (File, "        (Data         : Data_Type;");
         Put_Line (File, "         Seat         : Protocol.Seat;");
         Put_Line (File, "         Capabilities : Unsigned_32);");
         Put_Line (File, "");
         Put_Line (File, "      with procedure Seat_Name");
         Put_Line (File, "        (Data : Data_Type;");
         Put_Line (File, "         Seat : Protocol.Seat;");
         Put_Line (File, "         Name : String);");
         Put_Line (File, "   package Seat_Capability_Subscriber is");
         Put_Line (File, "");
         Put_Line (File, "      procedure Start_Subscription (S : in out Seat);");
         Put_Line (File, "");
         Put_Line (File, "   end Seat_Capability_Subscriber;");
         Put_Line (File, "");
         Put_Line (File, "   generic");
         Put_Line (File, "      type Data_Type is private;");
         Put_Line (File, "      Data : Data_Type;");
         Put_Line (File, "");
         Put_Line (File, "      with procedure Pointer_Enter");
         Put_Line (File, "        (Data      : Data_Type;");
         Put_Line (File, "         Pointer   : Protocol.Pointer;");
         Put_Line (File, "         Serial    : Unsigned_32;");
         Put_Line (File, "         Surface   : Protocol.Surface;");
         Put_Line (File, "         Surface_X : Fixed;");
         Put_Line (File, "         Surface_Y : Fixed);");
         Put_Line (File, "");
         Put_Line (File, "      with procedure Pointer_Leave");
         Put_Line (File, "        (Data    : Data_Type;");
         Put_Line (File, "         Pointer : Protocol.Pointer;");
         Put_Line (File, "         Serial  : Unsigned_32;");
         Put_Line (File, "         Surface : Protocol.Surface);");
         Put_Line (File, "");
         Put_Line (File, "      with procedure Pointer_Motion");
         Put_Line (File, "        (Data      : Data_Type;");
         Put_Line (File, "         Pointer   : Protocol.Pointer;");
         Put_Line (File, "         Time      : Unsigned_32;");
         Put_Line (File, "         Surface_X : Fixed;");
         Put_Line (File, "         Surface_Y : Fixed);");
         Put_Line (File, "");
         Put_Line (File, "      with procedure Pointer_Button");
         Put_Line (File, "        (Data    : Data_Type;");
         Put_Line (File, "         Pointer : Protocol.Pointer;");
         Put_Line (File, "         Serial  : Unsigned_32;");
         Put_Line (File, "         Time    : Unsigned_32;");
         Put_Line (File, "         Button  : Unsigned_32;");
         Put_Line (File, "         State   : Unsigned_32);");
         Put_Line (File, "");
         Put_Line (File, "      with procedure Pointer_Axis");
         Put_Line (File, "        (Data    : Data_Type;");
         Put_Line (File, "         Pointer : Protocol.Pointer;");
         Put_Line (File, "         Time    : Unsigned_32;");
         Put_Line (File, "         Axis    : Unsigned_32;");
         Put_Line (File, "         Value   : Fixed);");
         Put_Line (File, "");
         Put_Line (File, "      with procedure Pointer_Frame (Data    : Data_Type;");
         Put_Line (File, "                                    Pointer : Protocol.Pointer);");
         Put_Line (File, "");
         Put_Line (File, "      with procedure Pointer_Axis_Source");
         Put_Line (File, "        (Data        : Data_Type;");
         Put_Line (File, "         Pointer     : Protocol.Pointer;");
         Put_Line (File, "         Axis_Source : Unsigned_32);");
         Put_Line (File, "");
         Put_Line (File, "      with procedure Pointer_Axis_Stop");
         Put_Line (File, "        (Data    : Data_Type;");
         Put_Line (File, "         Pointer : Protocol.Pointer;");
         Put_Line (File, "         Time    : Unsigned_32;");
         Put_Line (File, "         Axis    : Unsigned_32);");
         Put_Line (File, "");
         Put_Line (File, "      with procedure Pointer_Axis_Discrete");
         Put_Line (File, "        (Data     : Data_Type;");
         Put_Line (File, "         Pointer  : Protocol.Pointer;");
         Put_Line (File, "         Axis     : Unsigned_32;");
         Put_Line (File, "         Discrete : Integer);");
         Put_Line (File, "");
         Put_Line (File, "   package Pointer_Subscriber is");
         Put_Line (File, "");
         Put_Line (File, "      procedure Start_Subscription (P : in out Pointer);");
         Put_Line (File, "");
         Put_Line (File, "   end Pointer_Subscriber;");
         Put_Line (File, "");
      end Generate_Manually_Edited_Partial_Type_Declarations;

      procedure Generate_Code_For_The_Private_Part is
      begin
         New_Line (File);
         Put_Line (File, "private");
         New_Line (File);
         Put_Line (File, "   subtype char_array is Interfaces.C.char_array;");
         New_Line (File);
         Put_Line (File, "   subtype chars_ptr is Interfaces.C.Strings.chars_ptr;");
         New_Line (File);
         Put_Line (File, "   function Value (C : chars_ptr) return String renames Interfaces.C.Strings.Value;");
         New_Line (File);
      end Generate_Code_For_The_Private_Part;

      procedure Create_Wl_Thin_Spec_File is

--           procedure Generate_Code_For_Numeric_Constants is
--
--              procedure Handle_Interface (Interface_Tag : Wayland_XML.Interface_Tag) is
--
--                 procedure Generate_Code_For_Opcodes is
--
--                    I : Integer := 0;
--
--                    procedure Generate_Code (Request_Tag : Wayland_XML.Request_Tag) is
--                       Name : constant String := Xml_Parser_Utils.Make_Upper_Case (Interface_Tag.Name & "_" & Request_Tag.Name);
--                    begin
--                       Ada.Text_IO.Put (File, Name);
--                       Ada.Text_IO.Put (File, " : constant := " & Aida.To_String (I));
--                       Put_Line (File, ";");
--                       Put_Line (File, "");
--
--                       I := I + 1;
--                    end Generate_Code;
--
--                 begin
--                    for Child of Interface_Tag.Children loop
--                       if Child.Kind_Id = Child_Request then
--                          Generate_Code (Child.Request_Tag.all);
--                       end if;
--                    end loop;
--                 end Generate_Code_For_Opcodes;
--
--                 procedure Generate_Code_For_Event_Since_Version is
--
--                    procedure Generate_Code (Event_Tag : Wayland_XML.Event_Tag) is
--                       Name : constant String := Xml_Parser_Utils.Make_Upper_Case (Interface_Tag.Name & "_" & Event_Tag.Name & "_SINCE_VERSION");
--                    begin
--                       if Event_Tag.Exists_Since_Attribute then
--                          Ada.Text_IO.Put (File, Name);
--                          Ada.Text_IO.Put (File, " : constant := " & Aida.To_String (Event_Tag.Since_Attribute_As_Pos32));
--                          Put_Line (File, ";");
--                          Put_Line (File, "");
--                       else
--                          Ada.Text_IO.Put (File, Name);
--                          Put_Line (File, " : constant := 1;");
--                          Put_Line (File, "");
--                       end if;
--                    end Generate_Code;
--
--                 begin
--                    for Child of Interface_Tag.Children loop
--                       if Child.Kind_Id = Child_Event then
--                          Generate_Code (Child.Event_Tag.all);
--                       end if;
--                    end loop;
--                 end Generate_Code_For_Event_Since_Version;
--
--                 procedure Generate_Code_For_Opcodes_Since_Version is
--
--                    procedure Generate_Code (Request_Tag : Wayland_XML.Request_Tag) is
--                       Name : constant String := Xml_Parser_Utils.Make_Upper_Case (Interface_Tag.Name & "_" & Request_Tag.Name & "_SINCE_VERSION");
--                    begin
--                       if Request_Tag.Exists_Since then
--                          Ada.Text_IO.Put (File, Name);
--                          Ada.Text_IO.Put (File, " : constant := " & Aida.To_String (Request_Tag.Since_As_Pos32));
--                          Put_Line (File, ";");
--                          Put_Line (File, "");
--                       else
--                          Ada.Text_IO.Put (File, Name);
--                          Put_Line (File, " : constant := 1;");
--                          Put_Line (File, "");
--                       end if;
--                    end Generate_Code;
--
--                 begin
--                    for Child of Interface_Tag.Children loop
--                       if Child.Kind_Id = Child_Request then
--                          Generate_Code (Child.Request_Tag.all);
--                       end if;
--                    end loop;
--                 end Generate_Code_For_Opcodes_Since_Version;
--
--              begin
--                 Generate_Code_For_Opcodes;
--                 Generate_Code_For_Event_Since_Version;
--                 Generate_Code_For_Opcodes_Since_Version;
--              end Handle_Interface;
--
--           begin
--              for Child of Protocol_Tag.Children loop
--                 if Child.Kind_Id = Child_Interface then
--                    Handle_Interface (Child.Interface_Tag.all);
--                 end if;
--              end loop;
--
--              Generate_Code_For_Interface_Constants;
--           end Generate_Code_For_Numeric_Constants;

         procedure Generate_Code_For_Interface_Constants is

            procedure Handle_Interface (Interface_Tag : Wayland_XML.Interface_Tag) is
               Name : constant String
                 := Xml_Parser_Utils.Adaify_Name
                   (Wayland_XML.Name (Interface_Tag) & "_Interface");
            begin
               Put_Line (File, "   " & Name & " : aliased Interface_T with");
               Put_Line (File, "      Import        => True,");
               Put_Line (File, "      Convention    => C,");
               Put_Line
                 (File,
                  "      External_Name => """ & Wayland_XML.Name (Interface_Tag) & "_interface"";");
               Put_Line (File, "");
            end Handle_Interface;

         begin
            for Child of Children (Protocol_Tag.all) loop
               if Child.Kind_Id = Child_Interface then
                  Handle_Interface (Child.Interface_Tag.all);
               end if;
            end loop;
         end Generate_Code_For_Interface_Constants;

         procedure Generate_Code_For_Interface_Ptrs is

            procedure Handle_Interface (Interface_Tag : Wayland_XML.Interface_Tag) is

               procedure Generate_Code_For_Interface_Ptr is
                  Name : constant String := Xml_Parser_Utils.Interface_Ptr_Name (Interface_Tag);
               begin
                  Put_Line (File, "   type " & Name & " is new Proxy_Ptr;");
               end Generate_Code_For_Interface_Ptr;

            begin
               Generate_Code_For_Interface_Ptr;
            end Handle_Interface;

         begin
            for Child of Children (Protocol_Tag.all) loop
               if Child.Kind_Id = Child_Interface then
                  Put_Line (File, "");
                  Handle_Interface (Child.Interface_Tag.all);
               end if;
            end loop;
         end Generate_Code_For_Interface_Ptrs;

         procedure Generate_Code_For_Each_Interface is

            procedure Handle_Interface
              (Interface_Tag : aliased Wayland_XML.Interface_Tag)
            is

--                 procedure Generate_Code_For_Enums is
--
--                    procedure Generate_Code (Enum_Tag : Wayland_XML.Enum_Tag) is
--                       Enum_Type_Name : constant String := Xml_Parser_Utils.Adaify_Name (Interface_Tag.Name & "_" & Enum_Tag.Name & "_T");
--
--                       procedure Generate_Code_For_Enum_Value (Entry_Tag : Wayland_XML.Entry_Tag) is
--                          Name : constant String := Xml_Parser_Utils.Adaify_Name (Interface_Tag.Name & "_" & Enum_Tag.Name & "_" & Entry_Tag.Name);
--                       begin
--                          Put_Line (File, "-- " & Entry_Tag.Summary);
--                          Put_Line (File, Name & " : constant " & Enum_Type_Name & " := " & Entry_Tag.Value_As_String & ";");
--                          Put_Line (File, "");
--                       end Generate_Code_For_Enum_Value;
--
--                    begin
--                       Put_Line (File, "type " & Enum_Type_Name & " is new Unsigned_32;");
--
--                       for Child of Enum_Tag.Children loop
--                          if Child.Kind_Id = Child_Entry then
--                             Generate_Code_For_Enum_Value (Child.Entry_Tag.all);
--                          end if;
--                       end loop;
--                       Put_Line (File, "");
--                    end Generate_Code;
--
--                 begin
--                    for Child of Interface_Tag.Children loop
--                       if Child.Kind_Id = Child_Enum then
--                          Generate_Code (Child.Enum_Tag.all);
--                       end if;
--                    end loop;
--                 end Generate_Code_For_Enums;

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
                           Put (File, "      " & Interface_Name_Aligned & " : " & Xml_Parser_Utils.Interface_Ptr_Name (Interface_Tag));
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
                                 Child.Arg_Tag.all,
                                 Max_Name_Length,
                                 Children (Event_Tag).Last_Element = Child);
                           end if;
                        end loop;
                     end;

                     New_Line (File);
                     Put_Line (File, "   with Convention => C;");
                  end Generate_Code_For_Subprogram;
               begin
                  for Child of Children (Interface_Tag) loop
                     if Child.Kind_Id = Child_Event then
                        Put_Line (File, "");
                        Generate_Code_For_Subprogram (Child.Event_Tag.all);
                     end if;
                  end loop;
               end Generate_Code_For_Subprogram_Ptrs;

               procedure Generate_Code_For_Listener_Type_Definition is
                  function Get_Name (Event_Tag   : Wayland_XML.Event_Tag) return String is
                    (Xml_Parser_Utils.Adaify_Name (Name (Event_Tag)));

                  procedure Generate_Code_For_Record_Component
                    (Event_Tag   : Wayland_XML.Event_Tag;
                     Name_Length : Natural)
                  is
                     Component_Name      : constant String := Get_Name (Event_Tag);

                     Component_Type_Name : constant String
                       := Xml_Parser_Utils.Adaify_Name
                         (Name (Interface_Tag) & "_" &
                              Name (Event_Tag) & "_Subprogram_Ptr");

                     Component_Name_Aligned : constant String := SF.Head (Component_Name, Name_Length, ' ');
                  begin
                     Put_Line (File, "      " & Component_Name_Aligned & " : " & Component_Type_Name & ";");
                  end Generate_Code_For_Record_Component;

                  Name     : constant String
                    := Xml_Parser_Utils.Adaify_Name
                      (Wayland_XML.Name (Interface_Tag) & "_Listener_T");
                  Ptr_Name : constant String
                    := Xml_Parser_Utils.Adaify_Name
                      (Wayland_XML.Name (Interface_Tag) & "_Listener_Ptr");

                  Max_Name_Length : Natural := 0;
               begin
                  Put_Line (File, "   type " & Name & " is record");

                  for Child of Children (Interface_Tag) loop
                     if Child.Kind_Id = Child_Event then
                        declare
                           Arg_Name : constant String := Get_Name (Child.Event_Tag.all);
                        begin
                           Max_Name_Length := Natural'Max (Max_Name_Length, Arg_Name'Length);
                        end;
                     end if;
                  end loop;

                  for Child of Children (Interface_Tag) loop
                     if Child.Kind_Id = Child_Event then
                        Generate_Code_For_Record_Component (Child.Event_Tag.all, Max_Name_Length);
                     end if;
                  end loop;
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
                              Comment : constant String := Ada.Strings.Fixed.Trim (Text (Interval.First .. Interval.Last), Ada.Strings.Both);
                           begin
                              if Comment'Length > 0 then
                                 Put_Line (File, "   --  " & Comment);
                              else
                                 Put_Line (File, "   --");
                              end if;
                           end;
                        end loop;
                     end Generate_Comment;

                     function Get_Name (Arg_Tag   : Wayland_XML.Arg_Tag) return String is
                       (Xml_Parser_Utils.Adaify_Variable_Name (Name (Arg_Tag)));

                     Subprogram_Name : constant String
                       := Xml_Parser_Utils.Adaify_Name
                         (Name (Interface_Tag) & "_" & Name (Request_Tag));
                     Name            : constant String
                       := Xml_Parser_Utils.Adaify_Name
                         (Wayland_XML.Name (Interface_Tag));
                     Ptr_Name        : constant String
                       := Xml_Parser_Utils.Adaify_Name
                         (Wayland_XML.Name (Interface_Tag) & "_Ptr");

                     procedure Generate_Pretty_Function_Code (Subprogram_Kind : String; Max_Name_Length : in out Natural) is
                        V : Wayland_XML.Request_Child_Vectors.Vector;

                        function Align (Value : String) return String is (SF.Head (Value, Max_Name_Length, ' '));
                     begin
                        for Child of Children (Request_Tag) loop
                           if Child.Kind_Id = Child_Arg then
                              if Type_Attribute (Child.Arg_Tag.all) /= Type_New_Id then
                                 V.Append (Child);

                                 declare
                                    Arg_Name : constant String := Get_Name (Child.Arg_Tag.all);
                                 begin
                                    Max_Name_Length := Natural'Max (Max_Name_Length, Arg_Name'Length);
                                 end;
                              end if;
                           end if;
                        end loop;

                        Put_Line (File, "   " & Subprogram_Kind & " " & Subprogram_Name);
                        Put_Line (File, "     (" & Align (Name) & " : " & Ptr_Name & ";");

                        for Child of V loop
                           if Child.Kind_Id = Child_Arg then
                              Generate_Code_For_Arg
                                (File, Child.Arg_Tag.all,
                                 Max_Name_Length,
                                 Child = Children (Request_Tag).Last_Element);
                           end if;
                        end loop;
                     end Generate_Pretty_Function_Code;
                  begin
                     if Xml_Parser_Utils.Is_New_Id_Argument_Present (Request_Tag) then
                        Put_Line (File, "");
                        if Exists_Description (Request_Tag) then
                           Generate_Comment
                             (Xml_Parser_Utils.Remove_Tabs
                                (Description (Request_Tag)));
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
                                 Put_Line (File, "   function " & Subprogram_Name & " (" & Name & " : " & Ptr_Name & ") return " & Return_Type & ";");
                              end if;
                           end;
                        else
                           if Xml_Parser_Utils.Number_Of_Args (Request_Tag) > 1 then
                              declare
                                 Max_Name_Length : Natural := Natural'Max (Name'Length, 11);

                                 --  TODO Use parameter list in Generate_Pretty_Function_Code
                                 function Align (Value : String) return String is (SF.Head (Value, Max_Name_Length, ' '));
                              begin
                                 Generate_Pretty_Function_Code ("function", Max_Name_Length);
                                 Put_Line (File, "      " & Align ("Interface_V") & " : Interface_Ptr;");
                                 Put_Line (File, "      " & Align ("New_Id") & " : Unsigned_32) return Proxy_Ptr;");
                              end;
                           else
                              Put_Line (File, "   function " & Subprogram_Name & " (" & Name & " : " & Ptr_Name & ") return Proxy_Ptr;");
                           end if;
                        end if;
                     elsif Xml_Parser_Utils.Is_Request_Destructor (Request_Tag) then
                        null; -- Already has generated declaration earlier in Generate_Code_For_Destroy_Subprogram
                     else
                        Put_Line (File, "");
                        if Exists_Description (Request_Tag) then
                           Generate_Comment
                             (Xml_Parser_Utils.Remove_Tabs
                                (Description (Request_Tag)));
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
                  for Child of Children (Interface_Tag) loop
                     if Child.Kind_Id = Child_Request then
                        Generate_Code_For_Subprogram_Declaration (Child.Request_Tag.all);
                     end if;
                  end loop;
               end Generate_Code_For_Requests;

            begin
--               Generate_Code_For_Enums;
               Generate_Code_For_Subprogram_Ptrs;

               if Xml_Parser_Utils.Exists_Any_Event_Tag (Interface_Tag) then
                  Put_Line (File, "");
                  Generate_Code_For_Listener_Type_Definition;
                  Put_Line (File, "");
                  Generate_Code_For_Add_Listener_Subprogram_Declaration;
               end if;

               Put_Line (File, "");
               Generate_Code_For_Set_User_Data_Subprogram_Declaration;
               Put_Line (File, "");
               Generate_Code_For_Get_User_Data_Subprogram_Declaration;
               Put_Line (File, "");
               Generate_Code_For_Get_Version_Subprogram_Declaration;
               Put_Line (File, "");
               Generate_Code_For_Destroy_Subprogram_Declaration;
               Generate_Code_For_Requests;
            end Handle_Interface;

         begin
            for Child of Children (Protocol_Tag.all) loop
               if Child.Kind_Id = Child_Interface then
                  Handle_Interface (Child.Interface_Tag.all);
               end if;
            end loop;
         end Generate_Code_For_Each_Interface;

      begin
         Generate_Code_For_Interface_Constants;
         Generate_Code_For_Interface_Ptrs;
         Generate_Code_For_Each_Interface;
      end Create_Wl_Thin_Spec_File;

      procedure Generate_Use_Type_Declarions is

         procedure Handle_Interface (Interface_Tag : Wayland_XML.Interface_Tag) is
            Name : constant String
              := Xml_Parser_Utils.Adaify_Name
                (Wayland_XML.Name (Interface_Tag));
         begin
            Put_Line (File, "   use type Thin." & Name & "_Ptr;");
         end Handle_Interface;

      begin
         for Child of Children (Protocol_Tag.all) loop
            if Child.Kind_Id = Child_Interface then
               Handle_Interface (Child.Interface_Tag.all);
            end if;
         end loop;

         New_Line (File);
      end Generate_Use_Type_Declarions;

      procedure Generate_Manually_Edited_Code_For_Type_Definitions is
      begin
         Put_Line (File, "   type Display is tagged limited record");
         Put_Line (File, "      My_Display : Thin.Display_Ptr;");
         Put_Line (File, "   end record;");
         New_Line (File);
         Put_Line (File, "   function Is_Connected (Display : Protocol.Display) return Boolean is (Display.My_Display /= null);");
         New_Line (File);
         Put_Line (File, "   type Registry is tagged limited record");
         Put_Line (File, "      My_Registry                 : Thin.Registry_Ptr;");
         Put_Line (File, "      My_Has_Started_Subscription : Boolean := False;");
         Put_Line (File, "   end record;");
         New_Line (File);
         Put_Line (File, "   function Has_Proxy (Registry : Protocol.Registry) return Boolean is");
         Put_Line (File, "     (Registry.My_Registry /= null);");
         New_Line (File);
         Put_Line (File, "   function Has_Started_Subscription (Registry : Wayland.Client.Registry) return Boolean is (Registry.My_Has_Started_Subscription);");
         New_Line (File);
         Put_Line (File, "   type Compositor is tagged limited record");
         Put_Line (File, "      My_Compositor : Thin.Compositor_Ptr;");
         Put_Line (File, "   end record;");
         New_Line (File);
         Put_Line (File, "   function Has_Proxy (Compositor : Protocol.Compositor) return Boolean is");
         Put_Line (File, "     (Compositor.My_Compositor /= null);");
         New_Line (File);
         Put_Line (File, "   type Pointer is tagged limited record");
         Put_Line (File, "      My_Pointer : Thin.Pointer_Ptr;");
         Put_Line (File, "   end record;");
         New_Line (File);
         Put_Line (File, "   function Has_Proxy (Pointer : Protocol.Pointer) return Boolean is");
         Put_Line (File, "     (Pointer.My_Pointer /= null);");
         New_Line (File);
         Put_Line (File, "   type Seat is tagged limited record");
         Put_Line (File, "      My_Seat : Thin.Seat_Ptr;");
         Put_Line (File, "      My_Has_Started_Subscription : Boolean := False;");
         Put_Line (File, "   end record;");
         New_Line (File);
         Put_Line (File, "   function Has_Proxy (Seat : Protocol.Seat) return Boolean is");
         Put_Line (File, "     (Seat.My_Seat /= null);");
         New_Line (File);
         Put_Line (File, "   type Shell is tagged limited record");
         Put_Line (File, "      My_Shell : Thin.Shell_Ptr;");
         Put_Line (File, "   end record;");
         New_Line (File);
         Put_Line (File, "   function Has_Proxy (Shell : Protocol.Shell) return Boolean is");
         Put_Line (File, "     (Shell.My_Shell /= null);");
         New_Line (File);
         Put_Line (File, "   type Shm is tagged limited record");
         Put_Line (File, "      My_Shm : Thin.Shm_Ptr;");
         Put_Line (File, "   end record;");
         New_Line (File);
         Put_Line (File, "   function Has_Proxy (Shm : Protocol.Shm) return Boolean is");
         Put_Line (File, "     (Shm.My_Shm /= null);");
         New_Line (File);
         Put_Line (File, "   type Shm_Pool is tagged limited record");
         Put_Line (File, "      My_Shm_Pool : Thin.Shm_Pool_Ptr;");
         Put_Line (File, "   end record;");
         New_Line (File);
         Put_Line (File, "   function Has_Proxy (Shm_Pool : Protocol.Shm_Pool) return Boolean is");
         Put_Line (File, "     (Shm_Pool.My_Shm_Pool /= null);");
         New_Line (File);
         Put_Line (File, "   type Buffer is tagged limited record");
         Put_Line (File, "      My_Buffer : Thin.Buffer_Ptr;");
         Put_Line (File, "   end record;");
         New_Line (File);
         Put_Line (File, "   function Has_Proxy (Buffer : Protocol.Buffer) return Boolean is");
         Put_Line (File, "     (Buffer.My_Buffer /= null);");
         New_Line (File);
         Put_Line (File, "   type Surface is tagged limited record");
         Put_Line (File, "      My_Surface : Thin.Surface_Ptr;");
         Put_Line (File, "   end record;");
         New_Line (File);
         Put_Line (File, "   function Has_Proxy (Surface : Protocol.Surface) return Boolean is");
         Put_Line (File, "     (Surface.My_Surface /= null);");
         New_Line (File);
         Put_Line (File, "   type Shell_Surface is tagged limited record");
         Put_Line (File, "      My_Shell_Surface : Thin.Shell_Surface_Ptr;");
         Put_Line (File, "   end record;");
         New_Line (File);
         Put_Line (File, "   function Has_Proxy (Shell_Surface : Protocol.Shell_Surface) return Boolean is");
         Put_Line (File, "     (Shell_Surface.My_Shell_Surface /= null);");
         New_Line (File);
         Put_Line (File, "   type Callback is tagged limited record");
         Put_Line (File, "      My_Callback : Thin.Callback_Ptr;");
         Put_Line (File, "   end record;");
         New_Line (File);
         Put_Line (File, "   type Data_Offer is tagged limited record");
         Put_Line (File, "      My_Data_Offer : Thin.Data_Offer_Ptr;");
         Put_Line (File, "   end record;");
         New_Line (File);
         Put_Line (File, "   type Data_Source is tagged limited record");
         Put_Line (File, "      My_Data_Source : Thin.Data_Source_Ptr;");
         Put_Line (File, "   end record;");
         New_Line (File);
         Put_Line (File, "   function Has_Proxy (Data_Source : Protocol.Data_Source) return Boolean is");
         Put_Line (File, "     (Data_Source.My_Data_Source /= null);");
         New_Line (File);
         Put_Line (File, "   type Data_Device is tagged limited record");
         Put_Line (File, "      My_Data_Device : Thin.Data_Device_Ptr;");
         Put_Line (File, "   end record;");
         New_Line (File);
         Put_Line (File, "   function Has_Proxy (Data_Device : Protocol.Data_Device) return Boolean is");
         Put_Line (File, "     (Data_Device.My_Data_Device /= null);");
         New_Line (File);
         Put_Line (File, "   type Data_Device_Manager is tagged limited record");
         Put_Line (File, "      My_Data_Device_Manager : Thin.Data_Device_Manager_Ptr;");
         Put_Line (File, "   end record;");
         New_Line (File);
         Put_Line (File, "   function Has_Proxy (Data_Device_Manager : Protocol.Data_Device_Manager) return Boolean is");
         Put_Line (File, "     (Data_Device_Manager.My_Data_Device_Manager /= null);");
         New_Line (File);
         Put_Line (File, "   type Keyboard is tagged limited record");
         Put_Line (File, "      My_Keyboard : Thin.Keyboard_Ptr;");
         Put_Line (File, "   end record;");
         New_Line (File);
         Put_Line (File, "   function Has_Proxy (Keyboard : Protocol.Keyboard) return Boolean is");
         Put_Line (File, "     (Keyboard.My_Keyboard /= null);");
         New_Line (File);
         Put_Line (File, "   type Touch is tagged limited record");
         Put_Line (File, "      My_Touch : Thin.Touch_Ptr;");
         Put_Line (File, "   end record;");
         New_Line (File);
         Put_Line (File, "   function Has_Proxy (Touch : Protocol.Touch) return Boolean is");
         Put_Line (File, "     (Touch.My_Touch /= null);");
         New_Line (File);
         Put_Line (File, "   type Output is tagged limited record");
         Put_Line (File, "      My_Output : Thin.Output_Ptr;");
         Put_Line (File, "   end record;");
         New_Line (File);
         Put_Line (File, "   function Has_Proxy (Output : Protocol.Output) return Boolean is");
         Put_Line (File, "     (Output.My_Output /= null);");
         New_Line (File);
         Put_Line (File, "   type Region is tagged limited record");
         Put_Line (File, "      My_Region : Thin.Region_Ptr;");
         Put_Line (File, "   end record;");
         New_Line (File);
         Put_Line (File, "   function Has_Proxy (Region : Protocol.Region) return Boolean is");
         Put_Line (File, "     (Region.My_Region /= null);");
         New_Line (File);
         Put_Line (File, "   type Subcompositor is tagged limited record");
         Put_Line (File, "      My_Subcompositor : Thin.Subcompositor_Ptr;");
         Put_Line (File, "   end record;");
         New_Line (File);
         Put_Line (File, "   function Has_Proxy (Subcompositor : Protocol.Subcompositor) return Boolean is");
         Put_Line (File, "     (Subcompositor.My_Subcompositor /= null);");
         New_Line (File);
         Put_Line (File, "   type Subsurface is tagged limited record");
         Put_Line (File, "      My_Subsurface : Thin.Subsurface_Ptr;");
         Put_Line (File, "   end record;");
         New_Line (File);
         Put_Line (File, "   function Has_Proxy (Subsurface : Protocol.Subsurface) return Boolean is");
         Put_Line (File, "     (Subsurface.My_Subsurface /= null);");
         New_Line (File);
      end Generate_Manually_Edited_Code_For_Type_Definitions;

      procedure Generate_Private_Code_For_The_Interface_Constants is

         procedure Handle_Interface (Interface_Tag : Wayland_XML.Interface_Tag) is
            Name : constant String
              := Xml_Parser_Utils.Adaify_Name
                (Wayland_XML.Name (Interface_Tag) & "_Interface");
         begin
            Put_Line (File, "   " & Name & " : constant Interface_Type :=");
            Put (File, "     (My_Interface => Thin.");
            Put_Line (File, Name & "'Access);");
            New_Line (File);
         end Handle_Interface;

      begin
         Put_Line (File, "   type Interface_Type is tagged limited record");
         Put_Line (File, "      My_Interface : not null Thin.Interface_Ptr;");
         Put_Line (File, "   end record;");
         New_Line (File);
         Put_Line (File, "   function Name (I : Interface_Type) return String is");
         Put_Line (File, "     (Value (I.My_Interface.Name));");
         New_Line (File);

         for Child of Children (Protocol_Tag.all) loop
            if Child.Kind_Id = Child_Interface then
               Handle_Interface (Child.Interface_Tag.all);
            end if;
         end loop;
      end Generate_Private_Code_For_The_Interface_Constants;

   begin
      Create_File;
   end Create_Wayland_Spec_File;

   procedure Create_Wayland_Body_File (File_Name : String) is
      File : Ada.Text_IO.File_Type;

      procedure Create_Wl_Thin_Body_File;

      procedure Generate_Manually_Edited_Code;

      procedure Create_File is
         Protocol_Name : constant String := Get_Protocol_Name (Name (Protocol_Tag.all));
         Package_Name  : constant String := Xml_Parser_Utils.Adaify_Name (Protocol_Name);
      begin
         if Protocol_Name = "client" then
            Ada.Text_IO.Create
              (File, Ada.Text_IO.Out_File, "wayland-" & Protocol_Name & "-protocol.adb");

            Put_Line (File, "package body Wayland." & Package_Name & ".Protocol is");
            New_Line (File);

            Generate_Manually_Edited_Code;

            Put_Line (File, "end Wayland." & Package_Name & ".Protocol;");

            Ada.Text_IO.Close (File);
         end if;

         -----------------------------------------------------------------------

         Ada.Text_IO.Create
           (File, Ada.Text_IO.Out_File, "wayland-" & Protocol_Name & "-thin.adb");

         Put_Line (File, "with Wayland." & Package_Name & ".Constants;");
         Put_Line (File, "");
         Put_Line (File, "use Wayland." & Package_Name & ".Constants;");
         Put_Line (File, "");
         Put_Line (File, "--  Mostly auto generated from " & File_Name);
         Put_Line (File, "package body Wayland." & Package_Name & ".Thin is");
         Put_Line (File, "");
         Put_Line (File, "   use type Proxy_Ptr;");
         Put_Line (File, "");
         Put_Line (File, "   function Display_Connect (Name : C_String) return Display_Ptr is");
         Put_Line (File, "   begin");
         Put_Line (File, "      return Wayland.API.Display_Connect (Name);");
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

         Create_Wl_Thin_Body_File;

         Put_Line (File, "");
         Put_Line (File, "end Wayland." & Package_Name & ".Thin;");

         Ada.Text_IO.Close (File);
      end Create_File;

      pragma Unmodified (File);

      procedure Create_Wl_Thin_Body_File is

         procedure Generate_Code_For_Protocol_Tag_Children is
            procedure Handle_Interface
              (Interface_Tag : aliased Wayland_XML.Interface_Tag)
            is
               procedure Generate_Code_For_Add_Listener_Subprogram_Implementations is
                  Name : constant String :=
                    Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Interface_Tag));
                  Function_Name     : constant String := Name & "_Add_Listener";
                  Ptr_Listener_Name : constant String :=
                    Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Interface_Tag) & "_Listener_Ptr");
               begin
                  Generate_Pretty_Code_For_Subprogram
                    (File, Implementation, Interface_Tag, "Add_Listener", "Interfaces.C.int",
                      ((+"Listener", +Ptr_Listener_Name),
                       (+"Data", +"Void_Ptr")));
                  Put_Line (File, "   begin");
                  Put_Line (File, "      return Wayland.API.Proxy_Add_Listener (" & Name & ".all, Listener.all'Address, Data);");
                  Put_Line (File, "   end " & Function_Name & ";");
               end Generate_Code_For_Add_Listener_Subprogram_Implementations;

               procedure Generate_Code_For_Set_User_Data_Subprogram_Implementations is
                  Name            : constant String :=
                    Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Interface_Tag));
                  Subprogram_Name : constant String := Name & "_Set_User_Data";
               begin
                  Generate_Pretty_Code_For_Subprogram
                    (File, Implementation, Interface_Tag, "Set_User_Data", "",
                      (1 => (+"Data", +"Void_Ptr")));
                  Put_Line (File, "   begin");
                  Put_Line (File, "      Wayland.API.Proxy_Set_User_Data (" & Name & ".all, Data);");
                  Put_Line (File, "   end " & Subprogram_Name & ";");
               end Generate_Code_For_Set_User_Data_Subprogram_Implementations;

               procedure Generate_Code_For_Get_User_Data_Subprogram_Implementations is
                  Name            : constant String :=
                    Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Interface_Tag));
                  Subprogram_Name : constant String := Name & "_Get_User_Data";
               begin
                  Generate_Pretty_Code_For_Subprogram
                    (File, Implementation, Interface_Tag, "Get_User_Data", "Void_Ptr");
                  Put_Line (File, "   begin");
                  Put_Line (File, "      return Wayland.API.Proxy_Get_User_Data (" & Name & ".all);");
                  Put_Line (File, "   end " & Subprogram_Name & ";");
               end Generate_Code_For_Get_User_Data_Subprogram_Implementations;

               procedure Generate_Code_For_Get_Version_Subprogram_Implementations is
                  Name            : constant String :=
                    Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Interface_Tag));
                  Subprogram_Name : constant String := Name & "_Get_Version";
               begin
                  Generate_Pretty_Code_For_Subprogram
                    (File, Implementation, Interface_Tag, "Get_Version", "Unsigned_32");
                  Put_Line (File, "   begin");
                  Put_Line (File, "      return Wayland.API.Proxy_Get_Version (" & Name & ".all);");
                  Put_Line (File, "   end " & Subprogram_Name & ";");
               end Generate_Code_For_Get_Version_Subprogram_Implementations;

               procedure Generate_Code_For_Destroy_Subprogram_Implementations is
                  Name            : constant String :=
                    Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Interface_Tag));
                  Subprogram_Name : constant String := Name & "_Destroy";
               begin
                  Generate_Pretty_Code_For_Subprogram
                    (File, Implementation, Interface_Tag, "Destroy", "");
                  Put_Line (File, "   begin");

                  if Xml_Parser_Utils.Exists_Destructor (Interface_Tag) then
                     Put_Line (File, "      Wayland.API.Proxy_Marshal (" & Name & ".all, Constants." &
                       Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Interface_Tag) & "_Destroy") & ");");
                     Put_Line (File, "");
                  end if;

                  Put_Line (File, "      Wayland.API.Proxy_Destroy (" & Name & ".all);");
                  Put_Line (File, "   end " & Subprogram_Name & ";");
               end Generate_Code_For_Destroy_Subprogram_Implementations;

               procedure Generate_Code_For_Requests is

                  function Get_Name (Arg_Tag   : Wayland_XML.Arg_Tag) return String is
                    (Xml_Parser_Utils.Adaify_Variable_Name (Name (Arg_Tag)));

                  procedure Generate_Code_For_Subprogram_Implementation
                    (Request_Tag : aliased Wayland_XML.Request_Tag)
                  is
                     Opcode          : constant String
                       := "Constants." & Xml_Parser_Utils.Adaify_Name
                         (Wayland_XML.Name
                            (Interface_Tag) & "_" & Name (Request_Tag));
                     Subprogram_Name : constant String
                       := Xml_Parser_Utils.Adaify_Name
                         (Wayland_XML.Name
                            (Interface_Tag) & "_" & Name (Request_Tag));
                     Name            : constant String
                       := Xml_Parser_Utils.Adaify_Name
                         (Wayland_XML.Name (Interface_Tag));
                     Ptr_Name        : constant String
                       := Xml_Parser_Utils.Adaify_Name
                         (Wayland_XML.Name (Interface_Tag) & "_Ptr");
                  begin
                     if Xml_Parser_Utils.Is_New_Id_Argument_Present (Request_Tag) then
                        Put_Line (File, "");
                        if Xml_Parser_Utils.Is_Interface_Specified (Request_Tag) then
                           declare
                              Return_Type : constant String := Xml_Parser_Utils.Adaify_Name (Xml_Parser_Utils.Find_Specified_Interface (Request_Tag) & "_Ptr");
                           begin
                              if Xml_Parser_Utils.Number_Of_Args (Request_Tag) > 1 then
                                 declare
                                    V : Wayland_XML.Request_Child_Vectors.Vector;

                                    Max_Name_Length : Natural := Name'Length;

                                    function Align (Value : String) return String is (SF.Head (Value, Max_Name_Length, ' '));
                                 begin
                                    for Child of Children (Request_Tag) loop
                                       if Child.Kind_Id = Child_Arg then
                                          if Type_Attribute (Child.Arg_Tag.all) /= Type_New_Id then
                                             V.Append (Child);

                                             declare
                                                Arg_Name : constant String := Get_Name (Child.Arg_Tag.all);
                                             begin
                                                Max_Name_Length := Natural'Max (Max_Name_Length, Arg_Name'Length);
                                             end;
                                          end if;
                                       end if;
                                    end loop;

                                    Put_Line (File, "   function " & Subprogram_Name);
                                    Put_Line (File, "     (" & Align (Name) & " : " & Ptr_Name & ";");

                                    for Child of V loop
                                       if Child.Kind_Id = Child_Arg then
                                          Generate_Code_For_Arg
                                            (File, Child.Arg_Tag.all,
                                             Max_Name_Length,
                                             Child = Children (Request_Tag).Last_Element);
                                       end if;
                                    end loop;

                                    Put_Line (File, " return " & Return_Type);
                                    Put_Line (File, "   is");
                                    Put_Line (File, "      P : constant Proxy_Ptr :=");
                                    Put_Line (File, "        Wayland.API.Proxy_Marshal_Constructor");
                                    Put_Line (File, "          (" & Name & ".all,");
                                    Put_Line
                                      (File,
                                       "           Constants." & Xml_Parser_Utils.Adaify_Name
                                         (Wayland_XML.Name (Interface_Tag) & "_" &
                                            Wayland_XML.Name (Request_Tag)) & ",");
                                    Put_Line
                                      (File,
                                       "           " & Xml_Parser_Utils.Adaify_Name (Xml_Parser_Utils.Find_Specified_Interface (Request_Tag)) & "_Interface'Access,");
                                    Put (File, "           0");

                                    for Child of V loop
                                       if Child.Kind_Id = Child_Arg then
                                          Put_Line (File, ",");
                                          if Type_Attribute (Child.Arg_Tag.all) /= Type_Object then
                                             Put (File, "           " & Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Child.Arg_Tag.all)));
                                          else
                                             Put (File, "           " & Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Child.Arg_Tag.all)) & ".all'Address");
                                          end if;
                                       end if;
                                    end loop;

                                    Put_Line (File, ");");
                                    Put_Line (File, "   begin");
                                    Put_Line (File, "      return (if P /= null then P.all'Access else null);");
                                    Put_Line (File, "   end " & Subprogram_Name & ";");

                                 end;
                              else
                                 Put_Line
                                   (File,
                                    "   function " & Subprogram_Name & " (" & Name & " : " & Ptr_Name & ") return " & Return_Type & " is");
                                 Put_Line (File, "      P : constant Proxy_Ptr :=");
                                 Put_Line (File, "        Wayland.API.Proxy_Marshal_Constructor");
                                 Put_Line (File, "          (" & Name & ".all,");
                                 Put_Line
                                   (File,
                                    "           Constants." & Xml_Parser_Utils.Adaify_Name
                                      (Wayland_XML.Name (Interface_Tag) & "_" &
                                         Wayland_XML.Name (Request_Tag)) & ",");
                                 Put_Line
                                   (File,
                                    "           " & Xml_Parser_Utils.Adaify_Name (Xml_Parser_Utils.Find_Specified_Interface (Request_Tag)) & "_Interface'Access,");
                                 Put_Line (File, "           0);");
                                 Put_Line (File, "   begin");
                                 Put_Line (File, "      return (if P /= null then P.all'Access else null);");
                                 Put_Line (File, "   end " & Subprogram_Name & ";");
                              end if;
                           end;
                        else
                           if Xml_Parser_Utils.Number_Of_Args (Request_Tag) > 1 then
                              declare
                                 V : Wayland_XML.Request_Child_Vectors.Vector;

                                 Max_Name_Length : Natural := Natural'Max(11, Name'Length);

                                 function Align (Value : String) return String is (SF.Head (Value, Max_Name_Length, ' '));
                              begin
                                 for Child of Children (Request_Tag) loop
                                    if Child.Kind_Id = Child_Arg then
                                       if Type_Attribute (Child.Arg_Tag.all) /= Type_New_Id then
                                          V.Append (Child);

                                          declare
                                             Arg_Name : constant String := Get_Name (Child.Arg_Tag.all);
                                          begin
                                             Max_Name_Length := Natural'Max (Max_Name_Length, Arg_Name'Length);
                                          end;
                                       end if;
                                    end if;
                                 end loop;

                                 Put_Line (File, "   function " & Subprogram_Name);
                                 Put_Line (File, "     (" & Align (Name) & " : " & Ptr_Name & ";");

                                 for Child of V loop
                                    if Child.Kind_Id = Child_Arg then
                                       Generate_Code_For_Arg (File, Child.Arg_Tag.all, Max_Name_Length, False);
                                    end if;
                                 end loop;

                                 Put_Line (File, "      " & Align ("Interface_V") & " : Interface_Ptr;");
                                 Put_Line (File, "      " & Align ("New_Id") & " : Unsigned_32) return Proxy_Ptr is");
                                 Put_Line (File, "   begin");
                                 Put_Line (File, "      return Wayland.API.Proxy_Marshal_Constructor_Versioned");
                                 Put_Line (File, "        (" & Name & ".all,");
                                 Put_Line
                                   (File,
                                    "         Constants." & Xml_Parser_Utils.Adaify_Name
                                      (Wayland_XML.Name (Interface_Tag) & "_" &
                                         Wayland_XML.Name (Request_Tag)) & ",");
                                 Put_Line (File, "         Interface_V,");
                                 Put_Line (File, "         New_Id,");

                                 for Child of V loop
                                    if Child.Kind_Id = Child_Arg then
                                       if Type_Attribute (Child.Arg_Tag.all) /= Type_Object then
                                          Put_Line
                                            (File, "         " &
                                               Xml_Parser_Utils.Adaify_Variable_Name
                                               (Wayland_XML.Name (Child.Arg_Tag.all)) & ",");
                                       else
                                          Put_Line
                                            (File, "         " &
                                               Xml_Parser_Utils.Adaify_Variable_Name
                                               (Wayland_XML.Name (Child.Arg_Tag.all)) & ".all'Address,");
                                       end if;
                                    end if;
                                 end loop;

                                 Put_Line (File, "         Interface_V.Name,");
                                 Put_Line (File, "         New_Id,");
                                 Put_Line (File, "         0);");
                                 Put_Line (File, "   end " & Subprogram_Name & ";");
                              end;
                           else
                              Put_Line (File, "   function " & Subprogram_Name & " (" & Name & " : " & Ptr_Name & ") return Proxy_Ptr is");
                              Put_Line (File, "      P : constant Proxy_Ptr :=");
                              Put_Line (File, "        Wayland.API.Proxy_Marshal_Constructor");
                              Put_Line (File, "          (" & Name & ".all,");
                              Put_Line
                                (File,
                                 "           Constants." & Xml_Parser_Utils.Adaify_Name
                                   (Wayland_XML.Name (Interface_Tag) & "_" &
                                      Wayland_XML.Name (Request_Tag)) & ",");
                              Put_Line
                                (File,
                                 "           " & Xml_Parser_Utils.Adaify_Name (Xml_Parser_Utils.Find_Specified_Interface (Request_Tag)) & "_Interface'Access,");
                              Put_Line (File, "           0);");
                              Put_Line (File, "   begin");
                              Put_Line (File, "      return (if P /= null then P.all'Access else null);");
                              Put_Line (File, "   end " & Subprogram_Name & ";");
                           end if;
                        end if;
                     elsif Xml_Parser_Utils.Is_Request_Destructor (Request_Tag) then
                        null; -- Body is generated in Generate_Code_For_Destroy_Subprogram_Implementation
                     else
                        Put_Line (File, "");
                        if Xml_Parser_Utils.Number_Of_Args (Request_Tag) > 0 then
                           declare
                              V : Wayland_XML.Request_Child_Vectors.Vector;

                              Max_Name_Length : Natural := Name'Length;

                              function Align (Value : String) return String is (SF.Head (Value, Max_Name_Length, ' '));
                           begin
                              for Child of Children (Request_Tag) loop
                                 if Child.Kind_Id = Child_Arg then
                                    if Type_Attribute (Child.Arg_Tag.all) /= Type_New_Id then
                                       V.Append (Child);

                                       declare
                                          Arg_Name : constant String := Get_Name (Child.Arg_Tag.all);
                                       begin
                                          Max_Name_Length := Natural'Max (Max_Name_Length, Arg_Name'Length);
                                       end;
                                    end if;
                                 end if;
                              end loop;

                              Put_Line (File, "   procedure " & Subprogram_Name);
                              Put_Line (File, "     (" & Align (Name) & " : " & Ptr_Name & ";");

                              for Child of V loop
                                 if Child.Kind_Id = Child_Arg then
                                    Generate_Code_For_Arg
                                      (File, Child.Arg_Tag.all,
                                       Max_Name_Length,
                                       Child = Children (Request_Tag).Last_Element);
                                 end if;
                              end loop;

                              Put_Line (File, " is");
                              Put_Line (File, "   begin");
                              Put_Line (File, "      Wayland.API.Proxy_Marshal");
                              Put_Line (File, "        (" & Name & ".all,");
                              Put (File, "         " & Opcode);

                              for Child of V loop
                                 if Child.Kind_Id = Child_Arg then
                                    Put_Line (File, ",");
                                    if Type_Attribute (Child.Arg_Tag.all) /= Type_Object then
                                       Put (File, "         " & Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Child.Arg_Tag.all)));
                                    else
                                       Put (File, "         " & Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Child.Arg_Tag.all)) & ".all'Address");
                                    end if;
                                 end if;
                              end loop;
                              Put_Line (File, ");");
                              Put_Line (File, "   end " & Subprogram_Name & ";");
                           end;
                        else
                           Put_Line (File, "   procedure " & Subprogram_Name & " (" & Name & " : " & Ptr_Name & ") is");
                           Put_Line (File, "   begin");
                           Put_Line (File, "      Wayland.API.Proxy_Marshal (" & Name & ".all, " & Opcode & ");");
                           Put_Line (File, "   end " & Subprogram_Name & ";");
                        end if;
                     end if;
                  end Generate_Code_For_Subprogram_Implementation;

               begin
                  for Child of Children (Interface_Tag) loop
                     if Child.Kind_Id = Child_Request then
                        Generate_Code_For_Subprogram_Implementation (Child.Request_Tag.all);
                     end if;
                  end loop;
               end Generate_Code_For_Requests;

            begin
               if Xml_Parser_Utils.Exists_Any_Event_Tag (Interface_Tag) then
                  Put_Line (File, "");
                  Generate_Code_For_Add_Listener_Subprogram_Implementations;
               end if;

               Put_Line (File, "");
               Generate_Code_For_Set_User_Data_Subprogram_Implementations;
               Put_Line (File, "");
               Generate_Code_For_Get_User_Data_Subprogram_Implementations;
               Put_Line (File, "");
               Generate_Code_For_Get_Version_Subprogram_Implementations;
               Put_Line (File, "");
               Generate_Code_For_Destroy_Subprogram_Implementations;
               Generate_Code_For_Requests;
            end Handle_Interface;

         begin
            for Child of Children (Protocol_Tag.all) loop
               if Child.Kind_Id = Child_Interface then
                  Handle_Interface (Child.Interface_Tag.all);
               end if;
            end loop;
         end Generate_Code_For_Protocol_Tag_Children;
      begin
         Generate_Code_For_Protocol_Tag_Children;
      end Create_Wl_Thin_Body_File;

      procedure Generate_Manually_Edited_Code is
      begin
         Put_Line (File, "   use type Wl_Thin.Proxy_Ptr;");
         Put_Line (File, "");
         Put_Line (File, "   subtype Registry_Ptr is Wl_Thin.Registry_Ptr;");
         Put_Line (File, "");
         Put_Line (File, "   subtype Registry_Global_Subprogram_Ptr is Wl_Thin.Registry_Global_Subprogram_Ptr;");
         Put_Line (File, "");
         Put_Line (File, "   subtype Registry_Global_Remove_Subprogram_Ptr is Wl_Thin.Registry_Global_Remove_Subprogram_Ptr;");
         Put_Line (File, "");
         Put_Line (File, "   subtype Registry_Listener_T is Wl_Thin.Registry_Listener_T;");
         Put_Line (File, "");
         Put_Line (File, "   subtype Registry_Listener_Ptr is Wl_Thin.Registry_Listener_Ptr;");
         Put_Line (File, "");
         Put_Line (File, "   package body Registry_Objects_Subscriber is");
         Put_Line (File, "");
         Put_Line (File, "      procedure Internal_Object_Added (Unused_Data : Void_Ptr;");
         Put_Line (File, "                                       Registry    : Wl.Registry_Ptr;");
         Put_Line (File, "                                       Id          : Wl.Unsigned_32;");
         Put_Line (File, "                                       Interface_V : Wl.chars_ptr;");
         Put_Line (File, "                                       Version     : Wl.Unsigned_32) with");
         Put_Line (File, "        Convention => C,");
         Put_Line (File, "        Global     => null;");
         Put_Line (File, "");
         Put_Line (File, "      procedure Internal_Object_Added (Unused_Data : Void_Ptr;");
         Put_Line (File, "                                       Registry    : Wl.Registry_Ptr;");
         Put_Line (File, "                                       Id          : Wl.Unsigned_32;");
         Put_Line (File, "                                       Interface_V : Wl.chars_ptr;");
         Put_Line (File, "                                       Version     : Wl.Unsigned_32)");
         Put_Line (File, "      is");
         Put_Line (File, "         pragma Unreferenced (Unused_Data);");
         Put_Line (File, "");
         Put_Line (File, "         R : Wl.Registry := (");
         Put_Line (File, "                            My_Registry                 => Registry,");
         Put_Line (File, "                            My_Has_Started_Subscription => True");
         Put_Line (File, "                           );");
         Put_Line (File, "      begin");
         Put_Line (File, "         Global_Object_Added (Data, R, Id, Value (Interface_V), Version);");
         Put_Line (File, "      end Internal_Object_Added;");
         Put_Line (File, "");
         Put_Line (File, "      procedure Internal_Object_Removed (Unused_Data : Void_Ptr;");
         Put_Line (File, "                                         Registry    : Wl.Registry_Ptr;");
         Put_Line (File, "                                         Id          : Wl.Unsigned_32) with");
         Put_Line (File, "        Convention => C;");
         Put_Line (File, "");
         Put_Line (File, "      procedure Internal_Object_Removed (Unused_Data : Void_Ptr;");
         Put_Line (File, "                                         Registry    : Wl.Registry_Ptr;");
         Put_Line (File, "                                         Id          : Wl.Unsigned_32)");
         Put_Line (File, "      is");
         Put_Line (File, "         R : Wl.Registry := (");
         Put_Line (File, "                            My_Registry                 => Registry,");
         Put_Line (File, "                            My_Has_Started_Subscription => True");
         Put_Line (File, "                           );");
         Put_Line (File, "      begin");
         Put_Line (File, "         Global_Object_Removed (Data, R, Id);");
         Put_Line (File, "      end Internal_Object_Removed;");
         Put_Line (File, "");
         Put_Line (File, "      Listener : aliased Wl.Registry_Listener_T :=");
         Put_Line (File, "        (");
         Put_Line (File, "         Global        => Internal_Object_Added'Unrestricted_Access,");
         Put_Line (File, "         Global_Remove => Internal_Object_Removed'Unrestricted_Access");
         Put_Line (File, "        );");
         Put_Line (File, "");
         Put_Line (File, "      procedure Start_Subscription (Registry : in out Wl.Registry) is");
         Put_Line (File, "         I : Px.int;");
         Put_Line (File, "      begin");
         Put_Line (File, "         I := Wl_Thin.Registry_Add_Listener (Registry.My_Registry,");
         Put_Line (File, "                                             Listener'Unchecked_Access,");
         Put_Line (File, "                                             Nil);");
         Put_Line (File, "      end Start_Subscription;");
         Put_Line (File, "");
         Put_Line (File, "   end Registry_Objects_Subscriber;");
         Put_Line (File, "");
         Put_Line (File, "   package body Shell_Surface_Subscriber is");
         Put_Line (File, "");
         Put_Line (File, "      procedure Internal_Shell_Surface_Ping");
         Put_Line (File, "        (Unused_Data : Void_Ptr;");
         Put_Line (File, "         Surface     : Wl_Thin.Shell_Surface_Ptr;");
         Put_Line (File, "         Serial      : Unsigned_32) with");
         Put_Line (File, "        Convention => C;");
         Put_Line (File, "");
         Put_Line (File, "      procedure Internal_Shell_Surface_Configure");
         Put_Line (File, "        (Unused_Data : Void_Ptr;");
         Put_Line (File, "         Surface     : Wl_Thin.Shell_Surface_Ptr;");
         Put_Line (File, "         Edges       : Unsigned_32;");
         Put_Line (File, "         Width       : Integer;");
         Put_Line (File, "         Height      : Integer) with");
         Put_Line (File, "        Convention => C;");
         Put_Line (File, "");
         Put_Line (File, "      procedure Internal_Shell_Surface_Popup_Done");
         Put_Line (File, "        (Unused_Data : Void_Ptr;");
         Put_Line (File, "         Surface     : Wl_Thin.Shell_Surface_Ptr) with");
         Put_Line (File, "        Convention => C;");
         Put_Line (File, "");
         Put_Line (File, "      procedure Internal_Shell_Surface_Ping");
         Put_Line (File, "        (Unused_Data : Void_Ptr;");
         Put_Line (File, "         Surface     : Wl_Thin.Shell_Surface_Ptr;");
         Put_Line (File, "         Serial      : Unsigned_32)");
         Put_Line (File, "      is");
         Put_Line (File, "         S : Wl.Shell_Surface := (My_Shell_Surface => Surface);");
         Put_Line (File, "      begin");
         Put_Line (File, "         Shell_Surface_Ping (Data, S, Serial);");
         Put_Line (File, "      end Internal_Shell_Surface_Ping;");
         Put_Line (File, "");
         Put_Line (File, "      procedure Internal_Shell_Surface_Configure");
         Put_Line (File, "        (Unused_Data : Void_Ptr;");
         Put_Line (File, "         Surface     : Wl_Thin.Shell_Surface_Ptr;");
         Put_Line (File, "         Edges       : Unsigned_32;");
         Put_Line (File, "         Width       : Integer;");
         Put_Line (File, "         Height      : Integer)");
         Put_Line (File, "      is");
         Put_Line (File, "         S : Wl.Shell_Surface := (My_Shell_Surface => Surface);");
         Put_Line (File, "      begin");
         Put_Line (File, "         Shell_Surface_Configure (Data, S, Edges, Width, Height);");
         Put_Line (File, "      end Internal_Shell_Surface_Configure;");
         Put_Line (File, "");
         Put_Line (File, "      procedure Internal_Shell_Surface_Popup_Done");
         Put_Line (File, "        (Unused_Data : Void_Ptr;");
         Put_Line (File, "         Surface     : Wl_Thin.Shell_Surface_Ptr)");
         Put_Line (File, "      is");
         Put_Line (File, "         S : Wl.Shell_Surface := (My_Shell_Surface => Surface);");
         Put_Line (File, "      begin");
         Put_Line (File, "         Shell_Surface_Popup_Done (Data, S);");
         Put_Line (File, "      end Internal_Shell_Surface_Popup_Done;");
         Put_Line (File, "");
         Put_Line (File, "      Listener : aliased Wl_Thin.Shell_Surface_Listener_T :=");
         Put_Line (File, "        (");
         Put_Line (File, "         Ping       => Internal_Shell_Surface_Ping'Unrestricted_Access,");
         Put_Line (File, "         Configure  => Internal_Shell_Surface_Configure'Unrestricted_Access,");
         Put_Line (File, "         Popup_Done => Internal_Shell_Surface_Popup_Done'Unrestricted_Access");
         Put_Line (File, "        );");
         Put_Line (File, "");
         Put_Line (File, "      procedure Start_Subscription (Surface : in out Wl.Shell_Surface) is");
         Put_Line (File, "         I : Px.int;");
         Put_Line (File, "      begin");
         Put_Line (File, "         I := Wl_Thin.Shell_Surface_Add_Listener");
         Put_Line (File, "           (Surface.My_Shell_Surface,");
         Put_Line (File, "            Listener'Unchecked_Access,");
         Put_Line (File, "            Nil);");
         Put_Line (File, "      end Start_Subscription;");
         Put_Line (File, "");
         Put_Line (File, "   end Shell_Surface_Subscriber;");
         Put_Line (File, "");
         Put_Line (File, "   package body Seat_Capability_Subscriber is");
         Put_Line (File, "");
         Put_Line (File, "      procedure Internal_Seat_Capabilities (Unused_Data  : Void_Ptr;");
         Put_Line (File, "                                   Seat         : Wl_Thin.Seat_Ptr;");
         Put_Line (File, "                                   Capabilities : Unsigned_32) with");
         Put_Line (File, "        Convention => C;");
         Put_Line (File, "");
         Put_Line (File, "      procedure Internal_Seat_Capabilities (Unused_Data  : Void_Ptr;");
         Put_Line (File, "                                   Seat         : Wl_Thin.Seat_Ptr;");
         Put_Line (File, "                                   Capabilities : Unsigned_32)");
         Put_Line (File, "      is");
         Put_Line (File, "         S : Wl.Seat := (");
         Put_Line (File, "                        My_Seat                     => Seat,");
         Put_Line (File, "                        My_Has_Started_Subscription => True");
         Put_Line (File, "                       );");
         Put_Line (File, "      begin");
         Put_Line (File, "         Seat_Capabilities (Data, S, Capabilities);");
         Put_Line (File, "      end Internal_Seat_Capabilities;");
         Put_Line (File, "");
         Put_Line (File, "      procedure Internal_Seat_Name (Unused_Data : Void_Ptr;");
         Put_Line (File, "                                    Seat        : Wl_Thin.Seat_Ptr;");
         Put_Line (File, "                                    Name        : Interfaces.C.Strings.chars_ptr) with");
         Put_Line (File, "        Convention => C;");
         Put_Line (File, "");
         Put_Line (File, "      procedure Internal_Seat_Name (Unused_Data : Void_Ptr;");
         Put_Line (File, "                                    Seat        : Wl_Thin.Seat_Ptr;");
         Put_Line (File, "                                    Name        : Interfaces.C.Strings.chars_ptr)");
         Put_Line (File, "      is");
         Put_Line (File, "         N : String := Interfaces.C.Strings.Value (Name);");
         Put_Line (File, "");
         Put_Line (File, "         S : Wl.Seat := (");
         Put_Line (File, "                        My_Seat                     => Seat,");
         Put_Line (File, "                        My_Has_Started_Subscription => True");
         Put_Line (File, "                       );");
         Put_Line (File, "      begin");
         Put_Line (File, "         Seat_Name (Data, S, N);");
         Put_Line (File, "      end Internal_Seat_Name;");
         Put_Line (File, "");
         Put_Line (File, "      Seat_Listener : aliased Wl_Thin.Seat_Listener_T :=");
         Put_Line (File, "        (");
         Put_Line (File, "         Capabilities => Internal_Seat_Capabilities'Unrestricted_Access,");
         Put_Line (File, "         Name         => Internal_Seat_Name'Unrestricted_Access");
         Put_Line (File, "        );");
         Put_Line (File, "");
         Put_Line (File, "      procedure Start_Subscription (S : in out Wl.Seat) is");
         Put_Line (File, "         I : Px.int;");
         Put_Line (File, "      begin");
         Put_Line (File, "         I := Wl_Thin.Seat_Add_Listener (Seat     => S.My_Seat,");
         Put_Line (File, "                                         Listener => Seat_Listener'Unchecked_Access,");
         Put_Line (File, "                                         Data     => Nil);");
         Put_Line (File, "      end Start_Subscription;");
         Put_Line (File, "");
         Put_Line (File, "   end Seat_Capability_Subscriber;");
         Put_Line (File, "");
         Put_Line (File, "   package body Pointer_Subscriber is");
         Put_Line (File, "");
         Put_Line (File, "      procedure Internal_Pointer_Enter");
         Put_Line (File, "        (Unused_Data : Void_Ptr;");
         Put_Line (File, "         Pointer     : Wl_Thin.Pointer_Ptr;");
         Put_Line (File, "         Serial      : Unsigned_32;");
         Put_Line (File, "         Surface     : Wl_Thin.Surface_Ptr;");
         Put_Line (File, "         Surface_X   : Wl.Fixed;");
         Put_Line (File, "         Surface_Y   : Wl.Fixed) with");
         Put_Line (File, "        Convention => C;");
         Put_Line (File, "");
         Put_Line (File, "      procedure Internal_Pointer_Leave");
         Put_Line (File, "        (Unused_Data : Void_Ptr;");
         Put_Line (File, "         Pointer     : Wl_Thin.Pointer_Ptr;");
         Put_Line (File, "         Serial      : Unsigned_32;");
         Put_Line (File, "         Surface     : Wl_Thin.Surface_Ptr) with");
         Put_Line (File, "        Convention => C;");
         Put_Line (File, "");
         Put_Line (File, "      procedure Internal_Pointer_Motion");
         Put_Line (File, "        (Data      : Void_Ptr;");
         Put_Line (File, "         Pointer   : Wl_Thin.Pointer_Ptr;");
         Put_Line (File, "         Time      : Unsigned_32;");
         Put_Line (File, "         Surface_X : Wl.Fixed;");
         Put_Line (File, "         Surface_Y : Wl.Fixed) with");
         Put_Line (File, "        Convention => C;");
         Put_Line (File, "");
         Put_Line (File, "      procedure Internal_Pointer_Button");
         Put_Line (File, "        (Unused_Data : Void_Ptr;");
         Put_Line (File, "         Pointer     : Wl_Thin.Pointer_Ptr;");
         Put_Line (File, "         Serial      : Unsigned_32;");
         Put_Line (File, "         Time        : Unsigned_32;");
         Put_Line (File, "         Button      : Unsigned_32;");
         Put_Line (File, "         State       : Unsigned_32) with");
         Put_Line (File, "        Convention => C;");
         Put_Line (File, "");
         Put_Line (File, "      procedure Internal_Pointer_Axis");
         Put_Line (File, "        (Data    : Void_Ptr;");
         Put_Line (File, "         Pointer : Wl_Thin.Pointer_Ptr;");
         Put_Line (File, "         Time    : Unsigned_32;");
         Put_Line (File, "         Axis    : Unsigned_32;");
         Put_Line (File, "         Value   : Wl.Fixed) with");
         Put_Line (File, "        Convention => C;");
         Put_Line (File, "");
         Put_Line (File, "      procedure Internal_Pointer_Frame (Data    : Void_Ptr;");
         Put_Line (File, "                                        Pointer : Wl_Thin.Pointer_Ptr) with");
         Put_Line (File, "        Convention => C;");
         Put_Line (File, "");
         Put_Line (File, "      procedure Internal_Pointer_Axis_Source");
         Put_Line (File, "        (Data        : Void_Ptr;");
         Put_Line (File, "         Pointer     : Wl_Thin.Pointer_Ptr;");
         Put_Line (File, "         Axis_Source : Unsigned_32) with");
         Put_Line (File, "        Convention => C;");
         Put_Line (File, "");
         Put_Line (File, "      procedure Internal_Pointer_Axis_Stop");
         Put_Line (File, "        (Data    : Void_Ptr;");
         Put_Line (File, "         Pointer : Wl_Thin.Pointer_Ptr;");
         Put_Line (File, "         Time    : Unsigned_32;");
         Put_Line (File, "         Axis    : Unsigned_32) with");
         Put_Line (File, "        Convention => C;");
         Put_Line (File, "");
         Put_Line (File, "      procedure Internal_Pointer_Axis_Discrete");
         Put_Line (File, "        (Data     : Void_Ptr;");
         Put_Line (File, "         Pointer  : Wl_Thin.Pointer_Ptr;");
         Put_Line (File, "         Axis     : Unsigned_32;");
         Put_Line (File, "         Discrete : Integer) with");
         Put_Line (File, "        Convention => C;");
         Put_Line (File, "");
         Put_Line (File, "      procedure Internal_Pointer_Enter");
         Put_Line (File, "        (Unused_Data : Void_Ptr;");
         Put_Line (File, "         Pointer     : Wl_Thin.Pointer_Ptr;");
         Put_Line (File, "         Serial      : Unsigned_32;");
         Put_Line (File, "         Surface     : Wl_Thin.Surface_Ptr;");
         Put_Line (File, "         Surface_X   : Wl.Fixed;");
         Put_Line (File, "         Surface_Y   : Wl.Fixed)");
         Put_Line (File, "      is");
         Put_Line (File, "         P : Wl.Pointer := (My_Pointer => Pointer);");
         Put_Line (File, "         S : Wl.Surface := (My_Surface => Surface);");
         Put_Line (File, "      begin");
         Put_Line (File, "         Pointer_Enter (Data, P, Serial, S, Surface_X, Surface_Y);");
         Put_Line (File, "      end Internal_Pointer_Enter;");
         Put_Line (File, "");
         Put_Line (File, "      procedure Internal_Pointer_Leave");
         Put_Line (File, "        (Unused_Data : Void_Ptr;");
         Put_Line (File, "         Pointer     : Wl_Thin.Pointer_Ptr;");
         Put_Line (File, "         Serial      : Unsigned_32;");
         Put_Line (File, "         Surface     : Wl_Thin.Surface_Ptr)");
         Put_Line (File, "      is");
         Put_Line (File, "         P : Wl.Pointer := (My_Pointer => Pointer);");
         Put_Line (File, "         S : Wl.Surface := (My_Surface => Surface);");
         Put_Line (File, "      begin");
         Put_Line (File, "         Pointer_Leave (Data, P, Serial, S);");
         Put_Line (File, "      end Internal_Pointer_Leave;");
         Put_Line (File, "");
         Put_Line (File, "      procedure Internal_Pointer_Motion");
         Put_Line (File, "        (Data      : Void_Ptr;");
         Put_Line (File, "         Pointer   : Wl_Thin.Pointer_Ptr;");
         Put_Line (File, "         Time      : Unsigned_32;");
         Put_Line (File, "         Surface_X : Wl.Fixed;");
         Put_Line (File, "         Surface_Y : Wl.Fixed) is");
         Put_Line (File, "      begin");
         Put_Line (File, "         null;");
         Put_Line (File, "      end Internal_Pointer_Motion;");
         Put_Line (File, "");
         Put_Line (File, "      procedure Internal_Pointer_Button");
         Put_Line (File, "        (Unused_Data : Void_Ptr;");
         Put_Line (File, "         Pointer     : Wl_Thin.Pointer_Ptr;");
         Put_Line (File, "         Serial      : Unsigned_32;");
         Put_Line (File, "         Time        : Unsigned_32;");
         Put_Line (File, "         Button      : Unsigned_32;");
         Put_Line (File, "         State       : Unsigned_32)");
         Put_Line (File, "      is");
         Put_Line (File, "         P : Wl.Pointer := (My_Pointer => Pointer);");
         Put_Line (File, "      begin");
         Put_Line (File, "         Pointer_Button (Data, P, Serial, Time, Button, State);");
         Put_Line (File, "      end Internal_Pointer_Button;");
         Put_Line (File, "");
         Put_Line (File, "      procedure Internal_Pointer_Axis");
         Put_Line (File, "        (Data    : Void_Ptr;");
         Put_Line (File, "         Pointer : Wl_Thin.Pointer_Ptr;");
         Put_Line (File, "         Time    : Unsigned_32;");
         Put_Line (File, "         Axis    : Unsigned_32;");
         Put_Line (File, "         Value   : Wl.Fixed) is");
         Put_Line (File, "      begin");
         Put_Line (File, "         null;");
         Put_Line (File, "      end Internal_Pointer_Axis;");
         Put_Line (File, "");
         Put_Line (File, "      procedure Internal_Pointer_Frame (Data    : Void_Ptr;");
         Put_Line (File, "                                        Pointer : Wl_Thin.Pointer_Ptr) is");
         Put_Line (File, "      begin");
         Put_Line (File, "         null;");
         Put_Line (File, "      end Internal_Pointer_Frame;");
         Put_Line (File, "");
         Put_Line (File, "      procedure Internal_Pointer_Axis_Source");
         Put_Line (File, "        (Data        : Void_Ptr;");
         Put_Line (File, "         Pointer     : Wl_Thin.Pointer_Ptr;");
         Put_Line (File, "         Axis_Source : Unsigned_32) is");
         Put_Line (File, "      begin");
         Put_Line (File, "         null;");
         Put_Line (File, "      end Internal_Pointer_Axis_Source;");
         Put_Line (File, "");
         Put_Line (File, "      procedure Internal_Pointer_Axis_Stop");
         Put_Line (File, "        (Data    : Void_Ptr;");
         Put_Line (File, "         Pointer : Wl_Thin.Pointer_Ptr;");
         Put_Line (File, "         Time    : Unsigned_32;");
         Put_Line (File, "         Axis    : Unsigned_32) is");
         Put_Line (File, "      begin");
         Put_Line (File, "         null;");
         Put_Line (File, "      end Internal_Pointer_Axis_Stop;");
         Put_Line (File, "");
         Put_Line (File, "      procedure Internal_Pointer_Axis_Discrete");
         Put_Line (File, "        (Data     : Void_Ptr;");
         Put_Line (File, "         Pointer  : Wl_Thin.Pointer_Ptr;");
         Put_Line (File, "         Axis     : Unsigned_32;");
         Put_Line (File, "         Discrete : Integer) is");
         Put_Line (File, "      begin");
         Put_Line (File, "         null;");
         Put_Line (File, "      end Internal_Pointer_Axis_Discrete;");
         Put_Line (File, "");
         Put_Line (File, "      Pointer_Listener : aliased Wl_Thin.Pointer_Listener_T :=");
         Put_Line (File, "        (");
         Put_Line (File, "         Enter         => Internal_Pointer_Enter'Unrestricted_Access,");
         Put_Line (File, "         Leave         => Internal_Pointer_Leave'Unrestricted_Access,");
         Put_Line (File, "         Motion        => Internal_Pointer_Motion'Unrestricted_Access,");
         Put_Line (File, "         Button        => Internal_Pointer_Button'Unrestricted_Access,");
         Put_Line (File, "         Axis          => Internal_Pointer_Axis'Unrestricted_Access,");
         Put_Line (File, "         Frame         => Internal_Pointer_Frame'Unrestricted_Access,");
         Put_Line (File, "         Axis_Source   => Internal_Pointer_Axis_Source'Unrestricted_Access,");
         Put_Line (File, "         Axis_Stop     => Internal_Pointer_Axis_Stop'Unrestricted_Access,");
         Put_Line (File, "         Axis_Discrete => Internal_Pointer_Axis_Discrete'Unrestricted_Access");
         Put_Line (File, "        );");
         Put_Line (File, "");
         Put_Line (File, "      procedure Start_Subscription (P : in out Wl.Pointer) is");
         Put_Line (File, "         I : Px.int;");
         Put_Line (File, "      begin");
         Put_Line (File, "         I := Wl_Thin.Pointer_Add_Listener (Pointer  => P.My_Pointer,");
         Put_Line (File, "                                            Listener => Pointer_Listener'Unrestricted_Access,");
         Put_Line (File, "                                            Data     => Nil);");
         Put_Line (File, "      end Start_Subscription;");
         Put_Line (File, "");
         Put_Line (File, "   end Pointer_Subscriber;");
         Put_Line (File, "");
         Put_Line (File, "   procedure Connect (Display : in out Wl.Display;");
         Put_Line (File, "                      Name    : C_String) is");
         Put_Line (File, "   begin");
         Put_Line (File, "      Display.My_Display := Wl_Thin.Display_Connect (Name);");
         Put_Line (File, "   end Connect;");
         Put_Line (File, "");
         Put_Line (File, "   procedure Disconnect (Display : in out Wl.Display) is");
         Put_Line (File, "   begin");
         Put_Line (File, "      if Display.My_Display /= null then");
         Put_Line (File, "         Wl_Thin.Display_Disconnect (Display.My_Display);");
         Put_Line (File, "      end if;");
         Put_Line (File, "   end Disconnect;");
         Put_Line (File, "");
         Put_Line (File, "   procedure Get_Registry (Display  : Wl.Display;");
         Put_Line (File, "                           Registry : in out Wl.Registry) is");
         Put_Line (File, "   begin");
         Put_Line (File, "      Registry.My_Registry := Wl_Thin.Display_Get_Registry (Display.My_Display);");
         Put_Line (File, "   end Get_Registry;");
         Put_Line (File, "");
         Put_Line (File, "   procedure Destroy (Registry : in out Wl.Registry) is");
         Put_Line (File, "   begin");
         Put_Line (File, "      if Registry.My_Registry /= null then");
         Put_Line (File, "         Wl_Thin.Registry_Destroy (Registry.My_Registry);");
         Put_Line (File, "         Registry.My_Registry := null;");
         Put_Line (File, "      end if;");
         Put_Line (File, "   end Destroy;");
         Put_Line (File, "");
         Put_Line (File, "   function Dispatch (Display : Wl.Display) return Int is");
         Put_Line (File, "   begin");
         Put_Line (File, "      return Wl_Thin.Display_Dispatch (Display.My_Display);");
         Put_Line (File, "   end Dispatch;");
         Put_Line (File, "");
         Put_Line (File, "   procedure Dispatch (Display : Wl.Display) is");
         Put_Line (File, "      I : Int;");
         Put_Line (File, "      pragma Unreferenced (I);");
         Put_Line (File, "   begin");
         Put_Line (File, "      I := Display.Dispatch;");
         Put_Line (File, "   end Dispatch;");
         Put_Line (File, "");
         Put_Line (File, "   function Roundtrip (Display : Wl.Display) return Int is");
         Put_Line (File, "   begin");
         Put_Line (File, "      return Wl_Thin.Display_Roundtrip (Display.My_Display);");
         Put_Line (File, "   end Roundtrip;");
         Put_Line (File, "");
         Put_Line (File, "   procedure Roundtrip (Display : Wl.Display) is");
         Put_Line (File, "      I : Int;");
         Put_Line (File, "      pragma Unreferenced (I);");
         Put_Line (File, "   begin");
         Put_Line (File, "      I := Display.Roundtrip;");
         Put_Line (File, "   end Roundtrip;");
         Put_Line (File, "");
         Put_Line (File, "   procedure Bind (Compositor  : in out Wl.Compositor;");
         Put_Line (File, "                   Registry    : Wl.Registry;");
         Put_Line (File, "                   Id          : Unsigned_32;");
         Put_Line (File, "                   Version     : Unsigned_32)");
         Put_Line (File, "   is");
         Put_Line (File, "      P : Wl_Thin.Proxy_Ptr :=");
         Put_Line (File, "        Wl_Thin.Registry_Bind (Registry    => Registry.My_Registry,");
         Put_Line (File, "                               Name        => Id,");
         Put_Line (File, "                               Interface_V => Wl_Thin.Compositor_Interface'Access,");
         Put_Line (File, "                               New_Id      => Version);");
         Put_Line (File, "");
         Put_Line (File, "   begin");
         Put_Line (File, "      if P /= null then");
         Put_Line (File, "         Compositor.My_Compositor := P.all'Access;");
         Put_Line (File, "      end if;");
         Put_Line (File, "   end Bind;");
         Put_Line (File, "");
         Put_Line (File, "   procedure Create_Surface (Compositor : Wl.Compositor;");
         Put_Line (File, "                             Surface    : in out Wl.Surface) is");
         Put_Line (File, "   begin");
         Put_Line (File, "      Surface.My_Surface :=");
         Put_Line (File, "        Wl_Thin.Compositor_Create_Surface (Compositor.My_Compositor);");
         Put_Line (File, "   end Create_Surface;");
         Put_Line (File, "");
         Put_Line (File, "   procedure Bind (Seat     : in out Wl.Seat;");
         Put_Line (File, "                   Registry : Wl.Registry;");
         Put_Line (File, "                   Id       : Unsigned_32;");
         Put_Line (File, "                   Version  : Unsigned_32)");
         Put_Line (File, "   is");
         Put_Line (File, "      P : Wl_Thin.Proxy_Ptr :=");
         Put_Line (File, "        Wl_Thin.Registry_Bind (Registry    => Registry.My_Registry,");
         Put_Line (File, "                               Name        => Id,");
         Put_Line (File, "                               Interface_V => Wl_Thin.Seat_Interface'Access,");
         Put_Line (File, "                               New_Id      => Version);");
         Put_Line (File, "");
         Put_Line (File, "   begin");
         Put_Line (File, "      if P /= null then");
         Put_Line (File, "         Seat.My_Seat := P.all'Access;");
         Put_Line (File, "      end if;");
         Put_Line (File, "   end Bind;");
         Put_Line (File, "");
         Put_Line (File, "   procedure Get_Pointer (Seat    : Wl.Seat;");
         Put_Line (File, "                          Pointer : in out Wl.Pointer) is");
         Put_Line (File, "   begin");
         Put_Line (File, "      Pointer.My_Pointer := Wl_Thin.Seat_Get_Pointer (Seat.My_Seat);");
         Put_Line (File, "   end Get_Pointer;");
         Put_Line (File, "");
         Put_Line (File, "   procedure Bind (Shell    : in out Wl.Shell;");
         Put_Line (File, "                   Registry : Wl.Registry;");
         Put_Line (File, "                   Id       : Unsigned_32;");
         Put_Line (File, "                   Version  : Unsigned_32)");
         Put_Line (File, "   is");
         Put_Line (File, "      P : Wl_Thin.Proxy_Ptr :=");
         Put_Line (File, "        Wl_Thin.Registry_Bind (Registry    => Registry.My_Registry,");
         Put_Line (File, "                               Name        => Id,");
         Put_Line (File, "                               Interface_V => Wl_Thin.Shell_Interface'Access,");
         Put_Line (File, "                               New_Id      => Version);");
         Put_Line (File, "");
         Put_Line (File, "   begin");
         Put_Line (File, "      if P /= null then");
         Put_Line (File, "         Shell.My_Shell := P.all'Access;");
         Put_Line (File, "      end if;");
         Put_Line (File, "   end Bind;");
         Put_Line (File, "");
         Put_Line (File, "   procedure Get_Shell_Surface (Shell         : Wl.Shell;");
         Put_Line (File, "                                Surface       : Wl.Surface;");
         Put_Line (File, "                                Shell_Surface : in out Wl.Shell_Surface) is");
         Put_Line (File, "   begin");
         Put_Line (File, "      Shell_Surface.My_Shell_Surface :=");
         Put_Line (File, "        Wl_Thin.Shell_Get_Shell_Surface (Shell.My_Shell, Surface.My_Surface);");
         Put_Line (File, "   end Get_Shell_Surface;");
         Put_Line (File, "");
         Put_Line (File, "   procedure Bind (Shm      : in out Wl.Shm;");
         Put_Line (File, "                   Registry : Wl.Registry;");
         Put_Line (File, "                   Id       : Unsigned_32;");
         Put_Line (File, "                   Version  : Unsigned_32)");
         Put_Line (File, "   is");
         Put_Line (File, "      P : Wl_Thin.Proxy_Ptr :=");
         Put_Line (File, "        Wl_Thin.Registry_Bind (Registry    => Registry.My_Registry,");
         Put_Line (File, "                               Name        => Id,");
         Put_Line (File, "                               Interface_V => Wl_Thin.Shm_Interface'Access,");
         Put_Line (File, "                               New_Id      => Version);");
         Put_Line (File, "");
         Put_Line (File, "   begin");
         Put_Line (File, "      if P /= null then");
         Put_Line (File, "         Shm.My_Shm := P.all'Access;");
         Put_Line (File, "      end if;");
         Put_Line (File, "   end Bind;");
         Put_Line (File, "");
         Put_Line (File, "   procedure Create_Pool (Shm             : Wl.Shm;");
         Put_Line (File, "                          File_Descriptor : Integer;");
         Put_Line (File, "                          Size            : Integer;");
         Put_Line (File, "                          Pool            : in out Wl.Shm_Pool) is");
         Put_Line (File, "   begin");
         Put_Line (File, "      Pool.My_Shm_Pool := Wl_Thin.Shm_Create_Pool (Shm.My_Shm, File_Descriptor, Size);");
         Put_Line (File, "   end Create_Pool;");
         Put_Line (File, "");
         Put_Line (File, "   procedure Create_Buffer (Pool   : Wl.Shm_Pool;");
         Put_Line (File, "                            Offset   : Integer;");
         Put_Line (File, "                            Width    : Integer;");
         Put_Line (File, "                            Height   : Integer;");
         Put_Line (File, "                            Stride   : Integer;");
         Put_Line (File, "                            Format   : Unsigned_32;");
         Put_Line (File, "                            Buffer : in out Wl.Buffer) is");
         Put_Line (File, "   begin");
         Put_Line (File, "      Buffer.My_Buffer := Wl_Thin.Shm_Pool_Create_Buffer (Pool.My_Shm_Pool,");
         Put_Line (File, "                                                          Offset,");
         Put_Line (File, "                                                          Width,");
         Put_Line (File, "                                                          Height,");
         Put_Line (File, "                                                          Stride,");
         Put_Line (File, "                                                          Format);");
         Put_Line (File, "   end Create_Buffer;");
         Put_Line (File, "");
         Put_Line (File, "   procedure Set_Toplevel (Surface : Wl.Shell_Surface) is");
         Put_Line (File, "   begin");
         Put_Line (File, "      Wl_Thin.Shell_Surface_Set_Toplevel (Surface.My_Shell_Surface);");
         Put_Line (File, "   end Set_Toplevel;");
         Put_Line (File, "");
         Put_Line (File, "   procedure Pong (Surface : Wl.Shell_Surface;");
         Put_Line (File, "                   Serial  : Unsigned_32) is");
         Put_Line (File, "   begin");
         Put_Line (File, "      Wl_Thin.Shell_Surface_Pong (Surface.My_Shell_Surface, Serial);");
         Put_Line (File, "   end Pong;");
         Put_Line (File, "");
         Put_Line (File, "   procedure Attach (Surface : Wl.Surface;");
         Put_Line (File, "                     Buffer  : Wl.Buffer;");
         Put_Line (File, "                     X       : Integer;");
         Put_Line (File, "                     Y       : Integer) is");
         Put_Line (File, "   begin");
         Put_Line (File, "      Wl_Thin.Surface_Attach (Surface.My_Surface, Buffer.My_Buffer, X, Y);");
         Put_Line (File, "   end Attach;");
         Put_Line (File, "");
         Put_Line (File, "   procedure Commit (Surface : Wl.Surface) is");
         Put_Line (File, "   begin");
         Put_Line (File, "      Wl_Thin.Surface_Commit (Surface.My_Surface);");
         Put_Line (File, "   end Commit;");
         Put_Line (File, "");
         Put_Line (File, "   procedure Destroy (Surface : in out Wl.Surface) is");
         Put_Line (File, "   begin");
         Put_Line (File, "      if Surface.My_Surface /= null then");
         Put_Line (File, "         Wl_Thin.Surface_Destroy (Surface.My_Surface);");
         Put_Line (File, "         Surface.My_Surface := null;");
         Put_Line (File, "      end if;");
         Put_Line (File, "   end Destroy;");
         Put_Line (File, "");
      end Generate_Manually_Edited_Code;
   begin
      Create_File;
   end Create_Wayland_Body_File;

begin
   Read_Wayland_XML_File (Ada.Command_Line.Argument (1));
exception
   when Unknown_Exception : others =>
      Put_Line (Ada.Exceptions.Exception_Information (Unknown_Exception));
end XML_Parser;

--  When using tagged types in Wayland_XML and object-prefix notation.
--  Storage size: 1_000_000
--  Storage used: 0_585_452
--
--  Latest:
--  Storage used: 0_544_260
