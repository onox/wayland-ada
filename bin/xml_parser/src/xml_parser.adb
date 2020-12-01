with Ada.Command_Line;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Aida.Deepend.XML_DOM_Parser;

with Wayland_XML;
with Xml_Parser_Utils;

with Standard_Extensions; use Standard_Extensions;

-- chars_ptr should be replaced with C_String in the thin Ada binding.
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
   use all type Wayland_XML.Enum_Child;

   package SF renames Ada.Strings.Fixed;
   package SU renames Ada.Strings.Unbounded;

   function "+" (Value : String) return SU.Unbounded_String renames SU.To_Unbounded_String;
   function "+" (Value : SU.Unbounded_String) return String renames SU.To_String;

   XML_Exception : exception;

   procedure Read_Wayland_XML_File (File_Name : String);
   procedure Create_Wayland_Spec_File (File_Name : String);
   procedure Create_Wayland_Body_File (File_Name : String);

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

   procedure Read_Wayland_XML_File (File_Name : String) is

      procedure Check_Wayland_XML_File_Exists;
      procedure Allocate_Space_For_Wayland_XML_Contents (File_Name : String);
      procedure Parse_Contents (File_Name : String);
      procedure Identify_Protocol_Tag;
      procedure Identify_Protocol_Children (File_Name : String);

      procedure Check_Wayland_XML_File_Exists is
      begin
         if not Ada.Directories.Exists (File_Name) then
            raise Constraint_Error with File_Name & " does not exist";
         end if;

         Allocate_Space_For_Wayland_XML_Contents (File_Name);
         Parse_Contents (File_Name);
      end Check_Wayland_XML_File_Exists;

      File_Contents : Aida.Deepend.String_Ptr;

      procedure Allocate_Space_For_Wayland_XML_Contents (File_Name : String) is
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

      pragma Unmodified (File_Contents);

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
              (Request_Tag : aliased Wayland_XML.Request_Tag)
            is
               Name : constant String
                 := Xml_Parser_Utils.Adaify_Name
                   (Wayland_XML.Name (Interface_Tag) & "_" &
                      Wayland_XML.Name (Request_Tag));
            begin
               Put_Line (File, "   " & Name & " : constant := " & Aida.To_String (I) & ";");

               I := I + 1;
            end Generate_Code;
         begin
            Iterate_Over_Requests (Interface_Tag, Generate_Code'Access);
         end Generate_Code_For_Opcodes;

         procedure Generate_Code_For_Event_Since_Version is
            procedure Generate_Code (Event_Tag : aliased Wayland_XML.Event_Tag) is
               Name : constant String
                 := Xml_Parser_Utils.Adaify_Name
                   (Wayland_XML.Name (Interface_Tag) & "_" &
                      Wayland_XML.Name (Event_Tag) & "_SINCE_VERSION");
            begin
               if Exists_Since_Attribute (Event_Tag) then
                  Put_Line (File, "   " & Name & " : constant := " & Aida.To_String (Since_Attribute_As_Pos32 (Event_Tag)) & ";");
               else
                  Put_Line (File, "   " & Name & " : constant := 1;");
               end if;
            end Generate_Code;
         begin
            Iterate_Over_Events (Interface_Tag, Generate_Code'Access);
         end Generate_Code_For_Event_Since_Version;

         procedure Generate_Code_For_Opcodes_Since_Version is
            procedure Generate_Code (Request_Tag : aliased Wayland_XML.Request_Tag) is
               Name : constant String
                 := Xml_Parser_Utils.Adaify_Name
                   (Wayland_XML.Name (Interface_Tag) & "_" &
                      Wayland_XML.Name (Request_Tag) & "_SINCE_VERSION");
            begin
               if Exists_Since (Request_Tag) then
                  Put_Line (File, "   " & Name & " : constant := " & Aida.To_String (Since_As_Pos32 (Request_Tag)) & ";");
               else
                  Put_Line (File, "   " & Name & " : constant := 1;");
               end if;
            end Generate_Code;
         begin
            Iterate_Over_Requests (Interface_Tag, Generate_Code'Access);
         end Generate_Code_For_Opcodes_Since_Version;

      begin
         Generate_Code_For_Opcodes;
         Generate_Code_For_Event_Since_Version;
         Generate_Code_For_Opcodes_Since_Version;
         New_Line (File);
      end Handle_Interface;

   begin
      Iterate_Over_Interfaces (Handle_Interface'Access);
   end Generate_Code_For_Numeric_Constants;

   procedure Create_Wayland_Spec_File (File_Name : String) is
      File : Ada.Text_IO.File_Type;

      procedure Create_Wl_Thin_Spec_File;

      procedure Generate_Code_For_Type_Declarations;
      procedure Generate_Code_For_The_Interface_Type;
      procedure Generate_Code_For_The_Interface_Constants;
      procedure Generate_Code_For_Enum_Constants;
      procedure Generate_Private_Code_For_Enum_Constants;
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
            Put_Line (File, "with Wayland." & Package_Name & ".Enums;");
            New_Line (File);
            Put_Line (File, "with C_Binding.Linux.Files;");
            New_Line (File);
            Put_Line (File, "package Wayland." & Package_Name & ".Protocol is");
            Put_Line (File, "   pragma Preelaborate;");
            New_Line (File);
            Put_Line (File, "   use Wayland." & Package_Name & ".Enums;");
            New_Line (File);

            Generate_Code_For_Type_Declarations;
            Generate_Code_For_The_Interface_Type;
            Generate_Code_For_The_Interface_Constants;
            Generate_Manually_Edited_Partial_Type_Declarations;

            Put_Line (File, "private");
            New_Line (File);

            Generate_Code_For_The_Private_Part;
            Generate_Use_Type_Declarions;
            Generate_Manually_Edited_Code_For_Type_Definitions;
            Generate_Private_Code_For_The_Interface_Constants;

            Put_Line (File, "end Wayland." & Package_Name & ".Protocol;");

            Ada.Text_IO.Close (File);
         end if;

         -----------------------------------------------------------------------

         Ada.Text_IO.Create
           (File, Ada.Text_IO.Out_File, "wayland-" & Protocol_Name & "-enums.ads");

         Put_Line (File, "package Wayland." & Package_Name & ".Enums is");
         Put_Line (File, "   pragma Preelaborate;");

         Generate_Code_For_Enum_Constants;

         New_Line (File);
         Put_Line (File, "private");
         New_Line (File);

         Generate_Private_Code_For_Enum_Constants;

         Put_Line (File, "end Wayland." & Package_Name & ".Enums;");

         Ada.Text_IO.Close (File);

         -----------------------------------------------------------------------

         Ada.Text_IO.Create
           (File, Ada.Text_IO.Out_File, "wayland-" & Protocol_Name & "-constants.ads");

         Put_Line (File, "private package Wayland." & Package_Name & ".Constants is");
         Put_Line (File, "   pragma Pure;");
         New_Line (File);

         Generate_Code_For_Numeric_Constants (File);

         Put_Line (File, "end Wayland." & Package_Name & ".Constants;");

         Ada.Text_IO.Close (File);

         -----------------------------------------------------------------------

         Ada.Text_IO.Create
           (File, Ada.Text_IO.Out_File, "wayland-" & Protocol_Name & "-thin.ads");

         Put_Line (File, "with Interfaces.C.Strings;");
         Put_Line (File, "");
         Put_Line (File, "with Wayland.API;");
         Put_Line (File, "with Wayland." & Package_Name & ".Enums;");
         Put_Line (File, "");
         Put_Line (File, "--  Mostly auto generated from " & File_Name);
         Put_Line (File, "private package Wayland." & Package_Name & ".Thin is");
         Put_Line (File, "   pragma Preelaborate;");
         Put_Line (File, "");
         Put_Line (File, "   use Wayland." & Package_Name & ".Enums;");
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
         Put_Line (File, "   function Display_Connect return Display_Ptr;");
         Put_Line (File, "");
         Put_Line (File, "   procedure Display_Disconnect (This : in out Display_Ptr);");
         Put_Line (File, "");
         Put_Line (File, "   --  End core parts");
         Put_Line (File, "");

         Create_Wl_Thin_Spec_File;

         Put_Line (File, "end Wayland." & Package_Name & ".Thin;");

         Ada.Text_IO.Close (File);
      end Create_File;

      pragma Unmodified (File);

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
         Iterate_Over_Interfaces (Handle_Interface'Access);
         New_Line (File);

         Put_Line (File, "   pragma Linker_Options (""-lwayland-client"");");
         Put_Line (File, "   --  Added this linker option here to avoid adding it");
         Put_Line (File, "   --  to each gpr file that with's this Wayland Ada binding.");
         New_Line (File);
      end Generate_Code_For_Type_Declarations;

      procedure Generate_Code_For_The_Interface_Type is
      begin
         Put_Line (File, "   type Call_Result_Code is (Success, Error);");
         New_Line (File);
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
           (Interface_Tag : aliased Wayland_XML.Interface_Tag)
         is
            Name : constant String
              := Xml_Parser_Utils.Adaify_Name
                (Wayland_XML.Name (Interface_Tag) & "_Interface");
         begin
            Put_Line (File, "   " & Name & " : constant Interface_Type;");
         end Handle_Interface;
      begin
         Iterate_Over_Interfaces (Handle_Interface'Access);
         New_Line (File);
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
                  Summary_String : constant String := Summary (Entry_Tag);
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
                  Put_Line (File, "   end record;");
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
                       " at 0 range " & Trim(Bit'Image) & " .. " & Trim (Bit'Image) & ";");
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

      procedure Generate_Manually_Edited_Partial_Type_Declarations is
         procedure Handle_Interface_Subprograms (Interface_Tag : aliased Wayland_XML.Interface_Tag) is
            Name : constant String
              := Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Interface_Tag));
         begin
            Put_Line (File, "   function Has_Proxy (Object : " & Name & ") return Boolean");
            Put_Line (File, "     with Global => null;");
            Put_Line (File, "");

            if Name /= "Display" then
               Put_Line (File, "   procedure Destroy (Object : in out " & Name & ")");
               Put_Line (File, "     with Pre  => Object.Has_Proxy,");
               Put_Line (File, "          Post => not Object.Has_Proxy;");
               Put_Line (File, "");
            end if;

            Put_Line (File, "   function Get_Version (Object : " & Name & ") return Unsigned_32");
            Put_Line (File, "     with Pre => Object.Has_Proxy;");
            Put_Line (File, "");

            if Name in "Data_Device" | "Seat" | "Pointer" | "Keyboard" | "Touch" | "Output" then
               Put_Line (File, "   procedure Release (Object : in out " & Name & ")");
               Put_Line (File, "     with Pre  => Object.Has_Proxy,");
               Put_Line (File, "          Post => not Object.Has_Proxy;");
               Put_Line (File, "");
            end if;

            if Name = "Display" then
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
               Put_Line (File, "      Timeout : Integer) return Check_For_Events_Status;");
               Put_Line (File, "   --  The timeout is given in milliseconds");
               Put_Line (File, "");
               Put_Line (File, "   function Dispatch (Object : Display) return Integer");
               Put_Line (File, "     with Pre => Object.Is_Connected;");
               Put_Line (File, "   --  Process incoming events");
               Put_Line (File, "");
               Put_Line (File, "   procedure Dispatch (Object : Display)");
               Put_Line (File, "     with Pre => Object.Is_Connected;");
               Put_Line (File, "   --  Process incoming events. Ignores error code. TODO To be removed?");
               Put_Line (File, "");
               Put_Line (File, "   function Dispatch_Pending (Object : Display) return Integer");
               Put_Line (File, "     with Pre => Object.Is_Connected;");
               Put_Line (File, "   --  Dispatch default queue events without reading from");
               Put_Line (File, "   --  the display file descriptor");
               Put_Line (File, "   --");
               Put_Line (File, "   --  This function dispatches events on the main event queue.");
               Put_Line (File, "   --  It does not attempt to read the display fd and simply returns zero");
               Put_Line (File, "   --  if the main queue is empty, i.e., it doesn't block.");
               Put_Line (File, "   --");
               Put_Line (File, "   --  Returns the number of dispatched events or -1 on failure");
               Put_Line (File, "");
               Put_Line (File, "   function Prepare_Read (Object : Display) return Integer");
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
               Put_Line (File, "      Registry : in out Protocol.Registry'Class)");
               Put_Line (File, "   with Pre => Object.Is_Connected and not Registry.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   function Sync (Object : Display) return Callback'Class");
               Put_Line (File, "     with Pre => Object.Is_Connected;");
               Put_Line (File, "");
            elsif Name = "Compositor" then
               Put_Line (File, "   procedure Bind (Object   : in out Compositor;");
               Put_Line (File, "                   Registry : Protocol.Registry'Class;");
               Put_Line (File, "                   Id       : Unsigned_32;");
               Put_Line (File, "                   Version  : Unsigned_32)");
               Put_Line (File, "     with Pre => not Object.Has_Proxy and Registry.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Create_Surface (Object  : Compositor;");
               Put_Line (File, "                             Surface : in out Protocol.Surface'Class)");
               Put_Line (File, "     with Pre => Object.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Create_Region (Object : Compositor;");
               Put_Line (File, "                            Region : in out Protocol.Region'Class)");
               Put_Line (File, "     with Pre => Object.Has_Proxy;");
               Put_Line (File, "");
            elsif Name = "Seat" then
               Put_Line (File, "   procedure Bind (Object   : in out Seat;");
               Put_Line (File, "                   Registry : Protocol.Registry'Class;");
               Put_Line (File, "                   Id       : Unsigned_32;");
               Put_Line (File, "                   Version  : Unsigned_32)");
               Put_Line (File, "     with Pre => not Object.Has_Proxy and Registry.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Get_Pointer (Object  : Seat;");
               Put_Line (File, "                          Pointer : in out Protocol.Pointer'Class)");
               Put_Line (File, "     with Pre => Object.Has_Proxy and not Pointer.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Get_Keyboard (Object   : Seat;");
               Put_Line (File, "                           Keyboard : in out Protocol.Keyboard'Class)");
               Put_Line (File, "     with Pre => Object.Has_Proxy and not Keyboard.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Get_Touch (Object : Seat;");
               Put_Line (File, "                        Touch  : in out Protocol.Touch'Class)");
               Put_Line (File, "     with Pre => Object.Has_Proxy and not Touch.Has_Proxy;");
               Put_Line (File, "");
            elsif Name = "Shm_Pool" then
               Put_Line (File, "   procedure Create_Buffer (Object : Shm_Pool;");
               Put_Line (File, "                            Offset : Natural;");
               Put_Line (File, "                            Width  : Natural;");
               Put_Line (File, "                            Height : Natural;");
               Put_Line (File, "                            Stride : Natural;");
               Put_Line (File, "                            Format : Shm_Format;");
               Put_Line (File, "                            Buffer : in out Protocol.Buffer'Class)");
               Put_Line (File, "     with Pre => Object.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Resize (Object : Shm_Pool;");
               Put_Line (File, "                     Size   : Positive)");
               Put_Line (File, "     with Pre => Object.Has_Proxy;");
               Put_Line (File, "");
            elsif Name = "Shm" then
               Put_Line (File, "   procedure Bind (Object   : in out Shm;");
               Put_Line (File, "                   Registry : Protocol.Registry'Class;");
               Put_Line (File, "                   Id       : Unsigned_32;");
               Put_Line (File, "                   Version  : Unsigned_32)");
               Put_Line (File, "     with Pre => not Object.Has_Proxy and Registry.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Create_Pool (Object          : Shm;");
               Put_Line (File, "                          File_Descriptor : C_Binding.Linux.Files.File;");
               Put_Line (File, "                          Size            : Positive;");
               Put_Line (File, "                          Pool            : in out Shm_Pool'Class);");
               Put_Line (File, "");
            elsif Name = "Data_Offer" then
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
               Put_Line (File, "                      File_Descriptor : Integer)");
               Put_Line (File, "     with Pre => Object.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Finish (Object : Data_Offer)");
               Put_Line (File, "     with Pre => Object.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_Actions (Object           : Data_Offer;");
               Put_Line (File, "                          Dnd_Actions      : Unsigned_32;");
               Put_Line (File, "                          Preferred_Action : Unsigned_32)");
               Put_Line (File, "     with Pre => Object.Has_Proxy;");
               Put_Line (File, "");
            elsif Name = "Data_Source" then
               Put_Line (File, "   procedure Offer (Object    : Data_Source;");
               Put_Line (File, "                    Mime_Type : String)");
               Put_Line (File, "     with Pre => Object.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_Actions (Object      : Data_Source;");
               Put_Line (File, "                          Dnd_Actions : Unsigned_32)");
               Put_Line (File, "     with Pre => Object.Has_Proxy;");
               Put_Line (File, "");
            elsif Name = "Data_Device" then
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
               Put_Line (File, "");
            elsif Name = "Data_Device_Manager" then
               Put_Line (File, "   procedure Create_Data_Source (Object : Data_Device_Manager;");
               Put_Line (File, "                                 Source : in out Data_Source'Class)");
               Put_Line (File, "     with Pre => Object.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Get_Data_Device (Object : Data_Device_Manager;");
               Put_Line (File, "                              Seat   : Protocol.Seat'Class;");
               Put_Line (File, "                              Device : in out Data_Device'Class)");
               Put_Line (File, "     with Pre => Object.Has_Proxy;");
               Put_Line (File, "");
            elsif Name = "Surface" then
               Put_Line (File, "   procedure Attach (Object : Surface;");
               Put_Line (File, "                     Buffer : Protocol.Buffer'Class;");
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
               Put_Line (File, "                                Region : Protocol.Region'Class)");
               Put_Line (File, "     with Pre => Object.Has_Proxy;");
               Put_Line (File, "");
               Put_Line (File, "   procedure Set_Input_Region (Object : Surface;");
               Put_Line (File, "                               Region : Protocol.Region'Class)");
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
               Put_Line (File, "");
            elsif Name = "Pointer" then
               Put_Line (File, "   procedure Set_Cursor (Object    : Pointer;");
               Put_Line (File, "                         Serial    : Unsigned_32;");
               Put_Line (File, "                         Surface   : Protocol.Surface'Class;");
               Put_Line (File, "                         Hotspot_X : Integer;");
               Put_Line (File, "                         Hotspot_Y : Integer)");
               Put_Line (File, "     with Pre => Object.Has_Proxy;");
               Put_Line (File, "");
            elsif Name = "Region" then
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
               Put_Line (File, "");
            elsif Name = "Subcompositor" then
               Put_Line (File, "   procedure Get_Subsurface (Object     : Subcompositor;");
               Put_Line (File, "                             Surface    : Protocol.Surface'Class;");
               Put_Line (File, "                             Parent     : Protocol.Surface'Class;");
               Put_Line (File, "                             Subsurface : in out Protocol.Subsurface'Class)");
               Put_Line (File, "     with Pre => Object.Has_Proxy and Surface.Has_Proxy and Parent.Has_Proxy;");
               Put_Line (File, "");
            elsif Name = "Subsurface" then
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
               Put_Line (File, "");
            end if;
         end Handle_Interface_Subprograms;

         procedure Handle_Interface_Events (Interface_Tag : aliased Wayland_XML.Interface_Tag) is
            Name : constant String
              := Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Interface_Tag));
         begin
            if Name not in "Display" | "Registry" | "Callback" | "Shm" | "Buffer" | "Data_Offer" | "Data_Source" | "Data_Device" | "Surface" | "Seat" | "Pointer" | "Keyboard" | "Touch" | "Output" then
               return;
            end if;

            Put_Line (File, "   generic");

            if Name = "Display" then
               Put_Line (File, "      with procedure Error");
               Put_Line (File, "        (Display   : in out Protocol.Display'Class;");
               Put_Line (File, "         Object_Id : Void_Ptr;");
               Put_Line (File, "         Code      : Unsigned_32;");
               Put_Line (File, "         Message   : String);");
               Put_Line (File, "      --  TODO Should really Object_Id really be exposed here? This part");
               Put_Line (File, "      --  of the API can potentially be improved upon.");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Delete_Id");
               Put_Line (File, "        (Display : in out Protocol.Display'Class;");
               Put_Line (File, "         Id      : Unsigned_32);");
            elsif Name = "Registry" then
               Put_Line (File, "      with procedure Global_Object_Added");
               Put_Line (File, "        (Registry : in out Protocol.Registry'Class;");
               Put_Line (File, "         Id       : Unsigned_32;");
               Put_Line (File, "         Name     : String;");
               Put_Line (File, "         Version  : Unsigned_32);");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Global_Object_Removed");
               Put_Line (File, "        (Registry : in out Protocol.Registry'Class;");
               Put_Line (File, "         Id       : Unsigned_32);");
            elsif Name = "Callback" then
               Put_Line (File, "      with procedure Done");
               Put_Line (File, "        (Callback      : in out Protocol.Callback'Class;");
               Put_Line (File, "         Callback_Data : Unsigned_32);");
            elsif Name = "Shm" then
               Put_Line (File, "      with procedure Format");
               Put_Line (File, "        (Shm    : in out Protocol.Shm'Class;");
               Put_Line (File, "         Format : Shm_Format);");
            elsif Name = "Buffer" then
               Put_Line (File, "      with procedure Release");
               Put_Line (File, "        (Buffer : in out Protocol.Buffer'Class);");
            elsif Name = "Data_Offer" then
               Put_Line (File, "      with procedure Offer");
               Put_Line (File, "        (Data_Offer : in out Protocol.Data_Offer'Class;");
               Put_Line (File, "         Mime_Type  : String);");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Source_Actions");
               Put_Line (File, "        (Data_Offer     : in out Protocol.Data_Offer'Class;");
               Put_Line (File, "         Source_Actions : Unsigned_32);");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Action");
               Put_Line (File, "        (Data_Offer : in out Protocol.Data_Offer'Class;");
               Put_Line (File, "         Dnd_Action : Unsigned_32);");
            elsif Name = "Data_Source" then
               Put_Line (File, "      with procedure Target");
               Put_Line (File, "        (Data_Source : in out Protocol.Data_Source'Class;");
               Put_Line (File, "         Mime_Type   : String);");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Send");
               Put_Line (File, "        (Data_Source : in out Protocol.Data_Source'Class;");
               Put_Line (File, "         Mime_Type   : String;");
               Put_Line (File, "         Fd          : Integer);");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Cancelled");
               Put_Line (File, "        (Data_Source : in out Protocol.Data_Source'Class);");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Dnd_Drop_Performed");
               Put_Line (File, "        (Data_Source : in out Protocol.Data_Source'Class);");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Dnd_Finished");
               Put_Line (File, "        (Data_Source : in out Protocol.Data_Source'Class);");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Action");
               Put_Line (File, "        (Data_Source : in out Protocol.Data_Source'Class;");
               Put_Line (File, "         Dnd_Action  : Unsigned_32);");
            elsif Name = "Data_Device" then
               Put_Line (File, "      with procedure Data_Offer");
               Put_Line (File, "        (Data_Device : in out Protocol.Data_Device'Class;");
               Put_Line (File, "         Id          : Unsigned_32);");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Enter");
               Put_Line (File, "        (Data_Device : in out Protocol.Data_Device'Class;");
               Put_Line (File, "         Serial      : Unsigned_32;");
               Put_Line (File, "         Surface     : Protocol.Surface;");
               Put_Line (File, "         X, Y        : Fixed;");
               Put_Line (File, "         Id          : Protocol.Data_Offer);");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Leave");
               Put_Line (File, "        (Data_Device : in out Protocol.Data_Device'Class);");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Motion");
               Put_Line (File, "        (Data_Device : in out Protocol.Data_Device'Class;");
               Put_Line (File, "         Time        : Unsigned_32;");
               Put_Line (File, "         X, Y        : Fixed);");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Drop");
               Put_Line (File, "        (Data_Device : in out Protocol.Data_Device'Class);");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Selection");
               Put_Line (File, "        (Data_Device : in out Protocol.Data_Device'Class;");
               Put_Line (File, "         Id          : Protocol.Data_Offer);");
            elsif Name = "Surface" then
               Put_Line (File, "      with procedure Enter");
               Put_Line (File, "        (Surface : in out Protocol.Surface'Class;");
               Put_Line (File, "         Output  : Protocol.Output);");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Leave");
               Put_Line (File, "        (Surface : in out Protocol.Surface'Class;");
               Put_Line (File, "         Output  : Protocol.Output);");
            elsif Name = "Seat" then
               Put_Line (File, "      with procedure Seat_Capabilities");
               Put_Line (File, "        (Seat         : in out Protocol.Seat'Class;");
               Put_Line (File, "         Capabilities : Seat_Capability);");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Seat_Name");
               Put_Line (File, "        (Seat : in out Protocol.Seat'Class;");
               Put_Line (File, "         Name : String);");
            elsif Name = "Pointer" then
               Put_Line (File, "      with procedure Pointer_Enter");
               Put_Line (File, "        (Pointer   : in out Protocol.Pointer'Class;");
               Put_Line (File, "         Serial    : Unsigned_32;");
               Put_Line (File, "         Surface   : Protocol.Surface;");
               Put_Line (File, "         Surface_X : Fixed;");
               Put_Line (File, "         Surface_Y : Fixed);");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Pointer_Leave");
               Put_Line (File, "        (Pointer : in out Protocol.Pointer'Class;");
               Put_Line (File, "         Serial  : Unsigned_32;");
               Put_Line (File, "         Surface : Protocol.Surface);");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Pointer_Motion");
               Put_Line (File, "        (Pointer   : in out Protocol.Pointer'Class;");
               Put_Line (File, "         Time      : Unsigned_32;");
               Put_Line (File, "         Surface_X : Fixed;");
               Put_Line (File, "         Surface_Y : Fixed);");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Pointer_Button");
               Put_Line (File, "        (Pointer : in out Protocol.Pointer'Class;");
               Put_Line (File, "         Serial  : Unsigned_32;");
               Put_Line (File, "         Time    : Unsigned_32;");
               Put_Line (File, "         Button  : Unsigned_32;");
               Put_Line (File, "         State   : Pointer_Button_State);");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Pointer_Scroll");
               Put_Line (File, "        (Pointer : in out Protocol.Pointer'Class;");
               Put_Line (File, "         Time    : Unsigned_32;");
               Put_Line (File, "         Axis    : Pointer_Axis;");
               Put_Line (File, "         Value   : Fixed);");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Pointer_Frame");
               Put_Line (File, "        (Pointer : in out Protocol.Pointer'Class);");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Pointer_Scroll_Source");
               Put_Line (File, "        (Pointer     : in out Protocol.Pointer'Class;");
               Put_Line (File, "         Axis_Source : Pointer_Axis_Source);");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Pointer_Scroll_Stop");
               Put_Line (File, "        (Pointer : in out Protocol.Pointer'Class;");
               Put_Line (File, "         Time    : Unsigned_32;");
               Put_Line (File, "         Axis    : Pointer_Axis);");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Pointer_Scroll_Discrete");
               Put_Line (File, "        (Pointer  : in out Protocol.Pointer'Class;");
               Put_Line (File, "         Axis     : Pointer_Axis;");
               Put_Line (File, "         Discrete : Integer);");
            elsif Name = "Keyboard" then
               Put_Line (File, "      with procedure Keymap");
               Put_Line (File, "        (Keyboard : in out Protocol.Keyboard'Class;");
               Put_Line (File, "         Format   : Keyboard_Keymap_Format;");
               Put_Line (File, "         Fd       : Integer;");
               Put_Line (File, "         Size     : Unsigned_32);");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Enter");
               Put_Line (File, "        (Keyboard : in out Protocol.Keyboard'Class;");
               Put_Line (File, "         Serial   : Unsigned_32;");
               Put_Line (File, "         Surface  : Protocol.Surface;");
               Put_Line (File, "         Keys     : Wayland_Array_T);");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Leave");
               Put_Line (File, "        (Keyboard : in out Protocol.Keyboard'Class;");
               Put_Line (File, "         Serial   : Unsigned_32;");
               Put_Line (File, "         Surface  : Protocol.Surface);");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Key");
               Put_Line (File, "        (Keyboard : in out Protocol.Keyboard'Class;");
               Put_Line (File, "         Serial   : Unsigned_32;");
               Put_Line (File, "         Time     : Unsigned_32;");
               Put_Line (File, "         Key      : Unsigned_32;");
               Put_Line (File, "         State    : Keyboard_Key_State);");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Modifiers");
               Put_Line (File, "        (Keyboard       : in out Protocol.Keyboard'Class;");
               Put_Line (File, "         Serial         : Unsigned_32;");
               Put_Line (File, "         Mods_Depressed : Unsigned_32;");
               Put_Line (File, "         Mods_Latched   : Unsigned_32;");
               Put_Line (File, "         Mods_Locked    : Unsigned_32;");
               Put_Line (File, "         Group          : Unsigned_32);");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Repeat_Info");
               Put_Line (File, "        (Keyboard : in out Protocol.Keyboard'Class;");
               Put_Line (File, "         Rate     : Integer;");
               Put_Line (File, "         Delay_V  : Integer);");
            elsif Name = "Touch" then
               Put_Line (File, "      with procedure Down");
               Put_Line (File, "        (Touch   : in out Protocol.Touch'Class;");
               Put_Line (File, "         Serial  : Unsigned_32;");
               Put_Line (File, "         Time    : Unsigned_32;");
               Put_Line (File, "         Surface : Protocol.Surface;");
               Put_Line (File, "         Id      : Integer;");
               Put_Line (File, "         X, Y    : Fixed);");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Up");
               Put_Line (File, "        (Touch  : in out Protocol.Touch'Class;");
               Put_Line (File, "         Serial : Unsigned_32;");
               Put_Line (File, "         Time   : Unsigned_32;");
               Put_Line (File, "         Id     : Integer);");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Motion");
               Put_Line (File, "        (Touch : in out Protocol.Touch'Class;");
               Put_Line (File, "         Time  : Unsigned_32;");
               Put_Line (File, "         Id    : Integer;");
               Put_Line (File, "         X, Y  : Fixed);");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Frame");
               Put_Line (File, "        (Touch : in out Protocol.Touch'Class);");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Cancel");
               Put_Line (File, "        (Touch : in out Protocol.Touch'Class);");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Shape");
               Put_Line (File, "        (Touch : in out Protocol.Touch'Class;");
               Put_Line (File, "         Id    : Integer;");
               Put_Line (File, "         Major : Fixed;");
               Put_Line (File, "         Minor : Fixed);");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Orientation");
               Put_Line (File, "        (Touch       : in out Protocol.Touch'Class;");
               Put_Line (File, "         Id          : Integer;");
               Put_Line (File, "         Orientation : Fixed);");
            elsif Name = "Output" then
               Put_Line (File, "      with procedure Geometry");
               Put_Line (File, "        (Output          : in out Protocol.Output'Class;");
               Put_Line (File, "         X, Y            : Integer;");
               Put_Line (File, "         Physical_Width  : Integer;");
               Put_Line (File, "         Physical_Height : Integer;");
               Put_Line (File, "         Subpixel        : Output_Subpixel;");
               Put_Line (File, "         Make            : String;");
               Put_Line (File, "         Model           : String;");
               Put_Line (File, "         Transform       : Output_Transform);");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Mode");
               Put_Line (File, "        (Output  : in out Protocol.Output'Class;");
               Put_Line (File, "         Flags   : Output_Mode;");
               Put_Line (File, "         Width   : Integer;");
               Put_Line (File, "         Height  : Integer;");
               Put_Line (File, "         Refresh : Integer);");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Done");
               Put_Line (File, "        (Output : in out Protocol.Output'Class);");
               Put_Line (File, "");
               Put_Line (File, "      with procedure Scale");
               Put_Line (File, "        (Output : in out Protocol.Output'Class;");
               Put_Line (File, "         Factor : Integer);");
            end if;

            Put_Line (File, "   package " & Name & "_Events is");
            Put_Line (File, "");
            Put_Line (File, "      function Subscribe");
            Put_Line (File, "        (Object : aliased in out " & Name & "'Class) return Call_Result_Code;");
            Put_Line (File, "");
            Put_Line (File, "   end " & Name & "_Events;");
            Put_Line (File, "");
         end Handle_Interface_Events;
      begin
         Iterate_Over_Interfaces (Handle_Interface_Subprograms'Access);
         Iterate_Over_Interfaces (Handle_Interface_Events'Access);
      end Generate_Manually_Edited_Partial_Type_Declarations;

      procedure Generate_Code_For_The_Private_Part is
      begin
         Put_Line (File, "   subtype char_array is Interfaces.C.char_array;");
         New_Line (File);
         Put_Line (File, "   subtype chars_ptr is Interfaces.C.Strings.chars_ptr;");
         New_Line (File);
         Put_Line (File, "   function Value (C : chars_ptr) return String renames Interfaces.C.Strings.Value;");
         New_Line (File);
      end Generate_Code_For_The_Private_Part;

      procedure Create_Wl_Thin_Spec_File is

         procedure Generate_Code_For_Interface_Constants is
            procedure Handle_Interface (Interface_Tag : aliased Wayland_XML.Interface_Tag) is
               Name : constant String
                 := Xml_Parser_Utils.Adaify_Name
                   (Wayland_XML.Name (Interface_Tag) & "_Interface");
            begin
               Put_Line (File, "   " & Name & " : aliased Interface_T with");
               Put_Line (File, "      Import        => True,");
               Put_Line (File, "      Convention    => C,");
               Put_Line (File, "      External_Name => """ & Wayland_XML.Name (Interface_Tag) & "_interface"";");
               New_Line (File);
            end Handle_Interface;
         begin
            Iterate_Over_Interfaces (Handle_Interface'Access);
         end Generate_Code_For_Interface_Constants;

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
               begin
                  Put_Line (File, "   type " & Name & " is record");

                  for Child of Children (Interface_Tag) loop
                     if Child.Kind_Id = Child_Event then
                        declare
                           Arg_Name : constant String := Get_Name (Child.Event_Tag.all);
                        begin
                           Name_Length := Natural'Max (Name_Length, Arg_Name'Length);
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
                                 Child = Children (Request_Tag).Last_Element,
                                 Dont_Care);
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
               Generate_Code_For_Get_Version_Subprogram_Declaration;

               if Wayland_XML.Name (Interface_Tag) /= "wl_display" then
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
         Generate_Code_For_Interface_Constants;
         Generate_Code_For_Interface_Ptrs;
         Generate_Code_For_Each_Interface;
      end Create_Wl_Thin_Spec_File;

      procedure Generate_Use_Type_Declarions is
         procedure Handle_Interface (Interface_Tag : aliased Wayland_XML.Interface_Tag) is
            Name : constant String
              := Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Interface_Tag));
         begin
            Put_Line (File, "   use type Thin." & Name & "_Ptr;");
         end Handle_Interface;
      begin
         Iterate_Over_Interfaces (Handle_Interface'Access);

         New_Line (File);
      end Generate_Use_Type_Declarions;

      procedure Generate_Manually_Edited_Code_For_Type_Definitions is
         procedure Handle_Interface (Interface_Tag : aliased Wayland_XML.Interface_Tag) is
            Name : constant String
              := Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Interface_Tag));
         begin
            Put_Line (File, "   type " & Name & " is tagged limited record");
            Put_Line (File, "      Proxy : Thin." & Name & "_Ptr;");
            Put_Line (File, "   end record;");
            New_Line (File);
            Put_Line (File, "   function Has_Proxy (Object : " & Name & ") return Boolean is");
            Put_Line (File, "     (Object.Proxy /= null);");
            New_Line (File);
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
            Put_Line (File, "   " & Name & " : constant Interface_Type :=");
            Put_Line (File, "     (My_Interface => Thin." & Name & "'Access);");
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

         Iterate_Over_Interfaces (Handle_Interface'Access);
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

            Put_Line (File, "with System.Address_To_Access_Conversions;");
            New_Line (File);
            Put_Line (File, "with Interfaces.C;");
            New_Line (File);
            Put_Line (File, "with C_Binding.Linux;");
            New_Line (File);
            Put_Line (File, "with Wayland.API;");
            Put_Line (File, "with Wayland.Client.Constants;");
            New_Line (File);
            Put_Line (File, "package body Wayland." & Package_Name & ".Protocol is");
            New_Line (File);

            Generate_Manually_Edited_Code;

            Put_Line (File, "end Wayland." & Package_Name & ".Protocol;");

            Ada.Text_IO.Close (File);
         end if;

         -----------------------------------------------------------------------

         Ada.Text_IO.Create
           (File, Ada.Text_IO.Out_File, "wayland-" & Protocol_Name & "-thin.adb");

         Put_Line (File, "with Ada.Unchecked_Conversion;");
         Put_Line (File, "");
         Put_Line (File, "with Wayland." & Package_Name & ".Constants;");
         Put_Line (File, "");
         Put_Line (File, "use Wayland." & Package_Name & ".Constants;");
         Put_Line (File, "");
         Put_Line (File, "--  Mostly auto generated from " & File_Name);
         Put_Line (File, "package body Wayland." & Package_Name & ".Thin is");
         Put_Line (File, "");
         Put_Line (File, "   use type Proxy_Ptr;");
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
               procedure Generate_Code_For_Add_Listener_Subprogram_Implementations (Name : String) is
                  Ptr_Listener_Name : constant String := Name & "_Listener_Ptr";
               begin
                  Generate_Pretty_Code_For_Subprogram
                    (File, Implementation, Interface_Tag, "Add_Listener", "Interfaces.C.int",
                      ((+"Listener", +Ptr_Listener_Name),
                       (+"Data", +"Void_Ptr")));
                  Put_Line (File, "   begin");
                  Put_Line (File, "      return Wayland.API.Proxy_Add_Listener (" & Name & ".all, Listener.all'Address, Data);");
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

                  if Xml_Parser_Utils.Exists_Destructor (Interface_Tag) then
                     Put_Line (File, "      Wayland.API.Proxy_Marshal (" & Name & ".all, Constants." &
                       Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Interface_Tag) & "_Destroy") & ");");
                     Put_Line (File, "");
                  end if;

                  Put_Line (File, "      Wayland.API.Proxy_Destroy (" & Name & ".all);");
                  Put_Line (File, "   end " & Name & "_Destroy;");
               end Generate_Code_For_Destroy_Subprogram_Implementations;

               procedure Generate_Code_For_Requests is

                  procedure Generate_Code_For_Subprogram_Implementation
                    (Request_Tag : aliased Wayland_XML.Request_Tag)
                  is
                     Subprogram_Name : constant String
                       := Xml_Parser_Utils.Adaify_Name
                            (Name (Interface_Tag) & "_" & Name (Request_Tag));
                     Opcode : constant String := "Constants." & Subprogram_Name;
                     Name            : constant String
                       := Xml_Parser_Utils.Adaify_Name
                         (Wayland_XML.Name (Interface_Tag));
                     Ptr_Name        : constant String
                       := Xml_Parser_Utils.Adaify_Name
                         (Wayland_XML.Name (Interface_Tag) & "_Ptr");

                     procedure Generate_Arguments (Spaces : Natural; V : Wayland_XML.Request_Child_Vectors.Vector) is
                        use SF;

                        function Get_Value (Child : Wayland_XML.Arg_Tag; Value : String) return String is
                          (if Exists_Enum (Child) then "Convert (" & Value & ")" else Value);
                     begin
                        for Child of V loop
                           if Child.Kind_Id = Child_Arg then
                              Put_Line (File, ",");
                              if Type_Attribute (Child.Arg_Tag.all) /= Type_Object then
                                 Put (File, Spaces * " " & Get_Value (Child.Arg_Tag.all, Xml_Parser_Utils.Adaify_Variable_Name (Wayland_XML.Name (Child.Arg_Tag.all))));
                              else
                                 Put (File, Spaces * " " & Get_Value (Child.Arg_Tag.all, Xml_Parser_Utils.Adaify_Variable_Name (Wayland_XML.Name (Child.Arg_Tag.all))) & ".all'Address");
                              end if;
                           end if;
                        end loop;
                     end Generate_Arguments;

                     procedure Generate_Code_Before_Arguments is
                     begin
                        Put_Line (File, "      P : constant Proxy_Ptr :=");
                        Put_Line (File, "        Wayland.API.Proxy_Marshal_Constructor");
                        Put_Line (File, "          (" & Name & ".all,");
                        Put_Line (File, "           " & Opcode & ",");
                        Put_Line
                          (File,
                           "           " & Xml_Parser_Utils.Adaify_Name (Xml_Parser_Utils.Find_Specified_Interface (Request_Tag)) & "_Interface'Access,");
                        Put (File, "           0");
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
                           Put_Line (File, "      function Convert is new Ada.Unchecked_Conversion (" & From & ", " & To & ");");
                        end Generate_Convert;
                     begin
                        Enum_Types.Iterate (Generate_Convert'Access);
                     end Generate_Conversion_Code_For_Args;

                     V : Wayland_XML.Request_Child_Vectors.Vector;

                     Max_Name_Length : Natural := Name'Length;

                     function Align (Value : String) return String is (SF.Head (Value, Max_Name_Length, ' '));
                  begin
                     if Xml_Parser_Utils.Is_New_Id_Argument_Present (Request_Tag) then
                        Put_Line (File, "");
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
                                          Child = Children (Request_Tag).Last_Element,
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
                              Generate_Arguments (11, V);
                              Generate_Code_After_Arguments;
                           end;
                        else
                           if Xml_Parser_Utils.Number_Of_Args (Request_Tag) > 1 then
                              Max_Name_Length := Natural'Max(11, Name'Length);
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
                              Put (File, "         New_Id");

                              Generate_Arguments (9, V);
                              Put_Line (File, ",");

                              Put_Line (File, "         Interface_V.Name,");
                              Put_Line (File, "         New_Id,");
                              Put_Line (File, "         0);");
                              Put_Line (File, "   end " & Subprogram_Name & ";");
                           else
                              Put_Line (File, "   function " & Subprogram_Name & " (" & Name & " : " & Ptr_Name & ") return Proxy_Ptr is");
                              Generate_Code_Before_Arguments;
                              Generate_Code_After_Arguments;
                           end if;
                        end if;
                     elsif Xml_Parser_Utils.Is_Request_Destructor (Request_Tag) then
                        null; -- Body is generated in Generate_Code_For_Destroy_Subprogram_Implementation
                     else
                        Put_Line (File, "");
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
                           Put (File, "         " & Opcode);

                           Generate_Arguments (9, V);

                           Put_Line (File, ");");
                           Put_Line (File, "   end " & Subprogram_Name & ";");
                        else
                           Put_Line (File, "   procedure " & Subprogram_Name & " (" & Name & " : " & Ptr_Name & ") is");
                           Put_Line (File, "   begin");
                           Put_Line (File, "      Wayland.API.Proxy_Marshal (" & Name & ".all, " & Opcode & ");");
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
               Generate_Code_For_Get_Version_Subprogram_Implementations (Name);

               if Wayland_XML.Name (Interface_Tag) /= "wl_display" then
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

      procedure Generate_Manually_Edited_Code is
         procedure Handle_Interface (Interface_Tag : aliased Wayland_XML.Interface_Tag) is
            Name : constant String
              := Xml_Parser_Utils.Adaify_Name (Wayland_XML.Name (Interface_Tag));
         begin
            if Name /= "Display" then
               Put_Line (File, "   procedure Destroy (Object : in out " & Name & ") is");
               Put_Line (File, "   begin");
               Put_Line (File, "      if Object.Proxy /= null then");
               Put_Line (File, "         Thin." & Name & "_Destroy (Object.Proxy);");
               Put_Line (File, "         Object.Proxy := null;");
               Put_Line (File, "      end if;");
               Put_Line (File, "   end Destroy;");
               Put_Line (File, "");
            end if;

            Put_Line (File, "   function Get_Version (Object : " & Name & ") return Unsigned_32 is");
            Put_Line (File, "     (Thin." & Name & "_Get_Version (Object.Proxy));");
            Put_Line (File, "");

            if Name in "Data_Device" | "Seat" | "Pointer" | "Keyboard" | "Touch" | "Output" then
               Put_Line (File, "   procedure Release (Object : in out " & Name & ") is");
               Put_Line (File, "   begin");
               Put_Line (File, "      if Object.Proxy /= null then");
               Put_Line (File, "         Thin." & Name & "_Release (Object.Proxy);");
               Put_Line (File, "         Object.Proxy := null;");
               Put_Line (File, "      end if;");
               Put_Line (File, "   end Release;");
               Put_Line (File, "");
            end if;
         end Handle_Interface;
      begin
         Put_Line (File, "   subtype int is Interfaces.C.int;");
         Put_Line (File, "");
         Put_Line (File, "   use type int;");
         Put_Line (File, "");
         Put_Line (File, "   use type Thin.Proxy_Ptr;");
         Put_Line (File, "");
         Put_Line (File, "   subtype Registry_Global_Subprogram_Ptr is Thin.Registry_Global_Subprogram_Ptr;");
         Put_Line (File, "");
         Put_Line (File, "   subtype Registry_Global_Remove_Subprogram_Ptr is Thin.Registry_Global_Remove_Subprogram_Ptr;");
         Put_Line (File, "");
         Put_Line (File, "   subtype Registry_Listener_T is Thin.Registry_Listener_T;");
         Put_Line (File, "");
         Put_Line (File, "   subtype Registry_Listener_Ptr is Thin.Registry_Listener_Ptr;");
         Put_Line (File, "");
         Put_Line (File, "   package body Display_Events is");
         Put_Line (File, "");
         Put_Line (File, "      package Conversion is new System.Address_To_Access_Conversions (Display'Class);");
         Put_Line (File, "");
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
         Put_Line (File, "         Error (Conversion.To_Pointer (Data).all, Object_Id, Code, M);");
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
         Put_Line (File, "      Listener : aliased Thin.Display_Listener_T");
         Put_Line (File, "        := (Error     => Internal_Error'Unrestricted_Access,");
         Put_Line (File, "            Delete_Id => Internal_Delete_Id'Unrestricted_Access);");
         Put_Line (File, "");
         Put_Line (File, "      function Subscribe");
         Put_Line (File, "        (Object : aliased in out Display'Class) return Call_Result_Code");
         Put_Line (File, "      is");
         Put_Line (File, "         I : int;");
         Put_Line (File, "      begin");
         Put_Line (File, "         I := Thin.Display_Add_Listener");
         Put_Line (File, "           (Display  => Object.Proxy,");
         Put_Line (File, "            Listener => Listener'Access,");
         Put_Line (File, "            Data     => Conversion.To_Address (Object'Access));");
         Put_Line (File, "         return (if I = 0 then Success else Error);");
         Put_Line (File, "      end Subscribe;");
         Put_Line (File, "");
         Put_Line (File, "   end Display_Events;");
         Put_Line (File, "");
         Put_Line (File, "   package body Registry_Events is");
         Put_Line (File, "");
         Put_Line (File, "      package Conversion is new System.Address_To_Access_Conversions (Registry'Class);");
         Put_Line (File, "");
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
         Put_Line (File, "      Registry_Listener : aliased Protocol.Registry_Listener_T :=");
         Put_Line (File, "        (Global        => Internal_Object_Added'Unrestricted_Access,");
         Put_Line (File, "         Global_Remove => Internal_Object_Removed'Unrestricted_Access);");
         Put_Line (File, "      --  Note: It should be safe to use Unrestricted_Access here since");
         Put_Line (File, "      --  this generic can only be instantiated at library level");
         Put_Line (File, "");
         Put_Line (File, "      function Subscribe");
         Put_Line (File, "        (Object : aliased in out Registry'Class) return Call_Result_Code");
         Put_Line (File, "      is");
         Put_Line (File, "         I : int;");
         Put_Line (File, "      begin");
         Put_Line (File, "         I := Thin.Registry_Add_Listener");
         Put_Line (File, "           (Registry => Object.Proxy,");
         Put_Line (File, "            Listener => Registry_Listener'Access,");
         Put_Line (File, "            Data     => Conversion.To_Address (Object'Access));");
         Put_Line (File, "         return (if I = 0 then Success else Error);");
         Put_Line (File, "      end Subscribe;");
         Put_Line (File, "");
         Put_Line (File, "   end Registry_Events;");
         Put_Line (File, "");
         Put_Line (File, "   package body Callback_Events is");
         Put_Line (File, "");
         Put_Line (File, "      package Conversion is new System.Address_To_Access_Conversions (Callback'Class);");
         Put_Line (File, "");
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
         Put_Line (File, "      Listener : aliased Thin.Callback_Listener_T");
         Put_Line (File, "        := (Done => Internal_Done'Unrestricted_Access);");
         Put_Line (File, "");
         Put_Line (File, "      function Subscribe");
         Put_Line (File, "        (Object : aliased in out Callback'Class) return Call_Result_Code");
         Put_Line (File, "      is");
         Put_Line (File, "         I : int;");
         Put_Line (File, "      begin");
         Put_Line (File, "         I := Thin.Callback_Add_Listener");
         Put_Line (File, "           (Callback => Object.Proxy,");
         Put_Line (File, "            Listener => Listener'Access,");
         Put_Line (File, "            Data     => Conversion.To_Address (Object'Access));");
         Put_Line (File, "         return (if I = 0 then Success else Error);");
         Put_Line (File, "      end Subscribe;");
         Put_Line (File, "");
         Put_Line (File, "   end Callback_Events;");
         Put_Line (File, "");
         Put_Line (File, "   package body Shm_Events is");
         Put_Line (File, "");
         Put_Line (File, "      package Conversion is new System.Address_To_Access_Conversions (Shm'Class);");
         Put_Line (File, "");
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
         Put_Line (File, "      Listener : aliased Thin.Shm_Listener_T");
         Put_Line (File, "        := (Format => Internal_Format'Unrestricted_Access);");
         Put_Line (File, "");
         Put_Line (File, "      function Subscribe");
         Put_Line (File, "        (Object : aliased in out Shm'Class) return Call_Result_Code");
         Put_Line (File, "      is");
         Put_Line (File, "         I : int;");
         Put_Line (File, "      begin");
         Put_Line (File, "         I := Thin.Shm_Add_Listener");
         Put_Line (File, "           (Shm      => Object.Proxy,");
         Put_Line (File, "            Listener => Listener'Access,");
         Put_Line (File, "            Data     => Conversion.To_Address (Object'Access));");
         Put_Line (File, "         return (if I = 0 then Success else Error);");
         Put_Line (File, "      end Subscribe;");
         Put_Line (File, "");
         Put_Line (File, "   end Shm_Events;");
         Put_Line (File, "");
         Put_Line (File, "   package body Buffer_Events is");
         Put_Line (File, "");
         Put_Line (File, "      package Conversion is new System.Address_To_Access_Conversions (Buffer'Class);");
         Put_Line (File, "");
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
         Put_Line (File, "      Listener : aliased Thin.Buffer_Listener_T");
         Put_Line (File, "        := (Release => Internal_Release'Unrestricted_Access);");
         Put_Line (File, "");
         Put_Line (File, "      function Subscribe");
         Put_Line (File, "        (Object : aliased in out Buffer'Class) return Call_Result_Code");
         Put_Line (File, "      is");
         Put_Line (File, "         I : int;");
         Put_Line (File, "      begin");
         Put_Line (File, "         I := Thin.Buffer_Add_Listener");
         Put_Line (File, "           (Buffer   => Object.Proxy,");
         Put_Line (File, "            Listener => Listener'Access,");
         Put_Line (File, "            Data     => Conversion.To_Address (Object'Access));");
         Put_Line (File, "         return (if I = 0 then Success else Error);");
         Put_Line (File, "      end Subscribe;");
         Put_Line (File, "");
         Put_Line (File, "   end Buffer_Events;");
         Put_Line (File, "");
         Put_Line (File, "   package body Data_Offer_Events is");
         Put_Line (File, "");
         Put_Line (File, "      package Conversion is new System.Address_To_Access_Conversions (Data_Offer'Class);");
         Put_Line (File, "");
         Put_Line (File, "      procedure Internal_Offer");
         Put_Line (File, "        (Data       : Void_Ptr;");
         Put_Line (File, "         Data_Offer : Thin.Data_Offer_Ptr;");
         Put_Line (File, "         Mime_Type  : chars_ptr)");
         Put_Line (File, "      with Convention => C;");
         Put_Line (File, "");
         Put_Line (File, "      procedure Internal_Source_Actions");
         Put_Line (File, "        (Data           : Void_Ptr;");
         Put_Line (File, "         Data_Offer     : Thin.Data_Offer_Ptr;");
         Put_Line (File, "         Source_Actions : Unsigned_32)");
         Put_Line (File, "      with Convention => C;");
         Put_Line (File, "");
         Put_Line (File, "      procedure Internal_Action");
         Put_Line (File, "        (Data       : Void_Ptr;");
         Put_Line (File, "         Data_Offer : Thin.Data_Offer_Ptr;");
         Put_Line (File, "         Dnd_Action : Unsigned_32)");
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
         Put_Line (File, "         Source_Actions : Unsigned_32)");
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
         Put_Line (File, "         Dnd_Action : Unsigned_32)");
         Put_Line (File, "      is");
         Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = Data_Offer);");
         Put_Line (File, "      begin");
         Put_Line (File, "         Action (Conversion.To_Pointer (Data).all, Dnd_Action);");
         Put_Line (File, "      end Internal_Action;");
         Put_Line (File, "");
         Put_Line (File, "      Listener : aliased Thin.Data_Offer_Listener_T");
         Put_Line (File, "        := (Offer          => Internal_Offer'Unrestricted_Access,");
         Put_Line (File, "            Source_Actions => Internal_Source_Actions'Unrestricted_Access,");
         Put_Line (File, "            Action         => Internal_Action'Unrestricted_Access);");
         Put_Line (File, "");
         Put_Line (File, "      function Subscribe");
         Put_Line (File, "        (Object : aliased in out Data_Offer'Class) return Call_Result_Code");
         Put_Line (File, "      is");
         Put_Line (File, "         I : int;");
         Put_Line (File, "      begin");
         Put_Line (File, "         I := Thin.Data_Offer_Add_Listener");
         Put_Line (File, "           (Data_Offer => Object.Proxy,");
         Put_Line (File, "            Listener   => Listener'Access,");
         Put_Line (File, "            Data       => Conversion.To_Address (Object'Access));");
         Put_Line (File, "         return (if I = 0 then Success else Error);");
         Put_Line (File, "      end Subscribe;");
         Put_Line (File, "");
         Put_Line (File, "   end Data_Offer_Events;");
         Put_Line (File, "");
         Put_Line (File, "   package body Data_Source_Events is");
         Put_Line (File, "");
         Put_Line (File, "      package Conversion is new System.Address_To_Access_Conversions (Data_Source'Class);");
         Put_Line (File, "");
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
         Put_Line (File, "         Fd          : Integer)");
         Put_Line (File, "      with Convention => C;");
         Put_Line (File, "");
         Put_Line (File, "      procedure Internal_Cancelled");
         Put_Line (File, "        (Data        : Void_Ptr;");
         Put_Line (File, "         Data_Source : Thin.Data_Source_Ptr)");
         Put_Line (File, "      with Convention => C;");
         Put_Line (File, "");
         Put_Line (File, "      procedure Internal_Dnd_Drop_Performed");
         Put_Line (File, "        (Data        : Void_Ptr;");
         Put_Line (File, "         Data_Source : Thin.Data_Source_Ptr)");
         Put_Line (File, "      with Convention => C;");
         Put_Line (File, "");
         Put_Line (File, "      procedure Internal_Dnd_Finished");
         Put_Line (File, "        (Data        : Void_Ptr;");
         Put_Line (File, "         Data_Source : Thin.Data_Source_Ptr)");
         Put_Line (File, "      with Convention => C;");
         Put_Line (File, "");
         Put_Line (File, "      procedure Internal_Action");
         Put_Line (File, "        (Data        : Void_Ptr;");
         Put_Line (File, "         Data_Source : Thin.Data_Source_Ptr;");
         Put_Line (File, "         Dnd_Action  : Unsigned_32)");
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
         Put_Line (File, "         Fd          : Integer)");
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
         Put_Line (File, "      procedure Internal_Dnd_Drop_Performed");
         Put_Line (File, "        (Data        : Void_Ptr;");
         Put_Line (File, "         Data_Source : Thin.Data_Source_Ptr)");
         Put_Line (File, "      is");
         Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = Data_Source);");
         Put_Line (File, "      begin");
         Put_Line (File, "         Dnd_Drop_Performed (Conversion.To_Pointer (Data).all);");
         Put_Line (File, "      end Internal_Dnd_Drop_Performed;");
         Put_Line (File, "");
         Put_Line (File, "      procedure Internal_Dnd_Finished");
         Put_Line (File, "        (Data        : Void_Ptr;");
         Put_Line (File, "         Data_Source : Thin.Data_Source_Ptr)");
         Put_Line (File, "      is");
         Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = Data_Source);");
         Put_Line (File, "      begin");
         Put_Line (File, "         Dnd_Drop_Performed (Conversion.To_Pointer (Data).all);");
         Put_Line (File, "      end Internal_Dnd_Finished;");
         Put_Line (File, "");
         Put_Line (File, "      procedure Internal_Action");
         Put_Line (File, "        (Data        : Void_Ptr;");
         Put_Line (File, "         Data_Source : Thin.Data_Source_Ptr;");
         Put_Line (File, "         Dnd_Action  : Unsigned_32)");
         Put_Line (File, "      is");
         Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = Data_Source);");
         Put_Line (File, "      begin");
         Put_Line (File, "         Action (Conversion.To_Pointer (Data).all, Dnd_Action);");
         Put_Line (File, "      end Internal_Action;");
         Put_Line (File, "");
         Put_Line (File, "      Listener : aliased Thin.Data_Source_Listener_T");
         Put_Line (File, "        := (Target             => Internal_Target'Unrestricted_Access,");
         Put_Line (File, "            Send               => Internal_Send'Unrestricted_Access,");
         Put_Line (File, "            Cancelled          => Internal_Cancelled'Unrestricted_Access,");
         Put_Line (File, "            Dnd_Drop_Performed =>");
         Put_Line (File, "              Internal_Dnd_Drop_Performed'Unrestricted_Access,");
         Put_Line (File, "            Dnd_Finished       => Internal_Dnd_Finished'Unrestricted_Access,");
         Put_Line (File, "            Action             => Internal_Action'Unrestricted_Access);");
         Put_Line (File, "");
         Put_Line (File, "      function Subscribe");
         Put_Line (File, "        (Object : aliased in out Data_Source'Class) return Call_Result_Code");
         Put_Line (File, "      is");
         Put_Line (File, "         I : int;");
         Put_Line (File, "      begin");
         Put_Line (File, "         I := Thin.Data_Source_Add_Listener");
         Put_Line (File, "           (Data_Source => Object.Proxy,");
         Put_Line (File, "            Listener    => Listener'Access,");
         Put_Line (File, "            Data        => Conversion.To_Address (Object'Access));");
         Put_Line (File, "         return (if I = 0 then Success else Error);");
         Put_Line (File, "      end Subscribe;");
         Put_Line (File, "");
         Put_Line (File, "   end Data_Source_Events;");
         Put_Line (File, "");
         Put_Line (File, "   package body Data_Device_Events is");
         Put_Line (File, "");
         Put_Line (File, "      package Conversion is new System.Address_To_Access_Conversions (Data_Device'Class);");
         Put_Line (File, "");
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
         Put_Line (File, "         S : constant Protocol.Surface     := (Proxy => Surface);");
         Put_Line (File, "         Offer : constant Protocol.Data_Offer := (Proxy => Id);");
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
         Put_Line (File, "         Offer : constant Protocol.Data_Offer := (Proxy => Id);");
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
         Put_Line (File, "      function Subscribe");
         Put_Line (File, "        (Object : aliased in out Data_Device'Class) return Call_Result_Code");
         Put_Line (File, "      is");
         Put_Line (File, "         I : int;");
         Put_Line (File, "      begin");
         Put_Line (File, "         I := Thin.Data_Device_Add_Listener");
         Put_Line (File, "           (Data_Device => Object.Proxy,");
         Put_Line (File, "            Listener    => Listener'Access,");
         Put_Line (File, "            Data        => Conversion.To_Address (Object'Access));");
         Put_Line (File, "         return (if I = 0 then Success else Error);");
         Put_Line (File, "      end Subscribe;");
         Put_Line (File, "");
         Put_Line (File, "   end Data_Device_Events;");
         Put_Line (File, "");
         Put_Line (File, "   package body Surface_Events is");
         Put_Line (File, "");
         Put_Line (File, "      package Conversion is new System.Address_To_Access_Conversions (Surface'Class);");
         Put_Line (File, "");
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
         Put_Line (File, "         O : constant Protocol.Output := (Proxy => Output);");
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
         Put_Line (File, "         O : constant Protocol.Output := (Proxy => Output);");
         Put_Line (File, "      begin");
         Put_Line (File, "         Leave (Conversion.To_Pointer (Data).all, O);");
         Put_Line (File, "      end Internal_Leave;");
         Put_Line (File, "");
         Put_Line (File, "      Listener : aliased Thin.Surface_Listener_T");
         Put_Line (File, "        := (Enter => Internal_Enter'Unrestricted_Access,");
         Put_Line (File, "            Leave => Internal_Leave'Unrestricted_Access);");
         Put_Line (File, "");
         Put_Line (File, "      function Subscribe");
         Put_Line (File, "        (Object : aliased in out Surface'Class) return Call_Result_Code");
         Put_Line (File, "      is");
         Put_Line (File, "         I : int;");
         Put_Line (File, "      begin");
         Put_Line (File, "         I := Thin.Surface_Add_Listener");
         Put_Line (File, "           (Surface  => Object.Proxy,");
         Put_Line (File, "            Listener => Listener'Access,");
         Put_Line (File, "            Data     => Conversion.To_Address (Object'Access));");
         Put_Line (File, "         return (if I = 0 then Success else Error);");
         Put_Line (File, "      end Subscribe;");
         Put_Line (File, "");
         Put_Line (File, "   end Surface_Events;");
         Put_Line (File, "");
         Put_Line (File, "   package body Seat_Events is");
         Put_Line (File, "");
         Put_Line (File, "      package Conversion is new System.Address_To_Access_Conversions (Seat'Class);");
         Put_Line (File, "");
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
         Put_Line (File, "      Seat_Listener : aliased Thin.Seat_Listener_T :=");
         Put_Line (File, "        (Capabilities => Internal_Seat_Capabilities'Unrestricted_Access,");
         Put_Line (File, "         Name         => Internal_Seat_Name'Unrestricted_Access);");
         Put_Line (File, "");
         Put_Line (File, "      function Subscribe");
         Put_Line (File, "        (Object : aliased in out Seat'Class) return Call_Result_Code");
         Put_Line (File, "      is");
         Put_Line (File, "         I : int;");
         Put_Line (File, "      begin");
         Put_Line (File, "         I := Thin.Seat_Add_Listener");
         Put_Line (File, "           (Seat     => Object.Proxy,");
         Put_Line (File, "            Listener => Seat_Listener'Access,");
         Put_Line (File, "            Data     => Conversion.To_Address (Object'Access));");
         Put_Line (File, "         return (if I = 0 then Success else Error);");
         Put_Line (File, "      end Subscribe;");
         Put_Line (File, "");
         Put_Line (File, "   end Seat_Events;");
         Put_Line (File, "");
         Put_Line (File, "   package body Pointer_Events is");
         Put_Line (File, "");
         Put_Line (File, "      package Conversion is new System.Address_To_Access_Conversions (Pointer'Class);");
         Put_Line (File, "");
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
         Put_Line (File, "         S : constant Protocol.Surface := (Proxy => Surface);");
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
         Put_Line (File, "         S : constant Protocol.Surface := (Proxy => Surface);");
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
         Put_Line (File, "      Pointer_Listener : aliased Thin.Pointer_Listener_T :=");
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
         Put_Line (File, "      function Subscribe");
         Put_Line (File, "        (Object : aliased in out Pointer'Class) return Call_Result_Code");
         Put_Line (File, "      is");
         Put_Line (File, "         I : int;");
         Put_Line (File, "      begin");
         Put_Line (File, "         I := Thin.Pointer_Add_Listener");
         Put_Line (File, "           (Pointer  => Object.Proxy,");
         Put_Line (File, "            Listener => Pointer_Listener'Access,");
         Put_Line (File, "            Data     => Conversion.To_Address (Object'Access));");
         Put_Line (File, "         return (if I = 0 then Success else Error);");
         Put_Line (File, "      end Subscribe;");
         Put_Line (File, "");
         Put_Line (File, "   end Pointer_Events;");
         Put_Line (File, "");
         Put_Line (File, "   package body Keyboard_Events is");
         Put_Line (File, "");
         Put_Line (File, "      package Conversion is new System.Address_To_Access_Conversions (Keyboard'Class);");
         Put_Line (File, "");
         Put_Line (File, "      procedure Internal_Keymap");
         Put_Line (File, "        (Data     : Void_Ptr;");
         Put_Line (File, "         Keyboard : Thin.Keyboard_Ptr;");
         Put_Line (File, "         Format   : Keyboard_Keymap_Format;");
         Put_Line (File, "         Fd       : Integer;");
         Put_Line (File, "         Size     : Unsigned_32)");
         Put_Line (File, "      with Convention => C;");
         Put_Line (File, "");
         Put_Line (File, "      procedure Internal_Enter");
         Put_Line (File, "        (Data     : Void_Ptr;");
         Put_Line (File, "         Keyboard : Thin.Keyboard_Ptr;");
         Put_Line (File, "         Serial   : Unsigned_32;");
         Put_Line (File, "         Surface  : Thin.Surface_Ptr;");
         Put_Line (File, "         Keys     : Wayland_Array_T)");
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
         Put_Line (File, "         Fd       : Integer;");
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
         Put_Line (File, "         Keys     : Wayland_Array_T)");
         Put_Line (File, "      is");
         Put_Line (File, "         pragma Assert (Conversion.To_Pointer (Data).Proxy = Keyboard);");
         Put_Line (File, "");
         Put_Line (File, "         S : constant Protocol.Surface  := (Proxy => Surface);");
         Put_Line (File, "      begin");
         Put_Line (File, "         Enter (Conversion.To_Pointer (Data).all, Serial, S, Keys);");
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
         Put_Line (File, "         S : constant Protocol.Surface  := (Proxy => Surface);");
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
         Put_Line (File, "      Listener : aliased Thin.Keyboard_Listener_T");
         Put_Line (File, "        := (Keymap      => Internal_Keymap'Unrestricted_Access,");
         Put_Line (File, "            Enter       => Internal_Enter'Unrestricted_Access,");
         Put_Line (File, "            Leave       => Internal_Leave'Unrestricted_Access,");
         Put_Line (File, "            Key         => Internal_Key'Unrestricted_Access,");
         Put_Line (File, "            Modifiers   => Internal_Modifiers'Unrestricted_Access,");
         Put_Line (File, "            Repeat_Info => Internal_Repeat_Info'Unrestricted_Access);");
         Put_Line (File, "");
         Put_Line (File, "      function Subscribe");
         Put_Line (File, "        (Object : aliased in out Keyboard'Class) return Call_Result_Code");
         Put_Line (File, "      is");
         Put_Line (File, "         I : int;");
         Put_Line (File, "      begin");
         Put_Line (File, "         I := Thin.Keyboard_Add_Listener");
         Put_Line (File, "           (Keyboard => Object.Proxy,");
         Put_Line (File, "            Listener => Listener'Access,");
         Put_Line (File, "            Data     => Conversion.To_Address (Object'Access));");
         Put_Line (File, "         return (if I = 0 then Success else Error);");
         Put_Line (File, "      end Subscribe;");
         Put_Line (File, "");
         Put_Line (File, "   end Keyboard_Events;");
         Put_Line (File, "");
         Put_Line (File, "   package body Touch_Events is");
         Put_Line (File, "");
         Put_Line (File, "      package Conversion is new System.Address_To_Access_Conversions (Touch'Class);");
         Put_Line (File, "");
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
         Put_Line (File, "         S : constant Protocol.Surface := (Proxy => Surface);");
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
         Put_Line (File, "      Listener : aliased Thin.Touch_Listener_T");
         Put_Line (File, "        := (Down        => Internal_Down'Unrestricted_Access,");
         Put_Line (File, "            Up          => Internal_Up'Unrestricted_Access,");
         Put_Line (File, "            Motion      => Internal_Motion'Unrestricted_Access,");
         Put_Line (File, "            Frame       => Internal_Frame'Unrestricted_Access,");
         Put_Line (File, "            Cancel      => Internal_Cancel'Unrestricted_Access,");
         Put_Line (File, "            Shape       => Internal_Shape'Unrestricted_Access,");
         Put_Line (File, "            Orientation => Internal_Orientation'Unrestricted_Access);");
         Put_Line (File, "");
         Put_Line (File, "      function Subscribe");
         Put_Line (File, "        (Object : aliased in out Touch'Class) return Call_Result_Code");
         Put_Line (File, "      is");
         Put_Line (File, "         I : int;");
         Put_Line (File, "      begin");
         Put_Line (File, "         I := Thin.Touch_Add_Listener");
         Put_Line (File, "           (Touch    => Object.Proxy,");
         Put_Line (File, "            Listener => Listener'Access,");
         Put_Line (File, "            Data     => Conversion.To_Address (Object'Access));");
         Put_Line (File, "         return (if I = 0 then Success else Error);");
         Put_Line (File, "      end Subscribe;");
         Put_Line (File, "");
         Put_Line (File, "   end Touch_Events;");
         Put_Line (File, "");
         Put_Line (File, "   package body Output_Events is");
         Put_Line (File, "");
         Put_Line (File, "      package Conversion is new System.Address_To_Access_Conversions (Output'Class);");
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
         Put_Line (File, "      function Subscribe");
         Put_Line (File, "        (Object : aliased in out Output'Class) return Call_Result_Code");
         Put_Line (File, "      is");
         Put_Line (File, "         I : int;");
         Put_Line (File, "      begin");
         Put_Line (File, "         I := Thin.Output_Add_Listener");
         Put_Line (File, "           (Output   => Object.Proxy,");
         Put_Line (File, "            Listener => Listener'Access,");
         Put_Line (File, "            Data     => Conversion.To_Address (Object'Access));");
         Put_Line (File, "         return (if I = 0 then Success else Error);");
         Put_Line (File, "      end Subscribe;");
         Put_Line (File, "");
         Put_Line (File, "   end Output_Events;");
         Put_Line (File, "");

         Iterate_Over_Interfaces (Handle_Interface'Access);

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
         Put_Line (File, "      Timeout : Integer) return Check_For_Events_Status");
         Put_Line (File, "   is");
         Put_Line (File, "      I : constant Integer :=");
         Put_Line (File, "        C_Binding.Linux.Poll_File_Descriptor_Until_Timeout");
         Put_Line (File, "          (Wayland.API.Display_Get_File_Descriptor (Object.Proxy), Timeout);");
         Put_Line (File, "   begin");
         Put_Line (File, "      case I is");
         Put_Line (File, "         when 1..Integer'Last   => return Events_Need_Processing;");
         Put_Line (File, "         when 0                 => return No_Events;");
         Put_Line (File, "         when Integer'First..-1 => return Error;");
         Put_Line (File, "      end case;");
         Put_Line (File, "   end Check_For_Events;");
         Put_Line (File, "");
         Put_Line (File, "   procedure Get_Registry (Object   : Display;");
         Put_Line (File, "                           Registry : in out Protocol.Registry'Class) is");
         Put_Line (File, "   begin");
         Put_Line (File, "      Registry.Proxy := Thin.Display_Get_Registry (Object.Proxy);");
         Put_Line (File, "   end Get_Registry;");
         Put_Line (File, "");
         Put_Line (File, "   function Dispatch (Object : Display) return Integer is");
         Put_Line (File, "   begin");
         Put_Line (File, "      return Integer (Wayland.API.Display_Dispatch (Object.Proxy));");
         Put_Line (File, "   end Dispatch;");
         Put_Line (File, "");
         Put_Line (File, "   procedure Dispatch (Object : Display) is");
         Put_Line (File, "      I : Integer;");
         Put_Line (File, "      pragma Unreferenced (I);");
         Put_Line (File, "   begin");
         Put_Line (File, "      I := Object.Dispatch;");
         Put_Line (File, "   end Dispatch;");
         Put_Line (File, "");
         Put_Line (File, "   function Dispatch_Pending (Object : Display) return Integer is");
         Put_Line (File, "   begin");
         Put_Line (File, "      return Wayland.API.Display_Dispatch_Pending (Object.Proxy);");
         Put_Line (File, "   end Dispatch_Pending;");
         Put_Line (File, "");
         Put_Line (File, "   function Prepare_Read (Object : Display) return Integer is");
         Put_Line (File, "   begin");
         Put_Line (File, "      return Wayland.API.Display_Prepare_Read (Object.Proxy);");
         Put_Line (File, "   end Prepare_Read;");
         Put_Line (File, "");
         Put_Line (File, "   procedure Cancel_Read (Object : Display) is");
         Put_Line (File, "   begin");
         Put_Line (File, "      Wayland.API.Display_Cancel_Read (Object.Proxy);");
         Put_Line (File, "   end Cancel_Read;");
         Put_Line (File, "");
         Put_Line (File, "   function Read_Events (Object : Display) return Call_Result_Code is");
         Put_Line (File, "      I : constant Integer");
         Put_Line (File, "        := Wayland.API.Display_Read_Events (Object.Proxy);");
         Put_Line (File, "   begin");
         Put_Line (File, "      return (if I = 0 then Success else Error);");
         Put_Line (File, "   end Read_Events;");
         Put_Line (File, "");
         Put_Line (File, "   function Roundtrip (Object : Display) return Integer is");
         Put_Line (File, "   begin");
         Put_Line (File, "      return Integer (Wayland.API.Display_Roundtrip (Object.Proxy));");
         Put_Line (File, "   end Roundtrip;");
         Put_Line (File, "");
         Put_Line (File, "   procedure Roundtrip (Object : Display) is");
         Put_Line (File, "      I : Integer;");
         Put_Line (File, "      pragma Unreferenced (I);");
         Put_Line (File, "   begin");
         Put_Line (File, "      I := Object.Roundtrip;");
         Put_Line (File, "   end Roundtrip;");
         Put_Line (File, "");
         Put_Line (File, "   procedure Bind (Object   : in out Compositor;");
         Put_Line (File, "                   Registry : Protocol.Registry'Class;");
         Put_Line (File, "                   Id       : Unsigned_32;");
         Put_Line (File, "                   Version  : Unsigned_32)");
         Put_Line (File, "   is");
         Put_Line (File, "      Proxy : constant Thin.Proxy_Ptr :=");
         Put_Line (File, "        Thin.Registry_Bind");
         Put_Line (File, "          (Registry    => Registry.Proxy,");
         Put_Line (File, "           Name        => Id,");
         Put_Line (File, "           Interface_V => Thin.Compositor_Interface'Access,");
         Put_Line (File, "           New_Id      => Version);");
         Put_Line (File, "");
         Put_Line (File, "   begin");
         Put_Line (File, "      if Proxy /= null then");
         Put_Line (File, "         Object.Proxy := Proxy.all'Access;");
         Put_Line (File, "      end if;");
         Put_Line (File, "   end Bind;");
         Put_Line (File, "");
         Put_Line (File, "   procedure Create_Surface (Object  : Compositor;");
         Put_Line (File, "                             Surface : in out Protocol.Surface'Class) is");
         Put_Line (File, "   begin");
         Put_Line (File, "      Surface.Proxy := Thin.Compositor_Create_Surface (Object.Proxy);");
         Put_Line (File, "   end Create_Surface;");
         Put_Line (File, "");
         Put_Line (File, "   procedure Create_Region (Object : Compositor;");
         Put_Line (File, "                            Region : in out Protocol.Region'Class) is");
         Put_Line (File, "   begin");
         Put_Line (File, "      Region.Proxy := Thin.Compositor_Create_Region (Object.Proxy);");
         Put_Line (File, "   end Create_Region;");
         Put_Line (File, "");
         Put_Line (File, "   procedure Bind (Object   : in out Seat;");
         Put_Line (File, "                   Registry : Protocol.Registry'Class;");
         Put_Line (File, "                   Id       : Unsigned_32;");
         Put_Line (File, "                   Version  : Unsigned_32)");
         Put_Line (File, "   is");
         Put_Line (File, "      Proxy : constant Thin.Proxy_Ptr :=");
         Put_Line (File, "        Thin.Registry_Bind");
         Put_Line (File, "          (Registry    => Registry.Proxy,");
         Put_Line (File, "           Name        => Id,");
         Put_Line (File, "           Interface_V => Thin.Seat_Interface'Access,");
         Put_Line (File, "           New_Id      => Version);");
         Put_Line (File, "");
         Put_Line (File, "   begin");
         Put_Line (File, "      if Proxy /= null then");
         Put_Line (File, "         Object.Proxy := Proxy.all'Access;");
         Put_Line (File, "      end if;");
         Put_Line (File, "   end Bind;");
         Put_Line (File, "");
         Put_Line (File, "   procedure Get_Pointer (Object  : Seat;");
         Put_Line (File, "                          Pointer : in out Protocol.Pointer'Class) is");
         Put_Line (File, "   begin");
         Put_Line (File, "      Pointer.Proxy := Thin.Seat_Get_Pointer (Object.Proxy);");
         Put_Line (File, "   end Get_Pointer;");
         Put_Line (File, "");
         Put_Line (File, "   procedure Get_Keyboard (Object   : Seat;");
         Put_Line (File, "                           Keyboard : in out Protocol.Keyboard'Class) is");
         Put_Line (File, "   begin");
         Put_Line (File, "      Keyboard.Proxy := Thin.Seat_Get_Keyboard (Object.Proxy);");
         Put_Line (File, "   end Get_Keyboard;");
         Put_Line (File, "");
         Put_Line (File, "   procedure Get_Touch (Object : Seat;");
         Put_Line (File, "                        Touch  : in out Protocol.Touch'Class) is");
         Put_Line (File, "   begin");
         Put_Line (File, "      Touch.Proxy := Thin.Seat_Get_Touch (Object.Proxy);");
         Put_Line (File, "   end Get_Touch;");
         Put_Line (File, "");
         Put_Line (File, "   procedure Bind (Object   : in out Shm;");
         Put_Line (File, "                   Registry : Protocol.Registry'Class;");
         Put_Line (File, "                   Id       : Unsigned_32;");
         Put_Line (File, "                   Version  : Unsigned_32)");
         Put_Line (File, "   is");
         Put_Line (File, "      Proxy : constant Thin.Proxy_Ptr :=");
         Put_Line (File, "        Thin.Registry_Bind");
         Put_Line (File, "          (Registry    => Registry.Proxy,");
         Put_Line (File, "           Name        => Id,");
         Put_Line (File, "           Interface_V => Thin.Shm_Interface'Access,");
         Put_Line (File, "           New_Id      => Version);");
         Put_Line (File, "");
         Put_Line (File, "   begin");
         Put_Line (File, "      if Proxy /= null then");
         Put_Line (File, "         Object.Proxy := Proxy.all'Access;");
         Put_Line (File, "      end if;");
         Put_Line (File, "   end Bind;");
         Put_Line (File, "");
         Put_Line (File, "   procedure Create_Pool");
         Put_Line (File, "     (Object          : Shm;");
         Put_Line (File, "      File_Descriptor : C_Binding.Linux.Files.File;");
         Put_Line (File, "      Size            : Positive;");
         Put_Line (File, "      Pool            : in out Protocol.Shm_Pool'Class) is");
         Put_Line (File, "   begin");
         Put_Line (File, "      Pool.Proxy := Thin.Shm_Create_Pool");
         Put_Line (File, "        (Object.Proxy,");
         Put_Line (File, "         C_Binding.Linux.Files.File_Descriptor (File_Descriptor),");
         Put_Line (File, "         Size);");
         Put_Line (File, "   end Create_Pool;");
         Put_Line (File, "");
         Put_Line (File, "   procedure Create_Buffer (Object : Shm_Pool;");
         Put_Line (File, "                            Offset : Natural;");
         Put_Line (File, "                            Width  : Natural;");
         Put_Line (File, "                            Height : Natural;");
         Put_Line (File, "                            Stride : Natural;");
         Put_Line (File, "                            Format : Shm_Format;");
         Put_Line (File, "                            Buffer : in out Protocol.Buffer'Class) is");
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
         Put_Line (File, "                      File_Descriptor : Integer)");
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
         Put_Line (File, "                          Dnd_Actions      : Unsigned_32;");
         Put_Line (File, "                          Preferred_Action : Unsigned_32) is");
         Put_Line (File, "   begin");
         Put_Line (File, "      Thin.Data_Offer_Set_Actions");
         Put_Line (File, "        (Object.Proxy, Dnd_Actions, Preferred_Action);");
         Put_Line (File, "   end Set_Actions;");
         Put_Line (File, "");
         Put_Line (File, "   procedure Attach (Object : Surface;");
         Put_Line (File, "                     Buffer : Protocol.Buffer'Class;");
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
         Put_Line (File, "                                Region : Protocol.Region'Class) is");
         Put_Line (File, "   begin");
         Put_Line (File, "      Thin.Surface_Set_Opaque_Region (Object.Proxy, Region.Proxy);");
         Put_Line (File, "   end Set_Opaque_Region;");
         Put_Line (File, "");
         Put_Line (File, "   procedure Set_Input_Region (Object : Surface;");
         Put_Line (File, "                               Region : Protocol.Region'Class) is");
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
         Put_Line (File, "      return Callback : Protocol.Callback do");
         Put_Line (File, "         Callback.Proxy := Thin.Display_Sync (Object.Proxy);");
         Put_Line (File, "      end return;");
         Put_Line (File, "   end Sync;");
         Put_Line (File, "");
         Put_Line (File, "   procedure Set_Cursor (Object    : Pointer;");
         Put_Line (File, "                         Serial    : Unsigned_32;");
         Put_Line (File, "                         Surface   : Protocol.Surface'Class;");
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
         Put_Line (File, "     (Object     : Protocol.Subcompositor;");
         Put_Line (File, "      Surface    : Protocol.Surface'Class;");
         Put_Line (File, "      Parent     : Protocol.Surface'Class;");
         Put_Line (File, "      Subsurface : in out Protocol.Subsurface'Class) is");
         Put_Line (File, "   begin");
         Put_Line (File, "      Subsurface.Proxy :=");
         Put_Line (File, "        Thin.Subcompositor_Get_Subsurface");
         Put_Line (File, "          (Object.Proxy,");
         Put_Line (File, "           Surface.Proxy,");
         Put_Line (File, "           Parent.Proxy);");
         Put_Line (File, "   end Get_Subsurface;");
         Put_Line (File, "");
         Put_Line (File, "   procedure Set_Position (Object : Protocol.Subsurface;");
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
         Put_Line (File, "                    Mime_Type : String) is");
         Put_Line (File, "   begin");
         Put_Line (File, "      Wayland.API.Proxy_Marshal");
         Put_Line (File, "        (Wayland.API.Proxy (Object.Proxy.all),");
         Put_Line (File, "         Constants.Data_Source_Offer,");
         Put_Line (File, "         +Mime_Type);");
         Put_Line (File, "   end Offer;");
         Put_Line (File, "");
         Put_Line (File, "   procedure Set_Actions (Object      : Data_Source;");
         Put_Line (File, "                          Dnd_Actions : Unsigned_32) is");
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
         Put_Line (File, "                              Seat   : Protocol.Seat'Class;");
         Put_Line (File, "                              Device : in out Data_Device'Class) is");
         Put_Line (File, "   begin");
         Put_Line (File, "      Device.Proxy := Thin.Data_Device_Manager_Get_Data_Device");
         Put_Line (File, "        (Object.Proxy, Seat.Proxy);");
         Put_Line (File, "   end Get_Data_Device;");
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
