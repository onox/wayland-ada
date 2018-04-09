with Ada.Text_IO;
with Ada.Exceptions;
with Aida.Deepend_XML_DOM_Parser;
with Aida.Text_IO;
with Aida.Subprogram_Call_Result;
with Dynamic_Pools;
with Ada.Directories;
with Aida.Sequential_Stream_IO;
with Aida.UTF8_Code_Point;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Containers;

with Wayland_XML;
with Xml_Parser_Utils;

-- Pretty print this file with "gnatpp -M140"
procedure XML_Parser is

   package Wx renames Wayland_XML;

   subtype File_Type is Ada.Text_IO.File_Type;

   subtype Node_Ptr is Aida.Deepend_XML_DOM_Parser.Node_Ptr;

   function Trim (Source : String) return String is
                  (Ada.Strings.Fixed.Trim (Source, Ada.Strings.Both));

   procedure Put (File : File_Type;
                  Item : String) renames Ada.Text_IO.Put;

   procedure Put_Line (File : File_Type;
                       Item : String) renames Ada.Text_IO.Put_Line;

   procedure New_Line (
                       File    : File_Type;
                       Spacing : Ada.Text_IO.Positive_Count := 1
                      )
                       renames Ada.Text_IO.New_Line;

   procedure Put_Line (Item : String) renames Ada.Text_IO.Put_Line;

   use type Ada.Containers.Count_Type;

   use type Wx.Event_Child;
   use type Wx.Request_Child;

   use all type Aida.Deepend_XML_DOM_Parser.Node_Kind_Id_T;

   use all type Ada.Strings.Unbounded.Unbounded_String;

   use all type Aida.UTF8_Code_Point.T;

   use all type Wx.Protocol_Child_Kind_Id;
   use all type Wx.Interface_Child_Kind_Id;
   use all type Wx.Enum_Child_Kind_Id;
   use all type Wx.Event_Child_Kind_Id;
   use all type Wx.Arg_Type_Attribute;
   use all type Wx.Request_Child_Kind_Id;

   XML_Exception : exception;

   Default_Subpool : Dynamic_Pools.Dynamic_Pool renames
     Aida.Deepend_XML_DOM_Parser.Default_Subpool;

   File_Name : constant String := "wayland.xml";

   Allocation_Block_Size : constant := 1_000_000;

   Scoped_Subpool : constant Dynamic_Pools.Scoped_Subpool
     := Dynamic_Pools.Create_Subpool (Default_Subpool, Allocation_Block_Size);

   Subpool : Dynamic_Pools.Subpool_Handle := Scoped_Subpool.Handle;

   package Utils renames Xml_Parser_Utils;

   procedure Allocate_Space_For_Wayland_XML_Contents;

   procedure Check_Wayland_XML_File_Exists is
   begin
      if Ada.Directories.Exists (File_Name) then
         Allocate_Space_For_Wayland_XML_Contents;
      else
         Put_Line ("Could not find " & File_Name & "!");
      end if;
   end Check_Wayland_XML_File_Exists;

   File_Size : Natural;

   File_Contents : Aida.Deepend_XML_DOM_Parser.String_Ptr;

   procedure Read_Contents_Of_Wayland_XML;

   procedure Allocate_Space_For_Wayland_XML_Contents is
   begin
      File_Size := Natural (Ada.Directories.Size (File_Name));

      if File_Size > 4 then
         File_Contents := new (Subpool) String (1 .. File_Size);
         Read_Contents_Of_Wayland_XML;
      else
         Put_Line ("File " & File_Name & " is too small!");
      end if;
   end Allocate_Space_For_Wayland_XML_Contents;

   pragma Unmodified (File_Size);
   pragma Unmodified (File_Contents);

   procedure Parse_Contents;

   procedure Read_Contents_Of_Wayland_XML is
   begin
      declare
         File : Aida.Sequential_Stream_IO.File_Type;
         SE   : Aida.Sequential_Stream_IO.Stream_Element;
      begin
         Aida.Sequential_Stream_IO.Open (File,
                                         Aida.Sequential_Stream_IO.In_File,
                                         File_Name);

         for I in File_Contents.all'First .. File_Contents.all'Last loop
            Aida.Sequential_Stream_IO.Read (File, SE);
            File_Contents (I) := Character'Val (SE);
         end loop;

         Aida.Sequential_Stream_IO.Close (File);
      end;

      Parse_Contents;
   end Read_Contents_Of_Wayland_XML;

   Root_Node : Aida.Deepend_XML_DOM_Parser.Node_Ptr;

   procedure Identify_Protocol_Tag;

   procedure Parse_Contents is
      Call_Result : Aida.Subprogram_Call_Result.T;
   begin
      declare
         Parser : Aida.Deepend_XML_DOM_Parser.DOM_Parser_T;
      begin
         Parser.Parse (Subpool, File_Contents.all, Call_Result, Root_Node);
      end;

      if Call_Result.Has_Failed then
         Aida.Text_IO.Put_Line (Call_Result.Message);
      else
         Identify_Protocol_Tag;
      end if;
   end Parse_Contents;

   pragma Unmodified (Root_Node);

   Protocol_Tag : Wx.Protocol_Tag_Ptr;

   procedure Identify_Protocol_Children;

   procedure Identify_Protocol_Tag is
   begin
      if Root_Node.Id = XML_Tag and then Root_Node.Tag.Name = "protocol" then
         Protocol_Tag := new (Subpool) Wx.Protocol_Tag;
         if
           Root_Node.Tag.Attributes.Length = 1 and then
           Root_Node.Tag.Attributes.Element (1).all.Name = "name"
         then
            Protocol_Tag.Set_Name
              (Root_Node.Tag.Attributes.Element (1).all.Value, Subpool);
            Identify_Protocol_Children;
         else
            Put_Line ("<protocol> node does not have name attribute?");
         end if;
      else
         Put_Line ("Root node is not <protocol> ???");
      end if;
   end Identify_Protocol_Tag;

   pragma Unmodified (Protocol_Tag);

   procedure Create_Wayland_Spec_File;

   procedure Identify_Protocol_Children is

      function Identify_Copyright (Node : not null Node_Ptr)
                                   return not null Wx.Copyright_Ptr
      is
         Copyright_Tag : constant not null Wx.Copyright_Ptr
           := new (Subpool) Wx.Copyright_Tag;
      begin
         if Node.Tag.Child_Nodes.Length = 1 then
            if Node.Tag.Child_Nodes.Element (1).Id = XML_Text then
               Copyright_Tag.Set_Text
                 (Trim (Node.Tag.Child_Nodes.Element (1).Text), Subpool);
            else
               raise XML_Exception;
            end if;
         else
            raise XML_Exception;
         end if;
         return Copyright_Tag;
      end Identify_Copyright;

      function Identify_Description
        (Node : not null Node_Ptr) return not null Wx.Description_Tag_Ptr
      is
         Description_Tag : constant not null Wx.Description_Tag_Ptr
           := new (Subpool) Wx.Description_Tag;
      begin
         if Node.Tag.Attributes.Length = 1 then
            if Node.Tag.Attributes.Element (1).Name = "summary" then
               Description_Tag.Set_Summary
                 (Node.Tag.Attributes.Element (1).Value, Subpool);
            else
               raise XML_Exception;
            end if;
         else
            raise XML_Exception;
         end if;

         if Node.Tag.Child_Nodes.Length = 1 then
            if Node.Tag.Child_Nodes.Element (1).Id = XML_Text then
               Description_Tag.Set_Text
                 (Trim (Node.Tag.Child_Nodes.Element (1).Text), Subpool);
            else
               raise XML_Exception;
            end if;
         elsif Node.Tag.Child_Nodes.Length > 1 then
            raise XML_Exception;
         end if;

         return Description_Tag;
      end Identify_Description;

      function Identify_Arg
        (Node : not null Node_Ptr) return not null Wx.Arg_Tag_Ptr
      is
         Arg_Tag : constant not null Wx.Arg_Tag_Ptr := new (Subpool) Wx.Arg_Tag;
      begin
         for A of Node.Tag.Attributes loop
            if A.Name = "name" then
               Arg_Tag.Set_Name (A.Value, Subpool);
            elsif A.Name = "type" then
               Arg_Tag.Set_Type_Attribute (A.Value);
            elsif A.Name = "summary" then
               Arg_Tag.Set_Summary (A.Value, Subpool);
            elsif A.Name = "interface" then
               Arg_Tag.Set_Interface_Attribute (A.Value, Subpool);
            elsif A.Name = "allow-null" then
               if A.Value = "true" then
                  Arg_Tag.Set_Allow_Null (True);
               elsif A.Value = "false" then
                  Arg_Tag.Set_Allow_Null (False);
               else
                  raise XML_Exception;
               end if;
            elsif A.Name = "enum" then
               Arg_Tag.Set_Enum (A.Value, Subpool);
            else
               raise XML_Exception;
            end if;
         end loop;

         return Arg_Tag;
      end Identify_Arg;

      function Identify_Request
        (Node : not null Node_Ptr) return not null Wx.Request_Tag_Ptr
      is
         Request_Tag : constant not null Wx.Request_Tag_Ptr
           := new (Subpool) Wx.Request_Tag;
      begin
         for A of Node.Tag.Attributes loop
            if A.Name = "name" then
               Request_Tag.Set_Name (A.Value, Subpool);
            elsif A.Name = "type" then
               Request_Tag.Set_Type_Attribute (A.Value, Subpool);
            elsif A.Name = "since" then
               declare
                  Value      : Aida.Int32_T;
                  Has_Failed : Boolean;
               begin
                  Aida.String.To_Int32 (A.Value, Value, Has_Failed);

                  if Has_Failed then
                     raise XML_Exception;
                  else
                     Request_Tag.Set_Since (Wx.Version_Number (Value));
                  end if;
               end;
            else
               raise XML_Exception;
            end if;
         end loop;

         for Child of Node.Tag.Child_Nodes loop
            if Child.Id = XML_Tag then
               if Child.Tag.Name = "description" then
                  Request_Tag.Append_Child (Identify_Description (Child));
               elsif Child.Tag.Name = "arg" then
                  Request_Tag.Append_Child (Identify_Arg (Child));
               else
                  raise XML_Exception;
               end if;
            else
               raise XML_Exception;
            end if;
         end loop;

         return Request_Tag;
      end Identify_Request;

      function Identify_Event
        (Node : not null Node_Ptr) return not null Wx.Event_Tag_Ptr
      is
         Event_Tag : constant not null Wx.Event_Tag_Ptr
           := new (Subpool) Wx.Event_Tag;
      begin
         for A of Node.Tag.Attributes loop
            if A.Name = "name" then
               Event_Tag.Set_Name (A.Value, Subpool);
            elsif A.Name = "since" then
               declare
                  Value      : Aida.Int32_T;
                  Has_Failed : Boolean;
               begin
                  Aida.String.To_Int32 (A.Value, Value, Has_Failed);

                  if Has_Failed then
                     raise XML_Exception;
                  else
                     Event_Tag.Set_Since_Attribute (Wx.Version_Number (Value));
                  end if;
               end;
            else
               raise XML_Exception;
            end if;
         end loop;

         for Child of Node.Tag.Child_Nodes loop
            if Child.Id = XML_Tag then
               if Child.Tag.Name = "description" then
                  Event_Tag.Append_Child (Identify_Description (Child));
               elsif Child.Tag.Name = "arg" then
                  Event_Tag.Append_Child (Identify_Arg (Child));
               else
                  raise XML_Exception;
               end if;
            else
               raise XML_Exception;
            end if;
         end loop;

         return Event_Tag;
      end Identify_Event;

      function Identify_Entry
        (Node : not null Node_Ptr) return not null Wx.Entry_Tag_Ptr
      is
         Entry_Tag : constant not null Wx.Entry_Tag_Ptr
           := new (Subpool) Wx.Entry_Tag;
      begin
         for A of Node.Tag.Attributes loop
            if A.Name = "name" then
               Entry_Tag.Set_Name (A.Value, Subpool);
            elsif A.Name = "value" then
               declare
                  Value      : Aida.Int32_T;
                  Has_Failed : Boolean;
               begin
                  Aida.String.To_Int32 (A.Value, Value, Has_Failed);

                  if Has_Failed then
                     if A.Value (A.Value'First .. A.Value'First + 1) = "0x" then
                        Value := Aida.Int32_T'Value
                          (
                           "16#" & String (A.Value (A.Value'First + 2 .. A.Value'Last)) & "#"
                          );

                        Entry_Tag.Set_Value (Wx.Entry_Value (Value));
                     else
                        raise XML_Exception;
                     end if;
                  else
                     Entry_Tag.Set_Value (Wx.Entry_Value (Value));
                  end if;
               end;
            elsif A.Name = "summary" then
               Entry_Tag.Set_Summary (A.Value, Subpool);
            elsif A.Name = "since" then
               declare
                  Value      : Aida.Int32_T;
                  Has_Failed : Boolean;
               begin
                  Aida.String.To_Int32 (A.Value, Value, Has_Failed);

                  if Has_Failed then
                     raise XML_Exception;
                  else
                     Entry_Tag.Set_Since (Wx.Version_Number (Value));
                  end if;
               end;
            else
               raise XML_Exception;
            end if;
         end loop;

         if Node.Tag.Child_Nodes.Length > 0 then
            raise XML_Exception;
         end if;

         return Entry_Tag;
      end Identify_Entry;

      function Identify_Enum
        (Node : not null Node_Ptr) return not null Wx.Enum_Tag_Ptr
      is
         Enum_Tag : constant not null Wx.Enum_Tag_Ptr
           := new (Subpool) Wx.Enum_Tag;
      begin
         for A of Node.Tag.Attributes loop
            if A.Name = "name" then
               Enum_Tag.Set_Name (A.Value, Subpool);
            elsif A.Name = "bitfield" then
               if A.Value = "true" then
                  Enum_Tag.Set_Bitfield (True);
               elsif A.Value = "false" then
                  Enum_Tag.Set_Bitfield (False);
               else
                  raise XML_Exception;
               end if;
            elsif A.Name = "since" then
               declare
                  Value      : Aida.Int32_T;
                  Has_Failed : Boolean;
               begin
                  Aida.String.To_Int32 (A.Value, Value, Has_Failed);

                  if Has_Failed then
                     raise XML_Exception;
                  else
                     Enum_Tag.Set_Since (Wx.Version_Number (Value));
                  end if;
               end;
            else
               raise XML_Exception;
            end if;
         end loop;

         for Child of Node.Tag.Child_Nodes loop
            if Child.Id = XML_Tag then
               if Child.Tag.Name = "description" then
                  Enum_Tag.Append_Child (Identify_Description (Child));
               elsif Child.Tag.Name = "entry" then
                  Enum_Tag.Append_Child (Identify_Entry (Child));
               else
                  raise XML_Exception;
               end if;
            else
               raise XML_Exception;
            end if;
         end loop;

         return Enum_Tag;
      end Identify_Enum;

      function Identify_Interface
        (Node : not null Node_Ptr) return not null Wx.Interface_Tag_Ptr
      is
         Interface_Tag : constant not null Wx.Interface_Tag_Ptr
           := new (Subpool) Wx.Interface_Tag;
      begin
         if Node.Tag.Attributes.Length = 2 then
            if Node.Tag.Attributes.Element (1).Name = "name" then
               Interface_Tag.Set_Name
                 (Node.Tag.Attributes.Element (1).Value, Subpool);
            else
               raise XML_Exception;
            end if;

            if Node.Tag.Attributes.Element (2).Name = "version" then
               declare
                  Value      : Aida.Int32_T;
                  Has_Failed : Boolean;
               begin
                  Aida.String.To_Int32
                    (Node.Tag.Attributes.Element (2).Value, Value, Has_Failed);

                  if Has_Failed then
                     raise XML_Exception;
                  else
                     Interface_Tag.Set_Version (Wx.Version_Number (Value));

                     for Child of Node.Tag.Child_Nodes loop
                        if Child.Id = XML_Tag then
                           if Child.Tag.Name = "description" then
                              Interface_Tag.Append_Child
                                (
                                 Identify_Description (Child)
                                );
                           elsif Child.Tag.Name = "request" then
                              Interface_Tag.Append_Child
                                (
                                 Identify_Request (Child)
                                );
                           elsif Child.Tag.Name = "event" then
                              Interface_Tag.Append_Child
                                (
                                 Identify_Event (Child)
                                );
                           elsif Child.Tag.Name = "enum" then
                              Interface_Tag.Append_Child
                                (
                                 Identify_Enum (Child)
                                );
                           else
                              raise XML_Exception;
                           end if;
                        elsif Child.Id = XML_Comment then
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

         return Interface_Tag;
      end Identify_Interface;

   begin
      for Child of Root_Node.Tag.Child_Nodes loop
         if Child.Id = XML_Tag then
            if Child.Tag.Name = "interface" then
               Protocol_Tag.Append_Child (Identify_Interface (Child));
            elsif Child.Tag.Name = "copyright" then
               Protocol_Tag.Append_Child (Identify_Copyright (Child));
            else
               raise XML_Exception;
            end if;
         else
            raise XML_Exception;
         end if;
      end loop;

      Create_Wayland_Spec_File;
   end Identify_Protocol_Children;

   procedure Create_Wl_Thin_Spec_File;

   -- This procedure creates the posix-wayland.ads file.
   procedure Create_Wayland_Spec_File is

      File : Ada.Text_IO.File_Type;

      procedure Generate_Code_For_Header;

      procedure Create_File is
      begin
         Ada.Text_IO.Create (File, Ada.Text_IO.Out_File, "posix-wayland.ads");

         Generate_Code_For_Header;

         Ada.Text_IO.Close (File);
      end Create_File;

      pragma Unmodified (File);

      procedure Generate_Code_For_Type_Declarations;

      procedure Generate_Code_For_Header is
      begin
         Put_Line (File, "with Interfaces.C.Strings;");
         Put_Line (File, "with System;");
         New_Line (File);
         Put_Line (File, "private with Interfaces.C.Strings;");
         New_Line (File);
         Put_Line (File, "-- Auto-generated from Wayland.xml");
         Put_Line (File, "package Posix.Wayland is");
         New_Line (File);
         Put_Line (File, "pragma Linker_Options (""-lwayland-client"");");
         Put_Line
           (File, "-- Added this linker option here to avoid adding it");
         Put_Line
           (File, "-- to each gpr file that with's this Wayland Ada binding.");
         New_Line (File);

         Generate_Code_For_Type_Declarations;
      end Generate_Code_For_Header;

      procedure Generate_Code_For_The_Interface_Type;

      procedure Generate_Code_For_Type_Declarations is

         procedure Handle_Interface (Interface_Tag : Wx.Interface_Tag) is
            Name : constant String := Utils.Adaify_Name (Interface_Tag.Name);
         begin
            Put (File, "type ");
            Put (File, Name);
            Put_Line (File, ";");
         end Handle_Interface;

      begin
         for Child of Protocol_Tag.Children loop
            case Child.Kind_Id is
               when Child_Dummy =>
                  null;
               when Child_Copyright =>
                  null;
               when Child_Interface =>
                  Handle_Interface (Child.Interface_Tag.all);
            end case;
         end loop;

         New_Line (File);

         Generate_Code_For_The_Interface_Type;
      end Generate_Code_For_Type_Declarations;

      procedure Generate_Code_For_The_Interface_Constants;

      procedure Generate_Code_For_The_Interface_Type is
      begin
         Put_Line (File, "subtype Unsigned_32 is Interfaces.Unsigned_32;");
         New_Line (File);
         Put_Line (File, "type Fixed is new Integer;");
         New_Line (File);
         Put_Line (File, "type Interface_Type is tagged limited private;");
         Put_Line
           (File, "-- This type name ends with _Type because 'interface'");
         Put_Line
           (File, "-- is a reserved keyword in the Ada programming language.");
         New_Line (File);
         Put_Line
           (File, "function Name (I : Interface_Type) return String with");
         Put_Line (File, "  Global => null;");
         New_Line (File);

         Generate_Code_For_The_Interface_Constants;
      end Generate_Code_For_The_Interface_Type;

      procedure Generate_Code_For_Enum_Constants;

      procedure Generate_Code_For_The_Interface_Constants is

         procedure Handle_Interface (Interface_Tag : Wx.Interface_Tag) is
            Name : constant String
              := Utils.Adaify_Name (Interface_Tag.Name & "_Interface");
         begin
            Ada.Text_IO.Put (File, Name);
            Put_Line (File, " : constant Interface_Type;");
            New_Line (File);
         end Handle_Interface;

      begin
         for Child of Protocol_Tag.Children loop
            case Child.Kind_Id is
               when Child_Dummy =>
                  null;
               when Child_Copyright =>
                  null;
               when Child_Interface =>
                  Handle_Interface (Child.Interface_Tag.all);
            end case;
         end loop;

         Put_Line
           (File,
            "Default_Display_Name : constant C_String := ""wayland-0"" & Nul;");
         New_Line (File);

         Generate_Code_For_Enum_Constants;
      end Generate_Code_For_The_Interface_Constants;

      procedure Generate_Code_For_The_Private_Part;

      procedure Generate_Code_For_Enum_Constants is

         procedure Handle_Interface (Interface_Tag : Wx.Interface_Tag) is

            procedure Generate_Code (Enum_Tag : Wx.Enum_Tag) is
               Enum_Type_Name : constant String := Utils.Adaify_Name
                 (Interface_Tag.Name & "_" & Enum_Tag.Name);

               procedure Generate_Code_For_Enum_Value
                 (Entry_Tag : Wx.Entry_Tag)
               is
                  Name : constant String := Utils.Adaify_Name
                    (Interface_Tag.Name & "_" & Enum_Tag.Name & "_" &
                       Entry_Tag.Name);
               begin
                  Put_Line (File, "-- " & Entry_Tag.Summary);
                  Put (File, Name & " : constant " & Enum_Type_Name);
                  Put_Line (File, " := " & Entry_Tag.Value_As_String & ";");
                  New_Line (File);
               end Generate_Code_For_Enum_Value;

            begin
               Put_Line
                 (File, "type " & Enum_Type_Name & " is new Unsigned_32;");

               for Child of Enum_Tag.Children loop
                  case Child.Kind_Id is
                     when Child_Dummy =>
                        null;
                     when Child_Description =>
                        null;
                     when Child_Entry =>
                        Generate_Code_For_Enum_Value (Child.Entry_Tag.all);
                  end case;
               end loop;
               Put_Line (File, "");
            end Generate_Code;

         begin
            for Child of Interface_Tag.Children loop
               case Child.Kind_Id is
                  when Child_Dummy =>
                     null;
                  when Child_Description =>
                     null;
                  when Child_Request =>
                     null;
                  when Child_Event =>
                     null;
                  when Child_Enum =>
                     Generate_Code (Child.Enum_Tag.all);
               end case;
            end loop;
         end Handle_Interface;

      begin
         for Child of Protocol_Tag.Children loop
            case Child.Kind_Id is
               when Child_Dummy =>
                  null;
               when Child_Copyright =>
                  null;
               when Child_Interface =>
                  Handle_Interface (Child.Interface_Tag.all);
            end case;
         end loop;

         Generate_Code_For_The_Private_Part;
      end Generate_Code_For_Enum_Constants;

      procedure Generate_Private_Code_For_The_Interface_Constants;

      procedure Generate_Code_For_The_Private_Part is
      begin
         New_Line (File);
         Put_Line (File, "private");
         New_Line (File);

         Generate_Private_Code_For_The_Interface_Constants;
      end Generate_Code_For_The_Private_Part;

      procedure Generate_Code_For_Footer;

      procedure Generate_Private_Code_For_The_Interface_Constants is

         procedure Handle_Interface (Interface_Tag : Wx.Interface_Tag) is
            Name : constant String
              := Utils.Adaify_Name (Interface_Tag.Name & "_Interface");
         begin
            Ada.Text_IO.Put (File, Name);
            Put_Line (File, " : constant Interface_Type :=");
            Put (File, "(My_Interface => Wl_Thin.");
            Put_Line (File, Name & "'Access);");
            New_Line (File);
         end Handle_Interface;

      begin
         Put_Line (File, " type Interface_Type is tagged limited record");
         Put_Line (File, "    My_Interface : not null Wl_Thin.Interface_Ptr;");
         Put_Line (File, " end record;");
         New_Line (File);
         Put_Line (File, "function Name (I : Interface_Type) return String is");
         Put_Line (File, "   (Value (I.My_Interface.Name));");
         New_Line (File);

         for Child of Protocol_Tag.Children loop
            case Child.Kind_Id is
               when Child_Dummy =>
                  null;
               when Child_Copyright =>
                  null;
               when Child_Interface =>
                  Handle_Interface (Child.Interface_Tag.all);
            end case;
         end loop;

         Generate_Code_For_Footer;
      end Generate_Private_Code_For_The_Interface_Constants;

      procedure Generate_Code_For_Footer is
      begin
         New_Line (File);
         Put_Line (File, "end Posix.Wayland;");
      end Generate_Code_For_Footer;

   begin
      Create_File;
      Create_Wl_Thin_Spec_File;
   end Create_Wayland_Spec_File;

   procedure Create_Wl_Thin_Body_File;

   procedure Create_Wl_Thin_Spec_File is

      File : Ada.Text_IO.File_Type;

      procedure Write_To_File;

      procedure Create_File is
      begin
         Ada.Text_IO.Create (File, Ada.Text_IO.Out_File, Name => "wl_thin.ads");

         Write_To_File;

         Ada.Text_IO.Close (File);
      end Create_File;

      pragma Unmodified (File);

      procedure Generate_Code_For_Numeric_Constants;

      procedure Write_To_File is
      begin
         Put_Line (File, "with Interfaces.C.Strings;");
         Put_Line (File, "with System;");
         Put_Line (File, "");
         Put_Line (File, "-- Mostly auto generated from Wayland.xml");
         Put_Line (File, "package Wl_Thin is");
         Put_Line (File, "");
         Put_Line (File, "-- Begin core parts");
         Put_Line (File, "");
         Put_Line (File, " pragma Linker_Options (""-lwayland-client"");");
         Put_Line (File, " -- Added this linker option here to avoid adding");
         Put_Line (File, "it to each gpr file that with's");
         Put_Line (File, " -- this Ada binding to Wayland. If the");
         Put_Line (File, "wayland client lib changes its name it means");
         Put_Line (File, " -- there is only one place one needs to update.");
         Put_Line (File, "");
         Put_Line (File, " subtype Void_Ptr is System.Address;");
         Put_Line (File, "");
         Put_Line (File, " subtype char_array is Interfaces.C.char_array;");
         Put_Line (File, "");
         Put_Line (File, " Default_Display_Name : aliased char_array :=");
         Put_Line (File, "Interfaces.C.To_C (""wayland-0"");");
         Put_Line (File, "");
         Put_Line (File, " type Proxy_T is limited private;");
         Put_Line (File, "");
         Put_Line (File, " type Proxy_Ptr is access all Proxy_T;");
         Put_Line (File, "");
         Put_Line (File, " type Display_Ptr;");
         Put_Line (File, "");
         Put_Line (File, " function Display_Connect ");
         Put_Line (File, " (Name : Interfaces.C.Strings.char_array_access)");
         Put_Line (File, " return Display_Ptr;");
         Put_Line (File, "");
         Put_Line (File, " procedure Display_Disconnect");
         Put_Line (File, "(");
         Put_Line (File, " This : in out Display_Ptr");
         Put_Line (File, ");");
         Put_Line (File, "");
         Put_Line (File, " type Interface_T is limited record");
         Put_Line (File, "    Name         : Interfaces.C.Strings.chars_ptr;");
         Put_Line (File, "    Version      : Interfaces.C.int;");
         Put_Line (File, "    Method_Count : Interfaces.C.int;");
         Put_Line (File, "    Methods      : Void_Ptr; -- Can be improved upon.");
         Put_Line (File, "    Event_Count  : Interfaces.C.int;");
         Put_Line (File, "    Events       : Void_Ptr; -- Can be improved upon.");
         Put_Line (File, " end record with");
         Put_Line (File, "   Convention => C_Pass_By_Copy;");
         Put_Line (File, "");
         Put_Line (File, " type Interface_Ptr is access all Interface_T;");
         Put_Line (File, "");
         Put_Line (File, " function Proxy_Marshal_Constructor (Proxy       : Proxy_Ptr;");
         Put_Line (File, "Opcode      : Interfaces.Unsigned_32;");
         Put_Line (File, "Interface_V : Interface_Ptr;");
         Put_Line (File, "New_Id      : Interfaces.Unsigned_32) return Proxy_Ptr with");
         Put_Line (File, "   Convention    => C,");
         Put_Line (File, "   Import        => True,");
         Put_Line (File, "   External_Name => ""wl_proxy_marshal_constructor"";");
         Put_Line (File, "");
         Put_Line (File, " function Proxy_Marshal_Constructor (");
         Put_Line (File, "Proxy       : Proxy_Ptr;");
         Put_Line (File, "Opcode      : Interfaces.Unsigned_32;");
         Put_Line (File, "Interface_V : Interface_Ptr;");
         Put_Line (File, "New_Id      : Interfaces.Unsigned_32;");
         Put_Line (File, "Offset      : Integer;");
         Put_Line (File, "Width       : Integer;");
         Put_Line (File, "Height      : Integer;");
         Put_Line (File, "Stride      : Integer;");
         Put_Line (File, "Format      : Interfaces.Unsigned_32");
         Put_Line (File, ") return Proxy_Ptr with");
         Put_Line (File, " Convention    => C,");
         Put_Line (File, " Import        => True,");
         Put_Line (File, " External_Name => ""wl_proxy_marshal_constructor"";");
         Put_Line (File, "");
         Put_Line (File, " function Proxy_Marshal_Constructor_Versioned (Proxy          : Proxy_Ptr;");
         Put_Line (File, "Opcode         : Interfaces.Unsigned_32;");
         Put_Line (File, "Interface_V    : Interface_Ptr;");
         Put_Line (File, "New_Id_1       : Interfaces.Unsigned_32;");
         Put_Line (File, "Name           : Interfaces.Unsigned_32;");
         Put_Line (File, "Interface_Name : Interfaces.C.Strings.chars_ptr;");
         Put_Line (File, "New_Id_2       : Interfaces.Unsigned_32;");
         Put_Line (File, "Version        : Interfaces.Unsigned_32) return Proxy_Ptr with");
         Put_Line (File, "   Convention    => C,");
         Put_Line (File, "   Import        => True,");
         Put_Line (File, "External_Name => ""wl_proxy_marshal_constructor_versioned"";");
         Put_Line (File, "");
         Put_Line (File, "procedure Proxy_Marshal (Proxy : Proxy_Ptr;");
         Put_Line (File, "Opcode      : Interfaces.Unsigned_32) with");
         Put_Line (File, "   Convention    => C,");
         Put_Line (File, "   Import        => True,");
         Put_Line (File, "   External_Name => ""wl_proxy_marshal"";");
         Put_Line (File, "");
         Put_Line (File, " function Display_Dispatch (Display : Display_Ptr) return Interfaces.C.int with");
         Put_Line (File, "   Import => True,");
         Put_Line (File, "   Convention => C,");
         Put_Line (File, "   External_Name => ""wl_display_dispatch"";");
         Put_Line (File, "");
         Put_Line (File, " function Display_Roundtrip (Display : Display_Ptr) return Interfaces.C.int with");
         Put_Line (File, "   Import => True,");
         Put_Line (File, "   Convention => C,");
         Put_Line (File, "   External_Name => ""wl_display_roundtrip"";");
         Put_Line (File, "");
         Put_Line (File, "   function Proxy_Marshal_Constructor");
         Put_Line (File, "     (Proxy       : Proxy_Ptr;");
         Put_Line (File, "      Opcode      : Interfaces.Unsigned_32;");
         Put_Line (File, "      Interface_V : Interface_Ptr;");
         Put_Line (File, "      New_Id      : Interfaces.Unsigned_32;");
         Put_Line (File, "      Offset      : Void_Ptr) return Proxy_Ptr with");
         Put_Line (File, "      Convention    => C,");
         Put_Line (File, "      Import        => True,");
         Put_Line (File, "      External_Name => ""wl_proxy_marshal_constructor"";");
         Put_Line (File, "");
         Put_Line (File, "   procedure Proxy_Marshal (Proxy : Proxy_Ptr; Opcode : Interfaces.Unsigned_32; Arg_1 : Integer) with");
         Put_Line (File, "      Convention    => C,");
         Put_Line (File, "      Import        => True,");
         Put_Line (File, "      External_Name => ""wl_proxy_marshal"";");
         Put_Line (File, "");
         Put_Line
           (File,
            "   procedure Proxy_Marshal (Proxy : Proxy_Ptr; Opcode : Interfaces.Unsigned_32; Arg_1 : Interfaces.Unsigned_32) with");
         Put_Line (File, "      Convention    => C,");
         Put_Line (File, "      Import        => True,");
         Put_Line (File, "      External_Name => ""wl_proxy_marshal"";");
         Put_Line (File, "");
         Put_Line (File, "   procedure Proxy_Marshal");
         Put_Line (File, "     (Proxy  : Proxy_Ptr;");
         Put_Line (File, "      Opcode : Interfaces.Unsigned_32;");
         Put_Line (File, "      Arg_1  : Interfaces.Unsigned_32;");
         Put_Line (File, "      Arg_2  : Interfaces.C.Strings.chars_ptr) with");
         Put_Line (File, "      Convention    => C,");
         Put_Line (File, "      Import        => True,");
         Put_Line (File, "      External_Name => ""wl_proxy_marshal"";");
         Put_Line (File, "");
         Put_Line (File, "   procedure Proxy_Marshal");
         Put_Line (File, "     (Proxy  : Proxy_Ptr;");
         Put_Line (File, "      Opcode : Interfaces.Unsigned_32;");
         Put_Line (File, "      Arg_1  : Interfaces.Unsigned_32;");
         Put_Line (File, "      Arg_2  : Interfaces.Unsigned_32) with");
         Put_Line (File, "      Convention    => C,");
         Put_Line (File, "      Import        => True,");
         Put_Line (File, "      External_Name => ""wl_proxy_marshal"";");
         Put_Line (File, "");
         Put_Line (File, "   procedure Proxy_Marshal");
         Put_Line (File, "     (Proxy  : Proxy_Ptr;");
         Put_Line (File, "      Opcode : Interfaces.Unsigned_32;");
         Put_Line (File, "      Arg_1  : Interfaces.C.Strings.chars_ptr) with");
         Put_Line (File, "      Convention    => C,");
         Put_Line (File, "      Import        => True,");
         Put_Line (File, "      External_Name => ""wl_proxy_marshal"";");
         Put_Line (File, "");
         Put_Line (File, "   procedure Proxy_Marshal");
         Put_Line (File, "     (Proxy  : Proxy_Ptr;");
         Put_Line (File, "      Opcode : Interfaces.Unsigned_32;");
         Put_Line (File, "      Arg_1  : Interfaces.C.Strings.chars_ptr;");
         Put_Line (File, "      Arg_2  : Integer) with");
         Put_Line (File, "      Convention    => C,");
         Put_Line (File, "      Import        => True,");
         Put_Line (File, "      External_Name => ""wl_proxy_marshal"";");
         Put_Line (File, "");
         Put_Line
           (File,
            "   procedure Proxy_Marshal (Proxy : Proxy_Ptr; Opcode : Interfaces.Unsigned_32; Arg_1 : Integer; Arg_2 : Integer) with");
         Put_Line (File, "      Convention    => C,");
         Put_Line (File, "      Import        => True,");
         Put_Line (File, "      External_Name => ""wl_proxy_marshal"";");
         Put_Line (File, "");
         Put_Line (File, "   procedure Proxy_Marshal (Proxy : Proxy_Ptr; Opcode : Interfaces.Unsigned_32; Arg_1 : Void_Ptr) with");
         Put_Line (File, "      Convention    => C,");
         Put_Line (File, "      Import        => True,");
         Put_Line (File, "      External_Name => ""wl_proxy_marshal"";");
         Put_Line (File, "");
         Put_Line (File, "   procedure Proxy_Marshal");
         Put_Line (File, "     (Proxy  : Proxy_Ptr;");
         Put_Line (File, "      Opcode : Interfaces.Unsigned_32;");
         Put_Line (File, "      Arg_1  : Void_Ptr;");
         Put_Line (File, "      Arg_4  : Interfaces.Unsigned_32) with");
         Put_Line (File, "      Convention    => C,");
         Put_Line (File, "      Import        => True,");
         Put_Line (File, "      External_Name => ""wl_proxy_marshal"";");
         Put_Line (File, "");
         Put_Line (File, "   procedure Proxy_Marshal");
         Put_Line (File, "     (Proxy  : Proxy_Ptr;");
         Put_Line (File, "      Opcode : Interfaces.Unsigned_32;");
         Put_Line (File, "      Arg_1  : Void_Ptr;");
         Put_Line (File, "      Arg_2  : Interfaces.Unsigned_32;");
         Put_Line (File, "      Arg_3  : Interfaces.Unsigned_32) with");
         Put_Line (File, "      Convention    => C,");
         Put_Line (File, "      Import        => True,");
         Put_Line (File, "      External_Name => ""wl_proxy_marshal"";");
         Put_Line (File, "");
         Put_Line (File, "   procedure Proxy_Marshal");
         Put_Line (File, "     (Proxy  : Proxy_Ptr;");
         Put_Line (File, "      Opcode : Interfaces.Unsigned_32;");
         Put_Line (File, "      Arg_1  : Void_Ptr;");
         Put_Line (File, "      Arg_2  : Integer;");
         Put_Line (File, "      Arg_3  : Integer) with");
         Put_Line (File, "      Convention    => C,");
         Put_Line (File, "      Import        => True,");
         Put_Line (File, "      External_Name => ""wl_proxy_marshal"";");
         Put_Line (File, "");
         Put_Line (File, "   procedure Proxy_Marshal");
         Put_Line (File, "     (Proxy  : Proxy_Ptr;");
         Put_Line (File, "      Opcode : Interfaces.Unsigned_32;");
         Put_Line (File, "      Arg_1  : Interfaces.Unsigned_32;");
         Put_Line (File, "      Arg_2  : Interfaces.Unsigned_32;");
         Put_Line (File, "      Arg_3  : Void_Ptr) with");
         Put_Line (File, "      Convention    => C,");
         Put_Line (File, "      Import        => True,");
         Put_Line (File, "      External_Name => ""wl_proxy_marshal"";");
         Put_Line (File, "");
         Put_Line (File, "   procedure Proxy_Marshal");
         Put_Line (File, "     (Proxy  : Proxy_Ptr;");
         Put_Line (File, "      Opcode : Interfaces.Unsigned_32;");
         Put_Line (File, "      Arg_1  : Integer;");
         Put_Line (File, "      Arg_2  : Integer;");
         Put_Line (File, "      Arg_3  : Integer;");
         Put_Line (File, "      Arg_4  : Integer) with");
         Put_Line (File, "      Convention    => C,");
         Put_Line (File, "      Import        => True,");
         Put_Line (File, "      External_Name => ""wl_proxy_marshal"";");
         Put_Line (File, "");
         Put_Line (File, "   procedure Proxy_Marshal");
         Put_Line (File, "     (Proxy  : Proxy_Ptr;");
         Put_Line (File, "      Opcode : Interfaces.Unsigned_32;");
         Put_Line (File, "      Arg_1  : Void_Ptr;");
         Put_Line (File, "      Arg_2  : Integer;");
         Put_Line (File, "      Arg_3  : Integer;");
         Put_Line (File, "      Arg_4  : Interfaces.Unsigned_32) with");
         Put_Line (File, "      Convention    => C,");
         Put_Line (File, "      Import        => True,");
         Put_Line (File, "      External_Name => ""wl_proxy_marshal"";");
         Put_Line (File, "");
         Put_Line (File, "   procedure Proxy_Marshal");
         Put_Line (File, "     (Proxy  : Proxy_Ptr;");
         Put_Line (File, "      Opcode : Interfaces.Unsigned_32;");
         Put_Line (File, "      Arg_1  : Void_Ptr;");
         Put_Line (File, "      Arg_2  : Void_Ptr;");
         Put_Line (File, "      Arg_3  : Void_Ptr;");
         Put_Line (File, "      Arg_4  : Interfaces.Unsigned_32) with");
         Put_Line (File, "      Convention    => C,");
         Put_Line (File, "      Import        => True,");
         Put_Line (File, "      External_Name => ""wl_proxy_marshal"";");
         Put_Line (File, "");
         Put_Line (File, "   procedure Proxy_Marshal");
         Put_Line (File, "     (Proxy  : Proxy_Ptr;");
         Put_Line (File, "      Opcode : Interfaces.Unsigned_32;");
         Put_Line (File, "      Arg_1  : Interfaces.Unsigned_32;");
         Put_Line (File, "      Arg_2  : Void_Ptr;");
         Put_Line (File, "      Arg_3  : Integer;");
         Put_Line (File, "      Arg_4  : Integer) with");
         Put_Line (File, "      Convention    => C,");
         Put_Line (File, "      Import        => True,");
         Put_Line (File, "      External_Name => ""wl_proxy_marshal"";");
         Put_Line (File, "");
         Put_Line (File, "   procedure Proxy_Marshal");
         Put_Line (File, "     (Proxy  : Proxy_Ptr;");
         Put_Line (File, "      Opcode : Interfaces.Unsigned_32;");
         Put_Line (File, "      Arg_1  : Void_Ptr;");
         Put_Line (File, "      Arg_2  : Interfaces.Unsigned_32;");
         Put_Line (File, "      Arg_3  : Void_Ptr;");
         Put_Line (File, "      Arg_4  : Integer;");
         Put_Line (File, "      Arg_5  : Integer;");
         Put_Line (File, "      Arg_6  : Interfaces.Unsigned_32) with");
         Put_Line (File, "      Convention    => C,");
         Put_Line (File, "      Import        => True,");
         Put_Line (File, "      External_Name => ""wl_proxy_marshal"";");
         Put_Line (File, "");
         Put_Line (File, "   function Proxy_Marshal_Constructor");
         Put_Line (File, "     (Proxy       : Proxy_Ptr;");
         Put_Line (File, "      Opcode      : Interfaces.Unsigned_32;");
         Put_Line (File, "      Interface_V : Interface_Ptr;");
         Put_Line (File, "      New_Id      : Interfaces.Unsigned_32;");
         Put_Line (File, "      Arg_1       : Integer;");
         Put_Line (File, "      Arg_2       : Integer) return Proxy_Ptr with");
         Put_Line (File, "      Convention    => C,");
         Put_Line (File, "      Import        => True,");
         Put_Line (File, "      External_Name => ""wl_proxy_marshal_constructor"";");
         Put_Line (File, "");
         Put_Line (File, "   function Proxy_Marshal_Constructor");
         Put_Line (File, "     (Proxy       : Proxy_Ptr;");
         Put_Line (File, "      Opcode      : Interfaces.Unsigned_32;");
         Put_Line (File, "      Interface_V : Interface_Ptr;");
         Put_Line (File, "      New_Id      : Interfaces.Unsigned_32;");
         Put_Line (File, "      Arg_1       : Void_Ptr;");
         Put_Line (File, "      Arg_2       : Void_Ptr) return Proxy_Ptr with");
         Put_Line (File, "      Convention    => C,");
         Put_Line (File, "      Import        => True,");
         Put_Line (File, "      External_Name => ""wl_proxy_marshal_constructor"";");
         Put_Line (File, "");
         Put_Line (File, " -- End core parts");
         Put_Line (File, "");
         Put_Line (File, "type Fixed_T is new Interfaces.Integer_32;");
         Put_Line (File, "");
         Put_Line (File, "type Wayland_Array_T is record");
         Put_Line (File, "Size  : Interfaces.Unsigned_32;");
         Put_Line (File, "Alloc : Interfaces.Unsigned_32;");
         Put_Line (File, "Data : Void_Ptr;");
         Put_Line (File, "end record with");
         Put_Line (File, "   Convention => C_Pass_By_Copy;");
         Put_Line (File, "");

         Generate_Code_For_Numeric_Constants;

         Put_Line (File, "private");
         Put_Line (File, "");
         Put_Line (File, "   type Proxy_T is limited null record;");
         Put_Line (File, "");
         Put_Line (File, "end Wl_Thin;");
      end Write_To_File;

      procedure Generate_Code_For_Interface_Constants;

      procedure Generate_Code_For_Numeric_Constants is

         procedure Handle_Interface (Interface_Tag : Wx.Interface_Tag) is

            procedure Generate_Code_For_Opcodes is

               I : Aida.Int32_T := 0;

               procedure Generate_Code (Request_Tag : Wx.Request_Tag) is
                  Name : constant String := Utils.Make_Upper_Case (Interface_Tag.Name & "_" & Request_Tag.Name);
               begin
                  Ada.Text_IO.Put (File, Name);
                  Ada.Text_IO.Put (File, " : constant := " & Aida.Int32.To_String (I));
                  Put_Line (File, ";");
                  Put_Line (File, "");

                  I := I + 1;
               end Generate_Code;

            begin
               for Child of Interface_Tag.Children loop
                  case Child.Kind_Id is
                     when Child_Dummy =>
                        null;
                     when Child_Description =>
                        null;
                     when Child_Request =>
                        Generate_Code (Child.Request_Tag.all);
                     when Child_Event =>
                        null;
                     when Child_Enum =>
                        null;
                  end case;
               end loop;
            end Generate_Code_For_Opcodes;

            procedure Generate_Code_For_Event_Since_Version is

               procedure Generate_Code (Event_Tag : Wx.Event_Tag) is
                  Name : constant String := Utils.Make_Upper_Case (Interface_Tag.Name & "_" & Event_Tag.Name & "_SINCE_VERSION");
               begin
                  if Event_Tag.Exists_Since_Attribute then
                     Ada.Text_IO.Put (File, Name);
                     Ada.Text_IO.Put (File, " : constant := " & Aida.Int32.To_String (Event_Tag.Since_Attribute_As_Pos32));
                     Put_Line (File, ";");
                     Put_Line (File, "");
                  else
                     Ada.Text_IO.Put (File, Name);
                     Put_Line (File, " : constant := 1;");
                     Put_Line (File, "");
                  end if;
               end Generate_Code;

            begin
               for Child of Interface_Tag.Children loop
                  case Child.Kind_Id is
                     when Child_Dummy =>
                        null;
                     when Child_Description =>
                        null;
                     when Child_Request =>
                        null;
                     when Child_Event =>
                        Generate_Code (Child.Event_Tag.all);
                     when Child_Enum =>
                        null;
                  end case;
               end loop;
            end Generate_Code_For_Event_Since_Version;

            procedure Generate_Code_For_Opcodes_Since_Version is

               procedure Generate_Code (Request_Tag : Wx.Request_Tag) is
                  Name : constant String := Utils.Make_Upper_Case (Interface_Tag.Name & "_" & Request_Tag.Name & "_SINCE_VERSION");
               begin
                  if Request_Tag.Exists_Since then
                     Ada.Text_IO.Put (File, Name);
                     Ada.Text_IO.Put (File, " : constant := " & Aida.Int32.To_String (Request_Tag.Since_As_Pos32));
                     Put_Line (File, ";");
                     Put_Line (File, "");
                  else
                     Ada.Text_IO.Put (File, Name);
                     Put_Line (File, " : constant := 1;");
                     Put_Line (File, "");
                  end if;
               end Generate_Code;

            begin
               for Child of Interface_Tag.Children loop
                  case Child.Kind_Id is
                     when Child_Dummy =>
                        null;
                     when Child_Description =>
                        null;
                     when Child_Request =>
                        Generate_Code (Child.Request_Tag.all);
                     when Child_Event =>
                        null;
                     when Child_Enum =>
                        null;
                  end case;
               end loop;
            end Generate_Code_For_Opcodes_Since_Version;

         begin
            Generate_Code_For_Opcodes;
            Generate_Code_For_Event_Since_Version;
            Generate_Code_For_Opcodes_Since_Version;
         end Handle_Interface;

      begin
         for Child of Protocol_Tag.Children loop
            case Child.Kind_Id is
               when Child_Dummy =>
                  null;
               when Child_Copyright =>
                  null;
               when Child_Interface =>
                  Handle_Interface (Child.Interface_Tag.all);
            end case;
         end loop;

         Generate_Code_For_Interface_Constants;
      end Generate_Code_For_Numeric_Constants;

      procedure Generate_Code_For_Interface_Ptrs;

      procedure Generate_Code_For_Interface_Constants is

         procedure Handle_Interface (Interface_Tag : Wx.Interface_Tag) is
            Name : constant String := Utils.Adaify_Name (Interface_Tag.Name & "_Interface");
         begin
            Ada.Text_IO.Put (File, Name);
            Put_Line (File, " : aliased Interface_T with");
            Put_Line (File, "Import => True,");
            Put_Line (File, "Convention => C,");
            Put_Line (File, "External_Name => """ & Interface_Tag.Name & "_interface"";");
            Put_Line (File, "");
         end Handle_Interface;

      begin
         for Child of Protocol_Tag.Children loop
            case Child.Kind_Id is
               when Child_Dummy =>
                  null;
               when Child_Copyright =>
                  null;
               when Child_Interface =>
                  Handle_Interface (Child.Interface_Tag.all);
            end case;
         end loop;

         Generate_Code_For_Interface_Ptrs;
      end Generate_Code_For_Interface_Constants;

      procedure Generate_Code_For_Each_Interface;

      procedure Generate_Code_For_Interface_Ptrs is

         procedure Handle_Interface (Interface_Tag : Wx.Interface_Tag) is

            procedure Generate_Code_For_Interface_Ptr is
               Name : constant String := Utils.Interface_Ptr_Name (Interface_Tag);
            begin
               Put_Line (File, "type " & Name & " is new Proxy_Ptr;");
               Put_Line (File, "");
            end Generate_Code_For_Interface_Ptr;

         begin
            Generate_Code_For_Interface_Ptr;
         end Handle_Interface;

      begin
         for Child of Protocol_Tag.Children loop
            case Child.Kind_Id is
               when Child_Dummy =>
                  null;
               when Child_Copyright =>
                  null;
               when Child_Interface =>
                  Handle_Interface (Child.Interface_Tag.all);
            end case;
         end loop;

         Generate_Code_For_Each_Interface;
      end Generate_Code_For_Interface_Ptrs;

      procedure Generate_Code_For_Each_Interface is

         procedure Handle_Interface (Interface_Tag : Wx.Interface_Tag) is

            procedure Generate_Code_For_Enums is

               procedure Generate_Code (Enum_Tag : Wx.Enum_Tag) is
                  Enum_Type_Name : constant String := Utils.Adaify_Name (Interface_Tag.Name & "_" & Enum_Tag.Name & "_T");

                  procedure Generate_Code_For_Enum_Value (Entry_Tag : Wx.Entry_Tag) is
                     Name : constant String := Utils.Adaify_Name (Interface_Tag.Name & "_" & Enum_Tag.Name & "_" & Entry_Tag.Name);
                  begin
                     Put_Line (File, "-- " & Entry_Tag.Summary);
                     Put_Line (File, Name & " : constant " & Enum_Type_Name & " := " & Entry_Tag.Value_As_String & ";");
                     Put_Line (File, "");
                  end Generate_Code_For_Enum_Value;

               begin
                  Put_Line (File, "type " & Enum_Type_Name & " is new Interfaces.Unsigned_32;");

                  for Child of Enum_Tag.Children loop
                     case Child.Kind_Id is
                        when Child_Dummy =>
                           null;
                        when Child_Description =>
                           null;
                        when Child_Entry =>
                           Generate_Code_For_Enum_Value (Child.Entry_Tag.all);
                     end case;
                  end loop;
                  Put_Line (File, "");
               end Generate_Code;

            begin
               for Child of Interface_Tag.Children loop
                  case Child.Kind_Id is
                     when Child_Dummy =>
                        null;
                     when Child_Description =>
                        null;
                     when Child_Request =>
                        null;
                     when Child_Event =>
                        null;
                     when Child_Enum =>
                        Generate_Code (Child.Enum_Tag.all);
                  end case;
               end loop;
            end Generate_Code_For_Enums;

            procedure Generate_Code_For_Subprogram_Ptrs is

               procedure Generate_Code_For_Subprogram (Event_Tag : Wx.Event_Tag) is

                  procedure Generate_Code_For_Argument (Arg_Tag : Wx.Arg_Tag; Is_Last : Boolean) is
                  begin
                     declare
                        Arg_Name      : constant String := Utils.Adaify_Variable_Name (Arg_Tag.Name);
                        Arg_Type_Name : constant String := Utils.Arg_Type_As_String (Arg_Tag);
                     begin
                        if Is_Last then
                           Put_Line (File, "     " & Arg_Name & " : " & Arg_Type_Name);
                        else
                           Put_Line (File, "     " & Arg_Name & " : " & Arg_Type_Name & ";");
                        end if;
                     end;
                  end Generate_Code_For_Argument;

                  V : Wx.Event_Child_Vectors.Vector;

                  Subprogram_Name : constant String := Utils.Adaify_Name (Interface_Tag.Name & "_" & Event_Tag.Name & "_Subprogram_Ptr");
                  Interface_Name  : constant String := Utils.Adaify_Name (Interface_Tag.Name);
               begin
                  for Child of Event_Tag.Children loop
                     case Child.Kind_Id is
                        when Child_Dummy =>
                           null;
                        when Child_Description =>
                           null;
                        when Child_Arg =>
                           V.Append (Child);
                     end case;
                  end loop;

                  Put_Line (File, "type " & Subprogram_Name & " is access procedure (Data : Void_Ptr;");
                  Ada.Text_IO.Put (File, "     " & Interface_Name & " : " & Utils.Interface_Ptr_Name (Interface_Tag));

                  if V.Length = 0 then
                     Put_Line (File, "");
                  else
                     Put_Line (File, ";");
                  end if;

                  for Child of V loop
                     case Child.Kind_Id is
                        when Child_Dummy =>
                           null;
                        when Child_Description =>
                           null;
                        when Child_Arg =>
                           Generate_Code_For_Argument (Child.Arg_Tag.all, Event_Tag.Children.Last_Element = Child);
                     end case;
                  end loop;
                  Put_Line (File, "     ) with");
                  Put_Line (File, "Convention => C;");
                  Put_Line (File, "");
               end Generate_Code_For_Subprogram;

            begin
               for Child of Interface_Tag.Children loop
                  case Child.Kind_Id is
                     when Child_Dummy =>
                        null;
                     when Child_Description =>
                        null;
                     when Child_Request =>
                        null;
                     when Child_Event =>
                        Generate_Code_For_Subprogram (Child.Event_Tag.all);
                     when Child_Enum =>
                        null;
                  end case;
               end loop;
            end Generate_Code_For_Subprogram_Ptrs;

            procedure Generate_Code_For_Listener_Type_Definition is

               procedure Generate_Code_For_Record_Component (Event_Tag : Wx.Event_Tag) is
                  Component_Name      : constant String := Utils.Adaify_Name (Event_Tag.Name);
                  Component_Type_Name : constant String := Utils.Adaify_Name (Interface_Tag.Name & "_" & Event_Tag.Name & "_Subprogram_Ptr");
               begin
                  Put_Line (File, "   " & Component_Name & " : " & Component_Type_Name & ";");
               end Generate_Code_For_Record_Component;

               Name     : constant String := Utils.Adaify_Name (Interface_Tag.Name & "_Listener_T");
               Ptr_Name : constant String := Utils.Adaify_Name (Interface_Tag.Name & "_Listener_Ptr");
            begin
               Put_Line (File, "type " & Name & " is record");

               for Child of Interface_Tag.Children loop
                  case Child.Kind_Id is
                     when Child_Dummy =>
                        null;
                     when Child_Description =>
                        null;
                     when Child_Request =>
                        null;
                     when Child_Event =>
                        Generate_Code_For_Record_Component (Child.Event_Tag.all);
                     when Child_Enum =>
                        null;
                  end case;
               end loop;
               Put_Line (File, "end record with");
               Put_Line (File, "   Convention => C_Pass_By_Copy;");
               Put_Line (File, "");
               Put_Line (File, "type " & Ptr_Name & " is access all " & Name & ";");
               Put_Line (File, "");
            end Generate_Code_For_Listener_Type_Definition;

            procedure Generate_Code_For_Add_Listener_Subprogram_Declaration is
               Name              : constant String := Utils.Adaify_Name (Interface_Tag.Name);
               Ptr_Name          : constant String := Utils.Adaify_Name (Interface_Tag.Name & "_Ptr");
               Ptr_Listener_Name : constant String := Utils.Adaify_Name (Interface_Tag.Name & "_Listener_Ptr");
            begin
               Put_Line (File, "function " & Name & "_Add_Listener (" & Name & " : " & Ptr_Name & ";");
               Put_Line (File, "Listener : " & Ptr_Listener_Name & ";");
               Put_Line (File, "Data : Void_Ptr) return Interfaces.C.int;");
               Put_Line (File, "");
            end Generate_Code_For_Add_Listener_Subprogram_Declaration;

            procedure Generate_Code_For_Set_User_Data_Subprogram_Declaration is
               Name     : constant String := Utils.Adaify_Name (Interface_Tag.Name);
               Ptr_Name : constant String := Utils.Adaify_Name (Interface_Tag.Name & "_Ptr");
            begin
               Put_Line (File, "procedure " & Name & "_Set_User_Data (" & Name & " : " & Ptr_Name & ";");
               Put_Line (File, "Data : Void_Ptr);");
               Put_Line (File, "");
            end Generate_Code_For_Set_User_Data_Subprogram_Declaration;

            procedure Generate_Code_For_Get_User_Data_Subprogram_Declaration is
               Name     : constant String := Utils.Adaify_Name (Interface_Tag.Name);
               Ptr_Name : constant String := Utils.Adaify_Name (Interface_Tag.Name & "_Ptr");
            begin
               Put_Line (File, "function " & Name & "_Get_User_Data (" & Name & " : " & Ptr_Name & ") return Void_Ptr;");
               Put_Line (File, "");
            end Generate_Code_For_Get_User_Data_Subprogram_Declaration;

            procedure Generate_Code_For_Get_Version_Subprogram_Declaration is
               Name     : constant String := Utils.Adaify_Name (Interface_Tag.Name);
               Ptr_Name : constant String := Utils.Adaify_Name (Interface_Tag.Name & "_Ptr");
            begin
               Put_Line (File, "function " & Name & "_Get_Version (" & Name & " : " & Ptr_Name & ") return Interfaces.Unsigned_32;");
               Put_Line (File, "");
            end Generate_Code_For_Get_Version_Subprogram_Declaration;

            procedure Generate_Code_For_Destroy_Subprogram_Declaration is
               Name     : constant String := Utils.Adaify_Name (Interface_Tag.Name);
               Ptr_Name : constant String := Utils.Adaify_Name (Interface_Tag.Name & "_Ptr");
            begin
               Put_Line (File, "procedure " & Name & "_Destroy (" & Name & " : " & Ptr_Name & ");");
               Put_Line (File, "");
            end Generate_Code_For_Destroy_Subprogram_Declaration;

            procedure Generate_Code_For_Requests is

               procedure Generate_Code_For_Subprogram_Declaration (Request_Tag : Wx.Request_Tag) is

                  procedure Generate_Code_For_Arg (Arg_Tag : Wx.Arg_Tag; Is_Last : Boolean) is
                  begin
                     declare
                        Arg_Name      : constant String := Utils.Adaify_Variable_Name (Arg_Tag.Name);
                        Arg_Type_Name : constant String := Utils.Arg_Type_As_String (Arg_Tag);
                     begin
                        if Is_Last then
                           Put_Line (File, "     " & Arg_Name & " : " & Arg_Type_Name);
                        else
                           Put_Line (File, "     " & Arg_Name & " : " & Arg_Type_Name & ";");
                        end if;
                     end;
                  end Generate_Code_For_Arg;

                  procedure Generate_Comment (Text : String) is
                     Interval_Identifier : constant Utils.Interval_Identifier := Utils.Make (Text);
                  begin
                     for Interval of Interval_Identifier.Intervals loop
                        Put_Line (File, "-- " & Ada.Strings.Fixed.Trim (Text (Interval.First .. Interval.Last), Ada.Strings.Both));
                     end loop;
                  end Generate_Comment;

                  Subprogram_Name : constant String := Utils.Adaify_Name (Interface_Tag.Name & "_" & Request_Tag.Name);
                  Name            : constant String := Utils.Adaify_Name (Interface_Tag.Name);
                  Ptr_Name        : constant String := Utils.Adaify_Name (Interface_Tag.Name & "_Ptr");
               begin
                  if Utils.Is_New_Id_Argument_Present (Request_Tag) then
                     if Request_Tag.Exists_Description then
                        Generate_Comment (Utils.Remove_Tabs (Request_Tag.Description));
                     end if;
                     if Utils.Is_Interface_Specified (Request_Tag) then
                        declare
                           Return_Type : constant String := Utils.Adaify_Name (Utils.Find_Specified_Interface (Request_Tag) & "_Ptr");
                        begin
                           if Utils.Number_Of_Args (Request_Tag) > 1 then
                              Put_Line (File, "function " & Subprogram_Name & " (" & Name & " : " & Ptr_Name & ";");

                              declare
                                 V : Wx.Request_Child_Vectors.Vector;
                              begin
                                 for Child of Request_Tag.Children loop
                                    case Child.Kind_Id is
                                       when Child_Dummy =>
                                          null;
                                       when Child_Description =>
                                          null;
                                       when Child_Arg =>
                                          if Child.Arg_Tag.Type_Attribute /= Type_New_Id then
                                             V.Append (Child);
                                          end if;
                                    end case;
                                 end loop;

                                 for Child of V loop
                                    case Child.Kind_Id is
                                       when Child_Dummy =>
                                          null;
                                       when Child_Description =>
                                          null;
                                       when Child_Arg =>
                                          Generate_Code_For_Arg (Child.Arg_Tag.all, Child = Request_Tag.Children.Last_Element);
                                    end case;
                                 end loop;
                              end;

                              Put_Line (File, "   ) return " & Return_Type & ";");
                           else
                              Put_Line
                                (File,
                                 "function " & Subprogram_Name & " (" & Name & " : " & Ptr_Name & ") return " & Return_Type & ";");
                           end if;
                        end;
                     else
                        if Utils.Number_Of_Args (Request_Tag) > 1 then
                           Put_Line (File, "function " & Subprogram_Name & " (" & Name & " : " & Ptr_Name & ";");

                           declare
                              V : Wx.Request_Child_Vectors.Vector;
                           begin
                              for Child of Request_Tag.Children loop
                                 case Child.Kind_Id is
                                    when Child_Dummy =>
                                       null;
                                    when Child_Description =>
                                       null;
                                    when Child_Arg =>
                                       if Child.Arg_Tag.Type_Attribute /= Type_New_Id then
                                          V.Append (Child);
                                       end if;
                                 end case;
                              end loop;

                              for Child of V loop
                                 case Child.Kind_Id is
                                    when Child_Dummy =>
                                       null;
                                    when Child_Description =>
                                       null;
                                    when Child_Arg =>
                                       Generate_Code_For_Arg (Child.Arg_Tag.all, Child = Request_Tag.Children.Last_Element);
                                 end case;
                              end loop;
                           end;

                           Put_Line (File, "   Interface_V : Interface_Ptr;");
                           Put_Line (File, "   New_Id : Interfaces.Unsigned_32) return Proxy_Ptr;");
                        else
                           Put_Line (File, "function " & Subprogram_Name & " (" & Name & " : " & Ptr_Name & ") return Proxy_Ptr;");
                        end if;
                     end if;
                  elsif Utils.Is_Request_Destructor (Request_Tag) then
                     null; -- Already has generated declaration earlier in Generate_Code_For_Destroy_Subprogram
                  else
                     if Request_Tag.Exists_Description then
                        Generate_Comment (Utils.Remove_Tabs (Request_Tag.Description));
                     end if;
                     if Utils.Number_Of_Args (Request_Tag) > 0 then
                        declare
                           V : Wx.Request_Child_Vectors.Vector;
                        begin
                           Put_Line (File, "procedure " & Subprogram_Name & " (" & Name & " : " & Ptr_Name & ";");
                           for Child of Request_Tag.Children loop
                              case Child.Kind_Id is
                                 when Child_Dummy =>
                                    null;
                                 when Child_Description =>
                                    null;
                                 when Child_Arg =>
                                    if Child.Arg_Tag.Type_Attribute /= Type_New_Id then
                                       V.Append (Child);
                                    end if;
                              end case;
                           end loop;

                           for Child of V loop
                              case Child.Kind_Id is
                                 when Child_Dummy =>
                                    null;
                                 when Child_Description =>
                                    null;
                                 when Child_Arg =>
                                    Generate_Code_For_Arg (Child.Arg_Tag.all, Child = Request_Tag.Children.Last_Element);
                              end case;
                           end loop;
                           Put_Line (File, "   );");
                        end;
                     else
                        Put_Line (File, "procedure " & Subprogram_Name & " (" & Name & " : " & Ptr_Name & ");");
                     end if;
                  end if;
                  Put_Line (File, "");
               end Generate_Code_For_Subprogram_Declaration;

            begin
               for Child of Interface_Tag.Children loop
                  case Child.Kind_Id is
                     when Child_Dummy =>
                        null;
                     when Child_Description =>
                        null;
                     when Child_Request =>
                        Generate_Code_For_Subprogram_Declaration (Child.Request_Tag.all);
                     when Child_Event =>
                        null;
                     when Child_Enum =>
                        null;
                  end case;
               end loop;
            end Generate_Code_For_Requests;

         begin
            Generate_Code_For_Enums;
            Generate_Code_For_Subprogram_Ptrs;

            if Utils.Exists_Any_Event_Tag (Interface_Tag) then
               Generate_Code_For_Listener_Type_Definition;
               Generate_Code_For_Add_Listener_Subprogram_Declaration;
            end if;

            Generate_Code_For_Set_User_Data_Subprogram_Declaration;
            Generate_Code_For_Get_User_Data_Subprogram_Declaration;
            Generate_Code_For_Get_Version_Subprogram_Declaration;
            Generate_Code_For_Destroy_Subprogram_Declaration;
            Generate_Code_For_Requests;
         end Handle_Interface;

      begin
         for Child of Protocol_Tag.Children loop
            case Child.Kind_Id is
               when Child_Dummy =>
                  null;
               when Child_Copyright =>
                  null;
               when Child_Interface =>
                  Handle_Interface (Child.Interface_Tag.all);
            end case;
         end loop;
      end Generate_Code_For_Each_Interface;

   begin
      Create_File;
      Create_Wl_Thin_Body_File;
   end Create_Wl_Thin_Spec_File;

   procedure Create_Wl_Thin_Body_File is

      File : Ada.Text_IO.File_Type;

      procedure Write_To_File;

      procedure Create_File is
      begin
         Ada.Text_IO.Create (File => File, Mode => Ada.Text_IO.Out_File, Name => "wl_thin.adb");

         Write_To_File;

         Ada.Text_IO.Close (File);
      end Create_File;

      pragma Unmodified (File);

      procedure Generate_Code_For_Protocol_Tag_Children;

      procedure Write_To_File is

      begin
         Put_Line (File, "-- Auto generated from Wayland.xml");
         Put_Line (File, "package body Wl_Thin is");
         Put_Line (File, "");
         Put_Line (File, "   procedure wl_proxy_destroy (Registry : Proxy_Ptr) with");
         Put_Line (File, "     Convention    => C,");
         Put_Line (File, "     Import        => True,");
         Put_Line (File, "     External_Name => ""wl_proxy_destroy"";");
         Put_Line (File, "");
         Put_Line (File, "function Display_Connect (Name : Interfaces.C.Strings.char_array_access) return Display_Ptr is");
         Put_Line (File, "");
         Put_Line (File, "function wl_display_connect (Name : in Interfaces.C.Strings.chars_ptr) return Display_Ptr with");
         Put_Line (File, "        Convention    => C,");
         Put_Line (File, "        Import        => True,");
         Put_Line (File, "        External_Name => ""wl_display_connect"";");
         Put_Line (File, "");
         Put_Line (File, "   begin");
         Put_Line (File, "      return wl_display_connect(Interfaces.C.Strings.To_Chars_Ptr (Name));");
         Put_Line (File, "   end Display_Connect;");
         Put_Line (File, "");
         Put_Line (File, "   procedure Display_Disconnect (This : in out Display_Ptr) is");
         Put_Line (File, "");
         Put_Line (File, "      procedure wl_display_disconnect (Display : in Display_Ptr) with");
         Put_Line (File, "        Convention => C,");
         Put_Line (File, "        Import     => True;");
         Put_Line (File, "");
         Put_Line (File, "   begin");
         Put_Line (File, "      if This /= null then");
         Put_Line (File, "         wl_display_disconnect (This);");
         Put_Line (File, "         This := null;");
         Put_Line (File, "      end if;");
         Put_Line (File, "   end Display_Disconnect;");
         Put_Line (File, "   ");
         Put_Line (File, "   function wl_proxy_add_listener");
         Put_Line (File, "     (arg1 : Proxy_Ptr;");
         Put_Line (File, "      arg2 : Void_Ptr;");
         Put_Line (File, "      arg3 : Void_Ptr) return Interfaces.C.int with");
         Put_Line (File, "     Import        => True,");
         Put_Line (File, "     Convention    => C,");
         Put_Line (File, "     External_Name => ""wl_proxy_add_listener"";");
         Put_Line (File, "");
         Put_Line (File, "   procedure wl_proxy_set_user_data");
         Put_Line (File, "     (arg1 : Proxy_Ptr;");
         Put_Line (File, "      arg3 : Void_Ptr) with");
         Put_Line (File, "     Import        => True,");
         Put_Line (File, "     Convention    => C,");
         Put_Line (File, "     External_Name => ""wl_proxy_set_user_data"";");
         Put_Line (File, "");
         Put_Line (File, "   function wl_proxy_get_user_data");
         Put_Line (File, "     (arg1 : Proxy_Ptr) return Void_Ptr with");
         Put_Line (File, "     Import        => True,");
         Put_Line (File, "     Convention    => C,");
         Put_Line (File, "     External_Name => ""wl_proxy_get_user_data"";");
         Put_Line (File, "");
         Put_Line (File, "   function wl_proxy_get_version");
         Put_Line (File, "     (arg1 : Proxy_Ptr) return Interfaces.Unsigned_32 with");
         Put_Line (File, "     Import        => True,");
         Put_Line (File, "     Convention    => C,");
         Put_Line (File, "     External_Name => ""wl_proxy_get_version"";");
         Put_Line (File, "");

         Generate_Code_For_Protocol_Tag_Children;

         Put_Line (File, "");
         Put_Line (File, "end Wl_Thin;");
      end Write_To_File;

      procedure Generate_Code_For_Protocol_Tag_Children is

         procedure Handle_Interface (Interface_Tag : Wx.Interface_Tag) is

            procedure Generate_Code_For_Add_Listener_Subprogram_Implementations is
               Name              : constant String := Utils.Adaify_Name (Interface_Tag.Name);
               Function_Name     : constant String := Name & "_Add_Listener";
               Ptr_Name          : constant String := Utils.Adaify_Name (Interface_Tag.Name & "_Ptr");
               Ptr_Listener_Name : constant String := Utils.Adaify_Name (Interface_Tag.Name & "_Listener_Ptr");
            begin
               Put_Line (File, "function " & Function_Name & " (" & Name & " : " & Ptr_Name & ";");
               Put_Line (File, "Listener : " & Ptr_Listener_Name & ";");
               Put_Line (File, "Data : Void_Ptr) return Interfaces.C.int is");
               Put_Line (File, "begin");
               Put_Line (File, "return wl_proxy_add_listener (" & Name & ".all'Access, Listener.all'Address, Data);");
               Put_Line (File, "end " & Function_Name & ";");
               Put_Line (File, "");
            end Generate_Code_For_Add_Listener_Subprogram_Implementations;

            procedure Generate_Code_For_Set_User_Data_Subprogram_Implementations is
               Name            : constant String := Utils.Adaify_Name (Interface_Tag.Name);
               Subprogram_Name : constant String := Name & "_Set_User_Data";
               Ptr_Name        : constant String := Utils.Adaify_Name (Interface_Tag.Name & "_Ptr");
            begin
               Put_Line (File, "procedure " & Subprogram_Name & " (" & Name & " : " & Ptr_Name & ";");
               Put_Line (File, "Data : Void_Ptr) is");
               Put_Line (File, "begin");
               Put_Line (File, "wl_proxy_set_user_data (" & Name & ".all'Access, Data);");
               Put_Line (File, "end " & Subprogram_Name & ";");
               Put_Line (File, "");
            end Generate_Code_For_Set_User_Data_Subprogram_Implementations;

            procedure Generate_Code_For_Get_User_Data_Subprogram_Implementations is
               Name            : constant String := Utils.Adaify_Name (Interface_Tag.Name);
               Subprogram_Name : constant String := Name & "_Get_User_Data";
               Ptr_Name        : constant String := Utils.Adaify_Name (Interface_Tag.Name & "_Ptr");
            begin
               Put_Line (File, "function " & Subprogram_Name & " (" & Name & " : " & Ptr_Name & ") return Void_Ptr is");
               Put_Line (File, "begin");
               Put_Line (File, "return wl_proxy_get_user_data (" & Name & ".all'Access);");
               Put_Line (File, "end " & Subprogram_Name & ";");
               Put_Line (File, "");
            end Generate_Code_For_Get_User_Data_Subprogram_Implementations;

            procedure Generate_Code_For_Get_Version_Subprogram_Implementations is
               Name            : constant String := Utils.Adaify_Name (Interface_Tag.Name);
               Subprogram_Name : constant String := Name & "_Get_Version";
               Ptr_Name        : constant String := Utils.Adaify_Name (Interface_Tag.Name & "_Ptr");
            begin
               Put_Line (File, "function " & Subprogram_Name & " (" & Name & " : " & Ptr_Name & ") return Interfaces.Unsigned_32 is");
               Put_Line (File, "begin");
               Put_Line (File, "return wl_proxy_get_version (" & Name & ".all'Access);");
               Put_Line (File, "end " & Subprogram_Name & ";");
               Put_Line (File, "");
            end Generate_Code_For_Get_Version_Subprogram_Implementations;

            procedure Generate_Code_For_Destroy_Subprogram_Implementations is
               Name            : constant String := Utils.Adaify_Name (Interface_Tag.Name);
               Subprogram_Name : constant String := Name & "_Destroy";
               Ptr_Name        : constant String := Utils.Adaify_Name (Interface_Tag.Name & "_Ptr");
            begin
               if Utils.Exists_Destructor (Interface_Tag) then
                  Put_Line (File, "procedure " & Subprogram_Name & " (" & Name & " : " & Ptr_Name & ") is");
                  Put_Line (File, "begin");
                  Put_Line (File, "Proxy_Marshal (Proxy_Ptr'(" & Name & ".all'Access),");
                  Put_Line (File, "     " & Utils.Make_Upper_Case (Interface_Tag.Name & "_Destroy") & ");");
                  Put_Line (File, "");
                  Put_Line (File, "wl_proxy_destroy (" & Name & ".all'Access);");
                  Put_Line (File, "end " & Subprogram_Name & ";");
                  Put_Line (File, "");
               else
                  Put_Line (File, "procedure " & Subprogram_Name & " (" & Name & " : " & Ptr_Name & ") is");
                  Put_Line (File, "begin");
                  Put_Line (File, "wl_proxy_destroy (" & Name & ".all'Access);");
                  Put_Line (File, "end " & Subprogram_Name & ";");
                  Put_Line (File, "");
               end if;
            end Generate_Code_For_Destroy_Subprogram_Implementations;

            procedure Generate_Code_For_Requests is

               procedure Generate_Code_For_Subprogram_Implementation (Request_Tag : Wx.Request_Tag) is

                  procedure Generate_Code_For_Arg (Arg_Tag : Wx.Arg_Tag; Is_Last : Boolean) is
                  begin
                     declare
                        Arg_Name      : constant String := Utils.Adaify_Variable_Name (Arg_Tag.Name);
                        Arg_Type_Name : constant String := Utils.Arg_Type_As_String (Arg_Tag);
                     begin
                        if Is_Last then
                           Put_Line (File, "     " & Arg_Name & " : " & Arg_Type_Name);
                        else
                           Put_Line (File, "     " & Arg_Name & " : " & Arg_Type_Name & ";");
                        end if;
                     end;
                  end Generate_Code_For_Arg;

                  Opcode          : constant String := Utils.Make_Upper_Case (Interface_Tag.Name & "_" & Request_Tag.Name);
                  Subprogram_Name : constant String := Utils.Adaify_Name (Interface_Tag.Name & "_" & Request_Tag.Name);
                  Name            : constant String := Utils.Adaify_Name (Interface_Tag.Name);
                  Ptr_Name        : constant String := Utils.Adaify_Name (Interface_Tag.Name & "_Ptr");
               begin
                  if Utils.Is_New_Id_Argument_Present (Request_Tag) then
                     if Utils.Is_Interface_Specified (Request_Tag) then
                        declare
                           Return_Type : constant String := Utils.Adaify_Name (Utils.Find_Specified_Interface (Request_Tag) & "_Ptr");
                        begin
                           if Utils.Number_Of_Args (Request_Tag) > 1 then
                              declare
                                 V : Wx.Request_Child_Vectors.Vector;
                              begin
                                 Put_Line (File, "function " & Subprogram_Name & " (" & Name & " : " & Ptr_Name & ";");
                                 for Child of Request_Tag.Children loop
                                    case Child.Kind_Id is
                                       when Child_Dummy =>
                                          null;
                                       when Child_Description =>
                                          null;
                                       when Child_Arg =>
                                          if Child.Arg_Tag.Type_Attribute /= Type_New_Id then
                                             V.Append (Child);
                                          end if;
                                    end case;
                                 end loop;

                                 for Child of V loop
                                    case Child.Kind_Id is
                                       when Child_Dummy =>
                                          null;
                                       when Child_Description =>
                                          null;
                                       when Child_Arg =>
                                          Generate_Code_For_Arg (Child.Arg_Tag.all, Child = Request_Tag.Children.Last_Element);
                                    end case;
                                 end loop;

                                 Put_Line (File, "   ) return " & Return_Type & " is");
                                 Put_Line (File, "P : Proxy_Ptr := Proxy_Marshal_Constructor (" & Name & ".all'Access,");
                                 Put_Line (File, "    " & Utils.Make_Upper_Case (Interface_Tag.Name & "_" & Request_Tag.Name) & ",");
                                 Put_Line
                                   (File,
                                    "    " & Utils.Adaify_Name (Utils.Find_Specified_Interface (Request_Tag)) & "_Interface'Access,");
                                 Ada.Text_IO.Put (File, "    0");

                                 for Child of V loop
                                    case Child.Kind_Id is
                                       when Child_Dummy =>
                                          null;
                                       when Child_Description =>
                                          null;
                                       when Child_Arg =>
                                          Put_Line (File, ",");
                                          if Child.Arg_Tag.Type_Attribute /= Type_Object then
                                             Ada.Text_IO.Put (File, "    " & Utils.Adaify_Name (Child.Arg_Tag.Name));
                                          else
                                             Ada.Text_IO.Put (File, "    " & Utils.Adaify_Name (Child.Arg_Tag.Name) & ".all'Address");
                                          end if;
                                    end case;
                                 end loop;

                                 Put_Line (File, "    );");
                                 Put_Line (File, "begin");
                                 Put_Line (File, "    return (if P /= null then P.all'Access else null);");
                                 Put_Line (File, "end " & Subprogram_Name & ";");

                              end;
                           else
                              Put_Line
                                (File,
                                 "function " & Subprogram_Name & " (" & Name & " : " & Ptr_Name & ") return " & Return_Type & " is");
                              Put_Line (File, "P : Proxy_Ptr := Proxy_Marshal_Constructor (" & Name & ".all'Access,");
                              Put_Line (File, "    " & Utils.Make_Upper_Case (Interface_Tag.Name & "_" & Request_Tag.Name) & ",");
                              Put_Line
                                (File,
                                 "    " & Utils.Adaify_Name (Utils.Find_Specified_Interface (Request_Tag)) & "_Interface'Access,");
                              Put_Line (File, "    0);");
                              Put_Line (File, "begin");
                              Put_Line (File, "    return (if P /= null then P.all'Access else null);");
                              Put_Line (File, "end " & Subprogram_Name & ";");
                           end if;
                        end;
                     else
                        if Utils.Number_Of_Args (Request_Tag) > 1 then
                           declare
                              V : Wx.Request_Child_Vectors.Vector;
                           begin
                              Put_Line (File, "function " & Subprogram_Name & " (" & Name & " : " & Ptr_Name & ";");

                              for Child of Request_Tag.Children loop
                                 case Child.Kind_Id is
                                    when Child_Dummy =>
                                       null;
                                    when Child_Description =>
                                       null;
                                    when Child_Arg =>
                                       if Child.Arg_Tag.Type_Attribute /= Type_New_Id then
                                          V.Append (Child);
                                       end if;
                                 end case;
                              end loop;

                              for Child of V loop
                                 case Child.Kind_Id is
                                    when Child_Dummy =>
                                       null;
                                    when Child_Description =>
                                       null;
                                    when Child_Arg =>
                                       Generate_Code_For_Arg (Child.Arg_Tag.all, False);
                                 end case;
                              end loop;

                              Put_Line (File, "   Interface_V : Interface_Ptr;");
                              Put_Line (File, "   New_Id : Interfaces.Unsigned_32) return Proxy_Ptr is");
                              Put_Line (File, "begin");
                              Put_Line (File, "    return Proxy_Marshal_Constructor_Versioned (" & Name & ".all'Access,");
                              Put_Line (File, "    " & Utils.Make_Upper_Case (Interface_Tag.Name & "_" & Request_Tag.Name) & ",");
                              Put_Line (File, "    Interface_V,");
                              Put_Line (File, "    New_Id,");

                              for Child of V loop
                                 case Child.Kind_Id is
                                    when Child_Dummy =>
                                       null;
                                    when Child_Description =>
                                       null;
                                    when Child_Arg =>
                                       if Child.Arg_Tag.Type_Attribute /= Type_Object then
                                          Put_Line (File, "    " & Utils.Adaify_Variable_Name (Child.Arg_Tag.Name) & ",");
                                       else
                                          Put_Line (File, "    " & Utils.Adaify_Variable_Name (Child.Arg_Tag.Name) & ".all'Address,");
                                       end if;
                                 end case;
                              end loop;

                              Put_Line (File, "    Interface_V.Name,");
                              Put_Line (File, "    New_Id,");
                              Put_Line (File, "    0);");
                              Put_Line (File, "end " & Subprogram_Name & ";");
                           end;
                        else
                           Put_Line (File, "function " & Subprogram_Name & " (" & Name & " : " & Ptr_Name & ") return Proxy_Ptr is");
                           Put_Line (File, "P : Proxy_Ptr := Proxy_Marshal_Constructor (" & Name & ".all'Access,");
                           Put_Line (File, "    " & Utils.Make_Upper_Case (Interface_Tag.Name & "_" & Request_Tag.Name) & ",");
                           Put_Line
                             (File,
                              "    " & Utils.Adaify_Name (Utils.Find_Specified_Interface (Request_Tag)) & "_Interface'Access,");
                           Put_Line (File, "    0);");
                           Put_Line (File, "begin");
                           Put_Line (File, "    return (if P /= null then P.all'Access else null);");
                           Put_Line (File, "end " & Subprogram_Name & ";");
                        end if;
                     end if;
                  elsif Utils.Is_Request_Destructor (Request_Tag) then
                     null; -- Body is generated in Generate_Code_For_Destroy_Subprogram_Implementation
                  else
                     if Utils.Number_Of_Args (Request_Tag) > 0 then
                        declare
                           V : Wx.Request_Child_Vectors.Vector;
                        begin
                           Put_Line (File, "procedure " & Subprogram_Name & " (" & Name & " : " & Ptr_Name & ";");
                           for Child of Request_Tag.Children loop
                              case Child.Kind_Id is
                                 when Child_Dummy =>
                                    null;
                                 when Child_Description =>
                                    null;
                                 when Child_Arg =>
                                    if Child.Arg_Tag.Type_Attribute /= Type_New_Id then
                                       V.Append (Child);
                                    end if;
                              end case;
                           end loop;

                           for Child of V loop
                              case Child.Kind_Id is
                                 when Child_Dummy =>
                                    null;
                                 when Child_Description =>
                                    null;
                                 when Child_Arg =>
                                    Generate_Code_For_Arg (Child.Arg_Tag.all, Child = Request_Tag.Children.Last_Element);
                              end case;
                           end loop;

                           Put_Line (File, "   ) is");
                           Put_Line (File, "begin");
                           Put_Line (File, "Proxy_Marshal (Proxy_Ptr'(" & Name & ".all'Access),");
                           Ada.Text_IO.Put (File, "    " & Opcode);

                           for Child of V loop
                              case Child.Kind_Id is
                                 when Child_Dummy =>
                                    null;
                                 when Child_Description =>
                                    null;
                                 when Child_Arg =>
                                    Put_Line (File, ",");
                                    if Child.Arg_Tag.Type_Attribute /= Type_Object then
                                       Ada.Text_IO.Put (File, "    " & Utils.Adaify_Name (Child.Arg_Tag.Name));
                                    else
                                       Ada.Text_IO.Put (File, "    " & Utils.Adaify_Name (Child.Arg_Tag.Name) & ".all'Address");
                                    end if;
                              end case;
                           end loop;
                           Put_Line (File, "    );");
                           Put_Line (File, "end " & Subprogram_Name & ";");
                        end;
                     else
                        Put_Line (File, "procedure " & Subprogram_Name & " (" & Name & " : " & Ptr_Name & ") is");
                        Put_Line (File, "begin");
                        Put_Line (File, "Proxy_Marshal (Proxy_Ptr'(" & Name & ".all'Access), " & Opcode & ");");
                        Put_Line (File, "end " & Subprogram_Name & ";");
                     end if;
                  end if;

                  Put_Line (File, "");
               end Generate_Code_For_Subprogram_Implementation;

            begin
               for Child of Interface_Tag.Children loop
                  case Child.Kind_Id is
                     when Child_Dummy =>
                        null;
                     when Child_Description =>
                        null;
                     when Child_Request =>
                        Generate_Code_For_Subprogram_Implementation (Child.Request_Tag.all);
                     when Child_Event =>
                        null;
                     when Child_Enum =>
                        null;
                  end case;
               end loop;
            end Generate_Code_For_Requests;

         begin
            if Utils.Exists_Any_Event_Tag (Interface_Tag) then
               Generate_Code_For_Add_Listener_Subprogram_Implementations;
            end if;
            Generate_Code_For_Set_User_Data_Subprogram_Implementations;
            Generate_Code_For_Get_User_Data_Subprogram_Implementations;
            Generate_Code_For_Get_Version_Subprogram_Implementations;
            Generate_Code_For_Destroy_Subprogram_Implementations;
            Generate_Code_For_Requests;
         end Handle_Interface;

      begin
         for Child of Protocol_Tag.Children loop
            case Child.Kind_Id is
               when Child_Dummy =>
                  null;
               when Child_Copyright =>
                  null;
               when Child_Interface =>
                  Handle_Interface (Child.Interface_Tag.all);
            end case;
         end loop;
      end Generate_Code_For_Protocol_Tag_Children;

   begin
      Create_File;
   end Create_Wl_Thin_Body_File;

begin
   Check_Wayland_XML_File_Exists;
exception
   when Ada.Text_IO.Name_Error =>
      Put_Line ("Could not find file!");
   when Unknown_Exception : others =>
      Put_Line (Ada.Exceptions.Exception_Information (Unknown_Exception));
end XML_Parser;
