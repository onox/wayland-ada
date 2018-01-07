with Ada.Text_IO;
with Ada.Exceptions;
with Aida.Deepend_XML_DOM_Parser;
with Aida.Text_IO;
with Aida.Subprogram_Call_Result;
with Dynamic_Pools;
with Ada.Directories;
with Aida.Sequential_Stream_IO;
with Ada.Containers;
with Ada.Strings.Fixed;

with Wayland_XML.Protocol_Tag;
with Wayland_XML.Copyright_Tag;
with Wayland_XML.Interface_Tag;
with Wayland_XML.Description_Tag;
with Wayland_XML.Request_Tag;
with Wayland_XML.Arg_Tag;

procedure XML_Parser is

   use all type Aida.String_T;
   use all type Aida.Int32_T;

   use type Ada.Containers.Count_Type;

   use all type Aida.Deepend_XML_DOM_Parser.Node_Kind_Id_T;

   XML_Exception : exception;

   Default_Subpool : Dynamic_Pools.Dynamic_Pool renames Aida.Deepend_XML_DOM_Parser.Default_Subpool;

   File_Name : constant String := "wayland.xml";

   Allocation_Block_Size : constant := 1_000_000;

   Scoped_Subpool : constant Dynamic_Pools.Scoped_Subpool := Dynamic_Pools.Create_Subpool (Default_Subpool,
                                                                                           Allocation_Block_Size);

   Subpool : Dynamic_Pools.Subpool_Handle := Scoped_Subpool.Handle;

   procedure Allocate_Space_For_Wayland_XML_Contents;

   procedure Check_Wayland_XML_File_Exists is
   begin
      if Ada.Directories.Exists (File_Name) then
         Allocate_Space_For_Wayland_XML_Contents;
      else
         Ada.Text_IO.Put_Line ("Could not find " & File_Name & "!");
      end if;
   end Check_Wayland_XML_File_Exists;

   File_Size : Natural;

   File_Contents : Aida.Deepend_XML_DOM_Parser.String_Ptr;

   procedure Read_Contents_Of_Wayland_XML;

   procedure Allocate_Space_For_Wayland_XML_Contents is
   begin
      File_Size := Natural (Ada.Directories.Size (File_Name));

      if File_Size > 4 then
         File_Contents := new (Subpool) Aida.String_T (1..File_Size);
         Read_Contents_Of_Wayland_XML;
      else
         Ada.Text_IO.Put_Line ("File " & File_Name & " is too small!");
      end if;
   end Allocate_Space_For_Wayland_XML_Contents;

   pragma Unmodified (File_Size);
   pragma Unmodified (File_Contents);

   procedure Parse_Contents;

   procedure Read_Contents_Of_Wayland_XML is
   begin
      declare
         File : Aida.Sequential_Stream_IO.File_Type;
         SE : Aida.Sequential_Stream_IO.Stream_Element;
      begin
         Aida.Sequential_Stream_IO.Open (File => File,
                                         Mode => Aida.Sequential_Stream_IO.In_File,
                                         Name => File_Name);

         for I in File_Contents.all'First..File_Contents.all'Last loop
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

   Protocol_Tag : Wayland_XML.Protocol_Tag.Protocol_Tag_Ptr;

   procedure Identify_Protocol_Children;

   procedure Identify_Protocol_Tag is
   begin
      if Root_Node.Id = XML_Tag and then Root_Node.Tag.Name = "protocol" then
         Protocol_Tag := new (Subpool) Wayland_XML.Protocol_Tag.Protocol_Tag_T;
         if Root_Node.Tag.Attributes.Length = 1 and then Root_Node.Tag.Attributes.Element (1).all.Name = "name" then
            Protocol_Tag.Set_Name (Root_Node.Tag.Attributes.Element (1).all.Value, Subpool);
            Identify_Protocol_Children;
         else
            Aida.Text_IO.Put_Line ("<protocol> node does not have name attribute?");
         end if;
      else
         Aida.Text_IO.Put_Line ("Root node is not <protocol> ???");
      end if;
   end Identify_Protocol_Tag;

   pragma Unmodified (Protocol_Tag);

   procedure Identify_Protocol_Children is

      function Identify_Copyright (Node : not null Aida.Deepend_XML_DOM_Parser.Node_Ptr) return not null Wayland_XML.Copyright_Tag.Copyright_Ptr is
         Copyright_Tag : not null Wayland_XML.Copyright_Tag.Copyright_Ptr :=
           new (Subpool) Wayland_XML.Copyright_Tag.Copyright_Tag_T;
      begin
         if Node.Tag.Child_Nodes.Length = 1 then
            if Node.Tag.Child_Nodes.Element (1).Id = XML_Text then
               Copyright_Tag.Set_Text (Aida.String_T (Ada.Strings.Fixed.Trim (Standard.String (Node.Tag.Child_Nodes.Element (1).Text), Ada.Strings.Both)),
                                       Subpool);
            else
               raise XML_Exception;
            end if;
         else
            raise XML_Exception;
         end if;
         return Copyright_Tag;
      end Identify_Copyright;

      function Identify_Description (Node : not null Aida.Deepend_XML_DOM_Parser.Node_Ptr) return not null Wayland_XML.Description_Tag.Description_Tag_Ptr is
         Description_Tag : not null Wayland_XML.Description_Tag.Description_Tag_Ptr :=
           new (Subpool) Wayland_XML.Description_Tag.Description_Tag_T;
      begin
         if Node.Tag.Attributes.Length = 1 then
            if Node.Tag.Attributes.Element (1).Name = "summary" then
               Description_Tag.Set_Summary (Node.Tag.Attributes.Element (1).Value,
                                            Subpool);
            else
               raise XML_Exception;
            end if;
         else
            raise XML_Exception;
         end if;

         if Node.Tag.Child_Nodes.Length = 1 then
            if Node.Tag.Child_Nodes.Element (1).Id = XML_Text then
               Description_Tag.Set_Text (Aida.String_T (Ada.Strings.Fixed.Trim (Standard.String (Node.Tag.Child_Nodes.Element (1).Text), Ada.Strings.Both)),
                                         Subpool);
            else
               raise XML_Exception;
            end if;
         elsif Node.Tag.Child_Nodes.Length > 1 then
            raise XML_Exception;
         end if;

         return Description_Tag;
      end Identify_Description;

      function Identify_Arg (Node : not null Aida.Deepend_XML_DOM_Parser.Node_Ptr) return not null Wayland_XML.Arg_Tag.Arg_Tag_Ptr is
         Arg_Tag : not null Wayland_XML.Arg_Tag.Arg_Tag_Ptr:=
           new (Subpool) Wayland_XML.Arg_Tag.Arg_Tag_T;
      begin
         for A of Node.Tag.Attributes loop
            if A.Name = "name" then
               Arg_Tag.Set_Name (A.Value,
                                 Subpool);
            elsif A.Name = "type" then
               Arg_Tag.Set_Type_Attribute (A.Value,
                                           Subpool);
            elsif A.Name = "summary" then
               Arg_Tag.Set_Summary (A.Value,
                                    Subpool);
            elsif A.Name = "interface" then
               Arg_Tag.Set_Interface_Attribute (A.Value,
                                                Subpool);
            elsif A.Name = "allow-null" then
               if A.Value ="true" then
                  Arg_Tag.Set_Allow_Null (True);
               elsif A.Value ="false" then
                  Arg_Tag.Set_Allow_Null (False);
               else
                  raise XML_Exception;
               end if;
            elsif A.Name = "enum" then
               Arg_Tag.Set_Enum (A.Value,
                                 Subpool);
            else
               raise XML_Exception;
            end if;
         end loop;

         return Arg_Tag;
      end Identify_Arg;

      function Identify_Request (Node : not null Aida.Deepend_XML_DOM_Parser.Node_Ptr) return not null Wayland_XML.Request_Tag.Request_Tag_Ptr is
         Request_Tag : not null Wayland_XML.Request_Tag.Request_Tag_Ptr :=
           new (Subpool) Wayland_XML.Request_Tag.Request_Tag_T;
      begin
         for A of Node.Tag.Attributes loop
            if A.Name = "name" then
               Request_Tag.Set_Name (A.Value,
                                     Subpool);
            elsif A.Name = "type" then
               Request_Tag.Set_Type_Attribute (A.Value,
                                               Subpool);
            elsif A.Name = "since" then
               declare
                  Value : Aida.Int32_T;
                  Has_Failed : Boolean;
               begin
                  Aida.To_Int32 (A.Value, Value, Has_Failed);

                  if Has_Failed then
                     raise XML_Exception;
                  else
                     Request_Tag.Set_Since (Wayland_XML.Request_Tag.Version_T (Value));
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

      function Identify_Interface (Node : not null Aida.Deepend_XML_DOM_Parser.Node_Ptr) return not null Wayland_XML.Interface_Tag.Interface_Tag_Ptr is
         Interface_Tag : not null Wayland_XML.Interface_Tag.Interface_Tag_Ptr :=
           new (Subpool) Wayland_XML.Interface_Tag.Interface_Tag_T;
      begin
         if Node.Tag.Attributes.Length = 2 then
            if Node.Tag.Attributes.Element (1).Name = "name" then
               Interface_Tag.Set_Name (Node.Tag.Attributes.Element (1).Value,
                                       Subpool);
            else
               raise XML_Exception;
            end if;

            if Node.Tag.Attributes.Element (2).Name = "version" then
               declare
                  Value : Aida.Int32_T;
                  Has_Failed : Boolean;
               begin
                  To_Int32 (Node.Tag.Attributes.Element (2).Value,
                            Value,
                            Has_Failed);

                  if Has_Failed then
                     raise XML_Exception;
                  else
                     Interface_Tag.Set_Version (Wayland_XML.Interface_Tag.Version_T (Value));

                     for Child of Node.Tag.Child_Nodes loop
                        if Child.Id = XML_Tag then
                           if Child.Tag.Name = "description" then
                              Interface_Tag.Append_Child (Identify_Description (Child));
                           elsif Child.Tag.Name = "request" then
                              Interface_Tag.Append_Child (Identify_Request (Child));
                           elsif Child.Tag.Name = "event" then
                              null;
                           elsif Child.Tag.Name = "enum" then
                              null;
                           else
                              raise XML_Exception;
                           end if;
                        elsif Child.Id = XML_Comment then
                           null;
                        else
                           raise XML_Exception with String (Child.Id'Img);
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
   end Identify_Protocol_Children;

begin
   Check_Wayland_XML_File_Exists;

--     Parser.Parse (Contents,
--                   Xcb,
--                   Error_Message,
--                   Is_Success);
--
--     if Is_Success then
--        Ada.Text_IO.Put_Line ("Successfully parsed " & File_Name & "! Will create xcb.ads");
--        Creator.Create_XCB_Package (Xcb.all);
--     else
--        Ada.Text_IO.Put_Line (To_String (Error_Message));
--     end if;
   null;
exception
   when Ada.Text_IO.Name_Error =>
      Ada.Text_IO.Put_Line ("Could not find file!");
   when Unknown_Exception : others =>
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (Unknown_Exception));
end XML_Parser;
