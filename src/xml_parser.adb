with Ada.Text_IO;
with Ada.Exceptions;
with Aida.Deepend_XML_DOM_Parser;
with Aida.Text_IO;
with Aida.Subprogram_Call_Result;
with Dynamic_Pools;
with Ada.Directories;
with Aida.Sequential_Stream_IO;

procedure XML_Parser is

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

   procedure Parse_Contents is
      Call_Result : Aida.Subprogram_Call_Result.T;

      Parser : Aida.Deepend_XML_DOM_Parser.DOM_Parser_T;

      Root_Node : Aida.Deepend_XML_DOM_Parser.Node_Ptr;
   begin
      Parser.Parse (Subpool, File_Contents.all, Call_Result, Root_Node);

      if Call_Result.Has_Failed then
         Aida.Text_IO.Put_Line (Call_Result.Message);
      else
         Aida.Text_IO.Put_Line (Root_Node.Tag.Name);
      end if;
   end Parse_Contents;

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
