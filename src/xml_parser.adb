with Ada.Direct_IO;
with Ada.Text_IO;
with Ada.Exceptions;
with Aida.Deepend_XML_DOM_Parser;
with Aida.Text_IO;
with Aida.Subprogram_Call_Result;
with Dynamic_Pools;

procedure XML_Parser is

   File_Name : constant String := "wayland.xml";

   function Determine_File_Size return Natural is

      type Char_T is new Character;
      for Char_T'Size use 8;

      package DIO is new Ada.Direct_IO (Char_T);

      File : DIO.File_Type;

      C : DIO.Count;
   begin
      DIO.Open (File => File,
                Mode => DIO.In_File,
                Name => File_Name);

      C := DIO.Size (File);

      DIO.Close (File);
      return Natural (C);
   end Determine_File_Size;

   File_Size : constant Natural := Determine_File_Size;

   subtype File_String    is String (1 .. File_Size);
   package File_String_IO is new Ada.Direct_IO (File_String);

   File : File_String_IO.File_Type;

   type Contents_Ptr is not null access File_String;

   Contents : Contents_Ptr := new File_String;

   Call_Result : Aida.Subprogram_Call_Result.T;

   Scoped_Subpool : Dynamic_Pools.Scoped_Subpool := Dynamic_Pools.Create_Subpool (Aida.Deepend_XML_DOM_Parser.Default_Subpool);

   Subpool : Dynamic_Pools.Subpool_Handle := Scoped_Subpool.Handle;

   Parser : Aida.Deepend_XML_DOM_Parser.DOM_Parser_T;

   Root_Node : Aida.Deepend_XML_DOM_Parser.Node_Ptr;
begin
   File_String_IO.Open  (File, Mode => File_String_IO.In_File,
                         Name => File_Name);
   File_String_IO.Read  (File, Item => Contents.all);
   File_String_IO.Close (File);

   Parser.Parse (Subpool, Aida.String_T (Contents.all), Call_Result, Root_Node);

   if Call_Result.Has_Failed then
      Aida.Text_IO.Put_Line (Call_Result.Message);
   else
      Aida.Text_IO.Put_Line (Root_Node.Tag.Name);
   end if;


--     XProto_XML.Parser.Parse (Storage     => Globals.Storage,
--                              Max_Indices => Globals.Max_Indices,
--                              Contents    => Aida.String_T (Contents),
--                              Call_Result => Call_Result);
--
--     if Has_Failed (Call_Result) then
--        Aida.Text_IO.Put_Line (Message (Call_Result));
--     else
--        Aida.Text_IO.Put_Line ("Parsing of xml file successfull!!");
--     end if;

--   Aida.Text_IO.Put_Line (To_String (Globals.Storage.Header_Comment));

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
exception
   when File_String_IO.Name_Error =>
      Ada.Text_IO.Put_Line ("Could not find file!");
   when Unknown_Exception : others =>
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (Unknown_Exception));
end XML_Parser;
