with Ada.Text_IO;
with Ada.Exceptions;
with Aida.Deepend_XML_DOM_Parser;
with Aida.Text_IO;
with Aida.Subprogram_Call_Result;
with Dynamic_Pools;
with Ada.Directories;
with Aida.Sequential_Stream_IO;
with Aida.UTF8;
with Aida.UTF8_Code_Point;
with Ada.Containers;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with Wayland_XML.Protocol_Tag;
with Wayland_XML.Copyright_Tag;
with Wayland_XML.Interface_Tag;
with Wayland_XML.Description_Tag;
with Wayland_XML.Request_Tag;
with Wayland_XML.Arg_Tag;
with Wayland_XML.Event_Tag;
with Wayland_XML.Enum_Tag;
with Wayland_XML.Entry_Tag;

procedure XML_Parser is

   use type Ada.Containers.Count_Type;

   use type Wayland_XML.Event_Tag.Child_T;
   use type Wayland_XML.Request_Tag.Child_T;

   use all type Aida.Deepend_XML_DOM_Parser.Node_Kind_Id_T;

   use all type Ada.Strings.Unbounded.Unbounded_String;

   use all type Aida.UTF8_Code_Point.T;

   use all type Wayland_XML.Protocol_Tag.Child_Kind_Id_T;
   use all type Wayland_XML.Interface_Tag.Child_Kind_Id_T;
   use all type Wayland_XML.Enum_Tag.Child_Kind_Id_T;
   use all type Wayland_XML.Event_Tag.Child_Kind_Id_T;
   use all type Wayland_XML.Arg_Tag.Type_Attribute_T;
   use all type Wayland_XML.Request_Tag.Child_Kind_Id_T;

   XML_Exception : exception;

   Default_Subpool : Dynamic_Pools.Dynamic_Pool renames Aida.Deepend_XML_DOM_Parser.Default_Subpool;

   File_Name : constant String := "wayland.xml";

   Allocation_Block_Size : constant := 1_000_000;

   Scoped_Subpool : constant Dynamic_Pools.Scoped_Subpool := Dynamic_Pools.Create_Subpool (Default_Subpool,
                                                                                           Allocation_Block_Size);

   Subpool : Dynamic_Pools.Subpool_Handle := Scoped_Subpool.Handle;

   package Utils is

      function Adaify_Name (Old_Name : String) return String;

      -- If the variable name is "Interface" then it will be recognized as
      -- a reserved word in Ada and it will be suffixed with "_V" resulting
      -- in "Interface_V".
      function Adaify_Variable_Name (Old_Name : String) return String;

      function Make_Upper_Case (Source : String) return String;

      function Arg_Type_As_String (Arg_Tag : Wayland_XML.Arg_Tag.Arg_Tag_T) return String;

      function Number_Of_Args (Request_Tag : Wayland_XML.Request_Tag.Request_Tag_T) return Aida.Nat32_T;

      function Is_New_Id_Argument_Present (Request_Tag : Wayland_XML.Request_Tag.Request_Tag_T) return Boolean;

      function Is_Interface_Specified (Request_Tag : Wayland_XML.Request_Tag.Request_Tag_T) return Boolean with
        Pre => Is_New_Id_Argument_Present (Request_Tag);

      Interface_Not_Found_Exception : exception;

      -- will raise Interface_Not_Found_Exception is pre-condition is not met.
      function Find_Specified_Interface (Request_Tag : Wayland_XML.Request_Tag.Request_Tag_T) return String with
        Pre => Is_Interface_Specified (Request_Tag);

      function Interface_Ptr_Name (Interface_Tag : Wayland_XML.Interface_Tag.Interface_Tag_T) return String;

      function Is_Request_Destructor (Request_Tag : Wayland_XML.Request_Tag.Request_Tag_T) return Boolean;

      function Exists_Destructor (Interface_Tag : Wayland_XML.Interface_Tag.Interface_Tag_T) return Boolean;

      function Exists_Any_Event_Tag (Interface_Tag : Wayland_XML.Interface_Tag.Interface_Tag_T) return Boolean;

   end Utils;

   package body Utils is

      -- If input is "wl_hello" then output is "hello". This procedure strips away the first
      -- three characters if they are "wl_".
      procedure Remove_Initial_Wl (New_Name : in out Ada.Strings.Unbounded.Unbounded_String) is
      begin
         if
           Length (New_Name) = 3 and then
           New_Name = "Wl_"
         then
            Set_Unbounded_String (New_Name, "");
         end if;
      end Remove_Initial_Wl;

      function Adaify_Variable_Name (Old_Name : String) return String is
         Result : String := Adaify_Name (Old_Name);
      begin
         if Result = "Interface" then
            return Result & "_V";
         else
            return Result;
         end if;
      end Adaify_Variable_Name;

      function Adaify_Name (Old_Name : String) return String is
         New_Name : Ada.Strings.Unbounded.Unbounded_String;

         P : Aida.Int32_T := Old_Name'First;

         CP : Aida.UTF8_Code_Point.T := 0;

         Is_Previous_Lowercase : Boolean := False;
         Is_Previous_A_Number  : Boolean := False;
         Is_Previous_An_Undercase  : Boolean := False;
      begin
         Aida.UTF8.Get (Source  => Old_Name,
                        Pointer => P,
                        Value   => CP);

         if Is_Uppercase (CP) then
            Append (New_Name, Image (CP));
         else
            Append (New_Name, Image (To_Uppercase (CP)));
         end if;

         while P <= Old_Name'Last loop
            Aida.UTF8.Get (Source  => Old_Name,
                           Pointer => P,
                           Value   => CP);

            if Image (CP) = "_" then
               Append (New_Name, "_");
               Remove_Initial_Wl (New_Name);
               Is_Previous_An_Undercase := True;
            else
               if Is_Digit (CP) then
                  if Is_Previous_A_Number then
                     Append (New_Name, Image (CP));
                  elsif Is_Previous_An_Undercase then
                     Append (New_Name, Image (CP));
                  else
                     Append (New_Name, "_");
                     Remove_Initial_Wl (New_Name);
                     Append (New_Name, Image (CP));
                  end if;

                  Is_Previous_A_Number := True;
               else
                  if Is_Uppercase (CP) then
                     if Is_Previous_An_Undercase then
                        Append (New_Name, Image (CP));
                     elsif Is_Previous_Lowercase then
                        Append (New_Name, "_");
                        Remove_Initial_Wl (New_Name);
                        Append (New_Name, Image (CP));
                        Is_Previous_Lowercase := False;
                     else
                        Append (New_Name, Image (To_Lowercase (CP)));
                     end if;
                  else
                     if Is_Previous_An_Undercase then
                        Append (New_Name, Image (To_Uppercase (CP)));
                     else
                        Append (New_Name, Image (CP));
                     end if;
                     Is_Previous_Lowercase := True;
                  end if;

                  Is_Previous_A_Number := False;
               end if;

               Is_Previous_An_Undercase := False;
            end if;

         end loop;


         if To_String (New_Name) = "Class_" then
            Set_Unbounded_String (New_Name, "Class_V");
--          To handle the following case:
--          <request name="set_class">
--            ...
--            <arg name="class_" type="string" summary="surface class"/>
--          </request>
--          Identifiers in Ada cannot end with underscore "_".
         end if;

         if To_String (New_Name) = "Delay" then
            Set_Unbounded_String (New_Name, "Delay_V");
--          To handle:
--          <arg name="delay" type="int" summary="delay in milliseconds since key down until repeating starts"/>
--          "delay" is a reserved word in Ada.
         end if;

         return To_String (New_Name);
      end Adaify_Name;

      function Make_Upper_Case (Source : String) return String is
         Target : Ada.Strings.Unbounded.Unbounded_String;

         P : Aida.Int32_T := Source'First;

         CP : Aida.UTF8_Code_Point.T := 0;
      begin
         Set_Unbounded_String (Target, "");
         while P <= Source'Last loop
            Aida.UTF8.Get (Source  => Source,
                           Pointer => P,
                           Value   => CP);

            if Image (CP) = "_" then
               Append (Target, "_");
            elsif Is_Digit (CP) then
               Append (Target, Image (CP));
            elsif Is_Lowercase (CP) then
               Append (Target, Image (To_Uppercase (CP)));
            else
               Append (Target, Image (CP));
            end if;

         end loop;

         return To_String (Target);
      end Make_Upper_Case;

      function Arg_Type_As_String (Arg_Tag : Wayland_XML.Arg_Tag.Arg_Tag_T) return String is
         N : Ada.Strings.Unbounded.Unbounded_String;
      begin
         case Arg_Tag.Type_Attribute is
            when Type_Integer          => Set_Unbounded_String (N, "Integer");
            when Type_Unsigned_Integer => Set_Unbounded_String (N, "Interfaces.Unsigned_32");
            when Type_String           => Set_Unbounded_String (N, "Interfaces.C.Strings.chars_ptr");
            when Type_FD               => Set_Unbounded_String (N, "Integer");
            when Type_New_Id           => Set_Unbounded_String (N, "Interfaces.Unsigned_32");
            when Type_Object           =>
               if Arg_Tag.Exists_Interface_Attribute then
                  Set_Unbounded_String (N, Utils.Adaify_Name (Arg_Tag.Interface_Attribute) & "_Ptr");
               else
                  Set_Unbounded_String (N, "Void_Ptr");
               end if;
            when Type_Fixed            => Set_Unbounded_String (N, "Fixed_T");
            when Type_Array            => Set_Unbounded_String (N, "Wayland_Array_T");
         end case;
         return To_String (N);
      end Arg_Type_As_String;

      function Number_Of_Args (Request_Tag : Wayland_XML.Request_Tag.Request_Tag_T) return Aida.Nat32_T is
         N : Aida.Nat32_T := 0;
      begin
         for Child of Request_Tag.Children loop
            if Child.Kind_Id = Child_Arg then
               N := N + 1;
            end if;
         end loop;
         return N;
      end Number_Of_Args;

      function Is_New_Id_Argument_Present (Request_Tag : Wayland_XML.Request_Tag.Request_Tag_T) return Boolean is
      begin
         for Child of Request_Tag.Children loop
            if
              Child.Kind_Id = Child_Arg and then
              Child.Arg_Tag.Exists_Type_Attribute and then
              Child.Arg_Tag.Type_Attribute = Type_New_Id
            then
               return True;
            end if;
         end loop;

         return False;
      end Is_New_Id_Argument_Present;

      function Is_Interface_Specified (Request_Tag : Wayland_XML.Request_Tag.Request_Tag_T) return Boolean is
      begin
         for Child of Request_Tag.Children loop
            if
              Child.Kind_Id = Child_Arg and then
              Child.Arg_Tag.Exists_Type_Attribute and then
              Child.Arg_Tag.Type_Attribute = Type_New_Id and then
              Child.Arg_Tag.Exists_Interface_Attribute
            then
               return True;
            end if;
         end loop;

         return False;
      end Is_Interface_Specified;

      function Find_Specified_Interface (Request_Tag : Wayland_XML.Request_Tag.Request_Tag_T) return String is
      begin
         for Child of Request_Tag.Children loop
            if
              Child.Kind_Id = Child_Arg and then
              Child.Arg_Tag.Exists_Type_Attribute and then
              Child.Arg_Tag.Type_Attribute = Type_New_Id and then
              Child.Arg_Tag.Exists_Interface_Attribute
            then
               return Child.Arg_Tag.Interface_Attribute;
            end if;
         end loop;

         raise Interface_Not_Found_Exception;
      end Find_Specified_Interface;

      function Interface_Ptr_Name (Interface_Tag : Wayland_XML.Interface_Tag.Interface_Tag_T) return String is
      begin
         return Utils.Adaify_Name (Interface_Tag.Name & "_Ptr");
      end Interface_Ptr_Name;

      function Is_Request_Destructor (Request_Tag : Wayland_XML.Request_Tag.Request_Tag_T) return Boolean is
         Result : Boolean := False;

         V : Wayland_XML.Request_Tag.Child_Vectors.Vector;
      begin
         for Child of Request_Tag.Children loop
            if
              Child.Kind_Id = Child_Arg
            then
               V.Append (Child);
            end if;
         end loop;

         if
           Request_Tag.Exists_Type_Attribute and then
           Request_Tag.Type_Attribute = "destructor" and then
           Request_Tag.Name = "destroy" and then
           V.Length = 0
         then
            Result := True;
         end if;

         return Result;
      end Is_Request_Destructor;

      function Exists_Destructor (Interface_Tag : Wayland_XML.Interface_Tag.Interface_Tag_T) return Boolean is
         Result : Boolean := False;
      begin
         for Child of Interface_Tag.Children loop
            case Child.Kind_Id is
               when Child_Dummy       => null;
               when Child_Description => null;
               when Child_Request     =>
                  if Utils.Is_Request_Destructor (Child.Request_Tag.all) then
                     Result := True;
                     exit;
                  end if;
               when Child_Event       => null;
               when Child_Enum        => null;
            end case;
         end loop;

         return Result;
      end Exists_Destructor;

      function Exists_Any_Event_Tag (Interface_Tag : Wayland_XML.Interface_Tag.Interface_Tag_T) return Boolean is
         Result : Boolean := False;
      begin
         for Child of Interface_Tag.Children loop
            case Child.Kind_Id is
               when Child_Dummy       => null;
               when Child_Description => null;
               when Child_Request     => null;
               when Child_Event       =>
                     Result := True;
                     exit;
               when Child_Enum        => null;
            end case;
         end loop;

         return Result;
      end Exists_Any_Event_Tag;

   end Utils;

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
         File_Contents := new (Subpool) String (1..File_Size);
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

   procedure Create_Wl_Thin_Spec_File;

   procedure Identify_Protocol_Children is

      function Identify_Copyright (Node : not null Aida.Deepend_XML_DOM_Parser.Node_Ptr) return not null Wayland_XML.Copyright_Tag.Copyright_Ptr is
         Copyright_Tag : not null Wayland_XML.Copyright_Tag.Copyright_Ptr :=
           new (Subpool) Wayland_XML.Copyright_Tag.Copyright_Tag_T;
      begin
         if Node.Tag.Child_Nodes.Length = 1 then
            if Node.Tag.Child_Nodes.Element (1).Id = XML_Text then
               Copyright_Tag.Set_Text (Ada.Strings.Fixed.Trim (Node.Tag.Child_Nodes.Element (1).Text, Ada.Strings.Both),
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
               Description_Tag.Set_Text (Ada.Strings.Fixed.Trim (Node.Tag.Child_Nodes.Element (1).Text, Ada.Strings.Both),
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
               Arg_Tag.Set_Type_Attribute (A.Value);
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
                  Aida.String.To_Int32 (A.Value, Value, Has_Failed);

                  if Has_Failed then
                     raise XML_Exception;
                  else
                     Request_Tag.Set_Since (Wayland_XML.Version_T (Value));
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

      function Identify_Event (Node : not null Aida.Deepend_XML_DOM_Parser.Node_Ptr) return not null Wayland_XML.Event_Tag.Event_Tag_Ptr is
         Event_Tag : not null Wayland_XML.Event_Tag.Event_Tag_Ptr :=
           new (Subpool) Wayland_XML.Event_Tag.Event_Tag_T;
      begin
         for A of Node.Tag.Attributes loop
            if A.Name = "name" then
               Event_Tag.Set_Name (A.Value,
                                   Subpool);
            elsif A.Name = "since" then
               declare
                  Value : Aida.Int32_T;
                  Has_Failed : Boolean;
               begin
                  Aida.String.To_Int32 (A.Value, Value, Has_Failed);

                  if Has_Failed then
                     raise XML_Exception;
                  else
                     Event_Tag.Set_Since_Attribute (Wayland_XML.Version_T (Value));
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

      function Identify_Entry (Node : not null Aida.Deepend_XML_DOM_Parser.Node_Ptr) return not null Wayland_XML.Entry_Tag.Entry_Tag_Ptr is
         Entry_Tag : not null Wayland_XML.Entry_Tag.Entry_Tag_Ptr :=
           new (Subpool) Wayland_XML.Entry_Tag.Entry_Tag_T;
      begin
         for A of Node.Tag.Attributes loop
            if A.Name = "name" then
               Entry_Tag.Set_Name (A.Value,
                                   Subpool);
            elsif A.Name = "value" then
               declare
                  Value : Aida.Int32_T;
                  Has_Failed : Boolean;
               begin
                  Aida.String.To_Int32 (A.Value, Value, Has_Failed);

                  if Has_Failed then
                     if A.Value (A.Value'First..A.Value'First + 1) = "0x" then
                        Value := Aida.Int32_T'Value ("16#" & String (A.Value (A.Value'First + 2..A.Value'Last)) & "#");

                        Entry_Tag.Set_Value (Wayland_XML.Entry_Tag.Value_T (Value));
                     else
                        raise XML_Exception;
                     end if;
                  else
                     Entry_Tag.Set_Value (Wayland_XML.Entry_Tag.Value_T (Value));
                  end if;
               end;
            elsif A.Name = "summary" then
               Entry_Tag.Set_Summary (A.Value,
                                      Subpool);
            elsif A.Name = "since" then
               declare
                  Value : Aida.Int32_T;
                  Has_Failed : Boolean;
               begin
                  Aida.String.To_Int32 (A.Value, Value, Has_Failed);

                  if Has_Failed then
                     raise XML_Exception;
                  else
                     Entry_Tag.Set_Since (Wayland_XML.Version_T (Value));
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

      function Identify_Enum (Node : not null Aida.Deepend_XML_DOM_Parser.Node_Ptr) return not null Wayland_XML.Enum_Tag.Enum_Tag_Ptr is
         Enum_Tag : not null Wayland_XML.Enum_Tag.Enum_Tag_Ptr :=
           new (Subpool) Wayland_XML.Enum_Tag.Enum_Tag_T;
      begin
         for A of Node.Tag.Attributes loop
            if A.Name = "name" then
               Enum_Tag.Set_Name (A.Value,
                                   Subpool);
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
                  Value : Aida.Int32_T;
                  Has_Failed : Boolean;
               begin
                  Aida.String.To_Int32 (A.Value, Value, Has_Failed);

                  if Has_Failed then
                     raise XML_Exception;
                  else
                     Enum_Tag.Set_Since (Wayland_XML.Version_T (Value));
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
                  Aida.String.To_Int32 (Node.Tag.Attributes.Element (2).Value,
                                        Value,
                                        Has_Failed);

                  if Has_Failed then
                     raise XML_Exception;
                  else
                     Interface_Tag.Set_Version (Wayland_XML.Version_T (Value));

                     for Child of Node.Tag.Child_Nodes loop
                        if Child.Id = XML_Tag then
                           if Child.Tag.Name = "description" then
                              Interface_Tag.Append_Child (Identify_Description (Child));
                           elsif Child.Tag.Name = "request" then
                              Interface_Tag.Append_Child (Identify_Request (Child));
                           elsif Child.Tag.Name = "event" then
                              Interface_Tag.Append_Child (Identify_Event (Child));
                           elsif Child.Tag.Name = "enum" then
                              Interface_Tag.Append_Child (Identify_Enum (Child));
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

      Create_Wl_Thin_Spec_File;
   end Identify_Protocol_Children;

   procedure Create_Wl_Thin_Body_File;

   procedure Create_Wl_Thin_Spec_File is

      File : Ada.Text_IO.File_Type;

      procedure Write_To_File;

      procedure Create_File is
      begin
         Ada.Text_IO.Create (File => File,
                             Mode => Ada.Text_IO.Out_File,
                             Name => "wl_thin.ads");

         Write_To_File;

         Ada.Text_IO.Close (File);
      end Create_File;

      pragma Unmodified (File);

      procedure Write_To_File is

         procedure Generate_Code_For_Numeric_Constants;

         procedure Start_Writing_To_File is
         begin
            Ada.Text_IO.Put_Line (File, "with Interfaces.C.Strings;");
            Ada.Text_IO.Put_Line (File, "with System;");
            Ada.Text_IO.Put_Line (File, "");
            Ada.Text_IO.Put_Line (File, "-- Auto generated from Wayland.xml except for the Wayland Core parts");
            Ada.Text_IO.Put_Line (File, "package Wl_Thin is");
            Ada.Text_IO.Put_Line (File, "");
            Ada.Text_IO.Put_Line (File, "-- Begin core parts");
            Ada.Text_IO.Put_Line (File, "");
            Ada.Text_IO.Put_Line (File, " pragma Linker_Options (""-lwayland-client"");");
            Ada.Text_IO.Put_Line (File, " -- Added this linker option here to avoid added it to each gpr file that with's");
            Ada.Text_IO.Put_Line (File, " -- this Ada binding to Wayland. If the wayland client lib changes its name it");
            Ada.Text_IO.Put_Line (File, " -- means there is only one place one needs to update.");
            Ada.Text_IO.Put_Line (File, "");
            Ada.Text_IO.Put_Line (File, " subtype Void_Ptr is System.Address;");
            Ada.Text_IO.Put_Line (File, "");
            Ada.Text_IO.Put_Line (File, " Default_Display_Name : aliased Interfaces.C.char_array := Interfaces.C.To_C (""wayland-0"");");
            Ada.Text_IO.Put_Line (File, "");
            Ada.Text_IO.Put_Line (File, " type Proxy_T is limited private;");
            Ada.Text_IO.Put_Line (File, "");
            Ada.Text_IO.Put_Line (File, " type Proxy_Ptr is access all Proxy_T;");
            Ada.Text_IO.Put_Line (File, "");
            Ada.Text_IO.Put_Line (File, " type Display_Ptr;");
            Ada.Text_IO.Put_Line (File, "");
            Ada.Text_IO.Put_Line (File, " function Display_Connect (Name : Interfaces.C.Strings.char_array_access) return Display_Ptr;");
            Ada.Text_IO.Put_Line (File, "");
            Ada.Text_IO.Put_Line (File, " procedure Display_Disconnect (This : in out Display_Ptr);");
            Ada.Text_IO.Put_Line (File, "");
            Ada.Text_IO.Put_Line (File, " type Interface_T is limited record");
            Ada.Text_IO.Put_Line (File, "    Name         : Interfaces.C.Strings.chars_ptr;");
            Ada.Text_IO.Put_Line (File, "    Version      : Interfaces.C.int;");
            Ada.Text_IO.Put_Line (File, "    Method_Count : Interfaces.C.int;");
            Ada.Text_IO.Put_Line (File, "    Methods      : Void_Ptr; -- Can be improved upon.");
            Ada.Text_IO.Put_Line (File, "    Event_Count  : Interfaces.C.int;");
            Ada.Text_IO.Put_Line (File, "    Events       : Void_Ptr; -- Can be improved upon.");
            Ada.Text_IO.Put_Line (File, " end record with");
            Ada.Text_IO.Put_Line (File, "   Convention => C_Pass_By_Copy;");
            Ada.Text_IO.Put_Line (File, "");
            Ada.Text_IO.Put_Line (File, " type Interface_Ptr is access all Interface_T;");
            Ada.Text_IO.Put_Line (File, "");
            Ada.Text_IO.Put_Line (File, " function Proxy_Marshal_Constructor (Proxy       : Proxy_Ptr;");
            Ada.Text_IO.Put_Line (File, "                                     Opcode      : Interfaces.Unsigned_32;");
            Ada.Text_IO.Put_Line (File, "                                     Interface_V : Interface_Ptr;");
            Ada.Text_IO.Put_Line (File, "                                     New_Id      : Interfaces.Unsigned_32) return Proxy_Ptr with");
            Ada.Text_IO.Put_Line (File, "   Convention    => C,");
            Ada.Text_IO.Put_Line (File, "   Import        => True,");
            Ada.Text_IO.Put_Line (File, "   External_Name => ""wl_proxy_marshal_constructor"";");
            Ada.Text_IO.Put_Line (File, "");
            Ada.Text_IO.Put_Line (File, " function Display_Dispatch (Display : Display_Ptr) return Interfaces.C.int with");
            Ada.Text_IO.Put_Line (File, "   Import => True,");
            Ada.Text_IO.Put_Line (File, "   Convention => C,");
            Ada.Text_IO.Put_Line (File, "   External_Name => ""wl_display_dispatch"";");
            Ada.Text_IO.Put_Line (File, "");
            Ada.Text_IO.Put_Line (File, " function Display_Roundtrip (Display : Display_Ptr) return Interfaces.C.int with");
            Ada.Text_IO.Put_Line (File, "   Import => True,");
            Ada.Text_IO.Put_Line (File, "   Convention => C,");
            Ada.Text_IO.Put_Line (File, "   External_Name => ""wl_display_roundtrip"";");
            Ada.Text_IO.Put_Line (File, "");
            Ada.Text_IO.Put_Line (File, " -- End core parts");
            Ada.Text_IO.Put_Line (File, "");
            Ada.Text_IO.Put_Line (File, "type Fixed_T is new Interfaces.Integer_32;");
            Ada.Text_IO.Put_Line (File, "");
            Ada.Text_IO.Put_Line (File, "type Wayland_Array_T is record");
            Ada.Text_IO.Put_Line (File, "Size  : Interfaces.Unsigned_32;");
            Ada.Text_IO.Put_Line (File, "Alloc : Interfaces.Unsigned_32;");
            Ada.Text_IO.Put_Line (File, "Data : Void_Ptr;");
            Ada.Text_IO.Put_Line (File, "end record with");
            Ada.Text_IO.Put_Line (File, "   Convention => C_Pass_By_Copy;");
            Ada.Text_IO.Put_Line (File, "");

            Generate_Code_For_Numeric_Constants;

            Ada.Text_IO.Put_Line (File, "private");
            Ada.Text_IO.Put_Line (File, "");
            Ada.Text_IO.Put_Line (File, "   type Proxy_T is limited null record;");
            Ada.Text_IO.Put_Line (File, "");
            Ada.Text_IO.Put_Line (File, "end Wl_Thin;");
         end Start_Writing_To_File;

         procedure Generate_Code_For_Interface_Constants;

         procedure Generate_Code_For_Numeric_Constants is

            procedure Handle_Interface (Interface_Tag : Wayland_XML.Interface_Tag.Interface_Tag_T) is

               procedure Generate_Code_For_Opcodes is

                  I : Aida.Int32_T := 0;

                  procedure Generate_Code (Request_Tag : Wayland_XML.Request_Tag.Request_Tag_T) is
                     Name : String := Utils.Make_Upper_Case (Interface_Tag.Name & "_" & Request_Tag.Name);
                  begin
                     Ada.Text_IO.Put (File, Name);
                     Ada.Text_IO.Put (File, " : constant := " & Aida.Int32.To_String (I));
                     Ada.Text_IO.Put_Line (File, ";");
                     Ada.Text_IO.Put_Line (File, "");

                     I := I + 1;
                  end Generate_Code;

               begin
                  for Child of Interface_Tag.Children loop
                     case Child.Kind_Id is
                     when Child_Dummy       => null;
                     when Child_Description => null;
                     when Child_Request     => Generate_Code (Child.Request_Tag.all);
                     when Child_Event       => null;
                     when Child_Enum        => null;
                     end case;
                  end loop;
               end Generate_Code_For_Opcodes;

               procedure Generate_Code_For_Event_Since_Version is

                  procedure Generate_Code (Event_Tag : Wayland_XML.Event_Tag.Event_Tag_T) is
                     Name : String := Utils.Make_Upper_Case (Interface_Tag.Name & "_" & Event_Tag.Name & "_SINCE_VERSION");
                  begin
                     if Event_Tag.Exists_Since_Attribute then
                        Ada.Text_IO.Put (File, Name);
                        Ada.Text_IO.Put (File, " : constant := " & Aida.Int32.To_String (Event_Tag.Since_Attribute_As_Pos32));
                        Ada.Text_IO.Put_Line (File, ";");
                        Ada.Text_IO.Put_Line (File, "");
                     else
                        Ada.Text_IO.Put (File, Name);
                        Ada.Text_IO.Put_Line (File, " : constant := 1;");
                        Ada.Text_IO.Put_Line (File, "");
                     end if;
                  end Generate_Code;

               begin
                  for Child of Interface_Tag.Children loop
                     case Child.Kind_Id is
                     when Child_Dummy       => null;
                     when Child_Description => null;
                     when Child_Request     => null;
                     when Child_Event       => Generate_Code (Child.Event_Tag.all);
                     when Child_Enum        => null;
                     end case;
                  end loop;
               end Generate_Code_For_Event_Since_Version;

               procedure Generate_Code_For_Opcodes_Since_Version is

                  procedure Generate_Code (Request_Tag : Wayland_XML.Request_Tag.Request_Tag_T) is
                     Name : String := Utils.Make_Upper_Case (Interface_Tag.Name & "_" & Request_Tag.Name & "_SINCE_VERSION");
                  begin
                     if Request_Tag.Exists_Since then
                        Ada.Text_IO.Put (File, Name);
                        Ada.Text_IO.Put (File, " : constant := " & Aida.Int32.To_String (Request_Tag.Since_As_Pos32));
                        Ada.Text_IO.Put_Line (File, ";");
                        Ada.Text_IO.Put_Line (File, "");
                     else
                        Ada.Text_IO.Put (File, Name);
                        Ada.Text_IO.Put_Line (File, " : constant := 1;");
                        Ada.Text_IO.Put_Line (File, "");
                     end if;
                  end Generate_Code;

               begin
                  for Child of Interface_Tag.Children loop
                     case Child.Kind_Id is
                     when Child_Dummy       => null;
                     when Child_Description => null;
                     when Child_Request     => Generate_Code (Child.Request_Tag.all);
                     when Child_Event       => null;
                     when Child_Enum        => null;
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
                  when Child_Dummy     => null;
                  when Child_Copyright => null;
                  when Child_Interface => Handle_Interface (Child.Interface_Tag.all);
               end case;
            end loop;

            Generate_Code_For_Interface_Constants;
         end Generate_Code_For_Numeric_Constants;

         procedure Generate_Code_For_Interface_Ptrs;

         procedure Generate_Code_For_Interface_Constants is

            procedure Handle_Interface (Interface_Tag : Wayland_XML.Interface_Tag.Interface_Tag_T) is
               Name : String := Utils.Adaify_Name (Interface_Tag.Name & "_Interface");
            begin
               Ada.Text_IO.Put (File, Name);
               Ada.Text_IO.Put_Line (File, " : aliased Interface_T with");
               Ada.Text_IO.Put_Line (File, "Import => True,");
               Ada.Text_IO.Put_Line (File, "Convention => C,");
               Ada.Text_IO.Put_Line (File, "External_Name => """ & Interface_Tag.Name & "_interface"";");
               Ada.Text_IO.Put_Line (File, "");
            end Handle_Interface;

         begin
            for Child of Protocol_Tag.Children loop
               case Child.Kind_Id is
                  when Child_Dummy     => null;
                  when Child_Copyright => null;
                  when Child_Interface => Handle_Interface (Child.Interface_Tag.all);
               end case;
            end loop;

            Generate_Code_For_Interface_Ptrs;
         end Generate_Code_For_Interface_Constants;

         procedure Generate_Code_For_Each_Interface;

         procedure Generate_Code_For_Interface_Ptrs is

            procedure Handle_Interface (Interface_Tag : Wayland_XML.Interface_Tag.Interface_Tag_T) is

               procedure Generate_Code_For_Interface_Ptr is
                  Name : String := Utils.Interface_Ptr_Name (Interface_Tag);
               begin
                  Ada.Text_IO.Put_Line (File, "type " & Name & " is new Proxy_Ptr;");
                  Ada.Text_IO.Put_Line (File, "");
               end Generate_Code_For_Interface_Ptr;

            begin
               Generate_Code_For_Interface_Ptr;
            end Handle_Interface;

         begin
            for Child of Protocol_Tag.Children loop
               case Child.Kind_Id is
                  when Child_Dummy     => null;
                  when Child_Copyright => null;
                  when Child_Interface => Handle_Interface (Child.Interface_Tag.all);
               end case;
            end loop;

            Generate_Code_For_Each_Interface;
         end Generate_Code_For_Interface_Ptrs;

         procedure Generate_Code_For_Each_Interface is

            procedure Handle_Interface (Interface_Tag : Wayland_XML.Interface_Tag.Interface_Tag_T) is

               procedure Generate_Code_For_Enums is

                  procedure Generate_Code (Enum_Tag : Wayland_XML.Enum_Tag.Enum_Tag_T) is
                     Enum_Type_Name : String := Utils.Adaify_Name (Interface_Tag.Name & "_" & Enum_Tag.Name & "_T");

                     procedure Generate_Code_For_Enum_Value (Entry_Tag : Wayland_XML.Entry_Tag.Entry_Tag_T) is
                        Name : String := Utils.Adaify_Name (Interface_Tag.Name & "_" & Entry_Tag.Name & "_" & Entry_Tag.Name);
                     begin
                        Ada.Text_IO.Put_Line (File, Name & " : constant " & Enum_Type_Name & " := " & Entry_Tag.Value_As_String & "; -- " & Entry_Tag.Summary);
                     end Generate_Code_For_Enum_Value;

                  begin
                     Ada.Text_IO.Put_Line (File, "type " & Enum_Type_Name & " is new Interfaces.Unsigned_32;");

                     for Child of Enum_Tag.Children loop
                        case Child.Kind_Id is
                           when Child_Dummy       => null;
                           when Child_Description => null;
                           when Child_Entry       => Generate_Code_For_Enum_Value (Child.Entry_Tag.all);
                        end case;
                     end loop;
                     Ada.Text_IO.Put_Line (File, "");
                  end Generate_Code;

               begin
                  for Child of Interface_Tag.Children loop
                     case Child.Kind_Id is
                        when Child_Dummy       => null;
                        when Child_Description => null;
                        when Child_Request     => null;
                        when Child_Event       => null;
                        when Child_Enum        => Generate_Code (Child.Enum_Tag.all);
                     end case;
                  end loop;
               end Generate_Code_For_Enums;

               procedure Generate_Code_For_Subprogram_Ptrs is

                  procedure Generate_Code_For_Subprogram (Event_Tag : Wayland_XML.Event_Tag.Event_Tag_T) is

                     procedure Generate_Code_For_Argument (Arg_Tag : Wayland_XML.Arg_Tag.Arg_Tag_T;
                                                           Is_Last : Boolean)
                     is
                     begin
                        declare
                           Arg_Name      : String := Utils.Adaify_Variable_Name (Arg_Tag.Name);
                           Arg_Type_Name : String := Utils.Arg_Type_As_String (Arg_Tag);
                        begin
                           if Is_Last then
                              Ada.Text_IO.Put_Line (File, "     " & Arg_Name & " : " & Arg_Type_Name);
                           else
                              Ada.Text_IO.Put_Line (File, "     " & Arg_Name & " : " & Arg_Type_Name & ";");
                           end if;
                        end;
                     end Generate_Code_For_Argument;

                     V : Wayland_XML.Event_Tag.Child_Vectors.Vector;

                     Subprogram_Name : String := Utils.Adaify_Name (Interface_Tag.Name & "_" & Event_Tag.Name & "_Subprogram_Ptr");
                     Interface_Name : String := Utils.Adaify_Name (Interface_Tag.Name);
                  begin
                     for Child of Event_Tag.Children loop
                        case Child.Kind_Id is
                           when Child_Dummy       => null;
                           when Child_Description => null;
                           when Child_Arg         => V.Append (Child);
                        end case;
                     end loop;

                     Ada.Text_IO.Put_Line (File, "type " & Subprogram_Name & " is access procedure (Data : Void_Ptr;");
                     Ada.Text_IO.Put (File, "     " & Interface_Name & " : " & Utils.Interface_Ptr_Name (Interface_Tag));

                     if V.Length = 0 then
                        Ada.Text_IO.Put_Line (File, "");
                     else
                        Ada.Text_IO.Put_Line (File, ";");
                     end if;

                     for Child of V loop
                        case Child.Kind_Id is
                           when Child_Dummy       => null;
                           when Child_Description => null;
                           when Child_Arg         => Generate_Code_For_Argument (Child.Arg_Tag.all, Event_Tag.Children.Last_Element = Child);
                        end case;
                     end loop;
                     Ada.Text_IO.Put_Line (File, "     ) with");
                     Ada.Text_IO.Put_Line (File, "Convention => C;");
                     Ada.Text_IO.Put_Line (File, "");
                  end Generate_Code_For_Subprogram;

               begin
                  for Child of Interface_Tag.Children loop
                     case Child.Kind_Id is
                        when Child_Dummy       => null;
                        when Child_Description => null;
                        when Child_Request     => null;
                        when Child_Event       => Generate_Code_For_Subprogram (Child.Event_Tag.all);
                        when Child_Enum        => null;
                     end case;
                  end loop;
               end Generate_Code_For_Subprogram_Ptrs;

               procedure Generate_Code_For_Listener_Type_Definition is

                  procedure Generate_Code_For_Record_Component (Event_Tag : Wayland_XML.Event_Tag.Event_Tag_T) is
                     Component_Name : String := Utils.Adaify_Name (Event_Tag.Name);
                     Component_Type_Name : String := Utils.Adaify_Name (Interface_Tag.Name & "_" & Event_Tag.Name & "_Subprogram_Ptr");
                  begin
                     Ada.Text_IO.Put_Line (File, "   " & Component_Name & " : " & Component_Type_Name & ";");
                  end Generate_Code_For_Record_Component;

                  Name : String := Utils.Adaify_Name (Interface_Tag.Name & "_Listener_T");
                  Ptr_Name : String := Utils.Adaify_Name (Interface_Tag.Name & "_Listener_Ptr");
               begin
                  Ada.Text_IO.Put_Line (File, "type " & Name & " is record");

                  for Child of Interface_Tag.Children loop
                     case Child.Kind_Id is
                        when Child_Dummy       => null;
                        when Child_Description => null;
                        when Child_Request     => null;
                        when Child_Event       => Generate_Code_For_Record_Component (Child.Event_Tag.all);
                        when Child_Enum        => null;
                     end case;
                  end loop;
                  Ada.Text_IO.Put_Line (File, "end record with");
                  Ada.Text_IO.Put_Line (File, "   Convention => C_Pass_By_Copy;");
                  Ada.Text_IO.Put_Line (File, "");
                  Ada.Text_IO.Put_Line (File, "type " & Ptr_Name & " is access all " & Name & ";");
                  Ada.Text_IO.Put_Line (File, "");
               end Generate_Code_For_Listener_Type_Definition;

               procedure Generate_Code_For_Add_Listener_Subprogram_Declaration is
                  Name : String := Utils.Adaify_Name (Interface_Tag.Name);
                  Ptr_Name : String := Utils.Adaify_Name (Interface_Tag.Name & "_Ptr");
                  Ptr_Listener_Name : String := Utils.Adaify_Name (Interface_Tag.Name & "_Listener_Ptr");
               begin
                  Ada.Text_IO.Put_Line (File, "function " & Name & "_Add_Listener (" & Name & " : " & Ptr_Name & ";");
                  Ada.Text_IO.Put_Line (File, "Listener : " & Ptr_Listener_Name & ";");
                  Ada.Text_IO.Put_Line (File, "Data : Void_Ptr) return Interfaces.C.int;");
                  Ada.Text_IO.Put_Line (File, "");
               end Generate_Code_For_Add_Listener_Subprogram_Declaration;

               procedure Generate_Code_For_Set_User_Data_Subprogram_Declaration is
                  Name : String := Utils.Adaify_Name (Interface_Tag.Name);
                  Ptr_Name : String := Utils.Adaify_Name (Interface_Tag.Name & "_Ptr");
               begin
                  Ada.Text_IO.Put_Line (File, "procedure " & Name & "_Set_User_Data (" & Name & " : " & Ptr_Name & ";");
                  Ada.Text_IO.Put_Line (File, "Data : Void_Ptr);");
                  Ada.Text_IO.Put_Line (File, "");
               end Generate_Code_For_Set_User_Data_Subprogram_Declaration;

               procedure Generate_Code_For_Get_User_Data_Subprogram_Declaration is
                  Name : String := Utils.Adaify_Name (Interface_Tag.Name);
                  Ptr_Name : String := Utils.Adaify_Name (Interface_Tag.Name & "_Ptr");
               begin
                  Ada.Text_IO.Put_Line (File, "function " & Name & "_Get_User_Data (" & Name & " : " & Ptr_Name & ") return Void_Ptr;");
                  Ada.Text_IO.Put_Line (File, "");
               end Generate_Code_For_Get_User_Data_Subprogram_Declaration;

               procedure Generate_Code_For_Get_Version_Subprogram_Declaration is
                  Name : String := Utils.Adaify_Name (Interface_Tag.Name);
                  Ptr_Name : String := Utils.Adaify_Name (Interface_Tag.Name & "_Ptr");
               begin
                  Ada.Text_IO.Put_Line (File, "function " & Name & "_Get_Version (" & Name & " : " & Ptr_Name & ") return Interfaces.Unsigned_32;");
                  Ada.Text_IO.Put_Line (File, "");
               end Generate_Code_For_Get_Version_Subprogram_Declaration;

               procedure Generate_Code_For_Destroy_Subprogram_Declaration is
                  Name : String := Utils.Adaify_Name (Interface_Tag.Name);
                  Ptr_Name : String := Utils.Adaify_Name (Interface_Tag.Name & "_Ptr");
               begin
                  Ada.Text_IO.Put_Line (File, "procedure " & Name & "_Destroy (" & Name & " : " & Ptr_Name & ");");
                  Ada.Text_IO.Put_Line (File, "");
               end Generate_Code_For_Destroy_Subprogram_Declaration;

               procedure Generate_Code_For_Requests is

                  procedure Generate_Code_For_Subprogram_Declaration (Request_Tag : Wayland_XML.Request_Tag.Request_Tag_T) is

                     procedure Generate_Code_For_Arg (Arg_Tag : Wayland_XML.Arg_Tag.Arg_Tag_T;
                                                      Is_Last : Boolean) is
                     begin
                        declare
                           Arg_Name      : String := Utils.Adaify_Variable_Name (Arg_Tag.Name);
                           Arg_Type_Name : String := Utils.Arg_Type_As_String (Arg_Tag);
                        begin
                           if Is_Last then
                              Ada.Text_IO.Put_Line (File, "     " & Arg_Name & " : " & Arg_Type_Name);
                           else
                              Ada.Text_IO.Put_Line (File, "     " & Arg_Name & " : " & Arg_Type_Name & ";");
                           end if;
                        end;
                     end Generate_Code_For_Arg;

                     Subprogram_Name : String := Utils.Adaify_Name (Interface_Tag.Name & "_" & Request_Tag.Name);
                     Name            : String := Utils.Adaify_Name (Interface_Tag.Name);
                     Ptr_Name        : String := Utils.Adaify_Name (Interface_Tag.Name & "_Ptr");
                  begin
                     if Utils.Is_New_Id_Argument_Present (Request_Tag) then
                        if Utils.Is_Interface_Specified (Request_Tag) then
                           declare
                              Return_Type : String := Utils.Adaify_Name (Utils.Find_Specified_Interface (Request_Tag) & "_Ptr");
                           begin
                              if Utils.Number_Of_Args (Request_Tag) > 1 then
                                 Ada.Text_IO.Put_Line (File, "function " & Subprogram_Name & " (" & Name & " : " & Ptr_Name & ";");

                                 declare
                                    V : Wayland_XML.Request_Tag.Child_Vectors.Vector;
                                 begin
                                    for Child of Request_Tag.Children loop
                                       case Child.Kind_Id is
                                          when Child_Dummy       => null;
                                          when Child_Description => null;
                                          when Child_Arg         =>
                                             if Child.Arg_Tag.Type_Attribute /= Type_New_Id then
                                                V.Append (Child);
                                             end if;
                                       end case;
                                    end loop;

                                    for Child of V loop
                                       case Child.Kind_Id is
                                          when Child_Dummy       => null;
                                          when Child_Description => null;
                                          when Child_Arg         => Generate_Code_For_Arg (Child.Arg_Tag.all, Child = Request_Tag.Children.Last_Element);
                                       end case;
                                    end loop;
                                 end;

                                 Ada.Text_IO.Put_Line (File, "   ) return " & Return_Type & ";");
                              else
                                 Ada.Text_IO.Put_Line (File, "function " & Subprogram_Name & " (" & Name & " : " & Ptr_Name & ") return " & Return_Type & ";");
                              end if;
                           end;
                        else
                           if Utils.Number_Of_Args (Request_Tag) > 1 then
                              Ada.Text_IO.Put_Line (File, "function " & Subprogram_Name & " (" & Name & " : " & Ptr_Name & ";");
                              for Child of Request_Tag.Children loop
                                 case Child.Kind_Id is
                                    when Child_Dummy       => null;
                                    when Child_Description => null;
                                    when Child_Arg         => Generate_Code_For_Arg (Child.Arg_Tag.all, False);
                                 end case;
                              end loop;
                              Ada.Text_IO.Put_Line (File, "   Interface_V : Interface_Ptr;");
                              Ada.Text_IO.Put_Line (File, "   New_Id : Interfaces.Unsigned_32) return Proxy_Ptr;");
                           else
                              Ada.Text_IO.Put_Line (File, "function " & Subprogram_Name & " (" & Name & " : " & Ptr_Name & ") return Proxy_Ptr;");
                           end if;
                        end if;
                     elsif Utils.Is_Request_Destructor (Request_Tag) then
                        null; -- Already has generated declaration earlier in Generate_Code_For_Destroy_Subprogram
                     else
                        if Utils.Number_Of_Args (Request_Tag) > 0 then
                           Ada.Text_IO.Put_Line (File, "procedure " & Subprogram_Name & " (" & Name & " : " & Ptr_Name & ";");
                           for Child of Request_Tag.Children loop
                              case Child.Kind_Id is
                                 when Child_Dummy       => null;
                                 when Child_Description => null;
                                 when Child_Arg         => Generate_Code_For_Arg (Child.Arg_Tag.all, Child = Request_Tag.Children.Last_Element);
                              end case;
                           end loop;
                           Ada.Text_IO.Put_Line (File, "   );");
                        else
                           Ada.Text_IO.Put_Line (File, "procedure " & Subprogram_Name & " (" & Name & " : " & Ptr_Name & ");");
                        end if;
                     end if;
                     Ada.Text_IO.Put_Line (File, "");
                  end Generate_Code_For_Subprogram_Declaration;

               begin
                  for Child of Interface_Tag.Children loop
                     case Child.Kind_Id is
                        when Child_Dummy       => null;
                        when Child_Description => null;
                        when Child_Request     => Generate_Code_For_Subprogram_Declaration (Child.Request_Tag.all);
                        when Child_Event       => null;
                        when Child_Enum        => null;
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
                  when Child_Dummy     => null;
                  when Child_Copyright => null;
                  when Child_Interface => Handle_Interface (Child.Interface_Tag.all);
               end case;
            end loop;
         end Generate_Code_For_Each_Interface;

      begin
         Start_Writing_To_File;
      end Write_To_File;

   begin
      Create_File;
      Create_Wl_Thin_Body_File;
   end Create_Wl_Thin_Spec_File;

   procedure Create_Wl_Thin_Body_File is

      File : Ada.Text_IO.File_Type;

      procedure Write_To_File;

      procedure Create_File is
      begin
         Ada.Text_IO.Create (File => File,
                             Mode => Ada.Text_IO.Out_File,
                             Name => "wl_thin.adb");

         Write_To_File;

         Ada.Text_IO.Close (File);
      end Create_File;

      pragma Unmodified (File);

      procedure Generate_Code_For_Add_Listener_Subprogram_Implementations;

      procedure Write_To_File is

      begin
         Ada.Text_IO.Put_Line (File, "-- Auto generated from Wayland.xml");
         Ada.Text_IO.Put_Line (File, "package body Wl_Thin is");
         Ada.Text_IO.Put_Line (File, "");

         Generate_Code_For_Add_Listener_Subprogram_Implementations;

         Ada.Text_IO.Put_Line (File, "");
         Ada.Text_IO.Put_Line (File, "end Wl_Thin;");
      end Write_To_File;

      procedure Generate_Code_For_Add_Listener_Subprogram_Implementations is

         procedure Handle_Interface (Interface_Tag : Wayland_XML.Interface_Tag.Interface_Tag_T) is

            procedure Generate_Code_For_Add_Listener_Subprogram_Implementations is
               Name              : String := Utils.Adaify_Name (Interface_Tag.Name);
               Function_Name     : String := Name & "_Add_Listener";
               Ptr_Name          : String := Utils.Adaify_Name (Interface_Tag.Name & "_Ptr");
               Ptr_Listener_Name : String := Utils.Adaify_Name (Interface_Tag.Name & "_Listener_Ptr");
            begin
               Ada.Text_IO.Put_Line (File, "function " & Function_Name & " (" & Name & " : " & Ptr_Name & ";");
               Ada.Text_IO.Put_Line (File, "Listener : " & Ptr_Listener_Name & ";");
               Ada.Text_IO.Put_Line (File, "Data : Void_Ptr) return Interfaces.C.int is");
               Ada.Text_IO.Put_Line (File, "begin");
               Ada.Text_IO.Put_Line (File, "return wl_proxy_add_listener (" & Name &".all'Access, Listener.all'Access, Data);");
               Ada.Text_IO.Put_Line (File, "end " & Function_Name & ";");
               Ada.Text_IO.Put_Line (File, "");
            end Generate_Code_For_Add_Listener_Subprogram_Implementations;

            procedure Generate_Code_For_Set_User_Data_Subprogram_Implementations is
               Name            : String := Utils.Adaify_Name (Interface_Tag.Name);
               Subprogram_Name : String := Name & "_Set_User_Data";
               Ptr_Name        : String := Utils.Adaify_Name (Interface_Tag.Name & "_Ptr");
            begin
               Ada.Text_IO.Put_Line (File, "procedure " & Subprogram_Name & " (" & Name & " : " & Ptr_Name & ";");
               Ada.Text_IO.Put_Line (File, "Data : Void_Ptr) is");
               Ada.Text_IO.Put_Line (File, "begin");
               Ada.Text_IO.Put_Line (File, "wl_proxy_set_user_data (" & Name &".all'Access, Data);");
               Ada.Text_IO.Put_Line (File, "end " & Subprogram_Name & ";");
               Ada.Text_IO.Put_Line (File, "");
            end Generate_Code_For_Set_User_Data_Subprogram_Implementations;

            procedure Generate_Code_For_Get_User_Data_Subprogram_Implementations is
               Name            : String := Utils.Adaify_Name (Interface_Tag.Name);
               Subprogram_Name : String := Name & "_Get_User_Data";
               Ptr_Name        : String := Utils.Adaify_Name (Interface_Tag.Name & "_Ptr");
            begin
               Ada.Text_IO.Put_Line (File, "function " & Subprogram_Name & " (" & Name & " : " & Ptr_Name & ") return Void_Ptr is");
               Ada.Text_IO.Put_Line (File, "begin");
               Ada.Text_IO.Put_Line (File, "return wl_proxy_get_user_data (" & Name &".all'Access);");
               Ada.Text_IO.Put_Line (File, "end " & Subprogram_Name & ";");
               Ada.Text_IO.Put_Line (File, "");
            end Generate_Code_For_Get_User_Data_Subprogram_Implementations;

            procedure Generate_Code_For_Get_Version_Subprogram_Implementations is
               Name            : String := Utils.Adaify_Name (Interface_Tag.Name);
               Subprogram_Name : String := Name & "_Get_Version";
               Ptr_Name        : String := Utils.Adaify_Name (Interface_Tag.Name & "_Ptr");
            begin
               Ada.Text_IO.Put_Line (File, "function " & Subprogram_Name & " (" & Name & " : " & Ptr_Name & ") return Interfaces.Unsigned_32 is");
               Ada.Text_IO.Put_Line (File, "begin");
               Ada.Text_IO.Put_Line (File, "return wl_proxy_get_version (" & Name &".all'Access);");
               Ada.Text_IO.Put_Line (File, "end " & Subprogram_Name & ";");
               Ada.Text_IO.Put_Line (File, "");
            end Generate_Code_For_Get_Version_Subprogram_Implementations;

            procedure Generate_Code_For_Destroy_Subprogram_Implementations is
               Name            : String := Utils.Adaify_Name (Interface_Tag.Name);
               Subprogram_Name : String := Name & "_Destroy";
               Ptr_Name        : String := Utils.Adaify_Name (Interface_Tag.Name & "_Ptr");
            begin
               if Utils.Exists_Destructor (Interface_Tag) then
                  Ada.Text_IO.Put_Line (File, "procedure " & Subprogram_Name & " (" & Name & " : " & Ptr_Name & ") is");
                  Ada.Text_IO.Put_Line (File, "begin");
                  Ada.Text_IO.Put_Line (File, "Proxy_Marshal (" & Name &".all'Access,");
                  Ada.Text_IO.Put_Line (File, "     " & Utils.Make_Upper_Case (Interface_Tag.Name & "_Destroy") &");");
                  Ada.Text_IO.Put_Line (File, "");
                  Ada.Text_IO.Put_Line (File, "wl_proxy_destroy (" & Name &".all'Access);");
                  Ada.Text_IO.Put_Line (File, "end " & Subprogram_Name & ";");
                  Ada.Text_IO.Put_Line (File, "");
               else
                  Ada.Text_IO.Put_Line (File, "procedure " & Subprogram_Name & " (" & Name & " : " & Ptr_Name & ") is");
                  Ada.Text_IO.Put_Line (File, "begin");
                  Ada.Text_IO.Put_Line (File, "wl_proxy_destroy (" & Name &".all'Access);");
                  Ada.Text_IO.Put_Line (File, "end " & Subprogram_Name & ";");
                  Ada.Text_IO.Put_Line (File, "");
               end if;
            end Generate_Code_For_Destroy_Subprogram_Implementations;

            procedure Generate_Code_For_Requests is

               procedure Generate_Code_For_Subprogram_Implementation (Request_Tag : Wayland_XML.Request_Tag.Request_Tag_T) is

                  procedure Generate_Code_For_Arg (Arg_Tag : Wayland_XML.Arg_Tag.Arg_Tag_T;
                                                   Is_Last : Boolean) is
                  begin
                     declare
                        Arg_Name      : String := Utils.Adaify_Variable_Name (Arg_Tag.Name);
                        Arg_Type_Name : String := Utils.Arg_Type_As_String (Arg_Tag);
                     begin
                        if Is_Last then
                           Ada.Text_IO.Put_Line (File, "     " & Arg_Name & " : " & Arg_Type_Name);
                        else
                           Ada.Text_IO.Put_Line (File, "     " & Arg_Name & " : " & Arg_Type_Name & ";");
                        end if;
                     end;
                  end Generate_Code_For_Arg;

                  Opcode          : String := Utils.Make_Upper_Case (Interface_Tag.Name & "_" & Request_Tag.Name);
                  Subprogram_Name : String := Utils.Adaify_Name (Interface_Tag.Name & "_" & Request_Tag.Name);
                  Name            : String := Utils.Adaify_Name (Interface_Tag.Name);
                  Ptr_Name        : String := Utils.Adaify_Name (Interface_Tag.Name & "_Ptr");
               begin
                  if Utils.Is_New_Id_Argument_Present (Request_Tag) then
                     if Utils.Is_Interface_Specified (Request_Tag) then
                        declare
                           Return_Type : String := Utils.Adaify_Name (Utils.Find_Specified_Interface (Request_Tag) & "_Ptr");
                        begin
                           if Utils.Number_Of_Args (Request_Tag) > 1 then
                              declare
                                 V : Wayland_XML.Request_Tag.Child_Vectors.Vector;
                              begin
                                 Ada.Text_IO.Put_Line (File, "function " & Subprogram_Name & " (" & Name & " : " & Ptr_Name & ";");
                                 for Child of Request_Tag.Children loop
                                    case Child.Kind_Id is
                                       when Child_Dummy       => null;
                                       when Child_Description => null;
                                       when Child_Arg         =>
                                          if Child.Arg_Tag.Type_Attribute /= Type_New_Id then
                                             V.Append (Child);
                                          end if;
                                    end case;
                                 end loop;

                                 for Child of V loop
                                    case Child.Kind_Id is
                                       when Child_Dummy       => null;
                                       when Child_Description => null;
                                       when Child_Arg         => Generate_Code_For_Arg (Child.Arg_Tag.all, Child = Request_Tag.Children.Last_Element);
                                    end case;
                                 end loop;

                                 Ada.Text_IO.Put_Line (File, "   ) return " & Return_Type & " is");
                                 Ada.Text_IO.Put_Line (File, "P : Proxy_Ptr := Proxy_Marshal_Constructor (" & Name &".all'Access,");
                                 Ada.Text_IO.Put_Line (File, "    " & Utils.Make_Upper_Case (Interface_Tag.Name & "_" & Request_Tag.Name) & ",");
                                 Ada.Text_IO.Put_Line (File, "    " & Utils.Adaify_Name (Utils.Find_Specified_Interface (Request_Tag)) & "_Interface'Access,");
                                 Ada.Text_IO.Put (File, "    0");

                                 for Child of V loop
                                    case Child.Kind_Id is
                                       when Child_Dummy       => null;
                                       when Child_Description => null;
                                       when Child_Arg         =>
                                          Ada.Text_IO.Put_Line (File, ",");
                                          Ada.Text_IO.Put (File, "    " & Utils.Adaify_Name (Child.Arg_Tag.Name));
                                    end case;
                                 end loop;

                                 Ada.Text_IO.Put_Line (File, "    );");
                                 Ada.Text_IO.Put_Line (File, "begin");
                                 Ada.Text_IO.Put_Line (File, "    return (if P /= null then P.all'Access else null);");
                                 Ada.Text_IO.Put_Line (File, "end " & Subprogram_Name & ";");

                              end;
                           else
                              Ada.Text_IO.Put_Line (File, "function " & Subprogram_Name & " (" & Name & " : " & Ptr_Name & ") return " & Return_Type & " is");
                              Ada.Text_IO.Put_Line (File, "P : Proxy_Ptr := Proxy_Marshal_Constructor (" & Name &".all'Access,");
                              Ada.Text_IO.Put_Line (File, "    " & Utils.Make_Upper_Case (Interface_Tag.Name & "_" & Request_Tag.Name) & ",");
                              Ada.Text_IO.Put_Line (File, "    " & Utils.Adaify_Name (Utils.Find_Specified_Interface (Request_Tag)) & "_Interface'Access,");
                              Ada.Text_IO.Put_Line (File, "    0);");
                              Ada.Text_IO.Put_Line (File, "begin");
                              Ada.Text_IO.Put_Line (File, "    return (if P /= null then P.all'Access else null);");
                              Ada.Text_IO.Put_Line (File, "end " & Subprogram_Name & ";");
                           end if;
                        end;
                     else
                        if Utils.Number_Of_Args (Request_Tag) > 1 then
                           declare
                              V : Wayland_XML.Request_Tag.Child_Vectors.Vector;
                           begin
                              Ada.Text_IO.Put_Line (File, "function " & Subprogram_Name & " (" & Name & " : " & Ptr_Name & ";");

                              for Child of Request_Tag.Children loop
                                 case Child.Kind_Id is
                                    when Child_Dummy       => null;
                                    when Child_Description => null;
                                    when Child_Arg         =>
                                       if Child.Arg_Tag.Type_Attribute /= Type_New_Id then
                                          V.Append (Child);
                                       end if;
                                 end case;
                              end loop;

                              for Child of V loop
                                 case Child.Kind_Id is
                                    when Child_Dummy       => null;
                                    when Child_Description => null;
                                    when Child_Arg         => Generate_Code_For_Arg (Child.Arg_Tag.all, False);
                                 end case;
                              end loop;

                              Ada.Text_IO.Put_Line (File, "   Interface_V : Interface_Ptr;");
                              Ada.Text_IO.Put_Line (File, "   New_Id : Interfaces.Unsigned_32) return Proxy_Ptr is");
                              Ada.Text_IO.Put_Line (File, "begin");
                              Ada.Text_IO.Put_Line (File, "    return Proxy_Marshal_Constructor_Versioned (" & Name &".all'Access,");
                              Ada.Text_IO.Put_Line (File, "    " & Utils.Make_Upper_Case (Interface_Tag.Name & "_" & Request_Tag.Name) & ",");
                              Ada.Text_IO.Put_Line (File, "    Interface_V'Access,");
                              Ada.Text_IO.Put_Line (File, "    New_Id,");

                              for Child of V loop
                                 case Child.Kind_Id is
                                    when Child_Dummy       => null;
                                    when Child_Description => null;
                                    when Child_Arg         =>
                                       Ada.Text_IO.Put_Line (File, "    " & Utils.Adaify_Variable_Name (Child.Arg_Tag.Name) & ",");
                                 end case;
                              end loop;

                              Ada.Text_IO.Put_Line (File, "    Interface_V.Name,");
                              Ada.Text_IO.Put_Line (File, "    New_Id,");
                              Ada.Text_IO.Put_Line (File, "    0);");
                              Ada.Text_IO.Put_Line (File, "end " & Subprogram_Name & ";");
                           end;
                        else
                           Ada.Text_IO.Put_Line (File, "function " & Subprogram_Name & " (" & Name & " : " & Ptr_Name & ") return Proxy_Ptr is");
                           Ada.Text_IO.Put_Line (File, "P : Proxy_Ptr := Proxy_Marshal_Constructor (" & Name &".all'Access,");
                           Ada.Text_IO.Put_Line (File, "    " & Utils.Make_Upper_Case (Interface_Tag.Name & "_" & Request_Tag.Name) & ",");
                           Ada.Text_IO.Put_Line (File, "    " & Utils.Adaify_Name (Utils.Find_Specified_Interface (Request_Tag)) & "_Interface'Access,");
                           Ada.Text_IO.Put_Line (File, "    0);");
                           Ada.Text_IO.Put_Line (File, "begin");
                           Ada.Text_IO.Put_Line (File, "    return (if P /= null then P.all'Access else null);");
                           Ada.Text_IO.Put_Line (File, "end " & Subprogram_Name & ";");
                        end if;
                     end if;
                  elsif Utils.Is_Request_Destructor (Request_Tag) then
                     null; -- Body is generated in Generate_Code_For_Destroy_Subprogram_Implementation
                  else
                     if Utils.Number_Of_Args (Request_Tag) > 0 then
                        declare
                           V : Wayland_XML.Request_Tag.Child_Vectors.Vector;
                        begin
                           Ada.Text_IO.Put_Line (File, "procedure " & Subprogram_Name & " (" & Name & " : " & Ptr_Name & ";");
                           for Child of Request_Tag.Children loop
                              case Child.Kind_Id is
                                 when Child_Dummy       => null;
                                 when Child_Description => null;
                                 when Child_Arg         =>
                                    if Child.Arg_Tag.Type_Attribute /= Type_New_Id then
                                       V.Append (Child);
                                    end if;
                              end case;
                           end loop;

                           for Child of V loop
                              case Child.Kind_Id is
                                 when Child_Dummy       => null;
                                 when Child_Description => null;
                                 when Child_Arg         => Generate_Code_For_Arg (Child.Arg_Tag.all, Child = Request_Tag.Children.Last_Element);
                              end case;
                           end loop;

                           Ada.Text_IO.Put_Line (File, "   ) is");
                           Ada.Text_IO.Put_Line (File, "begin");
                           Ada.Text_IO.Put_Line (File, "Proxy_Marshal (" & Name &".all'Access,");
                           Ada.Text_IO.Put (File, "    " & Opcode);

                           for Child of V loop
                              case Child.Kind_Id is
                                 when Child_Dummy       => null;
                                 when Child_Description => null;
                                 when Child_Arg         =>
                                    Ada.Text_IO.Put_Line (File, ",");
                                    Ada.Text_IO.Put (File, "    " & Utils.Adaify_Name (Child.Arg_Tag.Name));
                              end case;
                           end loop;
                           Ada.Text_IO.Put_Line (File, "    );");
                           Ada.Text_IO.Put_Line (File, "end " & Subprogram_Name & ";");
                        end;
                     else
                        Ada.Text_IO.Put_Line (File, "procedure " & Subprogram_Name & " (" & Name & " : " & Ptr_Name & ") is");
                        Ada.Text_IO.Put_Line (File, "begin");
                        Ada.Text_IO.Put_Line (File, "Proxy_Marshal (" & Name &".all'Access, " & Opcode & ");");
                        Ada.Text_IO.Put_Line (File, "end " & Subprogram_Name & ";");
                     end if;
                  end if;

                  Ada.Text_IO.Put_Line (File, "");
               end Generate_Code_For_Subprogram_Implementation;

            begin
               for Child of Interface_Tag.Children loop
                  case Child.Kind_Id is
                     when Child_Dummy       => null;
                     when Child_Description => null;
                     when Child_Request     => Generate_Code_For_Subprogram_Implementation (Child.Request_Tag.all);
                     when Child_Event       => null;
                     when Child_Enum        => null;
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
               when Child_Dummy     => null;
               when Child_Copyright => null;
               when Child_Interface => Handle_Interface (Child.Interface_Tag.all);
            end case;
         end loop;
      end Generate_Code_For_Add_Listener_Subprogram_Implementations;

   begin
      Create_File;
   end Create_Wl_Thin_Body_File;

begin
   Check_Wayland_XML_File_Exists;
exception
   when Ada.Text_IO.Name_Error =>
      Ada.Text_IO.Put_Line ("Could not find file!");
   when Unknown_Exception : others =>
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (Unknown_Exception));
end XML_Parser;
