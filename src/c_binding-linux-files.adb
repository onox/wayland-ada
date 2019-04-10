package body C_Binding.Linux.Files is

   procedure Set_File_Descriptor
     (This  : in out File;
      Value : Integer) is
   begin
      This.My_File_Descriptor := Interfaces.C.int (Value);
   end Set_File_Descriptor;

   procedure Open
     (This        : in out File;
      File_Name   : in     String;
      Mode        : in     File_Mode;
      Permissions : in     File_Permissions)
   is
      M : O_FLag := 0;
      P : S_FLag := 0;
   begin
      case Mode is
         when Read_Only  => M := M or O_RDONLY;
         when Write_Only => M := M or O_WRONLY;
         when Read_Write => M := M or O_RDWR;
      end case;

      if Permissions (Owner_Read) then
         P := P or S_IRUSR;
      end if;

      if Permissions (Owner_Write) then
         P := P or S_IWUSR;
      end if;

      if Permissions (Owner_Execute) then
         P := P or S_IXGRP;
      end if;

      if Permissions (Group_Read) then
         P := P or S_IRGRP;
      end if;

      if Permissions (Group_Write) then
         P := P or S_IWGRP;
      end if;

      if Permissions (Group_Execute) then
         P := P or S_IXUSR;
      end if;

      if Permissions (Others_Read) then
         P := P or S_IROTH;
      end if;

      if Permissions (Others_Write) then
         P := P or S_IWOTH;
      end if;

      if Permissions (Others_Execute) then
         P := P or S_IXOTH;
      end if;

      This.My_File_Descriptor := C_Open (+File_Name, M, P);
   end Open;

   function Close (This : in out File) return Success_Flag is
      Result : Interfaces.C.int;
      Flag : Success_Flag;
   begin
      Result := C_Close (Interfaces.C.int (This.My_File_Descriptor));
      if Result = -1 then
         Flag := Failure;
      else
         Flag := Success;
      end if;
      return Flag;
   end Close;

   procedure Write
     (This : File;
      Bytes : Ada.Streams.Stream_Element_Array)
   is
      SSize : SSize_Type;
      pragma Unreferenced (SSize);
   begin
      SSize :=
        C_Write
          (File_Descriptor => Interfaces.C.int (This.My_File_Descriptor),
           Buffer          => Bytes,
           Count           => Bytes'Length);
   end Write;

   function Read
     (This : File;
      Bytes : in out Ada.Streams.Stream_Element_Array) return Read_Result
   is
      Result : SSize_Type
        := C_Read
          (Interfaces.C.int (This.My_File_Descriptor), Bytes, Bytes'Length);
   begin
      case Result is
         when SSize_Type'First .. -1 =>
            return (Kind_Id => Read_Failure);
         when 0 =>
            return (Kind_Id => End_Of_File_Reached);
         when 1 .. SSize_Type'Last =>
            return (Kind_Id       => Read_Success,
                    Element_Count =>
                       Ada.Streams.Stream_Element_Count (Result));
      end case;
   end Read;

end C_Binding.Linux.Files;
