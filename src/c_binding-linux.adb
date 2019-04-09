package body C_Binding.Linux is

   use type Stream_Element_Offset;
   use type Interfaces.C.unsigned;
   use type Interfaces.Unsigned_32;

   procedure Set_File_Descriptor
     (File  : in out Linux.File;
      Value : Integer) is
   begin
      File.My_File_Descriptor := Value;
   end Set_File_Descriptor;

   procedure Open
     (File        : in out Linux.File;
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

      File.My_File_Descriptor := Px_Thin.Open (+File_Name, M, P);
   end Open;

   function Close (File : in out Linux.File) return Success_Flag is
      Result : Interfaces.C.int;
      Flag : Success_Flag;
   begin
      Result := C_Close (Interfaces.C.int (File.My_File_Descriptor));
      if Result = -1 then
         Flag := Failure;
      else
         Flag := Success;
      end if;
      return Flag;
   end Close;

   procedure Get_File_Status
     (File   : in     Linux.File;
      Status : in out File_Status)
   is
      Result : constant Integer :=
        Px_Thin.Get_File_Status
          (Fd     => File.My_File_Descriptor,
           Status => Status.My_Status'Access);
   begin
      Status.My_Is_Valid := Result = 0;
   end Get_File_Status;

   procedure Write (File : Linux.File; Bytes : Stream_Element_Array) is
      SSize : SSize_Type;
      pragma Unreferenced (SSize);
   begin
      SSize :=
        Px_Thin.Write
          (File_Descriptor => File.My_File_Descriptor,
           Buffer          => Bytes,
           Count           => Bytes'Length);
   end Write;

   function Read (File : Linux.File; Bytes : in out Stream_Element_Array) return SSize_Type is
   begin
      return C_Read
        (Interfaces.C.int (File.My_File_Descriptor), Bytes, Bytes'Length);
   end Read;

   procedure Map_Memory
     (File    : in Linux.File;
      Address : Void_Ptr;
      Len     : Size_Type;
      Prot    : Prot_FLag;
      Flags   : int;
      Offset  : Linux.Offset;
      Memory_Map : in out Linux.Memory_Map) is
   begin
      Memory_Map.My_Mapping := Px_Thin.Mmap (Address,
                                             Len,
                                             Prot,
                                             Flags,
                                             File.My_File_Descriptor,
                                             Offset);
      Memory_Map.My_Length := Len;
   end Map_Memory;

   function Unmap_Memory (Map : in out Linux.Memory_Map) return Integer is
      R : Integer;
   begin
      R := Px_Thin.Munmap (Map.My_Mapping, Map.My_Length);
      if R = 0 then
         Map.My_Mapping := MAP_FAILED;
      end if;
      return R;
   end Unmap_Memory;

   function Memory_Unmap (Address : Void_Ptr;
                          Length  : Size_Type) return Integer is
   begin
      return Px_Thin.Munmap (Address, Length);
   end Memory_Unmap;

   New_Line : constant String := (1 => Character'Val (10));

   procedure Put_Line (Text : String) is
      SSize : SSize_Type;
      pragma Unreferenced (SSize);
   begin
      SSize := Px_Thin.Write (File_Descriptor => Px_Thin.STDOUT_FILENO,
                              Buffer          => Text,
                              Count           => Text'Length);
      SSize := Px_Thin.Write (File_Descriptor => Px_Thin.STDOUT_FILENO,
                              Buffer          => New_Line,
                              Count           => 1);
   end Put_Line;

   procedure Put (Text : String) is
      SSize : SSize_Type;
      pragma Unreferenced (SSize);
   begin
      SSize := Px_Thin.Write (File_Descriptor => Px_Thin.STDOUT_FILENO,
                              Buffer          => Text,
                              Count           => Text'Length);
   end Put;

   function Get_Line return String is
      SSize : SSize_Type;
      B : Stream_Element_Array (1..200);
   begin
      SSize := C_Read (File_Descriptor => Px_Thin.STDIN_FILENO,
                       Buffer          => B,
                       Count           => Size_Type (200));

      if SSize > 1 then
         declare
            S : String (1..Integer (SSize));
         begin
            for I in Integer range 1..Integer (SSize) loop
               S (I)
                 := Character'Val (B (Ada.Streams.Stream_Element_Offset (I)));
            end loop;
            return S (1..Integer (SSize - 1));
         end;
      else
         return "";
      end if;

   end Get_Line;

   procedure Send
     (This     : Socket;
      Elements : Ada.Streams.Stream_Element_Array;
      Last     : out Stream_Element_Offset)
   is
      Written_Count : SSize_Type;
   begin
      Written_Count :=
        Px_Thin.Write
          (File_Descriptor => This.My_File_Descriptor,
           Buffer          => Elements,
           Count           => Elements'Length);
      Last := Elements'First + Stream_Element_Offset (Written_Count) - 1;
   end Send;

   function Convert_Unchecked is new Ada.Unchecked_Conversion
     (Source => Interfaces.C.int,
      Target => O_FLag);

   function Convert_Unchecked is new Ada.Unchecked_Conversion
     (Source => O_FLag,
      Target => Interfaces.C.int);

   procedure Set_File_Descriptor_Flag_Non_Blocking
     (File_Descriptor : in out Interfaces.C.int)
   is
      Temp : O_FLag := Convert_Unchecked (File_Descriptor);
   begin
      Temp := Temp or O_NONBLOCK;
      File_Descriptor := Convert_Unchecked (Temp);
   end Set_File_Descriptor_Flag_Non_Blocking;

   function Convert_Unchecked is new Ada.Unchecked_Conversion
     (Source => Interfaces.C.unsigned,
      Target => Interfaces.Unsigned_32);

   function Convert_Unchecked is new Ada.Unchecked_Conversion
     (Source => Interfaces.Unsigned_32,
      Target => Interfaces.C.unsigned);

   function Is_Epoll_Error
     (Event_Flags : Interfaces.C.unsigned) return Boolean
   is
      Temp : constant Interfaces.Unsigned_32
        := Convert_Unchecked (Event_Flags);
   begin
      return (Temp and EPOLLERR) > 0;
   end Is_Epoll_Error;

   function Has_Hang_Up_Happened
     (Event_Flags : Interfaces.C.unsigned) return Boolean
   is
      Temp : constant Interfaces.Unsigned_32
        := Convert_Unchecked (Event_Flags);
   begin
      return (Temp and EPOLLHUP) > 0;
   end Has_Hang_Up_Happened;

   function Is_Data_Available_For_Reading
     (Event_Flags : Interfaces.C.unsigned) return Boolean
   is
      Temp : constant Interfaces.Unsigned_32
        := Convert_Unchecked (Event_Flags);
   begin
      return (Temp and EPOLLIN) > 0;
   end Is_Data_Available_For_Reading;

end C_Binding.Linux;
