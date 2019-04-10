package body C_Binding.Linux is

   use type Stream_Element_Offset;
   use type Interfaces.C.unsigned;
   use type Interfaces.Unsigned_32;

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

end C_Binding.Linux;
