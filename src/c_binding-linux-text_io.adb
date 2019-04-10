package body C_Binding.Linux.Text_IO is

   New_Line : constant String := (1 => Character'Val (10));

   procedure Put_Line (Text : String) is
      SSize : SSize_Type;
      pragma Unreferenced (SSize);
   begin
      SSize := C_Write (File_Descriptor => STDOUT_FILENO,
                        Buffer          => Text,
                        Count           => Text'Length);
      SSize := C_Write (File_Descriptor => STDOUT_FILENO,
                        Buffer          => New_Line,
                        Count           => 1);
   end Put_Line;

   procedure Put (Text : String) is
      SSize : SSize_Type;
      pragma Unreferenced (SSize);
   begin
      SSize := C_Write (File_Descriptor => STDOUT_FILENO,
                        Buffer          => Text,
                        Count           => Text'Length);
   end Put;

   function Get_Line return String is
      SSize : SSize_Type;
      B : Ada.Streams.Stream_Element_Array (1..200);
   begin
      SSize := C_Read (File_Descriptor => STDIN_FILENO,
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

end C_Binding.Linux.Text_IO;
