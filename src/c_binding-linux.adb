package body C_Binding.Linux is

   use type Ada.Streams.Stream_Element_Offset;
   use type Interfaces.C.unsigned;
   use type Interfaces.Unsigned_32;

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

   function Poll_File_Descriptor_Until_Timeout (Descriptor, Timeout : Integer) return Integer is
      File_Descriptors : constant C_Binding.Linux.Poll_File_Descriptor_Array
        := (1 => (Descriptor => Descriptor,
                  Events     => C_Binding.Linux.POLLIN,
                  Revents    => 0));
   begin
      return Poll (File_Descriptors, Timeout);
   end Poll_File_Descriptor_Until_Timeout;

end C_Binding.Linux;
