package body C_Binding.Linux.Posix_Select is

   procedure Clear (This : in out File_Descriptor_Set) is
   begin
      This.Descriptors := (Fds_Bits => (others => False));
   end Clear;

   procedure Set_File_Descriptor
     (This       : in out File_Descriptor_Set;
      Descriptor : Integer) is
   begin
      This.Descriptors.Fds_Bits (Fds_Bits_Index (Descriptor)) := True;
   end Set_File_Descriptor;

   function Call_Select
     (File_Descriptor         : Integer;
      Read_File_Descriptors   : access File_Descriptor_Set;
      Write_File_Descriptors  : access File_Descriptor_Set;
      Except_File_Descriptors : access File_Descriptor_Set;
      Time                    : Time_Value) return Call_Select_Result
   is
      Tv : aliased timeval
        := (
            tv_sec  => Interfaces.C.long (Time.Seconds),
            tv_usec => Interfaces.C.long (Time.Micro_Seconds)
           );

      Result : Interfaces.C.int;
   begin
      Result
        := C_Select
          (File_Descriptor,
           (if (Read_File_Descriptors /= null) then
              (Read_File_Descriptors.Descriptors'Access)
            else
              (null)),
           (if (Write_File_Descriptors /= null) then
              (Write_File_Descriptors.Descriptors'Access)
            else
              (null)),
           (if (Except_File_Descriptors /= null) then
              (Except_File_Descriptors.Descriptors'Access)
            else
              (null)),
           Tv'Access);
      case Result is
         when Interfaces.C.int'First .. -1 =>
            return (Id => Select_Failure);
         when 0 =>
            return (Id => Select_Timeout);
         when 1 .. 1024 =>
            return
              (Id               => Select_Success,
               Descriptor_Count => File_Descriptor_Count (Result));
         when 1025 .. Interfaces.C.int'Last =>
            return (Id => Select_Failure);
      end case;
   end Call_Select;

end C_Binding.Linux.Posix_Select;
