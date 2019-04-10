package body C_Binding.Linux.File_Status is

   procedure Get_File_Status
     (File : in     C_Binding.Linux.Files.File;
      This : in out Status)
   is
      Result : constant Interfaces.C.int :=
        C_Get_File_Status
          (Fd     => File.My_File_Descriptor,
           Status => This.My_Status'Access);
   begin
      This.My_Is_Valid := Result = 0;
   end Get_File_Status;

end C_Binding.Linux.File_Status;
