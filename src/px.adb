with Px_Thin;

package body Px is

   use type Interfaces.C.int;

   procedure Open (File      : in out File_T;
                   File_Name : in     C_String;
                   Flags     : in     O_FLag_T;
                   S_Flags   : in     S_FLag_T)
   is
   begin
      File.My_File_Descriptor := Px_Thin.Open (File_Name,
                                               Flags,
                                               S_Flags);
      File.My_Is_Open := File.My_File_Descriptor /= 0;
   end Open;

   procedure Close (File : in out File_T) is
   begin
      Px_Thin.Close (File.My_File_Descriptor);
      File.My_Is_Open := False;
   end Close;

   procedure Get_File_Status (File        : in     File_T;
                              File_Status : in out File_Status_T)
   is
      Result : constant int := Px_Thin.Get_File_Status
        (Fd     => File.My_File_Descriptor,
         Status => File_Status.My_File_Status'Access);
   begin
      File_Status.My_Is_Valid := Result = 0;
   end Get_File_Status;

end Px;
