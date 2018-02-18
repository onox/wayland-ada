with Px.Thin;

package body Px is

   use type Interfaces.C.int;

   procedure Open (File      : in out File_T;
                   File_Name : in     C_String;
                   Flags     : in     O_FLag_T;
                   S_Flags   : in     S_FLag_T)
   is
   begin
      File.My_File_Descriptor := Px.Thin.Open (File_Name,
                                               Flags,
                                               S_Flags);
      File.My_Is_Open := File.My_File_Descriptor /= 0;
   end Open;

   procedure Close (File : in out File_T) is
   begin
      Px.Thin.Close (File.My_File_Descriptor);
      File.My_Is_Open := False;
   end Close;

end Px;
