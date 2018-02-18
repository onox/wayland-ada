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
         Status => File_Status.My_Status'Access);
   begin
      File_Status.My_Is_Valid := Result = 0;
   end Get_File_Status;

   procedure Write (File  : File_T;
                    Bytes : Byte_Array_T)
   is
      SSize : SSize_T;
   begin
      SSize := Px_Thin.Write (File_Descriptor => File.My_File_Descriptor,
                              Buffer          => Bytes,
                              Count           => Bytes'Length);
   end Write;

   function Read (File  : File_T;
                  Bytes : Byte_Array_T) return SSize_T is
   begin
      return Px_Thin.Read (File.My_File_Descriptor, Bytes, Bytes'Length);
   end Read;

end Px;
