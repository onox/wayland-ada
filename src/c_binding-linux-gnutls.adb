package body C_Binding.Linux.GnuTLS is

   use type Interfaces.C.Strings.chars_ptr;

   function Check_Version (Version : String) return String_Result is
      V : aliased Interfaces.C.char_array
        := Interfaces.C.To_C (Version);
      C_Result : Interfaces.C.Strings.chars_ptr
        := C_Check_Version (V);
   begin
      if C_Result = Interfaces.C.Strings.Null_Ptr then
         declare
            Version_Doesnt_Match : constant String := "Version doesn't match";
         begin
            return (Is_Success => False,
                    Length     => Version_Doesnt_Match'Length,
                    Error      => Version_Doesnt_Match);
         end;
      else
         declare
            Result : constant String := Interfaces.C.Strings.Value (C_Result);
         begin
            return (Is_Success => True,
                    Length     => Result'Length,
                    Value      => Result);
         end;
      end if;
   end Check_Version;

   function Get_Version return String is
   begin
      return Interfaces.C.Strings.Value
        (C_Check_Version (Interfaces.C.Strings.Null_Ptr));
   end Get_Version;

   procedure Initialize_GnuTLS is
      Result : constant Interfaces.C.int := C_Global_Init;
   begin
      if Result = 0 then
         begin
            Handle_Success;
            C_Global_Deinit;
         exception
            when Error : others =>
               C_Global_Deinit;
               raise;
         end;
      else
         Handle_Failure;
      end if;
   end Initialize_GnuTLS;

end C_Binding.Linux.GnuTLS;
