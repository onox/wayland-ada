package body C_Binding.Linux.Udev is

   use type Interfaces.C.Strings.chars_ptr;

   function Get_String_Result
     (Text  : Interfaces.C.Strings.Chars_Ptr;
      Error : String) return String_Result is
   begin
      if Text /= Interfaces.C.Strings.Null_Ptr then
         declare
            Result : constant String := Interfaces.C.Strings.Value (Text);
         begin
            return
                (Is_Success => True,
                 Length     => Max_String_Length (Result'Length),
                 Value      => Result);
         end;
      else
         return
           (Is_Success => False,
            Length     => Max_String_Length (Error'Length),
            Error      => Error);
      end if;
   end Get_String_Result;

end C_Binding.Linux.Udev;
