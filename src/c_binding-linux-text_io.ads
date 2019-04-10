package C_Binding.Linux.Text_IO is

   -- Write to standard out. May be used instead of Ada.Text_IO.Put ().
   procedure Put (Text : String) with
     Global => null;

   -- Write to standard out. May be used instead of Ada.Text_IO.Put_Line ().
   procedure Put_Line (Text : String) with
     Global => null;

   function Get_Line return String;

end C_Binding.Linux.Text_IO;
