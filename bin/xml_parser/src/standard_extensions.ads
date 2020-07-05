with Ada.Strings.Fixed;
with Ada.Text_IO;

package Standard_Extensions is

   function Trim (Source : String) return String is
     (Ada.Strings.Fixed.Trim (Source, Ada.Strings.Both));

   procedure Put
     (File : Ada.Text_IO.File_Type;
      Item : String) renames Ada.Text_IO.Put;

   procedure Put_Line
     (File : Ada.Text_IO.File_Type;
      Item : String) renames Ada.Text_IO.Put_Line;

   procedure New_Line
     (File    : Ada.Text_IO.File_Type;
      Spacing : Ada.Text_IO.Positive_Count := 1) renames Ada.Text_IO.New_Line;

   procedure Put_Line (Item : String) renames Ada.Text_IO.Put_Line;

end Standard_Extensions;
