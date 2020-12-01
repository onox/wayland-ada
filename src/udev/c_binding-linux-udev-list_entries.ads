package C_Binding.Linux.Udev.List_Entries is

   type List_Entry;

   procedure Get_By_Name
     (
      Current : List_Entry;
      Name    : String;
      Found_Entry : out List_Entry
     ) with
       Pre => List_Entries.Exists (Current);

   type List_Entry is new List_Entry_Base with private;

   function Exists (List_Entry : List_Entries.List_Entry) return Boolean;

   procedure Next (List_Entry : in out List_Entries.List_Entry) with
     Pre => List_Entry.Exists;

   function Name  (List_Entry : List_Entries.List_Entry) return String_Result;
   function Value (List_Entry : List_Entries.List_Entry) return String_Result;

private

   type List_Entry is new List_Entry_Base with null record;

end C_Binding.Linux.Udev.List_Entries;
