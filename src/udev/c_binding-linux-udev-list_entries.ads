package C_Binding.Linux.Udev.List_Entries is

   type List_Entry;

   procedure Get_By_Name
     (
      Current : List_Entry;         --  current entry
      Name    : String;             --  name string to match
      Found_Entry : out List_Entry  --  The entry where name matched
     ) with
       Pre => List_Entries.Exists (Current);
   --  On success, Found_Entry.Exists = True
   --  On failure, Found_Entry.Exists = False

   type List_Entry is new List_Entry_Base with private;

   function Exists (List_Entry : List_Entries.List_Entry) return Boolean;

   procedure Next (List_Entry : in out List_Entries.List_Entry) with
     Pre => List_Entry.Exists;

   function Name  (List_Entry : List_Entries.List_Entry) return String_Result;
   function Value (List_Entry : List_Entries.List_Entry) return String_Result;

private

   type List_Entry is new List_Entry_Base with null record;

end C_Binding.Linux.Udev.List_Entries;
