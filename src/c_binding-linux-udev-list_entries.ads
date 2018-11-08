package C_Binding.Linux.Udev.List_Entries is

   type List_Entry is new List_Entry_Base with null record;

   function Exists (LE : List_Entry) return Boolean;

   procedure Next (LE : in out List_Entry) with
     Pre => LE.Exists;

   function Name  (LE : List_Entry) return String_Result;
   function Value (LE : List_Entry) return String_Result;

private

   function Exists (LE : List_Entry) return Boolean is
     (LE.My_Ptr /= null);

end C_Binding.Linux.Udev.List_Entries;
