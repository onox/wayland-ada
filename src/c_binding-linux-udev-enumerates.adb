package body C_Binding.Linux.Udev.Enumerates is

   use type int;

   procedure Create
     (Enum    : out Enumerate;
      Context : Contexts.Context) is
   begin
      Enum.My_Ptr := Thin.Udev_Enumerate_New (Context_Base (Context).My_Ptr);
   end Create;

   procedure Delete (Enum : in out Enumerate) is
   begin
      Enum.My_Ptr := Thin.Udev_Enumerate_Unref (Enum.My_Ptr);
      Enum.My_Ptr := null;
      --  Is unnecessary, but static code analyzers cannot know
      --  Thin.Udev_Enumerate_Unref (..) always returns null.
   end Delete;

   function Add_Match_Subsystem
     (Enum      : Enumerate;
      Subsystem : String) return Success_Flag is
   begin
      if
        Thin.Udev_Enumerate_Add_Match_Subsystem (Enum.My_Ptr, +Subsystem) >= 0
      then
         return Success;
      else
         return Failure;
      end if;
   end Add_Match_Subsystem;

   function Scan_Devices
     (Enum : Enumerate) return Success_Flag is
   begin
      if Thin.Udev_Enumerate_Scan_Devices (Enum.My_Ptr) >= 0 then
         return Success;
      else
         return Failure;
      end if;
   end Scan_Devices;

   procedure Get_List_Entry (Enum : Enumerate;
                             LE   : out List_Entries.List_Entry) is
   begin
      List_Entry_Base (LE).My_Ptr
        := Thin.Udev_Enumerate_Get_List_Entry (Enum.My_Ptr);
   end Get_List_Entry;

end C_Binding.Linux.Udev.Enumerates;
