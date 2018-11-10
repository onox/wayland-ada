with C_Binding.Linux.Udev.Contexts;
with C_Binding.Linux.Udev.List_Entries;

package C_Binding.Linux.Udev.Enumerates is

   type Enumerate is tagged limited private;

   procedure Create (Enum    : out Enumerate;
                     Context : Contexts.Context);

   function Exists (Enum : Enumerate) return Boolean;

   procedure Delete (Enum : in out Enumerate) with
     Pre  => Enum.Exists,
     Post => not Enum.Exists;

   function Add_Match_Subsystem
     (Enum      : Enumerate;
      Subsystem : String) return Success_Flag;

   function Scan_Devices
     (Enum : Enumerate) return Success_Flag;

   procedure Get_List_Entry (Enum : Enumerate;
                             LE   : out List_Entries.List_Entry);

private

   type Enumerate is tagged limited record
      My_Ptr : Udev_Enumerate_Ptr;
   end record;

end C_Binding.Linux.Udev.Enumerates;
