with C_Binding.Linux.Udev.Contexts;
with C_Binding.Linux.Udev.List_Entries;

with C_Binding.Linux.Udev.Devices;

package C_Binding.Linux.Udev.Enumerates is

   type Enumerate;

   procedure Acquire_Reference
     (Original  : Enumerate;
      Reference : out Enumerate) with
     Pre => Enumerates.Exists (Original);

   type Enumerate is new Enumerate_Base with private;

   function Exists (Enum : Enumerate) return Boolean;

   procedure Delete (Enum : in out Enumerate) with
     Pre  => Enum.Exists,
     Post => not Enum.Exists;

   function Add_Match_Subsystem
     (Enum      : Enumerate;
      Subsystem : String) return Success_Flag;

   function Scan_Devices
     (Enum : Enumerate) return Success_Flag with
     Pre => Enum.Exists;

   function Scan_Subsystems
     (Enum : Enumerate) return Success_Flag with
     Pre => Enum.Exists;

   procedure Get_List_Entry
     (Enum : Enumerate;
      LE   : out List_Entries.List_Entry);

   procedure Context
     (Enum    : Enumerate;
      Context : out Contexts.Context) with
     Pre => Enum.Exists;

   function Add_Nomatch_Subsystem
     (Enum      : Enumerate;
      Subsystem : String) return Success_Flag with
     Pre => Enum.Exists;

   function Add_Match_Sysattr
     (Enum    : Enumerate;
      Sysattr : String;
      Value   : String) return Success_Flag with
     Pre => Enum.Exists;

   function Add_Nomatch_Sysattr
     (Enum    : Enumerate;
      Sysattr : String;
      Value   : String) return Success_Flag with
     Pre => Enum.Exists;

   function Add_Match_Property
     (Enum     : Enumerate;
      Property : String;
      Value    : String) return Success_Flag with
     Pre => Enum.Exists;

   function Add_Match_Sysname
     (Enum    : Enumerate;
      Sysname : String) return Success_Flag with
     Pre => Enum.Exists;

   function Add_Match_Tag
     (Enum : Enumerate;
      Tag  : String) return Success_Flag with
     Pre => Enum.Exists;

   function Add_Match_Parent
     (Enum   : Enumerate;
      Parent : Devices.Device) return Success_Flag with
     Pre => Enum.Exists;

   function Add_Match_Is_Initialized
     (Enum : Enumerate) return Success_Flag with
     Pre => Enum.Exists;

   function Add_Syspath
     (Enum    : Enumerate;
      Syspath : String) return Success_Flag with
     Pre => Enum.Exists;

private

   type Enumerate is new Enumerate_Base with null record;

   overriding
   procedure Finalize (Enum : in out Enumerate);

end C_Binding.Linux.Udev.Enumerates;
