with C_Binding.Linux.Udev.List_Entries;

package C_Binding.Linux.Udev.Hardware_Databases is
   pragma Obsolescent;

   type Database;

   procedure Acquire
     (Original  : Database;
      Reference : out Database) with
     Pre => Hardware_Databases.Exists (Original);

   type Database is new Hwdb_Base with private;

   function Exists (Database : Hardware_Databases.Database) return Boolean;

   procedure Delete (Database : in out Hardware_Databases.Database) with
     Pre  => Database.Exists,
     Post => not Database.Exists;

   procedure Properties_List_Entry
     (Database   : Hardware_Databases.Database;
      Modalias   : String;
      List_Entry : out List_Entries.List_Entry);

private

   type Database is new Hwdb_Base with null record;

   overriding
   procedure Finalize (Database : in out Hardware_Databases.Database);

end C_Binding.Linux.Udev.Hardware_Databases;
