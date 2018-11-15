with C_Binding.Linux.Udev.List_Entries;

--  Libudev hardware database interface.
--
--  The hardware database is a key-value store for associating modalias-like
--  keys to udev-properties-like values. It is used primarily by udev to add
--  the relevant properties to matching devices, but it can also be queried
--  directly.
package C_Binding.Linux.Udev.Hardware_Databases is

   type Database;

   procedure Acquire
     (Original  : Database;
      Reference : out Database) with
     Pre => Hardware_Databases.Exists (Original);
   --  Acquire a reference to an existing udev database object.
   --  The reference count to Original goes up by 1.

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
