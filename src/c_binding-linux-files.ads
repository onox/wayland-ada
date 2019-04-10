package C_Binding.Linux.Files is

   type File is new File_Base;
   --with Default_Initial_Condition => Is_Closed (File);
   --  Represents a file on the hard disk.

   procedure Set_File_Descriptor
     (This  : in out File;
      Value : Integer);

   procedure Open
     (This        : in out File;
      File_Name   : in     String;
      Mode        : in     File_Mode;
      Permissions : in     File_Permissions) with
     Global => null,
     Pre    => Is_Closed (This);

   function Close (This : in out File) return Success_Flag with
     Global => null,
     Pre    => Is_Open (This),
     Post   => Is_Closed (This);

   procedure Write (This : File; Bytes : Stream_Element_Array) with
     Global => null,
     Pre    => Is_Open (This);

   function Read
     (This  : File;
      Bytes : in out Stream_Element_Array) return SSize_Type with
     Global => null,
     Pre    => Is_Open (This);

   function File_Descriptor (This : File) return Integer with
     Global => null,
     Pre    => Is_Open (This);

   function Is_Open (This : File) return Boolean with
     Global => null;

   function Is_Closed (This : File) return Boolean with
     Global => null;

private

   function File_Descriptor (This : File) return Integer is
     (Integer (This.My_File_Descriptor));

   function Is_Open (This : File) return Boolean is
     (This.My_File_Descriptor /= -1);

   function Is_Closed (This : File) return Boolean is
     (This.My_File_Descriptor = -1);

   --
   --  The following Ada bindings to C functions are defined here to
   --  be available in child packages:
   --

   function C_Open
     (File_Name : C_String;
      Flags     : O_FLag;
      S_Flags   : S_FLag) return Interfaces.C.int with
     Import        => True,
     Convention    => C,
     External_Name => "open";
   -- Establishes a connection between a file and a file descriptor.
   -- The file descriptor handle (a non-negative number)
   -- is returned upon success, otherwise -1.
   --
   -- Applications shall specify exactly one of the first three flags:
   -- O_RDONLY, O_WRONLY and O_RDWR. And then any combination of O_APPEND,
   -- O_CREAT, O_DSYNC, O_EXCL, O_NOCTTY, O_NONBLOCK, O_RSYNC,
   -- O_SYNC, O_TRUNC.

end C_Binding.Linux.Files;
