with C_Binding.Linux.Files;

package C_Binding.Linux.File_Status is

   use all type C_Binding.Linux.Files.File;

   type Status_Time is record
      Seconds      : Long_Integer;
      Nano_Seconds : Long_Integer;
   end record;

   subtype Device_Id_Type is unsigned_long;

   subtype Inode_Number_Type is unsigned_long;

   subtype Hard_Link_Count_Type is unsigned_long;

   subtype Mode_Type is unsigned;

   subtype User_Id_Type is unsigned;

   subtype Group_Id_Type is unsigned;

   subtype Block_Size_Type is long;

   subtype Block_Count_Type is long;

   subtype Time_Sec is long;

   subtype Time_Nano_Sec is long;

   subtype Offset is long;

   type Status is limited private;

   procedure Get_File_Status
     (File : in     C_Binding.Linux.Files.File;
      This : in out Status) with
     Global => null,
     Pre    => Is_Open (File);

   function Is_Valid (This : Status) return Boolean with
     Global => null;

   function Device_Id (This : Status) return Device_Id_Type with
     Global => null,
     Pre    => Is_Valid (This);

   function Inode_Number (This : Status) return Inode_Number_Type with
     Global => null,
     Pre    => Is_Valid (This);

   function Hard_Link_Count
     (This : Status) return Hard_Link_Count_Type with
     Global => null,
     Pre    => Is_Valid (This);

   function Mode (This : Status) return Mode_Type with
     Global => null,
     Pre    => Is_Valid (This);

   function User_Id (This : Status) return User_Id_Type with
     Global => null,
     Pre    => Is_Valid (This);

   function Group_Id (This : Status) return Group_Id_Type with
     Global => null,
     Pre    => Is_Valid (This);

   function Special_Device_Id (This : Status) return Device_Id_Type with
     Global => null,
     Pre    => Is_Valid (This);

   function Size (This : Status) return Ada.Streams.Stream_Element_Count with
     Global => null,
     Pre    => Is_Valid (This);
   -- The file size in bytes.

   function Block_Size (This : Status) return Block_Size_Type with
     Global => null,
     Pre    => Is_Valid (This);

   -- Number of 512B blocks allocated
   function Block_Count (This : Status) return Block_Size_Type with
     Global => null,
     Pre    => Is_Valid (This);

   function Last_Access_Time (This : Status) return Status_Time with
     Global => null,
     Pre    => Is_Valid (This);

   function Modification_Time (This : Status) return Status_Time with
     Global => null,
     Pre    => Is_Valid (This);

   -- Last status change time
   function Change_Time (This : Status) return Status_Time with
     Global => null,
     Pre    => Is_Valid (This);

private

   type C_Time is record
      Sec      : aliased Time_Sec;
      Nano_Sec : aliased Time_Nano_Sec;
   end record with
     Convention => C_Pass_By_Copy;

   type File_Status_T is record
      -- ID of device containing file
      Device_Id : aliased Device_Id_Type;

      Inode_Number    : aliased Inode_Number_Type;
      Hard_Link_Count : aliased Hard_Link_Count_Type;

      -- Protection
      Mode : aliased Mode_Type;

      User_Id   : aliased User_Id_Type;
      Group_Id  : aliased Group_Id_Type;
      Padding_0 : aliased int;

      -- Device ID (if special file)
      Special_Device_Id : aliased Device_Id_Type;

      -- Total size, in bytes
      Size : aliased Offset;

      -- Blocksize for file system I/O
      Block_Size : aliased Block_Size_Type;

      -- Number of 512B blocks allocated
      Block_Count : aliased Block_Count_Type;

      -- Time of last access
      Access_Time : aliased C_Time;

      -- Time of last modification
      Modification_Time : aliased C_Time;

      -- Time of last status change
      Change_Time : aliased C_Time;
      Padding_1   : long;
      Padding_2   : long;
      Padding_3   : long;
   end record with
     Convention => C_Pass_By_Copy;

   type Status is limited record
      My_Status   : aliased File_Status_T;
      My_Is_Valid : Boolean := False;
   end record;

   function Is_Valid
     (This : Status) return Boolean is
     (This.My_Is_Valid);

   function Device_Id
     (This : Status) return Device_Id_Type is
     (This.My_Status.Device_Id);

   function Inode_Number
     (This : Status) return Inode_Number_Type is
     (This.My_Status.Inode_Number);

   function Hard_Link_Count
     (This : Status) return Hard_Link_Count_Type is
     (This.My_Status.Hard_Link_Count);

   function Mode
     (This : Status) return Mode_Type is (This.My_Status.Mode);

   function User_Id
     (This : Status) return User_Id_Type is
     (This.My_Status.User_Id);

   function Group_Id
     (This : Status) return Group_Id_Type is
     (This.My_Status.Group_Id);

   function Special_Device_Id
     (This : Status) return Device_Id_Type is
     (This.My_Status.Special_Device_Id);

   function Size
     (This : Status) return Ada.Streams.Stream_Element_Count is
     (Ada.Streams.Stream_Element_Count (This.My_Status.Size));

   function Block_Size
     (This : Status) return Block_Size_Type is
     (This.My_Status.Block_Size);

   function Block_Count
     (This : Status) return Block_Size_Type is
     (This.My_Status.Block_Count);

   function Last_Access_Time
     (This : Status) return Status_Time is
     ((Seconds      => Long_Integer (This.My_Status.Access_Time.Sec),
       Nano_Seconds => Long_Integer (This.My_Status.Access_Time.Nano_Sec)));

   function Modification_Time
     (This : Status) return Status_Time is
     ((Seconds      => Long_Integer (This.My_Status.Modification_Time.Sec),
       Nano_Seconds =>
          Long_Integer (This.My_Status.Modification_Time.Nano_Sec)));

   function Change_Time
     (This : Status) return Status_Time is
     ((Seconds      => Long_Integer (This.My_Status.Change_Time.Sec),
       Nano_Seconds => Long_Integer (This.My_Status.Change_Time.Nano_Sec)));

   function C_Get_File_Status
     (Fd     : Interfaces.C.int;
      Status : access File_Status_T) return Interfaces.C.int with
     Import        => True,
     Convention    => C,
     External_Name => "fstat";

end C_Binding.Linux.File_Status;
