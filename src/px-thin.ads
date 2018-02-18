with Interfaces.C.Strings;
with System;

private package Px.Thin is

   type Time_T is record
      Sec      : aliased Time_Sec_T;
      Nano_Sec : aliased Time_Nano_Sec_T;
   end record with
     Convention => C_Pass_By_Copy;

   type File_Status_T is record
      Device_Id               : aliased Device_Id_T; -- ID of device containing file
      Inode_Number            : aliased Inode_Number_T;
      Number_Of_Hard_Links    : aliased Number_Of_Hard_Links_T;
      Mode                    : aliased Mode_T; -- Protection
      Owner_User_Id           : aliased Owner_User_Id_T;
      Owner_Groud_Id          : aliased Owner_Groud_Id_T;
      Padding_0               : aliased int;
      Special_Device_Id       : aliased Device_Id_T;  -- Device ID (if special file)
      Size                    : aliased Size_T;  -- Total size, in bytes
      Block_Size              : aliased Block_Size_T; -- Blocksize for file system I/O
      Number_Of_Blocks        : aliased Number_Of_Blocks_T; --Number of 512B blocks allocated
      Last_Access_Time        : aliased Time_T; -- Time of last access
      Modification_Time       : aliased Time_T; -- time of last modification
      Last_Status_Change_Time : aliased Time_T; -- time of last status change
      Padding_1               : long;
      Padding_2               : long;
      Padding_3               : long;
   end record with
     Convention => C_Pass_By_Copy;

   type File_Status_Ptr is access all File_Status_T;

   function Get_File_Status (Fd     : int;
                             Status : File_Status_Ptr) return int with
     Import        => True,
     Convention    => C,
     External_Name => "fstat";

   -- Establishes a connection between a file and a file descriptor.
   -- The file descriptor handle (a non-negative number)
   -- is returned upon success, otherwise -1.
   --
   -- Applications shall specify exactly one of the first three flags:
   -- O_RDONLY, O_WRONLY and O_RDWR. And then any combination of O_APPEND,
   -- O_CREAT, O_DSYNC, O_EXCL, O_NOCTTY, O_NONBLOCK, O_RSYNC,
   -- O_SYNC, O_TRUNC.
   function Open (File_Name : C_String;
                  Flags     : O_FLag_T;
                  S_Flags   : S_FLag_T) return int with
     Import        => True,
     Convention    => C,
     External_Name => "open";

   procedure Close (Fd : int) with
     Import        => True,
     Convention    => C,
     External_Name => "close";

   function Mmap (Addr   : Void_Ptr;
                  Len    : Size_T;
                  Prot   : int;
                  Flags  : int;
                  Fd     : int;
                  Offset : Off_T) return Void_Ptr with
     Import        => True,
     Convention    => C,
     External_Name => "mmap";

   function Munmap (Addr   : Void_Ptr;
                    Length : Size_T) return int with
     Import        => True,
     Convention    => C,
     External_Name => "munmap";

end Px.Thin;
