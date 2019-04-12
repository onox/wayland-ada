with Interfaces.C;

package C_Binding.Linux.Posix_Select is

   type File_Descriptor_Count is range 1 .. 1024;

   type Fds_Bits_Index is range 0 .. 1023;
   --  The interval is chosen 0 .. 1023 to be able to use file descriptors
   --  as index into the Fds_Bits_Array.

   type File_Descriptor_Set is limited private;

   procedure Clear (This : in out File_Descriptor_Set);

   procedure Set_File_Descriptor
     (This       : in out File_Descriptor_Set;
      Descriptor : Integer);

   type Time_Seconds is new Interfaces.C.long;

   type Time_Micro_Seconds is new Interfaces.C.long;

   type Time_Value is record
      Seconds       : Time_Seconds;
      Micro_Seconds : Time_Micro_Seconds;
   end record;

   type Call_Select_Result_Id is
     (
      Select_Failure, --  Check errno
      Select_Timeout, --  Select timed out, no file descriptor events detected
      Select_Success
     );

   type Call_Select_Result (Id : Call_Select_Result_Id) is record
      case Id is
         when Select_Failure =>
            null;
         when Select_Timeout =>
            null;
         when Select_Success =>
            Descriptor_Count : File_Descriptor_Count;
      end case;
   end record;

   function Call_Select
     (File_Descriptor : Integer;
      Read_File_Descriptors   : access File_Descriptor_Set;
      Write_File_Descriptors  : access File_Descriptor_Set;
      Except_File_Descriptors : access File_Descriptor_Set;
      Time                    : Time_Value) return Call_Select_Result;

private

   type Fds_Bits_Array is array (Fds_Bits_Index) of Boolean with Pack;

   type fd_set is record
      Fds_Bits : aliased Fds_Bits_Array;
   end record with
     Convention => C_Pass_By_Copy,
     Size       => 1024;

   type File_Descriptor_Set is limited record
      Descriptors : aliased fd_set;
   end record;

   type timeval is record
      tv_sec  : aliased long;
      tv_usec : aliased long; -- Microseconds!
   end record with
     Convention => C_Pass_By_Copy;

   function C_Select
     (File_Descriptor : Integer;
      Readfds   : access fd_set;
      Writefds  : access fd_set;
      Exceptfds : access fd_set;
      Time      : access timeval) return Interfaces.C.int;
   pragma Import (C, C_Select, "select");
   --  On success, returns the number of file descriptors contained
   --  in the three returned descriptor sets (that is,
   --  the total number of bits that are set in readfds, writefds, exceptfds)
   --  which may be zero if the timeout expires before
   --  anything interesting happens. On error, -1 is returned,
   --  and errno is set appropriately; the sets and timeout become undefined,
   --  so do not rely on their contents after an error.

end C_Binding.Linux.Posix_Select;
