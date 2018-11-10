with System;
with Interfaces.C.Strings;

--  Udev is "abbreviation" of Userspace /dev.
--  API for enumerating and introspecting local devices.
package C_Binding.Linux.Udev is
   pragma Elaborate_Body;

   subtype Max_Length is Natural range 0 .. 10_000;

   type String_Result
     (Is_Success : Boolean := False;
      Length     : Max_Length := 1)
   is record
      case Is_Success is
         when True  => Value : String (1 .. Length);
         when False => Error : String (1 .. Length);
      end case;
   end record;

   type Success_Flag is
     (
      Success,
      Failure
     );

   type Context_Base is tagged limited private;
   type Device_Base is tagged limited private;
   type Monitor_Base is tagged limited private;
   type List_Entry_Base is tagged limited private;

private

   function Get_String_Result
     (Text  : Interfaces.C.Strings.Chars_Ptr;
      Error : String) return String_Result;

   type Udev_Context is null record;
   --  Opaque object representing the udev library context.

   type Udev_Ptr is access Udev_Context;

   type Udev_List_Entry is null record;

   type Udev_List_Entry_Ptr is access Udev_List_Entry;

   type Udev_Device is null record;
   --  This object is opaque and must not be accessed by the caller via
   --  different means than functions provided by libudev.
   --  Initially, the reference count of the device is 1.
   --  You can acquire further references, and drop gained references via
   --  udev_device_ref() and udev_device_unref(). Once the reference count
   --  hits 0, the device object is destroyed and freed.

   type Udev_Device_Ptr is access Udev_Device;

   type Udev_Enumerate is null record;

   type Udev_Enumerate_Ptr is access Udev_Enumerate;

   type Udev_Monitor is null record;

   type Udev_Monitor_Ptr is access Udev_Monitor;

   package Thin is

      function Udev_Queue_Ref (Arg1 : System.Address) return System.Address;
      pragma Import (C, Udev_Queue_Ref, "udev_queue_ref");

      function Udev_Queue_Unref (Arg1 : System.Address) return System.Address;
      pragma Import (C, Udev_Queue_Unref, "udev_queue_unref");

      function Udev_Queue_Get_Udev
        (Arg1 : System.Address) return System.Address;
      pragma Import (C, Udev_Queue_Get_Udev, "udev_queue_get_udev");

      function Udev_Queue_New (Arg1 : System.Address) return System.Address;
      pragma Import (C, Udev_Queue_New, "udev_queue_new");

      function Udev_Queue_Get_Kernel_Seqnum
        (Arg1 : System.Address) return Interfaces.Integer_64;
      pragma Import
        (C, Udev_Queue_Get_Kernel_Seqnum, "udev_queue_get_kernel_seqnum");

      function Udev_Queue_Get_Udev_Seqnum
        (Arg1 : System.Address) return Interfaces.Integer_64;
      pragma Import
        (C, Udev_Queue_Get_Udev_Seqnum, "udev_queue_get_udev_seqnum");

      function Udev_Queue_Get_Udev_Is_Active
        (Arg1 : System.Address) return Int;
      pragma Import
        (C, Udev_Queue_Get_Udev_Is_Active, "udev_queue_get_udev_is_active");

      function Udev_Queue_Get_Queue_Is_Empty
        (Arg1 : System.Address) return Int;
      pragma Import
        (C, Udev_Queue_Get_Queue_Is_Empty, "udev_queue_get_queue_is_empty");

      function Udev_Queue_Get_Seqnum_Is_Finished
        (Arg1 : System.Address;
         Arg2 : Interfaces.Integer_64) return Int;
      pragma Import
        (C,
         Udev_Queue_Get_Seqnum_Is_Finished,
         "udev_queue_get_seqnum_is_finished");

      function Udev_Queue_Get_Seqnum_Sequence_Is_Finished
        (Arg1 : System.Address;
         Arg2 : Interfaces.Integer_64;
         Arg3 : Interfaces.Integer_64) return Int;
      pragma Import
        (C,
         Udev_Queue_Get_Seqnum_Sequence_Is_Finished,
         "udev_queue_get_seqnum_sequence_is_finished");

      function Udev_Queue_Get_Fd (Arg1 : System.Address) return Int;
      pragma Import (C, Udev_Queue_Get_Fd, "udev_queue_get_fd");

      function Udev_Queue_Flush (Arg1 : System.Address) return Int;
      pragma Import (C, Udev_Queue_Flush, "udev_queue_flush");

      function Udev_Queue_Get_Queued_List_Entry
        (Arg1 : System.Address) return System.Address;
      pragma Import
        (C,
         Udev_Queue_Get_Queued_List_Entry,
         "udev_queue_get_queued_list_entry");

      function Udev_Hwdb_New (Arg1 : System.Address) return System.Address;
      pragma Import (C, Udev_Hwdb_New, "udev_hwdb_new");

      function Udev_Hwdb_Ref (Arg1 : System.Address) return System.Address;
      pragma Import (C, Udev_Hwdb_Ref, "udev_hwdb_ref");

      function Udev_Hwdb_Unref (Arg1 : System.Address) return System.Address;
      pragma Import (C, Udev_Hwdb_Unref, "udev_hwdb_unref");

      function Udev_Hwdb_Get_Properties_List_Entry
        (Arg1 : System.Address;
         Arg2 : Interfaces.C.Strings.Chars_Ptr;
         Arg3 : Unsigned) return System.Address;
      pragma Import
        (C,
         Udev_Hwdb_Get_Properties_List_Entry,
         "udev_hwdb_get_properties_list_entry");

      function Udev_Util_Encode_String
        (Arg1 : Interfaces.C.Strings.Chars_Ptr;
         Arg2 : Interfaces.C.Strings.Chars_Ptr;
         Arg3 : Unsigned_Long) return Int;
      pragma Import (C, Udev_Util_Encode_String, "udev_util_encode_string");

   end Thin;

   type Monitor_Base is tagged limited record
      My_Ptr : Udev_Monitor_Ptr;
   end record;

   type Context_Base is tagged limited record
      My_Ptr : Udev_Ptr;
   end record;

   type Device_Base is tagged limited record
      My_Ptr : Udev_Device_Ptr;
   end record;

   type List_Entry_Base is tagged limited record
      My_Ptr : Udev_List_Entry_Ptr;
   end record;

end C_Binding.Linux.Udev;
