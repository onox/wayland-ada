with Interfaces.C.Strings;

private with Ada.Finalization;

--  Udev is "abbreviation" of Userspace /dev and libudev is the name of a
--  Linux specific library written in C that gives access to Linux
--  device and hardware management. By using libudev an application
--  can find out about:
--
--   - Installed hardware
--   - When a USB device is inserted or removed
--   - When a network cable is inserted or removed
--
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

   type Context_Base    is abstract tagged limited private;
   type Device_Base     is abstract tagged limited private;
   type Enumerate_Base  is abstract tagged limited private;
   type Monitor_Base    is abstract tagged limited private;
   type List_Entry_Base is abstract tagged limited private;
   type Queue_Base      is abstract tagged limited private;

   type Hwdb_Base       is abstract tagged limited private;
   --  Abbreviations are generally avoided when working with Ada,
   --  but hwdb (short for hardware database) is used here because this type
   --  can never used directly by a user of this API.

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

   type Udev_Queue is null record;

   type Udev_Queue_Ptr is access Udev_Queue;

   type Udev_Hwdb is null record;
   --  Opaque object representing the hardware database.

   type Udev_Hwdb_Ptr is access Udev_Hwdb;

   function Udev_Util_Encode_String
     (Arg1 : Interfaces.C.Strings.Chars_Ptr;
      Arg2 : Interfaces.C.Strings.Chars_Ptr;
      Arg3 : Unsigned_Long) return Int;
   pragma Import (C, Udev_Util_Encode_String, "udev_util_encode_string");
   --  Encode all potentially unsafe characters of a string to
   --  the corresponding 2 char hex value prefixed by '\x'.
   --
   --  What to do with this C-function?

   type Monitor_Base is new Ada.Finalization.Limited_Controlled with record
      My_Ptr : Udev_Monitor_Ptr;
   end record;

   type Context_Base is new Ada.Finalization.Limited_Controlled with record
      My_Ptr : Udev_Ptr;
   end record;

   type Device_Base is new Ada.Finalization.Limited_Controlled with record
      My_Ptr : Udev_Device_Ptr;
   end record;

   type Enumerate_Base is new Ada.Finalization.Limited_Controlled with record
      My_Ptr : Udev_Enumerate_Ptr;
   end record;

   type List_Entry_Base is tagged limited record
      My_Ptr : Udev_List_Entry_Ptr;
   end record;

   type Queue_Base is new Ada.Finalization.Limited_Controlled with record
      My_Ptr : Udev_Queue_Ptr;
   end record;

   type Hwdb_Base is new Ada.Finalization.Limited_Controlled with record
      My_Ptr : Udev_Hwdb_Ptr;
   end record;

end C_Binding.Linux.Udev;
