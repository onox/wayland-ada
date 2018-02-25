--with Ada.Text_IO;
with Ada.Containers.Hashed_Maps;
with System.Storage_Elements;

package body Wl is

   use type Wl_Thin.Proxy_Ptr;
   use type Wl_Thin.Display_Ptr;
   use type Wl_Thin.Registry_Ptr;

   use all type Ada.Strings.Unbounded.Unbounded_String;

   subtype Registry_Ptr is Wl_Thin.Registry_Ptr;

   subtype Registry_Global_Subprogram_Ptr is Wl_Thin.Registry_Global_Subprogram_Ptr;

   subtype Registry_Global_Remove_Subprogram_Ptr is Wl_Thin.Registry_Global_Remove_Subprogram_Ptr;

   subtype Registry_Listener_T is Wl_Thin.Registry_Listener_T;

   subtype Registry_Listener_Ptr is Wl_Thin.Registry_Listener_Ptr;

   package body Registry_Objects_Subscriber is

      procedure Internal_Object_Added (Unused_Data : Wl.Void_Ptr;
                                           Registry    : Wl.Registry_Ptr;
                                           Id          : Wl.Unsigned_32;
                                           Interface_V : Wl.chars_ptr;
                                           Version     : Wl.Unsigned_32) with
        Convention => C,
        Global     => null;

      procedure Internal_Object_Added (Unused_Data : Wl.Void_Ptr;
                                           Registry    : Wl.Registry_Ptr;
                                           Id          : Wl.Unsigned_32;
                                           Interface_V : Wl.chars_ptr;
                                           Version     : Wl.Unsigned_32)
      is
         pragma Unreferenced (Unused_Data);

         R : Registry_T := (
                            My_Registry                 => Registry,
                            My_Has_Started_Subscription => True
                           );
      begin
         Global_Object_Added (Data, R, Id, Value (Interface_V), Version);
      end Internal_Object_Added;

      procedure Internal_Object_Removed (Unused_Data : Wl.Void_Ptr;
                                         Registry    : Wl.Registry_Ptr;
                                         Id          : Wl.Unsigned_32) with
        Convention => C;

      procedure Internal_Object_Removed (Unused_Data : Wl.Void_Ptr;
                                         Registry    : Wl.Registry_Ptr;
                                         Id          : Wl.Unsigned_32)
      is
         R : Registry_T := (
                            My_Registry                 => Registry,
                            My_Has_Started_Subscription => True
                           );
      begin
         Global_Object_Removed (Data, R, Id);
      end Internal_Object_Removed;

      Listener : aliased Wl.Registry_Listener_T :=
        (
         Global        => Internal_Object_Added'Unrestricted_Access,
         Global_Remove => Internal_Object_Removed'Unrestricted_Access
        );

      procedure Start_Subscription (Registry : in out Registry_T) is
         I : Wl.int;
      begin
         I := Wl_Thin.Registry_Add_Listener (Registry.My_Registry,
                                             Listener'Unchecked_Access,
                                             Wl.Null_Address);
      end Start_Subscription;

   end Registry_Objects_Subscriber;

   procedure Connect (Display : in out Display_T;
                      Name    : Interfaces.C.Strings.char_array_access) is
   begin
      Display.My_Display := Wl_Thin.Display_Connect (Name);
   end Connect;

   procedure Disconnect (Display : in out Display_T) is
   begin
      if Display.My_Display /= null then
         Wl_Thin.Display_Disconnect (Display.My_Display);
      end if;
   end Disconnect;

   procedure Get (Registry : in out Registry_T;
                  Display  : Display_T) is
   begin
      Registry.My_Registry := Wl_Thin.Display_Get_Registry (Display.My_Display);
   end Get;

   procedure Destroy (Registry : in out Registry_T) is
   begin
      if Registry.My_Registry /= null then
         Wl_Thin.Registry_Destroy (Registry.My_Registry);
         Registry.My_Registry := null;
      end if;
   end Destroy;

   function Dispatch (Display : Display_T) return Interfaces.C.int is
   begin
      return Wl_Thin.Display_Dispatch (Display.My_Display);
   end Dispatch;

   procedure Dispatch (Display : Display_T) is
      I : Interfaces.C.int;
      pragma Unreferenced (I);
   begin
      I := Display.Dispatch;
   end Dispatch;

   function Roundtrip (Display : Display_T) return Interfaces.C.int is
   begin
      return Wl_Thin.Display_Roundtrip (Display.My_Display);
   end Roundtrip;

   procedure Roundtrip (Display : Display_T) is
      I : Interfaces.C.int;
      pragma Unreferenced (I);
   begin
      I := Display.Roundtrip;
   end Roundtrip;

   procedure Bind (Compositor  : in out Compositor_T;
                   Registry    : Registry_T;
                   Id          : Wl.Unsigned_32;
                   Version     : Wl.Unsigned_32)
   is
      P : Wl_Thin.Proxy_Ptr :=
        Wl_Thin.Registry_Bind (Registry    => Registry.My_Registry,
                               Name        => Id,
                               Interface_V => Wl_Thin.Compositor_Interface'Access,
                               New_Id      => Version);

   begin
      if P /= null then
         Compositor.My_Compositor := P.all'Access;
      end if;
   end Bind;

end Wl;
