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

   package body Global_Object is

      function Make (
                     Data        : Wl.Void_Ptr;
                     Id          : Wl.Unsigned_32;
                     Interface_V : String;
                     Version     : Wl.Unsigned_32
                    )
                     return Global_Object_T
      is
         This : Global_Object_T := (
                                    My_Data        => Data,
                                    My_Id          => Id,
                                    My_Interface_V => To_Unbounded_String (Interface_V),
                                    My_Version     => Version
                                   );
      begin
         return This;
      end Make;

   end Global_Object;

   package body Global_Objects_Subscriber is

      My_Global_Objects : aliased Global_Object_Vectors.Vector;

      procedure Global_Registry_Handler (Data        : Wl.Void_Ptr;
                                         Registry    : Wl.Registry_Ptr;
                                         Id          : Wl.Unsigned_32;
                                         Interface_V : Wl.chars_ptr;
                                         Version     : Wl.Unsigned_32) with
        Convention => C,
        Global     => My_Global_Objects;

      procedure Global_Registry_Handler (Data        : Wl.Void_Ptr;
                                         Registry    : Wl.Registry_Ptr;
                                         Id          : Wl.Unsigned_32;
                                         Interface_V : Wl.chars_ptr;
                                         Version     : Wl.Unsigned_32)
      is
         Temp : Wl.char_array := Wl.Value (Interface_V);
         Interface_Name : String := Wl.To_Ada (Temp);

         GO : Global_Object_T := Global_Object.Make (Data, Id, Interface_Name, Version);
      begin
--         Ada.Text_IO.Put_Line ("Got a registry event for " & Interface_Name & " id" & Id'Image);
         My_Global_Objects.Append (GO);
      end Global_Registry_Handler;

      procedure Global_Registry_Remover (Data     : Wl.Void_Ptr;
                                         Registry : Wl.Registry_Ptr;
                                         Id       : Wl.Unsigned_32) with
        Convention => C;

      procedure Global_Registry_Remover (Data     : Wl.Void_Ptr;
                                         Registry : Wl.Registry_Ptr;
                                         Id       : Wl.Unsigned_32) is
      begin
         null;
--         Ada.Text_IO.Put_Line ("Got a registry losing event for");
      end;

      function Add_Listener (Registry : Registry_T;
                             Listener : Registry_Listener_Ptr;
                             Data     : Wl.Void_Ptr) return Interfaces.C.int;

      function Add_Listener (Registry : Registry_T;
                             Listener : Registry_Listener_Ptr;
                             Data     : Wl.Void_Ptr) return Interfaces.C.int is
      begin
         return Wl_Thin.Registry_Add_Listener (Registry.My_Registry, Listener, Data);
      end Add_Listener;

      procedure Start_Subscription (Registry : in out Registry_T;
                                    Display  : in     Display_T) is
         I : Wl.int;

         Listener : aliased Wl.Registry_Listener_T :=
           (
            Global        => Global_Registry_Handler'Unrestricted_Access,
            Global_Remove => Global_Registry_Remover'Unrestricted_Access
           );
      begin
         I := Add_Listener (Registry, Listener'Unchecked_Access, Wl.Null_Address);

         Display.Dispatch;
         Display.Roundtrip;
      end Start_Subscription;

      function Global_Objects return Global_Objects_Ref is
        ((E => My_Global_Objects'Access));

   end Global_Objects_Subscriber;

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
