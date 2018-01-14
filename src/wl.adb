package body Wl is

   function Display_Connect (Name : Interfaces.C.Strings.char_array_access) return Display_T is

      function wl_display_connect (Name : in Interfaces.C.Strings.chars_ptr) return Proxy_Ptr with
        Convention => C,
        Import     => True;

   begin
      return This : Display_T do
         This.My_Proxy := wl_display_connect(Interfaces.C.Strings.To_Chars_Ptr (Name));
         if This.My_Proxy = null then
            raise Failed_To_Connect_Exception;
         end if;
      end return;
   end Display_Connect;

   procedure Finalize (This : in out Display_T) is

      procedure wl_display_disconnect (Display : in Proxy_Ptr) with
        Convention => C,
        Import     => True;

   begin
      if This.My_Proxy /= null then
         wl_display_disconnect (This.My_Proxy);
         This.My_Proxy := null;
      end if;
   end Finalize;







   WL_DISPLAY_GET_REGISTRY : constant := 1;

   wl_registry_interface : aliased Interface_T with
     Import        => True,
     Convention    => C,
     External_Name => "wl_registry_interface";

   function Display_Get_Registry (Display : Display_T) return Registry_T is
   begin
      return This : Registry_T do
         This.My_Proxy := Proxy_Marshal_Constructor(Display.My_Proxy,
                                                    WL_DISPLAY_GET_REGISTRY, wl_registry_interface'Access, 0);
      end return;
   end Display_Get_Registry;

   procedure wl_proxy_destroy (Registry : Proxy_Ptr) with
     Convention    => C,
     Import        => True,
     External_Name => "wl_proxy_destroy";

   procedure Finalize (This : in out Registry_T) is
   begin
      if This.My_Proxy /= null then
         wl_proxy_destroy (This.My_Proxy);
         This.My_Proxy := null;
      end if;
   end Finalize;

end Wl;
