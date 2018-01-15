package body Wl is

   procedure wl_proxy_destroy (Registry : Proxy_Ptr) with
     Convention    => C,
     Import        => True,
     External_Name => "wl_proxy_destroy";

   function Display_Connect (Name : Interfaces.C.Strings.char_array_access) return Display_Ptr is

      function wl_display_connect (Name : in Interfaces.C.Strings.chars_ptr) return Display_Ptr with
        Convention    => C,
        Import        => True,
        External_Name => "wl_display_connect";

   begin
      return wl_display_connect(Interfaces.C.Strings.To_Chars_Ptr (Name));
   end Display_Connect;

   procedure Display_Disconnect (This : in out Display_Ptr) is

      procedure wl_display_disconnect (Display : in Display_Ptr) with
        Convention => C,
        Import     => True;

   begin
      if This /= null then
         wl_display_disconnect (This);
         This := null;
      end if;
   end Display_Disconnect;





   WL_DISPLAY_GET_REGISTRY : constant := 1;

   wl_registry_interface : aliased Interface_T with
     Import        => True,
     Convention    => C,
     External_Name => "wl_registry_interface";

   function Display_Get_Registry (Display : Display_Ptr) return Registry_Ptr is
      P : Proxy_Ptr;
      This : Registry_Ptr;
   begin
      P := Proxy_Marshal_Constructor (Display.all'Access,
                                      WL_DISPLAY_GET_REGISTRY,
                                      wl_registry_interface'Access,
                                      0);

      if P /= null then
         This := P.all'Access;
      end if;
      return This;
   end Display_Get_Registry;


--  int
--  wl_proxy_add_listener(struct wl_proxy *proxy,
--  		      void (**implementation)(void), void *data);


   function wl_proxy_add_listener
     (arg1 : Proxy_Ptr;
      arg2 : Registry_Listener_Ptr;
      arg3 : Wl.Void_Ptr) return Interfaces.C.int;  -- /usr/include/wayland-client-core.h:171
   pragma Import (C, wl_proxy_add_listener, "wl_proxy_add_listener");

--  static inline int
--  wl_registry_add_listener(struct wl_registry *wl_registry,
--  			 const struct wl_registry_listener *listener, void *data)
--  {
--  	return wl_proxy_add_listener((struct wl_proxy *) wl_registry,
--  				     (void (**)(void)) listener, data);
--  }
   function Registry_Add_Listener (Registry : Registry_Ptr;
                                   Listener : Registry_Listener_Ptr;
                                   Data     : Wl.Void_Ptr) return Interfaces.C.int is
   begin
      return wl_proxy_add_listener (Registry.all'Access, Listener, Data);
   end Registry_Add_Listener;

   procedure Registry_Destroy (Registry : in out Registry_Ptr) is
   begin
      if Registry /= null then
         wl_proxy_destroy (Registry.all'Access);
         Registry := null;
      end if;
   end Registry_Destroy;

end Wl;
