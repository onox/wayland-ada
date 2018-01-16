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



   WL_DISPLAY_SYNC : constant := 0;

   WL_DISPLAY_GET_REGISTRY : constant := 1;

   WL_DISPLAY_ERROR_SINCE_VERSION : constant := 1;

   WL_DISPLAY_DELETE_ID_SINCE_VERSION : constant := 1;

   WL_DISPLAY_SYNC_SINCE_VERSION : constant := 1;

   WL_DISPLAY_GET_REGISTRY_SINCE_VERSION : constant := 1;

   WL_REGISTRY_BIND : constant := 0;

   WL_REGISTRY_GLOBAL_SINCE_VERSION : constant := 1;

   WL_REGISTRY_GLOBAL_REMOVE_SINCE_VERSION : constant := 1;

   WL_REGISTRY_BIND_SINCE_VERSION : constant := 1;

   WL_CALLBACK_DONE_SINCE_VERSION : constant := 1;

   WL_COMPOSITOR_CREATE_SURFACE : constant := 0;

   WL_COMPOSITOR_CREATE_REGION : constant := 1;

   WL_COMPOSITOR_CREATE_SURFACE_SINCE_VERSION : constant := 1;

   WL_COMPOSITOR_CREATE_REGION_SINCE_VERSION : constant := 1;

   WL_SHM_POOL_CREATE_BUFFER : constant := 0;

   WL_SHM_POOL_DESTROY : constant := 1;

   WL_SHM_POOL_RESIZE : constant := 2;

   WL_SHM_POOL_CREATE_BUFFER_SINCE_VERSION : constant := 1;

   WL_SHM_POOL_DESTROY_SINCE_VERSION : constant := 1;

   WL_SHM_POOL_RESIZE_SINCE_VERSION : constant := 1;

   WL_SHM_CREATE_POOL : constant := 0;

   WL_SHM_FORMAT_SINCE_VERSION : constant := 1;

   WL_SHM_CREATE_POOL_SINCE_VERSION : constant := 1;

   WL_BUFFER_DESTROY : constant := 0;

   WL_BUFFER_RELEASE_SINCE_VERSION : constant := 1;

   WL_BUFFER_DESTROY_SINCE_VERSION : constant := 1;

   WL_DATA_OFFER_ACCEPT : constant := 0;

   WL_DATA_OFFER_RECEIVE : constant := 1;

   WL_DATA_OFFER_DESTROY : constant := 2;

   WL_DATA_OFFER_FINISH : constant := 3;

   WL_DATA_OFFER_SET_ACTIONS : constant := 4;

   WL_DATA_OFFER_OFFER_SINCE_VERSION : constant := 1;

   WL_DATA_OFFER_SOURCE_ACTIONS_SINCE_VERSION : constant := 3;

   WL_DATA_OFFER_ACTION_SINCE_VERSION : constant := 3;

   WL_DATA_OFFER_ACCEPT_SINCE_VERSION : constant := 1;

   WL_DATA_OFFER_RECEIVE_SINCE_VERSION : constant := 1;

   WL_DATA_OFFER_DESTROY_SINCE_VERSION : constant := 1;

   WL_DATA_OFFER_FINISH_SINCE_VERSION : constant := 3;

   WL_DATA_OFFER_SET_ACTIONS_SINCE_VERSION : constant := 3;

   WL_DATA_SOURCE_OFFER : constant := 0;

   WL_DATA_SOURCE_DESTROY : constant := 1;

   WL_DATA_SOURCE_SET_ACTIONS : constant := 2;

   WL_DATA_SOURCE_TARGET_SINCE_VERSION : constant := 1;

   WL_DATA_SOURCE_SEND_SINCE_VERSION : constant := 1;

   WL_DATA_SOURCE_CANCELLED_SINCE_VERSION : constant := 1;

   WL_DATA_SOURCE_DND_DROP_PERFORMED_SINCE_VERSION : constant := 3;

   WL_DATA_SOURCE_DND_FINISHED_SINCE_VERSION : constant := 3;

   WL_DATA_SOURCE_ACTION_SINCE_VERSION : constant := 3;

   WL_DATA_SOURCE_OFFER_SINCE_VERSION : constant := 1;

   WL_DATA_SOURCE_DESTROY_SINCE_VERSION : constant := 1;

   WL_DATA_SOURCE_SET_ACTIONS_SINCE_VERSION : constant := 3;

   WL_DATA_DEVICE_START_DRAG : constant := 0;

   WL_DATA_DEVICE_SET_SELECTION : constant := 1;

   WL_DATA_DEVICE_RELEASE : constant := 2;

   WL_DATA_DEVICE_DATA_OFFER_SINCE_VERSION : constant := 1;

   WL_DATA_DEVICE_ENTER_SINCE_VERSION : constant := 1;

   WL_DATA_DEVICE_LEAVE_SINCE_VERSION : constant := 1;

   WL_DATA_DEVICE_MOTION_SINCE_VERSION : constant := 1;

   WL_DATA_DEVICE_DROP_SINCE_VERSION : constant := 1;

   WL_DATA_DEVICE_SELECTION_SINCE_VERSION : constant := 1;

   WL_DATA_DEVICE_START_DRAG_SINCE_VERSION : constant := 1;

   WL_DATA_DEVICE_SET_SELECTION_SINCE_VERSION : constant := 1;

   WL_DATA_DEVICE_RELEASE_SINCE_VERSION : constant := 2;

   WL_DATA_DEVICE_MANAGER_CREATE_DATA_SOURCE : constant := 0;

   WL_DATA_DEVICE_MANAGER_GET_DATA_DEVICE : constant := 1;

   WL_DATA_DEVICE_MANAGER_CREATE_DATA_SOURCE_SINCE_VERSION : constant := 1;

   WL_DATA_DEVICE_MANAGER_GET_DATA_DEVICE_SINCE_VERSION : constant := 1;

   WL_SHELL_GET_SHELL_SURFACE : constant := 0;

   WL_SHELL_GET_SHELL_SURFACE_SINCE_VERSION : constant := 1;

   WL_SHELL_SURFACE_PONG : constant := 0;

   WL_SHELL_SURFACE_MOVE : constant := 1;

   WL_SHELL_SURFACE_RESIZE : constant := 2;

   WL_SHELL_SURFACE_SET_TOPLEVEL : constant := 3;

   WL_SHELL_SURFACE_SET_TRANSIENT : constant := 4;

   WL_SHELL_SURFACE_SET_FULLSCREEN : constant := 5;

   WL_SHELL_SURFACE_SET_POPUP : constant := 6;

   WL_SHELL_SURFACE_SET_MAXIMIZED : constant := 7;

   WL_SHELL_SURFACE_SET_TITLE : constant := 8;

   WL_SHELL_SURFACE_SET_CLASS : constant := 9;

   WL_SHELL_SURFACE_PING_SINCE_VERSION : constant := 1;

   WL_SHELL_SURFACE_CONFIGURE_SINCE_VERSION : constant := 1;

   WL_SHELL_SURFACE_POPUP_DONE_SINCE_VERSION : constant := 1;

   WL_SHELL_SURFACE_PONG_SINCE_VERSION : constant := 1;

   WL_SHELL_SURFACE_MOVE_SINCE_VERSION : constant := 1;

   WL_SHELL_SURFACE_RESIZE_SINCE_VERSION : constant := 1;

   WL_SHELL_SURFACE_SET_TOPLEVEL_SINCE_VERSION : constant := 1;

   WL_SHELL_SURFACE_SET_TRANSIENT_SINCE_VERSION : constant := 1;

   WL_SHELL_SURFACE_SET_FULLSCREEN_SINCE_VERSION : constant := 1;

   WL_SHELL_SURFACE_SET_POPUP_SINCE_VERSION : constant := 1;

   WL_SHELL_SURFACE_SET_MAXIMIZED_SINCE_VERSION : constant := 1;

   WL_SHELL_SURFACE_SET_TITLE_SINCE_VERSION : constant := 1;

   WL_SHELL_SURFACE_SET_CLASS_SINCE_VERSION : constant := 1;

   WL_SURFACE_DESTROY : constant := 0;

   WL_SURFACE_ATTACH : constant := 1;

   WL_SURFACE_DAMAGE : constant := 2;

   WL_SURFACE_FRAME : constant := 3;

   WL_SURFACE_SET_OPAQUE_REGION : constant := 4;

   WL_SURFACE_SET_INPUT_REGION : constant := 5;

   WL_SURFACE_COMMIT : constant := 6;

   WL_SURFACE_SET_BUFFER_TRANSFORM : constant := 7;

   WL_SURFACE_SET_BUFFER_SCALE : constant := 8;

   WL_SURFACE_DAMAGE_BUFFER : constant := 9;

   WL_SURFACE_ENTER_SINCE_VERSION : constant := 1;

   WL_SURFACE_LEAVE_SINCE_VERSION : constant := 1;

   WL_SURFACE_DESTROY_SINCE_VERSION : constant := 1;

   WL_SURFACE_ATTACH_SINCE_VERSION : constant := 1;

   WL_SURFACE_DAMAGE_SINCE_VERSION : constant := 1;

   WL_SURFACE_FRAME_SINCE_VERSION : constant := 1;

   WL_SURFACE_SET_OPAQUE_REGION_SINCE_VERSION : constant := 1;

   WL_SURFACE_SET_INPUT_REGION_SINCE_VERSION : constant := 1;

   WL_SURFACE_COMMIT_SINCE_VERSION : constant := 1;

   WL_SURFACE_SET_BUFFER_TRANSFORM_SINCE_VERSION : constant := 2;

   WL_SURFACE_SET_BUFFER_SCALE_SINCE_VERSION : constant := 3;

   WL_SURFACE_DAMAGE_BUFFER_SINCE_VERSION : constant := 4;

   WL_SEAT_GET_POINTER : constant := 0;

   WL_SEAT_GET_KEYBOARD : constant := 1;

   WL_SEAT_GET_TOUCH : constant := 2;

   WL_SEAT_RELEASE : constant := 3;

   WL_SEAT_CAPABILITIES_SINCE_VERSION : constant := 1;

   WL_SEAT_NAME_SINCE_VERSION : constant := 2;

   WL_SEAT_GET_POINTER_SINCE_VERSION : constant := 1;

   WL_SEAT_GET_KEYBOARD_SINCE_VERSION : constant := 1;

   WL_SEAT_GET_TOUCH_SINCE_VERSION : constant := 1;

   WL_SEAT_RELEASE_SINCE_VERSION : constant := 5;

   WL_POINTER_SET_CURSOR : constant := 0;

   WL_POINTER_RELEASE : constant := 1;

   WL_POINTER_ENTER_SINCE_VERSION : constant := 1;

   WL_POINTER_LEAVE_SINCE_VERSION : constant := 1;

   WL_POINTER_MOTION_SINCE_VERSION : constant := 1;

   WL_POINTER_BUTTON_SINCE_VERSION : constant := 1;

   WL_POINTER_AXIS_SINCE_VERSION : constant := 1;

   WL_POINTER_FRAME_SINCE_VERSION : constant := 5;

   WL_POINTER_AXIS_SOURCE_SINCE_VERSION : constant := 5;

   WL_POINTER_AXIS_STOP_SINCE_VERSION : constant := 5;

   WL_POINTER_AXIS_DISCRETE_SINCE_VERSION : constant := 5;

   WL_POINTER_SET_CURSOR_SINCE_VERSION : constant := 1;

   WL_POINTER_RELEASE_SINCE_VERSION : constant := 3;

   WL_KEYBOARD_RELEASE : constant := 0;

   WL_KEYBOARD_KEYMAP_SINCE_VERSION : constant := 1;

   WL_KEYBOARD_ENTER_SINCE_VERSION : constant := 1;

   WL_KEYBOARD_LEAVE_SINCE_VERSION : constant := 1;

   WL_KEYBOARD_KEY_SINCE_VERSION : constant := 1;

   WL_KEYBOARD_MODIFIERS_SINCE_VERSION : constant := 1;

   WL_KEYBOARD_REPEAT_INFO_SINCE_VERSION : constant := 4;

   WL_KEYBOARD_RELEASE_SINCE_VERSION : constant := 3;

   WL_TOUCH_RELEASE : constant := 0;

   WL_TOUCH_DOWN_SINCE_VERSION : constant := 1;

   WL_TOUCH_UP_SINCE_VERSION : constant := 1;

   WL_TOUCH_MOTION_SINCE_VERSION : constant := 1;

   WL_TOUCH_FRAME_SINCE_VERSION : constant := 1;

   WL_TOUCH_CANCEL_SINCE_VERSION : constant := 1;

   WL_TOUCH_SHAPE_SINCE_VERSION : constant := 6;

   WL_TOUCH_ORIENTATION_SINCE_VERSION : constant := 6;

   WL_TOUCH_RELEASE_SINCE_VERSION : constant := 3;

   WL_OUTPUT_RELEASE : constant := 0;

   WL_OUTPUT_GEOMETRY_SINCE_VERSION : constant := 1;

   WL_OUTPUT_MODE_SINCE_VERSION : constant := 1;

   WL_OUTPUT_DONE_SINCE_VERSION : constant := 2;

   WL_OUTPUT_SCALE_SINCE_VERSION : constant := 2;

   WL_OUTPUT_RELEASE_SINCE_VERSION : constant := 3;

   WL_REGION_DESTROY : constant := 0;

   WL_REGION_ADD : constant := 1;

   WL_REGION_SUBTRACT : constant := 2;

   WL_REGION_DESTROY_SINCE_VERSION : constant := 1;

   WL_REGION_ADD_SINCE_VERSION : constant := 1;

   WL_REGION_SUBTRACT_SINCE_VERSION : constant := 1;

   WL_SUBCOMPOSITOR_DESTROY : constant := 0;

   WL_SUBCOMPOSITOR_GET_SUBSURFACE : constant := 1;

   WL_SUBCOMPOSITOR_DESTROY_SINCE_VERSION : constant := 1;

   WL_SUBCOMPOSITOR_GET_SUBSURFACE_SINCE_VERSION : constant := 1;

   WL_SUBSURFACE_DESTROY : constant := 0;

   WL_SUBSURFACE_SET_POSITION : constant := 1;

   WL_SUBSURFACE_PLACE_ABOVE : constant := 2;

   WL_SUBSURFACE_PLACE_BELOW : constant := 3;

   WL_SUBSURFACE_SET_SYNC : constant := 4;

   WL_SUBSURFACE_SET_DESYNC : constant := 5;

   WL_SUBSURFACE_DESTROY_SINCE_VERSION : constant := 1;

   WL_SUBSURFACE_SET_POSITION_SINCE_VERSION : constant := 1;

   WL_SUBSURFACE_PLACE_ABOVE_SINCE_VERSION : constant := 1;

   WL_SUBSURFACE_PLACE_BELOW_SINCE_VERSION : constant := 1;

   WL_SUBSURFACE_SET_SYNC_SINCE_VERSION : constant := 1;

   WL_SUBSURFACE_SET_DESYNC_SINCE_VERSION : constant := 1;

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
