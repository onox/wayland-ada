with System.Address_To_Access_Conversions;
package body C_Binding.Linux.Wayland_Client is

   use type int;

   -- Mostly auto generated from Wayland.xml
   package body Wl_Thin is

      procedure wl_proxy_destroy (Registry : Proxy_Ptr) with
        Convention    => C,
        Import        => True,
        External_Name => "wl_proxy_destroy";

      function Display_Connect (Name : C_String) return Display_Ptr is

         function Wl_Display_Connect
           (Name : in C_String) return Display_Ptr with
           Convention    => C,
           Import        => True,
           External_Name => "wl_display_connect";

      begin
         return wl_display_connect (Name);
      end Display_Connect;

      procedure Display_Disconnect (This : in out Display_Ptr) is

         procedure wl_display_disconnect (Display : Display_Ptr) with
           Convention => C,
           Import     => True;

      begin
         if This /= null then
            wl_display_disconnect (This);
            This := null;
         end if;
      end Display_Disconnect;

      function wl_proxy_add_listener
        (arg1 : Proxy_Ptr;
         arg2 : Void_Ptr;
         arg3 : Void_Ptr) return Interfaces.C.int with
        Import        => True,
        Convention    => C,
        External_Name => "wl_proxy_add_listener";
      -- Returns 0 on success, otherwise -1 on failure.

      procedure wl_proxy_set_user_data
        (arg1 : Proxy_Ptr;
         arg3 : Void_Ptr) with
        Import        => True,
        Convention    => C,
        External_Name => "wl_proxy_set_user_data";

      function wl_proxy_get_user_data
        (arg1 : Proxy_Ptr) return Void_Ptr with
        Import        => True,
        Convention    => C,
        External_Name => "wl_proxy_get_user_data";

      function wl_proxy_get_version
        (arg1 : Proxy_Ptr) return Unsigned_32 with
        Import        => True,
        Convention    => C,
        External_Name => "wl_proxy_get_version";

      function Display_Add_Listener
        (Display  : Display_Ptr;
         Listener : Display_Listener_Ptr;
         Data     : Void_Ptr) return Interfaces.C.Int is
      begin
         return Wl_Proxy_Add_Listener
           (Display.all'Access, Listener.all'Address, Data);
      end Display_Add_Listener;

      procedure Display_Set_User_Data
        (Display : Display_Ptr;
         Data    : Void_Ptr) is
      begin
         wl_proxy_set_user_data (Display.all'Access, Data);
      end Display_Set_User_Data;

      function Display_Get_User_Data
        (Display : Display_Ptr) return Void_Ptr is
      begin
         return wl_proxy_get_user_data (Display.all'Access);
      end Display_Get_User_Data;

      function Display_Get_Version
        (Display : Display_Ptr) return Unsigned_32 is
      begin
         return wl_proxy_get_version (Display.all'Access);
      end Display_Get_Version;

      procedure Display_Destroy (Display : Display_Ptr) is
      begin
         wl_proxy_destroy (Display.all'Access);
      end Display_Destroy;

      function Display_Sync (Display : Display_Ptr) return Callback_Ptr is
         P : constant Proxy_Ptr
           := Proxy_Marshal_Constructor
             (Display.all'Access,
              WL_DISPLAY_SYNC,
              Callback_Interface'Access,
              0);
      begin
         return (if P /= null then P.all'Access else null);
      end Display_Sync;

      function Display_Get_Registry
        (Display : Display_Ptr) return Registry_Ptr
      is
         P : constant Proxy_Ptr
           := Proxy_Marshal_Constructor (Display.all'Access,
                                         WL_DISPLAY_GET_REGISTRY,
                                         Registry_Interface'Access,
                                         0);
      begin
         return (if P /= null then P.all'Access else null);
      end Display_Get_Registry;

      function Registry_Add_Listener
        (Registry : Registry_Ptr;
         Listener : Registry_Listener_Ptr;
         Data     : Void_Ptr) return Interfaces.C.Int is
      begin
         return Wl_Proxy_Add_Listener
           (Registry.all'Access, Listener.all'Address, Data);
      end Registry_Add_Listener;

      procedure Registry_Set_User_Data (Registry : Registry_Ptr;
                                        Data : Void_Ptr) is
      begin
         wl_proxy_set_user_data (Registry.all'Access, Data);
      end Registry_Set_User_Data;

      function Registry_Get_User_Data
        (Registry : Registry_Ptr) return Void_Ptr is
      begin
         return wl_proxy_get_user_data (Registry.all'Access);
      end Registry_Get_User_Data;

      function Registry_Get_Version
        (Registry : Registry_Ptr) return Unsigned_32 is
      begin
         return wl_proxy_get_version (Registry.all'Access);
      end Registry_Get_Version;

      procedure Registry_Destroy (Registry : Registry_Ptr) is
      begin
         wl_proxy_destroy (Registry.all'Access);
      end Registry_Destroy;

      function Registry_Bind (Registry : Registry_Ptr;
                              Name : Unsigned_32;
                              Interface_V : Interface_Ptr;
                              New_Id : Unsigned_32) return Proxy_Ptr is
      begin
         return Proxy_Marshal_Constructor_Versioned (Registry.all'Access,
                                                     WL_REGISTRY_BIND,
                                                     Interface_V,
                                                     New_Id,
                                                     Name,
                                                     Interface_V.Name,
                                                     New_Id,
                                                     0);
      end Registry_Bind;

      function Callback_Add_Listener
        (Callback : Callback_Ptr;
         Listener : Callback_Listener_Ptr;
         Data     : Void_Ptr) return Interfaces.C.Int is
      begin
         return Wl_Proxy_Add_Listener
           (Callback.all'Access, Listener.all'Address, Data);
      end Callback_Add_Listener;

      procedure Callback_Set_User_Data
        (Callback : Callback_Ptr;
         Data     : Void_Ptr) is
      begin
         wl_proxy_set_user_data (Callback.all'Access, Data);
      end Callback_Set_User_Data;

      function Callback_Get_User_Data
        (Callback : Callback_Ptr) return Void_Ptr is
      begin
         return wl_proxy_get_user_data (Callback.all'Access);
      end Callback_Get_User_Data;

      function Callback_Get_Version
        (Callback : Callback_Ptr) return Unsigned_32 is
      begin
         return wl_proxy_get_version (Callback.all'Access);
      end Callback_Get_Version;

      procedure Callback_Destroy (Callback : Callback_Ptr) is
      begin
         wl_proxy_destroy (Callback.all'Access);
      end Callback_Destroy;

      procedure Compositor_Set_User_Data
        (Compositor : Compositor_Ptr;
         Data       : Void_Ptr) is
      begin
         wl_proxy_set_user_data (Compositor.all'Access, Data);
      end Compositor_Set_User_Data;

      function Compositor_Get_User_Data
        (Compositor : Compositor_Ptr) return Void_Ptr is
      begin
         return wl_proxy_get_user_data (Compositor.all'Access);
      end Compositor_Get_User_Data;

      function Compositor_Get_Version
        (Compositor : Compositor_Ptr) return Unsigned_32 is
      begin
         return wl_proxy_get_version (Compositor.all'Access);
      end Compositor_Get_Version;

      procedure Compositor_Destroy (Compositor : Compositor_Ptr) is
      begin
         wl_proxy_destroy (Compositor.all'Access);
      end Compositor_Destroy;

      function Compositor_Create_Surface
        (Compositor : Compositor_Ptr) return Surface_Ptr
      is
         P : constant Proxy_Ptr
           := Proxy_Marshal_Constructor (Compositor.all'Access,
                                         WL_COMPOSITOR_CREATE_SURFACE,
                                         Surface_Interface'Access,
                                         0);
      begin
         return (if P /= null then P.all'Access else null);
      end Compositor_Create_Surface;

      function Compositor_Create_Region
        (Compositor : Compositor_Ptr) return Region_Ptr
      is
         P : constant Proxy_Ptr
           := Proxy_Marshal_Constructor (Compositor.all'Access,
                                         WL_COMPOSITOR_CREATE_REGION,
                                         Region_Interface'Access,
                                         0);
      begin
         return (if P /= null then P.all'Access else null);
      end Compositor_Create_Region;

      procedure Shm_Pool_Set_User_Data (Shm_Pool : Shm_Pool_Ptr;
                                        Data : Void_Ptr) is
      begin
         wl_proxy_set_user_data (Shm_Pool.all'Access, Data);
      end Shm_Pool_Set_User_Data;

      function Shm_Pool_Get_User_Data
        (Shm_Pool : Shm_Pool_Ptr) return Void_Ptr is
      begin
         return wl_proxy_get_user_data (Shm_Pool.all'Access);
      end Shm_Pool_Get_User_Data;

      function Shm_Pool_Get_Version
        (Shm_Pool : Shm_Pool_Ptr) return Unsigned_32 is
      begin
         return wl_proxy_get_version (Shm_Pool.all'Access);
      end Shm_Pool_Get_Version;

      procedure Shm_Pool_Destroy (Shm_Pool : Shm_Pool_Ptr) is
      begin
         Proxy_Marshal (Proxy_Ptr'(Shm_Pool.all'Access),
                        WL_SHM_POOL_DESTROY);

         wl_proxy_destroy (Shm_Pool.all'Access);
      end Shm_Pool_Destroy;

      function Shm_Pool_Create_Buffer
        (Shm_Pool : Shm_Pool_Ptr;
         Offset   : Integer;
         Width    : Integer;
         Height   : Integer;
         Stride   : Integer;
         Format   : Unsigned_32
        ) return Buffer_Ptr
      is
         P : constant Proxy_Ptr
           := Proxy_Marshal_Constructor (Shm_Pool.all'Access,
                                         WL_SHM_POOL_CREATE_BUFFER,
                                         Buffer_Interface'Access,
                                         0,
                                         Offset,
                                         Width,
                                         Height,
                                         Stride,
                                         Format);
      begin
         return (if P /= null then P.all'Access else null);
      end Shm_Pool_Create_Buffer;


      procedure Shm_Pool_Resize (Shm_Pool : Shm_Pool_Ptr;
                                 Size : Integer
                                ) is
      begin
         Proxy_Marshal (Proxy_Ptr'(Shm_Pool.all'Access),
                        WL_SHM_POOL_RESIZE,
                        Size    );
      end Shm_Pool_Resize;

      function Shm_Add_Listener
        (Shm      : Shm_Ptr;
         Listener : Shm_Listener_Ptr;
         Data     : Void_Ptr) return Interfaces.C.Int is
      begin
         return Wl_Proxy_Add_Listener
           (Shm.all'Access, Listener.all'Address, Data);
      end Shm_Add_Listener;

      procedure Shm_Set_User_Data (Shm : Shm_Ptr;
                                   Data : Void_Ptr) is
      begin
         wl_proxy_set_user_data (Shm.all'Access, Data);
      end Shm_Set_User_Data;

      function Shm_Get_User_Data (Shm : Shm_Ptr) return Void_Ptr is
      begin
         return wl_proxy_get_user_data (Shm.all'Access);
      end Shm_Get_User_Data;

      function Shm_Get_Version (Shm : Shm_Ptr) return Unsigned_32 is
      begin
         return wl_proxy_get_version (Shm.all'Access);
      end Shm_Get_Version;

      procedure Shm_Destroy (Shm : Shm_Ptr) is
      begin
         wl_proxy_destroy (Shm.all'Access);
      end Shm_Destroy;

      function Shm_Create_Pool (Shm : Shm_Ptr;
                                Fd : Integer;
                                Size : Integer
                               ) return Shm_Pool_Ptr is
         P : constant Proxy_Ptr
           := Proxy_Marshal_Constructor (Shm.all'Access,
                                         WL_SHM_CREATE_POOL,
                                         Shm_Pool_Interface'Access,
                                         0,
                                         Fd,
                                         Size);
      begin
         return (if P /= null then P.all'Access else null);
      end Shm_Create_Pool;

      function Buffer_Add_Listener
        (Buffer   : Buffer_Ptr;
         Listener : Buffer_Listener_Ptr;
         Data     : Void_Ptr) return Interfaces.C.Int is
      begin
         return Wl_Proxy_Add_Listener
           (Buffer.all'Access, Listener.all'Address, Data);
      end Buffer_Add_Listener;

      procedure Buffer_Set_User_Data (Buffer : Buffer_Ptr;
                                      Data : Void_Ptr) is
      begin
         wl_proxy_set_user_data (Buffer.all'Access, Data);
      end Buffer_Set_User_Data;

      function Buffer_Get_User_Data (Buffer : Buffer_Ptr) return Void_Ptr is
      begin
         return wl_proxy_get_user_data (Buffer.all'Access);
      end Buffer_Get_User_Data;

      function Buffer_Get_Version (Buffer : Buffer_Ptr) return Unsigned_32 is
      begin
         return wl_proxy_get_version (Buffer.all'Access);
      end Buffer_Get_Version;

      procedure Buffer_Destroy (Buffer : Buffer_Ptr) is
      begin
         Proxy_Marshal (Proxy_Ptr'(Buffer.all'Access),
                        WL_BUFFER_DESTROY);

         wl_proxy_destroy (Buffer.all'Access);
      end Buffer_Destroy;

      function Data_Offer_Add_Listener
        (Data_Offer : Data_Offer_Ptr;
         Listener   : Data_Offer_Listener_Ptr;
         Data       : Void_Ptr) return Interfaces.C.Int is
      begin
         return Wl_Proxy_Add_Listener
           (Data_Offer.all'Access, Listener.all'Address, Data);
      end Data_Offer_Add_Listener;

      procedure Data_Offer_Set_User_Data
        (Data_Offer : Data_Offer_Ptr;
         Data       : Void_Ptr) is
      begin
         wl_proxy_set_user_data (Data_Offer.all'Access, Data);
      end Data_Offer_Set_User_Data;

      function Data_Offer_Get_User_Data
        (Data_Offer : Data_Offer_Ptr) return Void_Ptr is
      begin
         return wl_proxy_get_user_data (Data_Offer.all'Access);
      end Data_Offer_Get_User_Data;

      function Data_Offer_Get_Version
        (Data_Offer : Data_Offer_Ptr) return Unsigned_32 is
      begin
         return wl_proxy_get_version (Data_Offer.all'Access);
      end Data_Offer_Get_Version;

      procedure Data_Offer_Destroy (Data_Offer : Data_Offer_Ptr) is
      begin
         Proxy_Marshal (Proxy_Ptr'(Data_Offer.all'Access),
                        WL_DATA_OFFER_DESTROY);

         wl_proxy_destroy (Data_Offer.all'Access);
      end Data_Offer_Destroy;

      procedure Data_Offer_Accept (Data_Offer : Data_Offer_Ptr;
                                   Serial : Unsigned_32;
                                   Mime_Type : chars_ptr
                                  ) is
      begin
         Proxy_Marshal (Proxy_Ptr'(Data_Offer.all'Access),
                        WL_DATA_OFFER_ACCEPT,
                        Serial,
                        Mime_Type    );
      end Data_Offer_Accept;

      procedure Data_Offer_Receive (Data_Offer : Data_Offer_Ptr;
                                    Mime_Type : C_String;
                                    Fd : Integer
                                   ) is
      begin
         Proxy_Marshal (Proxy_Ptr'(Data_Offer.all'Access),
                        WL_DATA_OFFER_RECEIVE,
                        Mime_Type,
                        Fd    );
      end Data_Offer_Receive;


      procedure Data_Offer_Finish (Data_Offer : Data_Offer_Ptr) is
      begin
         Proxy_Marshal
           (Proxy_Ptr'(Data_Offer.all'Access), WL_DATA_OFFER_FINISH);
      end Data_Offer_Finish;

      procedure Data_Offer_Set_Actions
        (Data_Offer       : Data_Offer_Ptr;
         Dnd_Actions      : Unsigned_32;
         Preferred_Action : Unsigned_32) is
      begin
         Proxy_Marshal (Proxy_Ptr'(Data_Offer.all'Access),
                        WL_DATA_OFFER_SET_ACTIONS,
                        Dnd_Actions,
                        Preferred_Action    );
      end Data_Offer_Set_Actions;

      function Data_Source_Add_Listener
        (Data_Source : Data_Source_Ptr;
         Listener    : Data_Source_Listener_Ptr;
         Data        : Void_Ptr) return Interfaces.C.Int is
      begin
         return Wl_Proxy_Add_Listener
           (Data_Source.all'Access, Listener.all'Address, Data);
      end Data_Source_Add_Listener;

      procedure Data_Source_Set_User_Data
        (Data_Source : Data_Source_Ptr;
         Data        : Void_Ptr) is
      begin
         wl_proxy_set_user_data (Data_Source.all'Access, Data);
      end Data_Source_Set_User_Data;

      function Data_Source_Get_User_Data
        (Data_Source : Data_Source_Ptr) return Void_Ptr is
      begin
         return wl_proxy_get_user_data (Data_Source.all'Access);
      end Data_Source_Get_User_Data;

      function Data_Source_Get_Version
        (Data_Source : Data_Source_Ptr) return Unsigned_32 is
      begin
         return wl_proxy_get_version (Data_Source.all'Access);
      end Data_Source_Get_Version;

      procedure Data_Source_Destroy (Data_Source : Data_Source_Ptr) is
      begin
         Proxy_Marshal (Proxy_Ptr'(Data_Source.all'Access),
                        WL_DATA_SOURCE_DESTROY);

         wl_proxy_destroy (Data_Source.all'Access);
      end Data_Source_Destroy;

      procedure Data_Source_Offer (Data_Source : Data_Source_Ptr;
                                   Mime_Type : chars_ptr
                                  ) is
      begin
         Proxy_Marshal (Proxy_Ptr'(Data_Source.all'Access),
                        WL_DATA_SOURCE_OFFER,
                        Mime_Type    );
      end Data_Source_Offer;


      procedure Data_Source_Set_Actions (Data_Source : Data_Source_Ptr;
                                         Dnd_Actions : Unsigned_32
                                        ) is
      begin
         Proxy_Marshal (Proxy_Ptr'(Data_Source.all'Access),
                        WL_DATA_SOURCE_SET_ACTIONS,
                        Dnd_Actions    );
      end Data_Source_Set_Actions;

      function Data_Device_Add_Listener
        (Data_Device : Data_Device_Ptr;
         Listener    : Data_Device_Listener_Ptr;
         Data        : Void_Ptr) return Interfaces.C.Int is
      begin
         return Wl_Proxy_Add_Listener
           (Data_Device.all'Access, Listener.all'Address, Data);
      end Data_Device_Add_Listener;

      procedure Data_Device_Set_User_Data
        (Data_Device : Data_Device_Ptr;
         Data        : Void_Ptr) is
      begin
         wl_proxy_set_user_data (Data_Device.all'Access, Data);
      end Data_Device_Set_User_Data;

      function Data_Device_Get_User_Data
        (Data_Device : Data_Device_Ptr) return Void_Ptr is
      begin
         return wl_proxy_get_user_data (Data_Device.all'Access);
      end Data_Device_Get_User_Data;

      function Data_Device_Get_Version
        (Data_Device : Data_Device_Ptr) return Unsigned_32 is
      begin
         return wl_proxy_get_version (Data_Device.all'Access);
      end Data_Device_Get_Version;

      procedure Data_Device_Destroy (Data_Device : Data_Device_Ptr) is
      begin
         wl_proxy_destroy (Data_Device.all'Access);
      end Data_Device_Destroy;

      procedure Data_Device_Start_Drag
        (Data_Device : Data_Device_Ptr;
         Source      : Data_Source_Ptr;
         Origin      : Surface_Ptr;
         Icon        : Surface_Ptr;
         Serial      : Unsigned_32) is
      begin
         Proxy_Marshal (Proxy_Ptr'(Data_Device.all'Access),
                        WL_DATA_DEVICE_START_DRAG,
                        Source.all'Address,
                        Origin.all'Address,
                        Icon.all'Address,
                        Serial    );
      end Data_Device_Start_Drag;

      procedure Data_Device_Set_Selection
        (Data_Device : Data_Device_Ptr;
         Source      : Data_Source_Ptr;
         Serial      : Unsigned_32) is
      begin
         Proxy_Marshal (Proxy_Ptr'(Data_Device.all'Access),
                        WL_DATA_DEVICE_SET_SELECTION,
                        Source.all'Address,
                        Serial    );
      end Data_Device_Set_Selection;

      procedure Data_Device_Release (Data_Device : Data_Device_Ptr) is
      begin
         Proxy_Marshal
           (Proxy_Ptr'(Data_Device.all'Access), WL_DATA_DEVICE_RELEASE);
      end Data_Device_Release;

      procedure Data_Device_Manager_Set_User_Data
        (Data_Device_Manager : Data_Device_Manager_Ptr;
         Data                : Void_Ptr) is
      begin
         wl_proxy_set_user_data (Data_Device_Manager.all'Access, Data);
      end Data_Device_Manager_Set_User_Data;

      function Data_Device_Manager_Get_User_Data
        (Data_Device_Manager : Data_Device_Manager_Ptr) return Void_Ptr is
      begin
         return wl_proxy_get_user_data (Data_Device_Manager.all'Access);
      end Data_Device_Manager_Get_User_Data;

      function Data_Device_Manager_Get_Version
        (Data_Device_Manager : Data_Device_Manager_Ptr) return Unsigned_32 is
      begin
         return wl_proxy_get_version (Data_Device_Manager.all'Access);
      end Data_Device_Manager_Get_Version;

      procedure Data_Device_Manager_Destroy
        (Data_Device_Manager : Data_Device_Manager_Ptr) is
      begin
         wl_proxy_destroy (Data_Device_Manager.all'Access);
      end Data_Device_Manager_Destroy;

      function Data_Device_Manager_Create_Data_Source
        (
         Data_Device_Manager : Data_Device_Manager_Ptr
        ) return Data_Source_Ptr
      is
         P : constant Proxy_Ptr
           := Proxy_Marshal_Constructor (Data_Device_Manager.all'Access,
                                         WL_DATA_DEVICE_MANAGER_CREATE_DATA_SOURCE,
                                         Data_Source_Interface'Access,
                                         0);
      begin
         return (if P /= null then P.all'Access else null);
      end Data_Device_Manager_Create_Data_Source;

      function Data_Device_Manager_Get_Data_Device
        (Data_Device_Manager : Data_Device_Manager_Ptr;
         Seat                : Seat_Ptr) return Data_Device_Ptr is
         P : constant Proxy_Ptr
           := Proxy_Marshal_Constructor (Data_Device_Manager.all'Access,
                                         WL_DATA_DEVICE_MANAGER_GET_DATA_DEVICE,
                                         Data_Device_Interface'Access,
                                         0,
                                         Seat.all'Address);
      begin
         return (if P /= null then P.all'Access else null);
      end Data_Device_Manager_Get_Data_Device;

      procedure Shell_Set_User_Data (Shell : Shell_Ptr;
                                     Data : Void_Ptr) is
      begin
         wl_proxy_set_user_data (Shell.all'Access, Data);
      end Shell_Set_User_Data;

      function Shell_Get_User_Data (Shell : Shell_Ptr) return Void_Ptr is
      begin
         return wl_proxy_get_user_data (Shell.all'Access);
      end Shell_Get_User_Data;

      function Shell_Get_Version (Shell : Shell_Ptr) return Unsigned_32 is
      begin
         return wl_proxy_get_version (Shell.all'Access);
      end Shell_Get_Version;

      procedure Shell_Destroy (Shell : Shell_Ptr) is
      begin
         wl_proxy_destroy (Shell.all'Access);
      end Shell_Destroy;

      function Shell_Get_Shell_Surface
        (Shell   : Shell_Ptr;
         Surface : Surface_Ptr
        ) return Shell_Surface_Ptr
      is
         P : constant Proxy_Ptr
           := Proxy_Marshal_Constructor (Shell.all'Access,
                                         WL_SHELL_GET_SHELL_SURFACE,
                                         Shell_Surface_Interface'Access,
                                         0,
                                         Surface.all'Address);
      begin
         return (if P /= null then P.all'Access else null);
      end Shell_Get_Shell_Surface;

      function Shell_Surface_Add_Listener
        (Shell_Surface : Shell_Surface_Ptr;
         Listener      : Shell_Surface_Listener_Ptr;
         Data          : Void_Ptr) return Interfaces.C.Int is
      begin
         return Wl_Proxy_Add_Listener
           (Shell_Surface.all'Access, Listener.all'Address, Data);
      end Shell_Surface_Add_Listener;

      procedure Shell_Surface_Set_User_Data
        (Shell_Surface : Shell_Surface_Ptr;
         Data          : Void_Ptr) is
      begin
         wl_proxy_set_user_data (Shell_Surface.all'Access, Data);
      end Shell_Surface_Set_User_Data;

      function Shell_Surface_Get_User_Data
        (Shell_Surface : Shell_Surface_Ptr) return Void_Ptr is
      begin
         return wl_proxy_get_user_data (Shell_Surface.all'Access);
      end Shell_Surface_Get_User_Data;

      function Shell_Surface_Get_Version
        (Shell_Surface : Shell_Surface_Ptr) return Unsigned_32 is
      begin
         return wl_proxy_get_version (Shell_Surface.all'Access);
      end Shell_Surface_Get_Version;

      procedure Shell_Surface_Destroy (Shell_Surface : Shell_Surface_Ptr) is
      begin
         wl_proxy_destroy (Shell_Surface.all'Access);
      end Shell_Surface_Destroy;

      procedure Shell_Surface_Pong (Shell_Surface : Shell_Surface_Ptr;
                                    Serial : Unsigned_32
                                   ) is
      begin
         Proxy_Marshal (Proxy_Ptr'(Shell_Surface.all'Access),
                        WL_SHELL_SURFACE_PONG,
                        Serial    );
      end Shell_Surface_Pong;

      procedure Shell_Surface_Move (Shell_Surface : Shell_Surface_Ptr;
                                    Seat : Seat_Ptr;
                                    Serial : Unsigned_32
                                   ) is
      begin
         Proxy_Marshal (Proxy_Ptr'(Shell_Surface.all'Access),
                        WL_SHELL_SURFACE_MOVE,
                        Seat.all'Address,
                        Serial    );
      end Shell_Surface_Move;

      procedure Shell_Surface_Resize (Shell_Surface : Shell_Surface_Ptr;
                                      Seat : Seat_Ptr;
                                      Serial : Unsigned_32;
                                      Edges : Unsigned_32
                                     ) is
      begin
         Proxy_Marshal (Proxy_Ptr'(Shell_Surface.all'Access),
                        WL_SHELL_SURFACE_RESIZE,
                        Seat.all'Address,
                        Serial,
                        Edges    );
      end Shell_Surface_Resize;

      procedure Shell_Surface_Set_Toplevel
        (Shell_Surface : Shell_Surface_Ptr) is
      begin
         Proxy_Marshal
           (Proxy_Ptr'(Shell_Surface.all'Access),
            WL_SHELL_SURFACE_SET_TOPLEVEL);
      end Shell_Surface_Set_Toplevel;

      procedure Shell_Surface_Set_Transient
        (Shell_Surface : Shell_Surface_Ptr;
         Parent        : Surface_Ptr;
         X             : Integer;
         Y             : Integer;
         Flags         : Unsigned_32) is
      begin
         Proxy_Marshal (Proxy_Ptr'(Shell_Surface.all'Access),
                        WL_SHELL_SURFACE_SET_TRANSIENT,
                        Parent.all'Address,
                        X,
                        Y,
                        Flags    );
      end Shell_Surface_Set_Transient;

      procedure Shell_Surface_Set_Fullscreen
        (Shell_Surface : Shell_Surface_Ptr;
         Method        : Unsigned_32;
         Framerate     : Unsigned_32;
         Output        : Output_Ptr) is
      begin
         Proxy_Marshal (Proxy_Ptr'(Shell_Surface.all'Access),
                        WL_SHELL_SURFACE_SET_FULLSCREEN,
                        Method,
                        Framerate,
                        Output.all'Address);
      end Shell_Surface_Set_Fullscreen;

      procedure Shell_Surface_Set_Popup
        (Shell_Surface : Shell_Surface_Ptr;
         Seat          : Seat_Ptr;
         Serial        : Unsigned_32;
         Parent        : Surface_Ptr;
         X             : Integer;
         Y             : Integer;
         Flags         : Unsigned_32) is
      begin
         Proxy_Marshal (Proxy_Ptr'(Shell_Surface.all'Access),
                        WL_SHELL_SURFACE_SET_POPUP,
                        Seat.all'Address,
                        Serial,
                        Parent.all'Address,
                        X,
                        Y,
                        Flags    );
      end Shell_Surface_Set_Popup;

      procedure Shell_Surface_Set_Maximized
        (Shell_Surface : Shell_Surface_Ptr;
         Output        : Output_Ptr) is
      begin
         Proxy_Marshal (Proxy_Ptr'(Shell_Surface.all'Access),
                        WL_SHELL_SURFACE_SET_MAXIMIZED,
                        Output.all'Address    );
      end Shell_Surface_Set_Maximized;

      procedure Shell_Surface_Set_Title
        (Shell_Surface : Shell_Surface_Ptr;
         Title         : Chars_Ptr) is
      begin
         Proxy_Marshal (Proxy_Ptr'(Shell_Surface.all'Access),
                        WL_SHELL_SURFACE_SET_TITLE,
                        Title    );
      end Shell_Surface_Set_Title;

      procedure Shell_Surface_Set_Class
        (Shell_Surface : Shell_Surface_Ptr;
         Class_V       : Chars_Ptr) is
      begin
         Proxy_Marshal (Proxy_Ptr'(Shell_Surface.all'Access),
                        WL_SHELL_SURFACE_SET_CLASS,
                        Class_V    );
      end Shell_Surface_Set_Class;

      function Surface_Add_Listener
        (Surface  : Surface_Ptr;
         Listener : Surface_Listener_Ptr;
         Data     : Void_Ptr) return Interfaces.C.Int is
      begin
         return Wl_Proxy_Add_Listener
           (Surface.all'Access, Listener.all'Address, Data);
      end Surface_Add_Listener;

      procedure Surface_Set_User_Data (Surface : Surface_Ptr;
                                       Data : Void_Ptr) is
      begin
         wl_proxy_set_user_data (Surface.all'Access, Data);
      end Surface_Set_User_Data;

      function Surface_Get_User_Data
        (Surface : Surface_Ptr) return Void_Ptr is
      begin
         return wl_proxy_get_user_data (Surface.all'Access);
      end Surface_Get_User_Data;

      function Surface_Get_Version
        (Surface : Surface_Ptr) return Unsigned_32 is
      begin
         return wl_proxy_get_version (Surface.all'Access);
      end Surface_Get_Version;

      procedure Surface_Destroy (Surface : Surface_Ptr) is
      begin
         Proxy_Marshal (Proxy_Ptr'(Surface.all'Access),
                        WL_SURFACE_DESTROY);

         wl_proxy_destroy (Surface.all'Access);
      end Surface_Destroy;


      procedure Surface_Attach (Surface : Surface_Ptr;
                                Buffer : Buffer_Ptr;
                                X : Integer;
                                Y : Integer
                               ) is
      begin
         Proxy_Marshal (Proxy_Ptr'(Surface.all'Access),
                        WL_SURFACE_ATTACH,
                        Buffer.all'Address,
                        X,
                        Y    );
      end Surface_Attach;

      procedure Surface_Damage (Surface : Surface_Ptr;
                                X : Integer;
                                Y : Integer;
                                Width : Integer;
                                Height : Integer
                               ) is
      begin
         Proxy_Marshal (Proxy_Ptr'(Surface.all'Access),
                        WL_SURFACE_DAMAGE,
                        X,
                        Y,
                        Width,
                        Height    );
      end Surface_Damage;

      function Surface_Frame (Surface : Surface_Ptr) return Callback_Ptr is
         P : constant Proxy_Ptr
           := Proxy_Marshal_Constructor (Surface.all'Access,
                                         WL_SURFACE_FRAME,
                                         Callback_Interface'Access,
                                         0);
      begin
         return (if P /= null then P.all'Access else null);
      end Surface_Frame;

      procedure Surface_Set_Opaque_Region (Surface : Surface_Ptr;
                                           Region : Region_Ptr
                                          ) is
      begin
         Proxy_Marshal (Proxy_Ptr'(Surface.all'Access),
                        WL_SURFACE_SET_OPAQUE_REGION,
                        Region.all'Address    );
      end Surface_Set_Opaque_Region;

      procedure Surface_Set_Input_Region (Surface : Surface_Ptr;
                                          Region : Region_Ptr
                                         ) is
      begin
         Proxy_Marshal (Proxy_Ptr'(Surface.all'Access),
                        WL_SURFACE_SET_INPUT_REGION,
                        Region.all'Address    );
      end Surface_Set_Input_Region;

      procedure Surface_Commit (Surface : Surface_Ptr) is
      begin
         Proxy_Marshal (Proxy_Ptr'(Surface.all'Access), WL_SURFACE_COMMIT);
      end Surface_Commit;

      procedure Surface_Set_Buffer_Transform (Surface : Surface_Ptr;
                                              Transform : Integer
                                             ) is
      begin
         Proxy_Marshal (Proxy_Ptr'(Surface.all'Access),
                        WL_SURFACE_SET_BUFFER_TRANSFORM,
                        Transform    );
      end Surface_Set_Buffer_Transform;

      procedure Surface_Set_Buffer_Scale (Surface : Surface_Ptr;
                                          Scale : Integer
                                         ) is
      begin
         Proxy_Marshal (Proxy_Ptr'(Surface.all'Access),
                        WL_SURFACE_SET_BUFFER_SCALE,
                        Scale    );
      end Surface_Set_Buffer_Scale;

      procedure Surface_Damage_Buffer (Surface : Surface_Ptr;
                                       X : Integer;
                                       Y : Integer;
                                       Width : Integer;
                                       Height : Integer
                                      ) is
      begin
         Proxy_Marshal (Proxy_Ptr'(Surface.all'Access),
                        WL_SURFACE_DAMAGE_BUFFER,
                        X,
                        Y,
                        Width,
                        Height    );
      end Surface_Damage_Buffer;

      function Seat_Add_Listener (Seat : Seat_Ptr;
                                  Listener : Seat_Listener_Ptr;
                                  Data : Void_Ptr) return Interfaces.C.int is
      begin
         return Wl_Proxy_Add_Listener
           (Seat.all'Access, Listener.all'Address, Data);
      end Seat_Add_Listener;

      procedure Seat_Set_User_Data (Seat : Seat_Ptr;
                                    Data : Void_Ptr) is
      begin
         wl_proxy_set_user_data (Seat.all'Access, Data);
      end Seat_Set_User_Data;

      function Seat_Get_User_Data (Seat : Seat_Ptr) return Void_Ptr is
      begin
         return wl_proxy_get_user_data (Seat.all'Access);
      end Seat_Get_User_Data;

      function Seat_Get_Version (Seat : Seat_Ptr) return Unsigned_32 is
      begin
         return wl_proxy_get_version (Seat.all'Access);
      end Seat_Get_Version;

      procedure Seat_Destroy (Seat : Seat_Ptr) is
      begin
         wl_proxy_destroy (Seat.all'Access);
      end Seat_Destroy;

      function Seat_Get_Pointer (Seat : Seat_Ptr) return Pointer_Ptr is
         P : constant Proxy_Ptr
           := Proxy_Marshal_Constructor (Seat.all'Access,
                                         WL_SEAT_GET_POINTER,
                                         Pointer_Interface'Access,
                                         0);
      begin
         return (if P /= null then P.all'Access else null);
      end Seat_Get_Pointer;

      function Seat_Get_Keyboard (Seat : Seat_Ptr) return Keyboard_Ptr is
         P : constant Proxy_Ptr
           := Proxy_Marshal_Constructor (Seat.all'Access,
                                         WL_SEAT_GET_KEYBOARD,
                                         Keyboard_Interface'Access,
                                         0);
      begin
         return (if P /= null then P.all'Access else null);
      end Seat_Get_Keyboard;

      function Seat_Get_Touch (Seat : Seat_Ptr) return Touch_Ptr is
         P : constant Proxy_Ptr
           := Proxy_Marshal_Constructor (Seat.all'Access,
                                         WL_SEAT_GET_TOUCH,
                                         Touch_Interface'Access,
                                         0);
      begin
         return (if P /= null then P.all'Access else null);
      end Seat_Get_Touch;

      procedure Seat_Release (Seat : Seat_Ptr) is
      begin
         Proxy_Marshal (Proxy_Ptr'(Seat.all'Access), WL_SEAT_RELEASE);
      end Seat_Release;

      function Pointer_Add_Listener
        (Pointer  : Pointer_Ptr;
         Listener : Pointer_Listener_Ptr;
         Data     : Void_Ptr) return Interfaces.C.Int is
      begin
         return Wl_Proxy_Add_Listener
           (Pointer.all'Access, Listener.all'Address, Data);
      end Pointer_Add_Listener;

      procedure Pointer_Set_User_Data
        (Pointer : Pointer_Ptr;
         Data    : Void_Ptr) is
      begin
         wl_proxy_set_user_data (Pointer.all'Access, Data);
      end Pointer_Set_User_Data;

      function Pointer_Get_User_Data
        (Pointer : Pointer_Ptr) return Void_Ptr is
      begin
         return wl_proxy_get_user_data (Pointer.all'Access);
      end Pointer_Get_User_Data;

      function Pointer_Get_Version
        (Pointer : Pointer_Ptr) return Unsigned_32 is
      begin
         return wl_proxy_get_version (Pointer.all'Access);
      end Pointer_Get_Version;

      procedure Pointer_Destroy (Pointer : Pointer_Ptr) is
      begin
         wl_proxy_destroy (Pointer.all'Access);
      end Pointer_Destroy;

      procedure Pointer_Set_Cursor (Pointer : Pointer_Ptr;
                                    Serial : Unsigned_32;
                                    Surface : Surface_Ptr;
                                    Hotspot_X : Integer;
                                    Hotspot_Y : Integer
                                   ) is
      begin
         Proxy_Marshal (Proxy_Ptr'(Pointer.all'Access),
                        WL_POINTER_SET_CURSOR,
                        Serial,
                        Surface.all'Address,
                        Hotspot_X,
                        Hotspot_Y    );
      end Pointer_Set_Cursor;

      procedure Pointer_Release (Pointer : Pointer_Ptr) is
      begin
         Proxy_Marshal (Proxy_Ptr'(Pointer.all'Access), WL_POINTER_RELEASE);
      end Pointer_Release;

      function Keyboard_Add_Listener
        (Keyboard : Keyboard_Ptr;
         Listener : Keyboard_Listener_Ptr;
         Data     : Void_Ptr) return Interfaces.C.Int is
      begin
         return Wl_Proxy_Add_Listener
           (Keyboard.all'Access, Listener.all'Address, Data);
      end Keyboard_Add_Listener;

      procedure Keyboard_Set_User_Data (Keyboard : Keyboard_Ptr;
                                        Data : Void_Ptr) is
      begin
         wl_proxy_set_user_data (Keyboard.all'Access, Data);
      end Keyboard_Set_User_Data;

      function Keyboard_Get_User_Data
        (Keyboard : Keyboard_Ptr) return Void_Ptr is
      begin
         return wl_proxy_get_user_data (Keyboard.all'Access);
      end Keyboard_Get_User_Data;

      function Keyboard_Get_Version
        (Keyboard : Keyboard_Ptr) return Unsigned_32 is
      begin
         return wl_proxy_get_version (Keyboard.all'Access);
      end Keyboard_Get_Version;

      procedure Keyboard_Destroy (Keyboard : Keyboard_Ptr) is
      begin
         wl_proxy_destroy (Keyboard.all'Access);
      end Keyboard_Destroy;

      procedure Keyboard_Release (Keyboard : Keyboard_Ptr) is
      begin
         Proxy_Marshal
           (Proxy_Ptr'(Keyboard.all'Access), WL_KEYBOARD_RELEASE);
      end Keyboard_Release;

      function Touch_Add_Listener
        (Touch    : Touch_Ptr;
         Listener : Touch_Listener_Ptr;
         Data     : Void_Ptr) return Interfaces.C.Int is
      begin
         return Wl_Proxy_Add_Listener
           (Touch.all'Access, Listener.all'Address, Data);
      end Touch_Add_Listener;

      procedure Touch_Set_User_Data (Touch : Touch_Ptr;
                                     Data : Void_Ptr) is
      begin
         wl_proxy_set_user_data (Touch.all'Access, Data);
      end Touch_Set_User_Data;

      function Touch_Get_User_Data (Touch : Touch_Ptr) return Void_Ptr is
      begin
         return wl_proxy_get_user_data (Touch.all'Access);
      end Touch_Get_User_Data;

      function Touch_Get_Version (Touch : Touch_Ptr) return Unsigned_32 is
      begin
         return wl_proxy_get_version (Touch.all'Access);
      end Touch_Get_Version;

      procedure Touch_Destroy (Touch : Touch_Ptr) is
      begin
         wl_proxy_destroy (Touch.all'Access);
      end Touch_Destroy;

      procedure Touch_Release (Touch : Touch_Ptr) is
      begin
         Proxy_Marshal (Proxy_Ptr'(Touch.all'Access), WL_TOUCH_RELEASE);
      end Touch_Release;

      function Output_Add_Listener
        (Output   : Output_Ptr;
         Listener : Output_Listener_Ptr;
         Data     : Void_Ptr) return Interfaces.C.Int is
      begin
         return Wl_Proxy_Add_Listener
           (Output.all'Access, Listener.all'Address, Data);
      end Output_Add_Listener;

      procedure Output_Set_User_Data
        (Output : Output_Ptr;
         Data   : Void_Ptr) is
      begin
         wl_proxy_set_user_data (Output.all'Access, Data);
      end Output_Set_User_Data;

      function Output_Get_User_Data (Output : Output_Ptr) return Void_Ptr is
      begin
         return wl_proxy_get_user_data (Output.all'Access);
      end Output_Get_User_Data;

      function Output_Get_Version (Output : Output_Ptr) return Unsigned_32 is
      begin
         return wl_proxy_get_version (Output.all'Access);
      end Output_Get_Version;

      procedure Output_Destroy (Output : Output_Ptr) is
      begin
         wl_proxy_destroy (Output.all'Access);
      end Output_Destroy;

      procedure Output_Release (Output : Output_Ptr) is
      begin
         Proxy_Marshal (Proxy_Ptr'(Output.all'Access), WL_OUTPUT_RELEASE);
      end Output_Release;

      procedure Region_Set_User_Data (Region : Region_Ptr;
                                      Data : Void_Ptr) is
      begin
         wl_proxy_set_user_data (Region.all'Access, Data);
      end Region_Set_User_Data;

      function Region_Get_User_Data (Region : Region_Ptr) return Void_Ptr is
      begin
         return wl_proxy_get_user_data (Region.all'Access);
      end Region_Get_User_Data;

      function Region_Get_Version (Region : Region_Ptr) return Unsigned_32 is
      begin
         return wl_proxy_get_version (Region.all'Access);
      end Region_Get_Version;

      procedure Region_Destroy (Region : Region_Ptr) is
      begin
         Proxy_Marshal (Proxy_Ptr'(Region.all'Access),
                        WL_REGION_DESTROY);

         wl_proxy_destroy (Region.all'Access);
      end Region_Destroy;


      procedure Region_Add (Region : Region_Ptr;
                            X : Integer;
                            Y : Integer;
                            Width : Integer;
                            Height : Integer
                           ) is
      begin
         Proxy_Marshal (Proxy_Ptr'(Region.all'Access),
                        WL_REGION_ADD,
                        X,
                        Y,
                        Width,
                        Height    );
      end Region_Add;

      procedure Region_Subtract (Region : Region_Ptr;
                                 X : Integer;
                                 Y : Integer;
                                 Width : Integer;
                                 Height : Integer
                                ) is
      begin
         Proxy_Marshal (Proxy_Ptr'(Region.all'Access),
                        WL_REGION_SUBTRACT,
                        X,
                        Y,
                        Width,
                        Height    );
      end Region_Subtract;

      procedure Subcompositor_Set_User_Data
        (Subcompositor : Subcompositor_Ptr;
         Data          : Void_Ptr) is
      begin
         wl_proxy_set_user_data (Subcompositor.all'Access, Data);
      end Subcompositor_Set_User_Data;

      function Subcompositor_Get_User_Data
        (Subcompositor : Subcompositor_Ptr) return Void_Ptr is
      begin
         return wl_proxy_get_user_data (Subcompositor.all'Access);
      end Subcompositor_Get_User_Data;

      function Subcompositor_Get_Version
        (Subcompositor : Subcompositor_Ptr) return Unsigned_32 is
      begin
         return wl_proxy_get_version (Subcompositor.all'Access);
      end Subcompositor_Get_Version;

      procedure Subcompositor_Destroy (Subcompositor : Subcompositor_Ptr) is
      begin
         Proxy_Marshal (Proxy_Ptr'(Subcompositor.all'Access),
                        WL_SUBCOMPOSITOR_DESTROY);

         wl_proxy_destroy (Subcompositor.all'Access);
      end Subcompositor_Destroy;

      function Subcompositor_Get_Subsurface
        (Subcompositor : Subcompositor_Ptr;
         Surface       : Surface_Ptr;
         Parent        : Surface_Ptr
        ) return Subsurface_Ptr
      is
         P : constant Proxy_Ptr
           := Proxy_Marshal_Constructor (Subcompositor.all'Access,
                                         WL_SUBCOMPOSITOR_GET_SUBSURFACE,
                                         Subsurface_Interface'Access,
                                         0,
                                         Surface.all'Address,
                                         Parent.all'Address);
      begin
         return (if P /= null then P.all'Access else null);
      end Subcompositor_Get_Subsurface;

      procedure Subsurface_Set_User_Data (Subsurface : Subsurface_Ptr;
                                          Data : Void_Ptr) is
      begin
         wl_proxy_set_user_data (Subsurface.all'Access, Data);
      end Subsurface_Set_User_Data;

      function Subsurface_Get_User_Data
        (Subsurface : Subsurface_Ptr) return Void_Ptr is
      begin
         return wl_proxy_get_user_data (Subsurface.all'Access);
      end Subsurface_Get_User_Data;

      function Subsurface_Get_Version
        (Subsurface : Subsurface_Ptr) return Unsigned_32 is
      begin
         return wl_proxy_get_version (Subsurface.all'Access);
      end Subsurface_Get_Version;

      procedure Subsurface_Destroy (Subsurface : Subsurface_Ptr) is
      begin
         Proxy_Marshal (Proxy_Ptr'(Subsurface.all'Access),
                        WL_SUBSURFACE_DESTROY);

         wl_proxy_destroy (Subsurface.all'Access);
      end Subsurface_Destroy;


      procedure Subsurface_Set_Position (Subsurface : Subsurface_Ptr;
                                         X : Integer;
                                         Y : Integer
                                        ) is
      begin
         Proxy_Marshal (Proxy_Ptr'(Subsurface.all'Access),
                        WL_SUBSURFACE_SET_POSITION,
                        X,
                        Y    );
      end Subsurface_Set_Position;

      procedure Subsurface_Place_Above (Subsurface : Subsurface_Ptr;
                                        Sibling : Surface_Ptr
                                       ) is
      begin
         Proxy_Marshal (Proxy_Ptr'(Subsurface.all'Access),
                        WL_SUBSURFACE_PLACE_ABOVE,
                        Sibling.all'Address    );
      end Subsurface_Place_Above;

      procedure Subsurface_Place_Below (Subsurface : Subsurface_Ptr;
                                        Sibling : Surface_Ptr
                                       ) is
      begin
         Proxy_Marshal (Proxy_Ptr'(Subsurface.all'Access),
                        WL_SUBSURFACE_PLACE_BELOW,
                        Sibling.all'Address    );
      end Subsurface_Place_Below;

      procedure Subsurface_Set_Sync (Subsurface : Subsurface_Ptr) is
      begin
         Proxy_Marshal
           (Proxy_Ptr'(Subsurface.all'Access), WL_SUBSURFACE_SET_SYNC);
      end Subsurface_Set_Sync;

      procedure Subsurface_Set_Desync (Subsurface : Subsurface_Ptr) is
      begin
         Proxy_Marshal
           (Proxy_Ptr'(Subsurface.all'Access), WL_SUBSURFACE_SET_DESYNC);
      end Subsurface_Set_Desync;


   end Wl_Thin;

   use type Wl_Thin.Proxy_Ptr;

   subtype Registry_Global_Subprogram_Ptr is Wl_Thin.Registry_Global_Subprogram_Ptr;

   subtype Registry_Global_Remove_Subprogram_Ptr is Wl_Thin.Registry_Global_Remove_Subprogram_Ptr;

   subtype Registry_Listener_T is Wl_Thin.Registry_Listener_T;

   subtype Registry_Listener_Ptr is Wl_Thin.Registry_Listener_Ptr;

   package body Display_Events is

      package Conversion is new
        System.Address_To_Access_Conversions (Data_Type);

      procedure Internal_Error (Data      : Void_Ptr;
                                Display   : Wl_Thin.Display_Ptr;
                                Object_Id : Void_Ptr;
                                Code      : Unsigned_32;
                                Message   : chars_ptr) with
        Convention => C;

      procedure Internal_Delete_Id (Data    : Void_Ptr;
                                    Display : Wl_Thin.Display_Ptr;
                                    Id      : Unsigned_32) with
        Convention => C;

      procedure Internal_Error (Data      : Void_Ptr;
                                Display   : Wl_Thin.Display_Ptr;
                                Object_Id : Void_Ptr;
                                Code      : Unsigned_32;
                                Message   : chars_ptr)
      is
         D : constant Wayland_Client.Display
           := (
               My_Display                  => Display,
               My_Fd                       =>
                 Wl_Thin.Display_Get_File_Descriptor (Display)
              );
         M : constant String := Interfaces.C.Strings.Value (Message);
      begin
         Error (Data_Ptr (Conversion.To_Pointer (Data)),
                D,
                Object_Id,
                Code,
                M);
      end Internal_Error;

      procedure Internal_Delete_Id (Data    : Void_Ptr;
                                    Display : Wl_Thin.Display_Ptr;
                                    Id      : Unsigned_32)
      is
         D : constant Wayland_Client.Display
           := (
               My_Display                  => Display,
               My_Fd                       =>
                 Wl_Thin.Display_Get_File_Descriptor (Display)
              );
      begin
         Delete_Id (Data_Ptr (Conversion.To_Pointer (Data)),
                    D,
                    Id);
      end Internal_Delete_Id;

      Listener : aliased Wl_Thin.Display_Listener_T
        := (
            Error     => Internal_Error'Unrestricted_Access,
            Delete_Id => Internal_Delete_Id'Unrestricted_Access
           );

      function Subscribe
        (Display : in out Wayland_Client.Display;
         Data    : not null Data_Ptr) return Call_Result_Code
      is
         I : int;
      begin
         I := Wl_Thin.Display_Add_Listener (Display.My_Display,
                                            Listener'Access,
                                            Data.all'Address);
         if I = 0 then
            return Success;
         else
            return Error;
         end if;
      end Subscribe;

   end Display_Events;

   package body Registry_Events is

      package Conversion is new
        System.Address_To_Access_Conversions (Data_Type);

      procedure Internal_Object_Added
        (Data        : Void_Ptr;
         Registry    : Wl_Thin.Registry_Ptr;
         Id          : Unsigned_32;
         Interface_V : Wayland_Client.Chars_Ptr;
         Version     : Unsigned_32) with
        Convention => C,
        Global     => null;

      procedure Internal_Object_Added
        (Data        : Void_Ptr;
         Registry    : Wl_Thin.Registry_Ptr;
         Id          : Unsigned_32;
         Interface_V : Wayland_Client.Chars_Ptr;
         Version     : Unsigned_32)
      is
         R : constant Wayland_Client.Registry := (My_Registry => Registry);
      begin
         Global_Object_Added (Data_Ptr (Conversion.To_Pointer (Data)),
                              R,
                              Id,
                              Value (Interface_V),
                              Version);
      end Internal_Object_Added;

      procedure Internal_Object_Removed (Data     : Void_Ptr;
                                         Registry : Wl_Thin.Registry_Ptr;
                                         Id       : Unsigned_32) with
        Convention => C;

      procedure Internal_Object_Removed (Data     : Void_Ptr;
                                         Registry : Wl_Thin.Registry_Ptr;
                                         Id       : Unsigned_32)
      is
         R : constant Wayland_Client.Registry := (My_Registry => Registry);
      begin
         Global_Object_Removed (Data_Ptr (Conversion.To_Pointer (Data)), R, Id);
      end Internal_Object_Removed;

      Listener : aliased Wayland_Client.Registry_Listener_T :=
        (
         Global        => Internal_Object_Added'Unrestricted_Access,
         Global_Remove => Internal_Object_Removed'Unrestricted_Access
        );
      -- Note: It should be safe to use Unrestricted_Access here since
      -- this generic can only be instantiated at library level.

      function Subscribe
        (Registry : in out Wayland_Client.Registry;
         Data     : not null Data_Ptr) return Call_Result_Code is
         I : int;
      begin
         I := Wl_Thin.Registry_Add_Listener (Registry.My_Registry,
                                             Listener'Access,
                                             Data.all'Address);

         if I = 0 then
            return Success;
         else
            return Error;
         end if;
      end Subscribe;

   end Registry_Events;

   package body Callback_Events is

      package Conversion is new
        System.Address_To_Access_Conversions (Data_Type);

      procedure Internal_Done (Data          : Void_Ptr;
                               Callback      : Wl_Thin.Callback_Ptr;
                               Callback_Data : Unsigned_32) with
        Convention => C;

      procedure Internal_Done (Data          : Void_Ptr;
                               Callback      : Wl_Thin.Callback_Ptr;
                               Callback_Data : Unsigned_32)
      is
         C : constant Wayland_Client.Callback := (My_Callback => Callback);
      begin
         Done (Data_Ptr (Conversion.To_Pointer (Data)), C, Callback_Data);
      end Internal_Done;

      Listener : aliased Wl_Thin.Callback_Listener_T
        := (Done => Internal_Done'Unrestricted_Access);

      function Subscribe
        (Callback : in out Wayland_Client.Callback;
         Data     : not null Data_Ptr) return Call_Result_Code
      is
         I : int;
      begin
         I := Wl_Thin.Callback_Add_Listener (Callback.My_Callback,
                                             Listener'Access,
                                             Data.all'Address);
         if I = 0 then
            return Success;
         else
            return Error;
         end if;
      end Subscribe;

   end Callback_Events;

   package body Shm_Events is

      package Conversion is new
        System.Address_To_Access_Conversions (Data_Type);

      procedure Internal_Format (Data   : Void_Ptr;
                                 Shm    : Wl_Thin.Shm_Ptr;
                                 Format : Unsigned_32) with
        Convention => C;

      procedure Internal_Format (Data   : Void_Ptr;
                                 Shm    : Wl_Thin.Shm_Ptr;
                                 Format : Unsigned_32)
      is
         S : constant Wayland_Client.Shm := (My_Shm => Shm);
      begin
         Shm_Events.Format
           (Data_Ptr (Conversion.To_Pointer (Data)), S, Format);
      end Internal_Format;

      Listener : aliased Wl_Thin.Shm_Listener_T
        := (Format => Internal_Format'Unrestricted_Access);

      function Subscribe
        (Shm  : in out Wayland_Client.Shm;
         Data : not null Data_Ptr) return Call_Result_Code
      is
         I : int;
      begin
         I := Wl_Thin.Shm_Add_Listener (Shm.My_Shm,
                                        Listener'Access,
                                        Data.all'Address);
         if I = 0 then
            return Success;
         else
            return Error;
         end if;
      end Subscribe;

   end Shm_Events;

   package body Buffer_Events is

      package Conversion is new
        System.Address_To_Access_Conversions (Data_Type);

      procedure Internal_Release (Data   : Void_Ptr;
                                  Buffer : Wl_Thin.Buffer_Ptr) with
        Convention => C;

      procedure Internal_Release (Data   : Void_Ptr;
                                  Buffer : Wl_Thin.Buffer_Ptr)
      is
         B : constant Wayland_Client.Buffer := (My_Buffer => Buffer);
      begin
         Release (Data_Ptr (Conversion.To_Pointer (Data)), B);
      end Internal_Release;

      Listener : aliased Wl_Thin.Buffer_Listener_T
        := (Release => Internal_Release'Unrestricted_Access);

      function Subscribe
        (Buffer : in out Wayland_Client.Buffer;
         Data   : not null Data_Ptr) return Call_Result_Code
      is
         I : int;
      begin
         I := Wl_Thin.Buffer_Add_Listener (Buffer.My_Buffer,
                                           Listener'Access,
                                           Data.all'Address);
         if I = 0 then
            return Success;
         else
            return Error;
         end if;
      end Subscribe;

   end Buffer_Events;

   package body Data_Offer_Events is

      package Conversion is new
        System.Address_To_Access_Conversions (Data_Type);

      procedure Internal_Offer (Data       : Void_Ptr;
                                Data_Offer : Wl_Thin.Data_Offer_Ptr;
                                Mime_Type  : chars_ptr) with
        Convention => C;

      procedure Internal_Source_Actions
        (Data           : Void_Ptr;
         Data_Offer     : Wl_Thin.Data_Offer_Ptr;
         Source_Actions : Unsigned_32) with
        Convention => C;

      procedure Internal_Action (Data       : Void_Ptr;
                                 Data_Offer : Wl_Thin.Data_Offer_Ptr;
                                 Dnd_Action : Unsigned_32) with
        Convention => C;

      procedure Internal_Offer (Data       : Void_Ptr;
                                Data_Offer : Wl_Thin.Data_Offer_Ptr;
                                Mime_Type  : chars_ptr)
      is
         D : constant Wayland_Client.Data_Offer
           := (My_Data_Offer => Data_Offer);

         M : constant String := Interfaces.C.Strings.Value (Mime_Type);
      begin
         Offer (Data_Ptr (Conversion.To_Pointer (Data)), D, M);
      end Internal_Offer;

      procedure Internal_Source_Actions
        (Data           : Void_Ptr;
         Data_Offer     : Wl_Thin.Data_Offer_Ptr;
         Source_Actions : Unsigned_32)
      is
         D : constant Wayland_Client.Data_Offer
           := (My_Data_Offer => Data_Offer);
      begin
         Data_Offer_Events.Source_Actions
           (Data_Ptr (Conversion.To_Pointer (Data)),
            D,
            Source_Actions);
      end Internal_Source_Actions;

      procedure Internal_Action (Data       : Void_Ptr;
                                 Data_Offer : Wl_Thin.Data_Offer_Ptr;
                                 Dnd_Action : Unsigned_32)
      is
         D : constant Wayland_Client.Data_Offer
           := (My_Data_Offer => Data_Offer);
      begin
         Action (Data_Ptr (Conversion.To_Pointer (Data)),
                 D,
                 Dnd_Action);
      end Internal_Action;

      Listener : aliased Wl_Thin.Data_Offer_Listener_T
        := (Offer          => Internal_Offer'Unrestricted_Access,
            Source_Actions => Internal_Source_Actions'Unrestricted_Access,
            Action         => Internal_Action'Unrestricted_Access);

      function Subscribe
        (Data_Offer : in out Wayland_Client.Data_Offer;
         Data       : not null Data_Ptr) return Call_Result_Code
      is
         I : int;
      begin
         I := Wl_Thin.Data_Offer_Add_Listener (Data_Offer.My_Data_Offer,
                                               Listener'Access,
                                               Data.all'Address);
         if I = 0 then
            return Success;
         else
            return Error;
         end if;
      end Subscribe;

   end Data_Offer_Events;

   package body Data_Source_Events is

      package Conversion is new
        System.Address_To_Access_Conversions (Data_Type);

      procedure Internal_Target (Data        : Void_Ptr;
                                 Data_Source : Wl_Thin.Data_Source_Ptr;
                                 Mime_Type   : chars_ptr) with
        Convention => C;

      procedure Internal_Send (Data        : Void_Ptr;
                               Data_Source : Wl_Thin.Data_Source_Ptr;
                               Mime_Type   : chars_ptr;
                               Fd          : Integer) with
        Convention => C;

      procedure Internal_Cancelled (Data        : Void_Ptr;
                                    Data_Source : Wl_Thin.Data_Source_Ptr) with
        Convention => C;

      procedure Internal_Dnd_Drop_Performed
        (Data        : Void_Ptr;
         Data_Source : Wl_Thin.Data_Source_Ptr) with
        Convention => C;

      procedure Internal_Dnd_Finished
        (Data        : Void_Ptr;
         Data_Source : Wl_Thin.Data_Source_Ptr) with
        Convention => C;

      procedure Internal_Action (Data        : Void_Ptr;
                                 Data_Source : Wl_Thin.Data_Source_Ptr;
                                 Dnd_Action  : Unsigned_32) with
        Convention => C;

      procedure Internal_Target (Data        : Void_Ptr;
                                 Data_Source : Wl_Thin.Data_Source_Ptr;
                                 Mime_Type   : chars_ptr)
      is
         D : constant Wayland_Client.Data_Source
           := (My_Data_Source => Data_Source);

         M : constant String := Interfaces.C.Strings.Value (Mime_Type);
      begin
         Target (Data_Ptr (Conversion.To_Pointer (Data)), D, M);
      end Internal_Target;

      procedure Internal_Send (Data        : Void_Ptr;
                               Data_Source : Wl_Thin.Data_Source_Ptr;
                               Mime_Type   : chars_ptr;
                               Fd          : Integer)
      is
         D : constant Wayland_Client.Data_Source
           := (My_Data_Source => Data_Source);

         M : constant String := Interfaces.C.Strings.Value (Mime_Type);
      begin
         Send (Data_Ptr (Conversion.To_Pointer (Data)), D, M, Fd);
      end Internal_Send;

      procedure Internal_Cancelled (Data        : Void_Ptr;
                                    Data_Source : Wl_Thin.Data_Source_Ptr)
      is
         D : constant Wayland_Client.Data_Source
           := (My_Data_Source => Data_Source);
      begin
         Cancelled (Data_Ptr (Conversion.To_Pointer (Data)), D);
      end Internal_Cancelled;

      procedure Internal_Dnd_Drop_Performed
        (Data        : Void_Ptr;
         Data_Source : Wl_Thin.Data_Source_Ptr)
      is
         D : constant Wayland_Client.Data_Source
           := (My_Data_Source => Data_Source);
      begin
         Dnd_Drop_Performed (Data_Ptr (Conversion.To_Pointer (Data)), D);
      end Internal_Dnd_Drop_Performed;

      procedure Internal_Dnd_Finished (Data        : Void_Ptr;
                                       Data_Source : Wl_Thin.Data_Source_Ptr)
      is
         D : constant Wayland_Client.Data_Source
           := (My_Data_Source => Data_Source);
      begin
         Dnd_Drop_Performed (Data_Ptr (Conversion.To_Pointer (Data)), D);
      end Internal_Dnd_Finished;

      procedure Internal_Action (Data        : Void_Ptr;
                                 Data_Source : Wl_Thin.Data_Source_Ptr;
                                 Dnd_Action  : Unsigned_32)
      is
         D : constant Wayland_Client.Data_Source
           := (My_Data_Source => Data_Source);
      begin
         Action (Data_Ptr (Conversion.To_Pointer (Data)), D, Dnd_Action);
      end Internal_Action;

      Listener : aliased Wl_Thin.Data_Source_Listener_T
        := (Target             => Internal_Target'Unrestricted_Access,
            Send               => Internal_Send'Unrestricted_Access,
            Cancelled          => Internal_Cancelled'Unrestricted_Access,
            Dnd_Drop_Performed =>
              Internal_Dnd_Drop_Performed'Unrestricted_Access,
            Dnd_Finished       => Internal_Dnd_Finished'Unrestricted_Access,
            Action             => Internal_Action'Unrestricted_Access);

      function Subscribe
        (Data_Source : in out Wayland_Client.Data_Source;
         Data       : not null Data_Ptr) return Call_Result_Code
      is
         I : int;
      begin
         I := Wl_Thin.Data_Source_Add_Listener (Data_Source.My_Data_Source,
                                                Listener'Access,
                                                Data.all'Address);
         if I = 0 then
            return Success;
         else
            return Error;
         end if;
      end Subscribe;

   end Data_Source_Events;

   package body Data_Device_Events is

      package Conversion is new
        System.Address_To_Access_Conversions (Data_Type);

      procedure Internal_Data_Offer (Data        : Void_Ptr;
                                     Data_Device : Wl_Thin.Data_Device_Ptr;
                                     Id          : Unsigned_32) with
        Convention => C;

      procedure Internal_Enter (Data        : Void_Ptr;
                                Data_Device : Wl_Thin.Data_Device_Ptr;
                                Serial      : Unsigned_32;
                                Surface     : Wl_Thin.Surface_Ptr;
                                X           : Fixed;
                                Y           : Fixed;
                                Id          : Wl_Thin.Data_Offer_Ptr) with
        Convention => C;

      procedure Internal_Leave (Data        : Void_Ptr;
                                Data_Device : Wl_Thin.Data_Device_Ptr) with
        Convention => C;

      procedure Internal_Motion (Data        : Void_Ptr;
                                 Data_Device : Wl_Thin.Data_Device_Ptr;
                                 Time        : Unsigned_32;
                                 X           : Fixed;
                                 Y           : Fixed) with
        Convention => C;

      procedure Internal_Drop (Data        : Void_Ptr;
                               Data_Device : Wl_Thin.Data_Device_Ptr) with
        Convention => C;

      procedure Internal_Selection (Data        : Void_Ptr;
                                    Data_Device : Wl_Thin.Data_Device_Ptr;
                                    Id          : Wl_Thin.Data_Offer_Ptr) with
        Convention => C;

      procedure Internal_Data_Offer (Data        : Void_Ptr;
                                     Data_Device : Wl_Thin.Data_Device_Ptr;
                                     Id          : Unsigned_32)
      is
         D : constant Wayland_Client.Data_Device
           := (My_Data_Device => Data_Device);
      begin
         Data_Offer (Data_Ptr (Conversion.To_Pointer (Data)), D, Id);
      end Internal_Data_Offer;

      procedure Internal_Enter (Data        : Void_Ptr;
                                Data_Device : Wl_Thin.Data_Device_Ptr;
                                Serial      : Unsigned_32;
                                Surface     : Wl_Thin.Surface_Ptr;
                                X           : Fixed;
                                Y           : Fixed;
                                Id          : Wl_Thin.Data_Offer_Ptr)
      is
         D : constant Wayland_Client.Data_Device
           := (My_Data_Device => Data_Device);
         S : constant Wayland_Client.Surface
           := (My_Surface => Surface);
         Offer : constant Wayland_Client.Data_Offer
           := (My_Data_Offer => Id);
      begin
         Enter (Data_Ptr (Conversion.To_Pointer (Data)),
                D,
                Serial,
                S,
                X,
                Y,
                Offer);
      end Internal_Enter;

      procedure Internal_Leave (Data        : Void_Ptr;
                                Data_Device : Wl_Thin.Data_Device_Ptr)
      is
         D : constant Wayland_Client.Data_Device
           := (My_Data_Device => Data_Device);
      begin
         Leave (Data_Ptr (Conversion.To_Pointer (Data)), D);
      end Internal_Leave;

      procedure Internal_Motion (Data        : Void_Ptr;
                                 Data_Device : Wl_Thin.Data_Device_Ptr;
                                 Time        : Unsigned_32;
                                 X           : Fixed;
                                 Y           : Fixed)
      is
         D : constant Wayland_Client.Data_Device
           := (My_Data_Device => Data_Device);
      begin
         Motion (Data_Ptr (Conversion.To_Pointer (Data)), D, Time, X, Y);
      end Internal_Motion;

      procedure Internal_Drop (Data        : Void_Ptr;
                               Data_Device : Wl_Thin.Data_Device_Ptr)
      is
         D : constant Wayland_Client.Data_Device
           := (My_Data_Device => Data_Device);
      begin
         Drop (Data_Ptr (Conversion.To_Pointer (Data)), D);
      end Internal_Drop;

      procedure Internal_Selection (Data        : Void_Ptr;
                                    Data_Device : Wl_Thin.Data_Device_Ptr;
                                    Id          : Wl_Thin.Data_Offer_Ptr)
      is
         D : constant Wayland_Client.Data_Device
           := (My_Data_Device => Data_Device);
         Offer : constant Wayland_Client.Data_Offer
           := (My_Data_Offer => Id);
      begin
         Selection (Data_Ptr (Conversion.To_Pointer (Data)), D, Offer);
      end Internal_Selection;

      Listener : aliased Wl_Thin.Data_Device_Listener_T
        := (Data_Offer => Internal_Data_Offer'Unrestricted_Access,
            Enter      => Internal_Enter'Unrestricted_Access,
            Leave      => Internal_Leave'Unrestricted_Access,
            Motion     => Internal_Motion'Unrestricted_Access,
            Drop       => Internal_Drop'Unrestricted_Access,
            Selection  => Internal_Selection'Unrestricted_Access);

      function Subscribe
        (Data_Device : in out Wayland_Client.Data_Device;
         Data        : not null Data_Ptr) return Call_Result_Code
      is
         I : int;
      begin
         I := Wl_Thin.Data_Device_Add_Listener (Data_Device.My_Data_Device,
                                                Listener'Access,
                                                Data.all'Address);
         if I = 0 then
            return Success;
         else
            return Error;
         end if;
      end Subscribe;

   end Data_Device_Events;

   package body Shell_Surface_Events is

      package Conversion is new
        System.Address_To_Access_Conversions (Data_Type);

      procedure Internal_Shell_Surface_Ping
        (Data    : Void_Ptr;
         Surface : Wl_Thin.Shell_Surface_Ptr;
         Serial  : Unsigned_32) with
        Convention => C;

      procedure Internal_Shell_Surface_Configure
        (Data    : Void_Ptr;
         Surface : Wl_Thin.Shell_Surface_Ptr;
         Edges   : Unsigned_32;
         Width   : Integer;
         Height  : Integer) with
        Convention => C;

      procedure Internal_Shell_Surface_Popup_Done
        (Data    : Void_Ptr;
         Surface : Wl_Thin.Shell_Surface_Ptr) with
        Convention => C;

      procedure Internal_Shell_Surface_Ping
        (Data    : Void_Ptr;
         Surface : Wl_Thin.Shell_Surface_Ptr;
         Serial  : Unsigned_32)
      is
         S : constant Wayland_Client.Shell_Surface
           := (My_Shell_Surface => Surface);
      begin
         Shell_Surface_Ping (Data_Ptr (Conversion.To_Pointer (Data)),
                             S,
                             Serial);
      end Internal_Shell_Surface_Ping;

      procedure Internal_Shell_Surface_Configure
        (Data    : Void_Ptr;
         Surface : Wl_Thin.Shell_Surface_Ptr;
         Edges   : Unsigned_32;
         Width   : Integer;
         Height  : Integer)
      is
         S : constant Wayland_Client.Shell_Surface
           := (My_Shell_Surface => Surface);
      begin
         Shell_Surface_Configure (Data_Ptr (Conversion.To_Pointer (Data)),
                                  S,
                                  Edges,
                                  Width,
                                  Height);
      end Internal_Shell_Surface_Configure;

      procedure Internal_Shell_Surface_Popup_Done
        (Data    : Void_Ptr;
         Surface : Wl_Thin.Shell_Surface_Ptr)
      is
         S : constant Wayland_Client.Shell_Surface
           := (My_Shell_Surface => Surface);
      begin
         Shell_Surface_Popup_Done (Data_Ptr (Conversion.To_Pointer (Data)), S);
      end Internal_Shell_Surface_Popup_Done;

      Listener : aliased Wl_Thin.Shell_Surface_Listener_T :=
        (
         Ping       => Internal_Shell_Surface_Ping'Unrestricted_Access,
         Configure  => Internal_Shell_Surface_Configure'Unrestricted_Access,
         Popup_Done => Internal_Shell_Surface_Popup_Done'Unrestricted_Access
        );

      function Subscribe
        (Surface : in out Wayland_Client.Shell_Surface;
         Data    : not null Data_Ptr) return Call_Result_Code
      is
         I : int;
      begin
         I := Wl_Thin.Shell_Surface_Add_Listener
           (Surface.My_Shell_Surface,
            Listener'Access,
            Data.all'Address);

         if I = 0 then
            return Success;
         else
            return Error;
         end if;
      end Subscribe;

   end Shell_Surface_Events;

   package body Surface_Events is

      package Conversion is new
        System.Address_To_Access_Conversions (Data_Type);

      procedure Internal_Enter (Data    : Void_Ptr;
                                Surface : Wl_Thin.Surface_Ptr;
                                Output  : Wl_Thin.Output_Ptr) with
        Convention => C;

      procedure Internal_Leave (Data    : Void_Ptr;
                                Surface : Wl_Thin.Surface_Ptr;
                                Output  : Wl_Thin.Output_Ptr) with
        Convention => C;

      procedure Internal_Enter (Data    : Void_Ptr;
                                Surface : Wl_Thin.Surface_Ptr;
                                Output  : Wl_Thin.Output_Ptr)
      is
         S : constant Wayland_Client.Surface := (My_Surface => Surface);
         O : constant Wayland_Client.Output := (My_Output => Output);
      begin
         Enter (Data_Ptr (Conversion.To_Pointer (Data)), S, O);
      end Internal_Enter;

      procedure Internal_Leave (Data    : Void_Ptr;
                                Surface : Wl_Thin.Surface_Ptr;
                                Output  : Wl_Thin.Output_Ptr)
      is
         S : constant Wayland_Client.Surface := (My_Surface => Surface);
         O : constant Wayland_Client.Output := (My_Output => Output);
      begin
         Leave (Data_Ptr (Conversion.To_Pointer (Data)), S, O);
      end Internal_Leave;

      Listener : aliased Wl_Thin.Surface_Listener_T
        := (Enter => Internal_Enter'Unrestricted_Access,
            Leave => Internal_Leave'Unrestricted_Access);

      function Subscribe
        (Surface : in out Wayland_Client.Surface;
         Data    : not null Data_Ptr) return Call_Result_Code
      is
         I : int;
      begin
         I := Wl_Thin.Surface_Add_Listener (Surface.My_Surface,
                                            Listener'Access,
                                            Data.all'Address);

         if I = 0 then
            return Success;
         else
            return Error;
         end if;
      end Subscribe;

   end Surface_Events;

   package body Seat_Events is

      package Conversion is new
        System.Address_To_Access_Conversions (Data_Type);

      procedure Internal_Seat_Capabilities (Data         : Void_Ptr;
                                            Seat         : Wl_Thin.Seat_Ptr;
                                            Capabilities : Unsigned_32) with
        Convention => C;

      procedure Internal_Seat_Capabilities (Data         : Void_Ptr;
                                            Seat         : Wl_Thin.Seat_Ptr;
                                            Capabilities : Unsigned_32)
      is
         S : constant Wayland_Client.Seat := (My_Seat => Seat);
      begin
         Seat_Capabilities (Data_Ptr (Conversion.To_Pointer (Data)),
                            S,
                            Capabilities);
      end Internal_Seat_Capabilities;

      procedure Internal_Seat_Name
        (Data : Void_Ptr;
         Seat : Wl_Thin.Seat_Ptr;
         Name : Interfaces.C.Strings.Chars_Ptr) with
        Convention => C;

      procedure Internal_Seat_Name
        (Data : Void_Ptr;
         Seat : Wl_Thin.Seat_Ptr;
         Name : Interfaces.C.Strings.Chars_Ptr)
      is
         N : constant String := Interfaces.C.Strings.Value (Name);

         S : constant Wayland_Client.Seat := (My_Seat => Seat);
      begin
         Seat_Name (Data_Ptr (Conversion.To_Pointer (Data)), S, N);
      end Internal_Seat_Name;

      Seat_Listener : aliased Wl_Thin.Seat_Listener_T :=
        (
         Capabilities => Internal_Seat_Capabilities'Unrestricted_Access,
         Name         => Internal_Seat_Name'Unrestricted_Access
        );

      function Subscribe
        (Seat : in out Wayland_Client.Seat;
         Data : not null Data_Ptr) return Call_Result_Code
      is
         I : int;
      begin
         I := Wl_Thin.Seat_Add_Listener
           (Seat     => Seat.My_Seat,
            Listener => Seat_Listener'Access,
            Data     => Data.all'Address);
         if I = 0 then
            return Success;
         else
            return Error;
         end if;
      end Subscribe;

   end Seat_Events;

   package body Pointer_Events is

      package Conversion is new
        System.Address_To_Access_Conversions (Data_Type);

      procedure Internal_Pointer_Enter
        (Data      : Void_Ptr;
         Pointer   : Wl_Thin.Pointer_Ptr;
         Serial    : Unsigned_32;
         Surface   : Wl_Thin.Surface_Ptr;
         Surface_X : Wayland_Client.Fixed;
         Surface_Y : Wayland_Client.Fixed) with
        Convention => C;

      procedure Internal_Pointer_Leave
        (Data    : Void_Ptr;
         Pointer : Wl_Thin.Pointer_Ptr;
         Serial  : Unsigned_32;
         Surface : Wl_Thin.Surface_Ptr) with
        Convention => C;

      procedure Internal_Pointer_Motion
        (Data      : Void_Ptr;
         Pointer   : Wl_Thin.Pointer_Ptr;
         Time      : Unsigned_32;
         Surface_X : Wayland_Client.Fixed;
         Surface_Y : Wayland_Client.Fixed) with
        Convention => C;

      procedure Internal_Pointer_Button
        (Data    : Void_Ptr;
         Pointer : Wl_Thin.Pointer_Ptr;
         Serial  : Unsigned_32;
         Time    : Unsigned_32;
         Button  : Unsigned_32;
         State   : Unsigned_32) with
        Convention => C;

      procedure Internal_Pointer_Axis
        (Data    : Void_Ptr;
         Pointer : Wl_Thin.Pointer_Ptr;
         Time    : Unsigned_32;
         Axis    : Unsigned_32;
         Value   : Wayland_Client.Fixed) with
        Convention => C;

      procedure Internal_Pointer_Frame (Data    : Void_Ptr;
                                        Pointer : Wl_Thin.Pointer_Ptr) with
        Convention => C;

      procedure Internal_Pointer_Axis_Source
        (Data        : Void_Ptr;
         Pointer     : Wl_Thin.Pointer_Ptr;
         Axis_Source : Unsigned_32) with
        Convention => C;

      procedure Internal_Pointer_Axis_Stop
        (Data    : Void_Ptr;
         Pointer : Wl_Thin.Pointer_Ptr;
         Time    : Unsigned_32;
         Axis    : Unsigned_32) with
        Convention => C;

      procedure Internal_Pointer_Axis_Discrete
        (Data     : Void_Ptr;
         Pointer  : Wl_Thin.Pointer_Ptr;
         Axis     : Unsigned_32;
         Discrete : Integer) with
        Convention => C;

      procedure Internal_Pointer_Enter
        (Data      : Void_Ptr;
         Pointer   : Wl_Thin.Pointer_Ptr;
         Serial    : Unsigned_32;
         Surface   : Wl_Thin.Surface_Ptr;
         Surface_X : Wayland_Client.Fixed;
         Surface_Y : Wayland_Client.Fixed)
      is
         P : constant Wayland_Client.Pointer := (My_Pointer => Pointer);
         S : constant Wayland_Client.Surface := (My_Surface => Surface);
      begin
         Pointer_Enter (Data_Ptr (Conversion.To_Pointer (Data)),
                        P,
                        Serial,
                        S,
                        Surface_X,
                        Surface_Y);
      end Internal_Pointer_Enter;

      procedure Internal_Pointer_Leave
        (Data    : Void_Ptr;
         Pointer : Wl_Thin.Pointer_Ptr;
         Serial  : Unsigned_32;
         Surface : Wl_Thin.Surface_Ptr)
      is
         P : constant Wayland_Client.Pointer := (My_Pointer => Pointer);
         S : constant Wayland_Client.Surface := (My_Surface => Surface);
      begin
         Pointer_Leave (Data_Ptr (Conversion.To_Pointer (Data)), P, Serial, S);
      end Internal_Pointer_Leave;

      procedure Internal_Pointer_Motion
        (Data      : Void_Ptr;
         Pointer   : Wl_Thin.Pointer_Ptr;
         Time      : Unsigned_32;
         Surface_X : Wayland_Client.Fixed;
         Surface_Y : Wayland_Client.Fixed)
      is
         P : constant Wayland_Client.Pointer := (My_Pointer => Pointer);
      begin
         Pointer_Motion (Data_Ptr (Conversion.To_Pointer (Data)),
                         P,
                         Time,
                         Surface_X,
                         Surface_Y);
      end Internal_Pointer_Motion;

      procedure Internal_Pointer_Button
        (Data    : Void_Ptr;
         Pointer : Wl_Thin.Pointer_Ptr;
         Serial  : Unsigned_32;
         Time    : Unsigned_32;
         Button  : Unsigned_32;
         State   : Unsigned_32)
      is
         P : constant Wayland_Client.Pointer := (My_Pointer => Pointer);
      begin
         Pointer_Button (Data_Ptr (Conversion.To_Pointer (Data)),
                         P,
                         Serial,
                         Time,
                         Button,
                         State);
      end Internal_Pointer_Button;

      procedure Internal_Pointer_Axis
        (Data    : Void_Ptr;
         Pointer : Wl_Thin.Pointer_Ptr;
         Time    : Unsigned_32;
         Axis    : Unsigned_32;
         Value   : Wayland_Client.Fixed)
      is
         P : constant Wayland_Client.Pointer := (My_Pointer => Pointer);
      begin
         Pointer_Axis (Data_Ptr (Conversion.To_Pointer (Data)),
                       P,
                       Time,
                       Axis,
                       Value);
      end Internal_Pointer_Axis;

      procedure Internal_Pointer_Frame (Data    : Void_Ptr;
                                        Pointer : Wl_Thin.Pointer_Ptr)
      is
         P : constant Wayland_Client.Pointer := (My_Pointer => Pointer);
      begin
         Pointer_Frame (Data_Ptr (Conversion.To_Pointer (Data)), P);
      end Internal_Pointer_Frame;

      procedure Internal_Pointer_Axis_Source
        (Data        : Void_Ptr;
         Pointer     : Wl_Thin.Pointer_Ptr;
         Axis_Source : Unsigned_32)
      is
         P : constant Wayland_Client.Pointer := (My_Pointer => Pointer);
      begin
         Pointer_Axis_Source (Data_Ptr (Conversion.To_Pointer (Data)),
                              P,
                              Axis_Source);
      end Internal_Pointer_Axis_Source;

      procedure Internal_Pointer_Axis_Stop
        (Data    : Void_Ptr;
         Pointer : Wl_Thin.Pointer_Ptr;
         Time    : Unsigned_32;
         Axis    : Unsigned_32)
      is
         P : constant Wayland_Client.Pointer := (My_Pointer => Pointer);
      begin
         Pointer_Axis_Stop (Data_Ptr (Conversion.To_Pointer (Data)),
                            P,
                            Time,
                            Axis);
      end Internal_Pointer_Axis_Stop;

      procedure Internal_Pointer_Axis_Discrete
        (Data     : Void_Ptr;
         Pointer  : Wl_Thin.Pointer_Ptr;
         Axis     : Unsigned_32;
         Discrete : Integer)
      is
         P : constant Wayland_Client.Pointer := (My_Pointer => Pointer);
      begin
         Pointer_Axis_Discrete (Data_Ptr (Conversion.To_Pointer (Data)),
                                P,
                                Axis,
                                Discrete);
      end Internal_Pointer_Axis_Discrete;

      Pointer_Listener : aliased Wl_Thin.Pointer_Listener_T :=
        (
         Enter         => Internal_Pointer_Enter'Unrestricted_Access,
         Leave         => Internal_Pointer_Leave'Unrestricted_Access,
         Motion        => Internal_Pointer_Motion'Unrestricted_Access,
         Button        => Internal_Pointer_Button'Unrestricted_Access,
         Axis          => Internal_Pointer_Axis'Unrestricted_Access,
         Frame         => Internal_Pointer_Frame'Unrestricted_Access,
         Axis_Source   => Internal_Pointer_Axis_Source'Unrestricted_Access,
         Axis_Stop     => Internal_Pointer_Axis_Stop'Unrestricted_Access,
         Axis_Discrete => Internal_Pointer_Axis_Discrete'Unrestricted_Access
        );

      function Subscribe
        (Pointer : in out Wayland_Client.Pointer;
         Data    : not null Data_Ptr) return Call_Result_Code
      is
         I : int;
      begin
         I := Wl_Thin.Pointer_Add_Listener
           (Pointer  => Pointer.My_Pointer,
            Listener => Pointer_Listener'Access,
            Data     => Data.all'Address);
         if I = 0 then
            return Success;
         else
            return Error;
         end if;
      end Subscribe;

   end Pointer_Events;

   package body Keyboard_Events is

      package Conversion is new
        System.Address_To_Access_Conversions (Data_Type);

      procedure Internal_Keymap (Data     : Void_Ptr;
                                 Keyboard : Wl_Thin.Keyboard_Ptr;
                                 Format   : Unsigned_32;
                                 Fd       : Integer;
                                 Size     : Unsigned_32) with
        Convention => C;

      procedure Internal_Enter (Data     : Void_Ptr;
                                Keyboard : Wl_Thin.Keyboard_Ptr;
                                Serial   : Unsigned_32;
                                Surface  : Wl_Thin.Surface_Ptr;
                                Keys     : Wayland_Array_T) with
        Convention => C;

      procedure Internal_Leave (Data     : Void_Ptr;
                                Keyboard : Wl_Thin.Keyboard_Ptr;
                                Serial   : Unsigned_32;
                                Surface  : Wl_Thin.Surface_Ptr) with
        Convention => C;

      procedure Internal_Key (Data     : Void_Ptr;
                              Keyboard : Wl_Thin.Keyboard_Ptr;
                              Serial   : Unsigned_32;
                              Time     : Unsigned_32;
                              Key      : Unsigned_32;
                              State    : Unsigned_32) with
        Convention => C;

      procedure Internal_Modifiers (Data           : Void_Ptr;
                                    Keyboard       : Wl_Thin.Keyboard_Ptr;
                                    Serial         : Unsigned_32;
                                    Mods_Depressed : Unsigned_32;
                                    Mods_Latched   : Unsigned_32;
                                    Mods_Locked    : Unsigned_32;
                                    Group          : Unsigned_32) with
        Convention => C;

      procedure Internal_Repeat_Info (Data     : Void_Ptr;
                                      Keyboard : Wl_Thin.Keyboard_Ptr;
                                      Rate     : Integer;
                                      Delay_V  : Integer) with
        Convention => C;

      procedure Internal_Keymap (Data     : Void_Ptr;
                                 Keyboard : Wl_Thin.Keyboard_Ptr;
                                 Format   : Unsigned_32;
                                 Fd       : Integer;
                                 Size     : Unsigned_32)
      is
         K : constant Wayland_Client.Keyboard := (My_Keyboard => Keyboard);
      begin
         Keymap (Data_Ptr (Conversion.To_Pointer (Data)),
                 K,
                 Format,
                 Fd,
                 Size);
      end Internal_Keymap;

      procedure Internal_Enter (Data     : Void_Ptr;
                                Keyboard : Wl_Thin.Keyboard_Ptr;
                                Serial   : Unsigned_32;
                                Surface  : Wl_Thin.Surface_Ptr;
                                Keys     : Wayland_Array_T)
      is
         K : constant Wayland_Client.Keyboard := (My_Keyboard => Keyboard);
         S : constant Wayland_Client.Surface  := (My_Surface => Surface);
      begin
         Enter (Data_Ptr (Conversion.To_Pointer (Data)),
                K,
                Serial,
                S,
                Keys);
      end Internal_Enter;

      procedure Internal_Leave (Data     : Void_Ptr;
                                Keyboard : Wl_Thin.Keyboard_Ptr;
                                Serial   : Unsigned_32;
                                Surface  : Wl_Thin.Surface_Ptr)
      is
         K : constant Wayland_Client.Keyboard := (My_Keyboard => Keyboard);
         S : constant Wayland_Client.Surface  := (My_Surface => Surface);
      begin
         Leave (Data_Ptr (Conversion.To_Pointer (Data)),
                K,
                Serial,
                S);
      end Internal_Leave;

      procedure Internal_Key (Data     : Void_Ptr;
                              Keyboard : Wl_Thin.Keyboard_Ptr;
                              Serial   : Unsigned_32;
                              Time     : Unsigned_32;
                              Key      : Unsigned_32;
                              State    : Unsigned_32)
      is
         K : constant Wayland_Client.Keyboard := (My_Keyboard => Keyboard);
      begin
         Keyboard_Events.Key (Data_Ptr (Conversion.To_Pointer (Data)),
                              K,
                              Serial,
                              Time,
                              Key,
                              State);
      end Internal_Key;

      procedure Internal_Modifiers (Data           : Void_Ptr;
                                    Keyboard       : Wl_Thin.Keyboard_Ptr;
                                    Serial         : Unsigned_32;
                                    Mods_Depressed : Unsigned_32;
                                    Mods_Latched   : Unsigned_32;
                                    Mods_Locked    : Unsigned_32;
                                    Group          : Unsigned_32)
      is
         K : constant Wayland_Client.Keyboard := (My_Keyboard => Keyboard);
      begin
         Modifiers (Data_Ptr (Conversion.To_Pointer (Data)),
                    K,
                    Serial,
                    Mods_Depressed,
                    Mods_Latched,
                    Mods_Locked,
                    Group);
      end Internal_Modifiers;

      procedure Internal_Repeat_Info (Data     : Void_Ptr;
                                      Keyboard : Wl_Thin.Keyboard_Ptr;
                                      Rate     : Integer;
                                      Delay_V  : Integer)
      is
         K : constant Wayland_Client.Keyboard := (My_Keyboard => Keyboard);
      begin
         Repeat_Info (Data_Ptr (Conversion.To_Pointer (Data)),
                      K,
                      Rate,
                      Delay_V);
      end Internal_Repeat_Info;

      Listener : aliased Wl_Thin.Keyboard_Listener_T
        := (Keymap      => Internal_Keymap'Unrestricted_Access,
            Enter       => Internal_Enter'Unrestricted_Access,
            Leave       => Internal_Leave'Unrestricted_Access,
            Key         => Internal_Key'Unrestricted_Access,
            Modifiers   => Internal_Modifiers'Unrestricted_Access,
            Repeat_Info => Internal_Repeat_Info'Unrestricted_Access);

      function Subscribe
        (Keyboard : in out Wayland_Client.Keyboard;
         Data     : not null Data_Ptr) return Call_Result_Code
      is
         I : int;
      begin
         I := Wl_Thin.Keyboard_Add_Listener
           (Keyboard => Keyboard.My_Keyboard,
            Listener => Listener'Access,
            Data     => Data.all'Address);
         if I = 0 then
            return Success;
         else
            return Error;
         end if;
      end Subscribe;

   end Keyboard_Events;

   package body Touch_Events is

      package Conversion is new
        System.Address_To_Access_Conversions (Data_Type);

      procedure Internal_Down (Data    : Void_Ptr;
                               Touch   : Wl_Thin.Touch_Ptr;
                               Serial  : Unsigned_32;
                               Time    : Unsigned_32;
                               Surface : Wl_Thin.Surface_Ptr;
                               Id      : Integer;
                               X       : Fixed;
                               Y       : Fixed) with
        Convention => C;

      procedure Internal_Up (Data   : Void_Ptr;
                             Touch  : Wl_Thin.Touch_Ptr;
                             Serial : Unsigned_32;
                             Time   : Unsigned_32;
                             Id     : Integer) with
        Convention => C;

      procedure Internal_Motion (Data  : Void_Ptr;
                                 Touch : Wl_Thin.Touch_Ptr;
                                 Time  : Unsigned_32;
                                 Id    : Integer;
                                 X     : Fixed;
                                 Y     : Fixed) with
        Convention => C;

      procedure Internal_Frame (Data  : Void_Ptr;
                                Touch : Wl_Thin.Touch_Ptr) with
        Convention => C;

      procedure Internal_Cancel (Data  : Void_Ptr;
                                 Touch : Wl_Thin.Touch_Ptr) with
        Convention => C;

      procedure Internal_Shape (Data  : Void_Ptr;
                                Touch : Wl_Thin.Touch_Ptr;
                                Id    : Integer;
                                Major : Fixed;
                                Minor : Fixed) with
        Convention => C;

      procedure Internal_Orientation (Data        : Void_Ptr;
                                      Touch       : Wl_Thin.Touch_Ptr;
                                      Id          : Integer;
                                      Orientation : Fixed) with
        Convention => C;

      procedure Internal_Down (Data    : Void_Ptr;
                               Touch   : Wl_Thin.Touch_Ptr;
                               Serial  : Unsigned_32;
                               Time    : Unsigned_32;
                               Surface : Wl_Thin.Surface_Ptr;
                               Id      : Integer;
                               X       : Fixed;
                               Y       : Fixed)
      is
         T : constant Wayland_Client.Touch := (My_Touch => Touch);
         S : constant Wayland_Client.Surface := (My_Surface => Surface);
      begin
         Down (Data_Ptr (Conversion.To_Pointer (Data)),
               T,
               Serial,
               Time,
               S,
               Id,
               X,
               Y);
      end Internal_Down;

      procedure Internal_Up (Data   : Void_Ptr;
                             Touch  : Wl_Thin.Touch_Ptr;
                             Serial : Unsigned_32;
                             Time   : Unsigned_32;
                             Id     : Integer)
      is
         T : constant Wayland_Client.Touch := (My_Touch => Touch);
      begin
         Up (Data_Ptr (Conversion.To_Pointer (Data)),
             T,
             Serial,
             Time,
             Id);
      end Internal_Up;

      procedure Internal_Motion (Data  : Void_Ptr;
                                 Touch : Wl_Thin.Touch_Ptr;
                                 Time  : Unsigned_32;
                                 Id    : Integer;
                                 X     : Fixed;
                                 Y     : Fixed)
      is
         T : constant Wayland_Client.Touch := (My_Touch => Touch);
      begin
         Motion (Data_Ptr (Conversion.To_Pointer (Data)),
                 T,
                 Time,
                 Id,
                 X,
                 Y);
      end Internal_Motion;

      procedure Internal_Frame (Data  : Void_Ptr;
                                Touch : Wl_Thin.Touch_Ptr)
      is
         T : constant Wayland_Client.Touch := (My_Touch => Touch);
      begin
         Frame (Data_Ptr (Conversion.To_Pointer (Data)), T);
      end Internal_Frame;

      procedure Internal_Cancel (Data  : Void_Ptr;
                                 Touch : Wl_Thin.Touch_Ptr)
      is
         T : constant Wayland_Client.Touch := (My_Touch => Touch);
      begin
         Cancel (Data_Ptr (Conversion.To_Pointer (Data)), T);
      end Internal_Cancel;

      procedure Internal_Shape (Data  : Void_Ptr;
                                Touch : Wl_Thin.Touch_Ptr;
                                Id    : Integer;
                                Major : Fixed;
                                Minor : Fixed)
      is
         T : constant Wayland_Client.Touch := (My_Touch => Touch);
      begin
         Shape (Data_Ptr (Conversion.To_Pointer (Data)),
                T,
                Id,
                Major,
                Minor);
      end Internal_Shape;

      procedure Internal_Orientation (Data        : Void_Ptr;
                                      Touch       : Wl_Thin.Touch_Ptr;
                                      Id          : Integer;
                                      Orientation : Fixed)
      is
         T : constant Wayland_Client.Touch := (My_Touch => Touch);
      begin
         Touch_Events.Orientation (Data_Ptr (Conversion.To_Pointer (Data)),
                                   T,
                                   Id,
                                   Orientation);
      end Internal_Orientation;

      Listener : aliased Wl_Thin.Touch_Listener_T
        := (Down        => Internal_Down'Unrestricted_Access,
            Up          => Internal_Up'Unrestricted_Access,
            Motion      => Internal_Motion'Unrestricted_Access,
            Frame       => Internal_Frame'Unrestricted_Access,
            Cancel      => Internal_Cancel'Unrestricted_Access,
            Shape       => Internal_Shape'Unrestricted_Access,
            Orientation => Internal_Orientation'Unrestricted_Access);

      function Subscribe
        (Touch : in out Wayland_Client.Touch;
         Data  : not null Data_Ptr) return Call_Result_Code
      is
         I : int;
      begin
         I := Wl_Thin.Touch_Add_Listener (Touch    => Touch.My_Touch,
                                          Listener => Listener'Access,
                                          Data     => Data.all'Address);
         if I = 0 then
            return Success;
         else
            return Error;
         end if;
      end Subscribe;

   end Touch_Events;

   package body Output_Events is

      package Conversion is new
        System.Address_To_Access_Conversions (Data_Type);

      procedure Internal_Geometry (Data            : Void_Ptr;
                                   Output          : Wl_Thin.Output_Ptr;
                                   X               : Integer;
                                   Y               : Integer;
                                   Physical_Width  : Integer;
                                   Physical_Height : Integer;
                                   Subpixel        : Integer;
                                   Make            : chars_ptr;
                                   Model           : chars_ptr;
                                   Transform       : Integer) with
        Convention => C;

      procedure Internal_Mode (Data    : Void_Ptr;
                               Output  : Wl_Thin.Output_Ptr;
                               Flags   : Unsigned_32;
                               Width   : Integer;
                               Height  : Integer;
                               Refresh : Integer) with
        Convention => C;

      procedure Internal_Done (Data   : Void_Ptr;
                               Output : Wl_Thin.Output_Ptr) with
        Convention => C;

      procedure Internal_Scale (Data   : Void_Ptr;
                                Output : Wl_Thin.Output_Ptr;
                                Factor : Integer) with
        Convention => C;

      procedure Internal_Geometry (Data            : Void_Ptr;
                                   Output          : Wl_Thin.Output_Ptr;
                                   X               : Integer;
                                   Y               : Integer;
                                   Physical_Width  : Integer;
                                   Physical_Height : Integer;
                                   Subpixel        : Integer;
                                   Make            : chars_ptr;
                                   Model           : chars_ptr;
                                   Transform       : Integer)
      is
         O : constant Wayland_Client.Output := (My_Output => Output);
         Ma : constant String := Interfaces.C.Strings.Value (Make);
         Mo : constant String := Interfaces.C.Strings.Value (Model);
      begin
         Geometry (Data_Ptr (Conversion.To_Pointer (Data)),
                   O,
                   X,
                   Y,
                   Physical_Width,
                   Physical_Height,
                   Subpixel,
                   Ma,
                   Mo,
                   Transform);
      end Internal_Geometry;

      procedure Internal_Mode (Data    : Void_Ptr;
                               Output  : Wl_Thin.Output_Ptr;
                               Flags   : Unsigned_32;
                               Width   : Integer;
                               Height  : Integer;
                               Refresh : Integer)
      is
         O : constant Wayland_Client.Output := (My_Output => Output);
      begin
         Mode (Data_Ptr (Conversion.To_Pointer (Data)),
               O,
               Flags,
               Width,
               Height,
               Refresh);
      end Internal_Mode;

      procedure Internal_Done (Data   : Void_Ptr;
                               Output : Wl_Thin.Output_Ptr)
      is
         O : constant Wayland_Client.Output := (My_Output => Output);
      begin
         Done (Data_Ptr (Conversion.To_Pointer (Data)), O);
      end Internal_Done;

      procedure Internal_Scale (Data   : Void_Ptr;
                                Output : Wl_Thin.Output_Ptr;
                                Factor : Integer)
      is
         O : constant Wayland_Client.Output := (My_Output => Output);
      begin
         Scale (Data_Ptr (Conversion.To_Pointer (Data)), O, Factor);
      end Internal_Scale;

      Listener : aliased Wl_Thin.Output_Listener_T
        := (Geometry => Internal_Geometry'Unrestricted_Access,
            Mode     => Internal_Mode'Unrestricted_Access,
            Done     => Internal_Done'Unrestricted_Access,
            Scale    => Internal_Scale'Unrestricted_Access);

      function Subscribe
        (Output : in out Wayland_Client.Output;
         Data   : not null Data_Ptr) return Call_Result_Code
      is
         I : int;
      begin
         I := Wl_Thin.Output_Add_Listener (Output   => Output.My_Output,
                                           Listener => Listener'Access,
                                           Data     => Data.all'Address);
         if I = 0 then
            return Success;
         else
            return Error;
         end if;
      end Subscribe;

   end Output_Events;

   procedure Connect (Display : in out Wayland_Client.Display;
                      Name    : String := Default_Display_Name) is
   begin
      Display.My_Display := Wl_Thin.Display_Connect (+Name);
      if Display.My_Display /= null then
         Display.My_Fd :=
           Wl_Thin.Display_Get_File_Descriptor (Display.My_Display);
         if Display.My_Fd = -1 then
            Display.Disconnect;
            Display.My_Display := null;
         end if;
      end if;
   end Connect;

   procedure Disconnect (Display : in out Wayland_Client.Display) is
   begin
      if Display.My_Display /= null then
         Wl_Thin.Display_Disconnect (Display.My_Display);
      end if;
   end Disconnect;

   function Check_For_Events
     (Display : Wayland_Client.Display;
      Timeout : Integer) return Check_For_Events_Status
   is
      File_Descriptors : constant Linux.Poll_File_Descriptor_Array
        := (1 => (Descriptor => Display.My_Fd,
                  Events     => Linux.POLLIN,
                  Revents    => 0));
      I : constant Integer := Linux.Poll (File_Descriptors, Timeout);
   begin
      case I is
         when 1..Integer'Last   => return Events_Need_Processing;
         when 0                 => return No_Events;
         when Integer'First..-1 => return Error;
      end case;
   end Check_For_Events;

   function Get_Version
     (Display : Wayland_Client.Display) return Unsigned_32 is
   begin
      return Wl_Thin.Display_Get_Version (Display.My_Display);
   end Get_Version;

   procedure Get_Registry (Display  : Wayland_Client.Display;
                           Registry : in out Wayland_Client.Registry) is
   begin
      Registry.My_Registry
        := Wl_Thin.Display_Get_Registry (Display.My_Display);
   end Get_Registry;

   procedure Destroy (Registry : in out Wayland_Client.Registry) is
   begin
      if Registry.My_Registry /= null then
         Wl_Thin.Registry_Destroy (Registry.My_Registry);
         Registry.My_Registry := null;
      end if;
   end Destroy;

   function Dispatch (Display : Wayland_Client.Display) return Integer is
   begin
      return Integer (Wl_Thin.Display_Dispatch (Display.My_Display));
   end Dispatch;

   function Dispatch_Pending
     (Display : Wayland_Client.Display) return Integer is
   begin
      return Wl_Thin.Display_Dispatch_Pending (Display.My_Display);
   end Dispatch_Pending;

   function Prepare_Read
     (Display : Wayland_Client.Display) return Integer is
   begin
      return Wl_Thin.Display_Prepare_Read (Display.My_Display);
   end Prepare_Read;

   function Read_Events
     (Display : Wayland_Client.Display) return Call_Result_Code
   is
      I : constant Integer
        := Wl_Thin.Display_Read_Events (Display.My_Display);
   begin
      if I = 0 then
         return Success;
      else
         return Error;
      end if;
   end Read_Events;

   procedure Cancel_Read (Display : Wayland_Client.Display) is
   begin
      Wl_Thin.Display_Cancel_Read (Display.My_Display);
   end Cancel_Read;

   procedure Dispatch (Display : Wayland_Client.Display) is
      I : Integer;
      pragma Unreferenced (I);
   begin
      I := Display.Dispatch;
   end Dispatch;

   function Roundtrip (Display : Wayland_Client.Display) return Integer is
   begin
      return Integer (Wl_Thin.Display_Roundtrip (Display.My_Display));
   end Roundtrip;

   procedure Roundtrip (Display : Wayland_Client.Display) is
      I : Integer;
      pragma Unreferenced (I);
   begin
      I := Display.Roundtrip;
   end Roundtrip;

   procedure Get_Proxy (Compositor  : in out Wayland_Client.Compositor;
                        Registry    : Wayland_Client.Registry;
                        Id          : Unsigned_32;
                        Version     : Unsigned_32)
   is
      P : constant Wl_Thin.Proxy_Ptr :=
            Wl_Thin.Registry_Bind
              (Registry    => Registry.My_Registry,
               Name        => Id,
               Interface_V => Wl_Thin.Compositor_Interface'Access,
               New_Id      => Version);

   begin
      if P /= null then
         Compositor.My_Compositor := P.all'Access;
      end if;
   end Get_Proxy;

   procedure Get_Surface_Proxy (Compositor : Wayland_Client.Compositor;
                             Surface    : in out Wayland_Client.Surface) is
   begin
      Surface.My_Surface :=
        Wl_Thin.Compositor_Create_Surface (Compositor.My_Compositor);
   end Get_Surface_Proxy;

   procedure Get_Region_Proxy (Compositor : Wayland_Client.Compositor;
                               Region     : in out Wayland_Client.Region) is
   begin
      Region.My_Region :=
        Wl_Thin.Compositor_Create_Region (Compositor.My_Compositor);
   end Get_Region_Proxy;

   procedure Destroy (Compositor : in out Wayland_Client.Compositor) is
   begin
      if Compositor.My_Compositor /= null then
         Wl_Thin.Compositor_Destroy (Compositor.My_Compositor);
         Compositor.My_Compositor := null;
      end if;
   end Destroy;

   function Get_Version (Seat : Wayland_Client.Seat) return Unsigned_32 is
   begin
      return Wl_Thin.Seat_Get_Version (Seat.My_Seat);
   end Get_Version;

   procedure Get_Proxy (Seat     : in out Wayland_Client.Seat;
                        Registry : Wayland_Client.Registry;
                        Id       : Unsigned_32;
                        Version  : Unsigned_32)
   is
      P : constant Wl_Thin.Proxy_Ptr :=
        Wl_Thin.Registry_Bind (Registry    => Registry.My_Registry,
                               Name        => Id,
                               Interface_V => Wl_Thin.Seat_Interface'Access,
                               New_Id      => Version);

   begin
      if P /= null then
         Seat.My_Seat := P.all'Access;
      end if;
   end Get_Proxy;

   procedure Get_Pointer (Seat    : Wayland_Client.Seat;
                          Pointer : in out Wayland_Client.Pointer) is
   begin
      Pointer.My_Pointer := Wl_Thin.Seat_Get_Pointer (Seat.My_Seat);
   end Get_Pointer;

   procedure Get_Keyboard (Seat     : Wayland_Client.Seat;
                           Keyboard : in out Wayland_Client.Keyboard) is
   begin
      Keyboard.My_Keyboard := Wl_Thin.Seat_Get_Keyboard (Seat.My_Seat);
   end Get_Keyboard;

   procedure Get_Touch (Seat  : Wayland_Client.Seat;
                        Touch : in out Wayland_Client.Touch) is
   begin
      Touch.My_Touch := Wl_Thin.Seat_Get_Touch (Seat.My_Seat);
   end Get_Touch;

   procedure Release (Seat : in out Wayland_Client.Seat) is
   begin
      Wl_Thin.Seat_Release (Seat.My_Seat);
      Seat.My_Seat := null;
   end Release;

   procedure Get_Proxy (Shell    : in out Wayland_Client.Shell;
                        Registry : Wayland_Client.Registry;
                        Id       : Unsigned_32;
                        Version  : Unsigned_32)
   is
      P : constant Wl_Thin.Proxy_Ptr :=
        Wl_Thin.Registry_Bind (Registry    => Registry.My_Registry,
                               Name        => Id,
                               Interface_V => Wl_Thin.Shell_Interface'Access,
                               New_Id      => Version);

   begin
      if P /= null then
         Shell.My_Shell := P.all'Access;
      end if;
   end Get_Proxy;

   procedure Get_Shell_Surface (Shell         : Wayland_Client.Shell;
                                Surface       : Wayland_Client.Surface;
                                Shell_Surface : in out Wayland_Client.Shell_Surface) is
   begin
      Shell_Surface.My_Shell_Surface :=
        Wl_Thin.Shell_Get_Shell_Surface (Shell.My_Shell, Surface.My_Surface);
   end Get_Shell_Surface;

   procedure Get_Proxy (Shm      : in out Wayland_Client.Shm;
                        Registry : Wayland_Client.Registry;
                        Id       : Unsigned_32;
                        Version  : Unsigned_32)
   is
      P : constant Wl_Thin.Proxy_Ptr :=
        Wl_Thin.Registry_Bind (Registry    => Registry.My_Registry,
                               Name        => Id,
                               Interface_V => Wl_Thin.Shm_Interface'Access,
                               New_Id      => Version);

   begin
      if P /= null then
         Shm.My_Shm := P.all'Access;
      end if;
   end Get_Proxy;

   procedure Create_Pool
     (Shm             : Wayland_Client.Shm;
      File_Descriptor : C_Binding.Linux.Files.File;
      Size            : Integer;
      Pool            : in out Wayland_Client.Shm_Pool) is
   begin
      Pool.My_Shm_Pool := Wl_Thin.Shm_Create_Pool
        (Shm.My_Shm,
         Integer (File_Descriptor.My_File_Descriptor),
         Size);
   end Create_Pool;

   function Get_Version (Shm : Wayland_Client.Shm) return Unsigned_32 is
   begin
      return Wl_Thin.Shm_Get_Version (Shm.My_Shm);
   end Get_Version;

   procedure Destroy (Shm : in out Wayland_Client.Shm) is
   begin
      if Shm.My_Shm /= null then
         Wl_Thin.Shm_Destroy (Shm.My_Shm);
         Shm.My_Shm := null;
      end if;
   end Destroy;

   procedure Create_Buffer (Pool   : Wayland_Client.Shm_Pool;
                            Offset   : Integer;
                            Width    : Integer;
                            Height   : Integer;
                            Stride   : Integer;
                            Format   : Shm_Format;
                            Buffer : in out Wayland_Client.Buffer) is
   begin
      Buffer.My_Buffer := Wl_Thin.Shm_Pool_Create_Buffer (Pool.My_Shm_Pool,
                                                          Offset,
                                                          Width,
                                                          Height,
                                                          Stride,
                                                          Unsigned_32 (Format));
   end Create_Buffer;

   procedure Resize (Pool : Wayland_Client.Shm_Pool;
                     Size : Integer) is
   begin
      Wl_Thin.Shm_Pool_Resize (Pool.My_Shm_Pool, Size);
   end Resize;

   function Get_Version
     (Pool : Wayland_Client.Shm_Pool) return Unsigned_32 is
   begin
      return Wl_Thin.Shm_Pool_Get_Version (Pool.My_Shm_Pool);
   end Get_Version;

   procedure Destroy (Pool : in out Wayland_Client.Shm_Pool) is
   begin
      if Pool.My_Shm_Pool /= null then
         Wl_Thin.Shm_Pool_Destroy (Pool.My_Shm_Pool);
         Pool.My_Shm_Pool := null;
      end if;
   end Destroy;

   function Get_Version
     (Buffer : Wayland_Client.Buffer) return Unsigned_32 is
   begin
      return Wl_Thin.Buffer_Get_Version (Buffer.My_Buffer);
   end Get_Version;

   procedure Destroy (Buffer : in out Wayland_Client.Buffer) is
   begin
      if Buffer.My_Buffer /= null then
         Wl_Thin.Buffer_Destroy (Buffer.My_Buffer);
         Buffer.My_Buffer := null;
      end if;
   end Destroy;

   function Has_Proxy (Offer : Wayland_Client.Data_Offer) return Boolean is
      (Offer.My_Data_Offer /= null);

   function Get_Version
     (Offer : Wayland_Client.Data_Offer) return Unsigned_32 is
   begin
      return Wl_Thin.Data_Offer_Get_Version (Offer.My_Data_Offer);
   end Get_Version;

   procedure Destroy (Offer : in out Wayland_Client.Data_Offer) is
   begin
      if Offer.My_Data_Offer /= null then
         Wl_Thin.Data_Offer_Destroy (Offer.My_Data_Offer);
         Offer.My_Data_Offer := null;
      end if;
   end Destroy;

   procedure Do_Accept (Offer     : Data_Offer;
                        Serial    : Unsigned_32;
                        Mime_Type : String) is
   begin
      Wl_Thin.Proxy_Marshal
        (Wl_Thin.Proxy_Ptr'(Offer.My_Data_Offer.all'Access),
         WL_DATA_OFFER_ACCEPT,
         Serial,
         +Mime_Type);
   end Do_Accept;

   procedure Do_Not_Accept (Offer  : Data_Offer;
                            Serial : Unsigned_32) is
   begin
      Wl_Thin.Data_Offer_Accept (Offer.My_Data_Offer,
                                 Serial,
                                 Interfaces.C.Strings.Null_Ptr);
   end Do_Not_Accept;

   procedure Receive (Offer           : Data_Offer;
                      Mime_Type       : String;
                      File_Descriptor : Integer) is
   begin
      Wl_Thin.Data_Offer_Receive
        (Offer.My_Data_Offer, +Mime_Type, File_Descriptor);
   end Receive;

   procedure Finish (Offer : Data_Offer) is
   begin
      Wl_Thin.Data_Offer_Finish (Offer.My_Data_Offer);
   end Finish;

   procedure Set_Actions (Offer            : Data_Offer;
                          Dnd_Actions      : Unsigned_32;
                          Preferred_Action : Unsigned_32) is
   begin
      Wl_Thin.Data_Offer_Set_Actions (Offer.My_Data_Offer,
                                      Dnd_Actions,
                                      Preferred_Action);
   end Set_Actions;

   function Get_Version (Surface : Shell_Surface) return Unsigned_32 is
   begin
      return Wl_Thin.Shell_Surface_Get_Version (Surface.My_Shell_Surface);
   end Get_Version;

   procedure Destroy (Surface : in out Shell_Surface) is
   begin
      if Surface.My_Shell_Surface /= null then
         Wl_Thin.Shell_Surface_Destroy (Surface.My_Shell_Surface);
         Surface.My_Shell_Surface := null;
      end if;
   end Destroy;

   procedure Pong (Surface : Wayland_Client.Shell_Surface;
                   Serial  : Unsigned_32) is
   begin
      Wl_Thin.Shell_Surface_Pong (Surface.My_Shell_Surface, Serial);
   end Pong;

   procedure Move (Surface : Shell_Surface;
                   Seat    : Wayland_Client.Seat;
                   Serial  : Unsigned_32) is
   begin
      Wl_Thin.Shell_Surface_Move (Surface.My_Shell_Surface,
                                  Seat.My_Seat,
                                  Serial);
   end Move;

   procedure Resize (Surface : Shell_Surface;
                     Seat    : Wayland_Client.Seat;
                     Serial  : Unsigned_32;
                     Edges   : Unsigned_32) is
   begin
      Wl_Thin.Shell_Surface_Resize (Surface.My_Shell_Surface,
                                    Seat.My_Seat,
                                    Serial,
                                    Edges);
   end Resize;

   procedure Set_Toplevel (Surface : Wayland_Client.Shell_Surface) is
   begin
      Wl_Thin.Shell_Surface_Set_Toplevel (Surface.My_Shell_Surface);
   end Set_Toplevel;

   procedure Set_Transient (Surface : Shell_Surface;
                            Parent  : Wayland_Client.Surface;
                            X       : Integer;
                            Y       : Integer;
                            Flags   : Unsigned_32) is
   begin
      Wl_Thin.Shell_Surface_Set_Transient (Surface.My_Shell_Surface,
                                           Parent.My_Surface,
                                           X,
                                           Y,
                                           Flags);
   end Set_Transient;

   procedure Set_Fullscreen (Surface   : Shell_Surface;
                             Method    : Unsigned_32;
                             Framerate : Unsigned_32;
                             Output    : Wayland_Client.Output) is
   begin
      Wl_Thin.Shell_Surface_Set_Fullscreen (Surface.My_Shell_Surface,
                                            Method,
                                            Framerate,
                                            Output.My_Output);
   end Set_Fullscreen;

   procedure Set_Popup (Surface : Shell_Surface;
                        Seat    : Wayland_Client.Seat;
                        Serial  : Unsigned_32;
                        Parent  : Wayland_Client.Surface;
                        X       : Integer;
                        Y       : Integer;
                        Flags   : Unsigned_32) is
   begin
      Wl_Thin.Shell_Surface_Set_Popup (Surface.My_Shell_Surface,
                                       Seat.My_Seat,
                                       Serial,
                                       Parent.My_Surface,
                                       X,
                                       Y,
                                       Flags);
   end Set_Popup;

   procedure Set_Maximized (Surface : Shell_Surface;
                            Output  : Wayland_Client.Output) is
   begin
      Wl_Thin.Shell_Surface_Set_Maximized (Surface.My_Shell_Surface,
                                           Output.My_Output);
   end Set_Maximized;

   procedure Set_Title (Surface : Shell_Surface;
                        Title   : String) is
   begin
      Wl_Thin.Proxy_Marshal
        (Wl_Thin.Proxy_Ptr'(Surface.My_Shell_Surface.all'Access),
         WL_SHELL_SURFACE_SET_TITLE,
         +Title);
   end Set_Title;

   procedure Set_Class (Surface : Shell_Surface;
                        Class_V : String) is
   begin
      Wl_Thin.Proxy_Marshal
        (Wl_Thin.Proxy_Ptr'(Surface.My_Shell_Surface.all'Access),
         WL_SHELL_SURFACE_SET_CLASS,
         +Class_V);
   end Set_Class;

   procedure Attach (Surface : Wayland_Client.Surface;
                     Buffer  : Wayland_Client.Buffer;
                     X       : Integer;
                     Y       : Integer) is
   begin
      Wl_Thin.Surface_Attach (Surface.My_Surface, Buffer.My_Buffer, X, Y);
   end Attach;

   procedure Damage (Surface : Wayland_Client.Surface;
                     X       : Integer;
                     Y       : Integer;
                     Width   : Integer;
                     Height  : Integer) is
   begin
      Wl_Thin.Surface_Damage (Surface.My_Surface, X, Y, Width, Height);
   end Damage;

   function Frame (Surface : Wayland_Client.Surface) return Callback is
   begin
      return C : Callback do
         C.My_Callback := Wl_Thin.Surface_Frame (Surface.My_Surface);
      end return;
   end Frame;

   procedure Set_Opaque_Region (Surface : Wayland_Client.Surface;
                                Region  : Wayland_Client.Region) is
   begin
      Wl_Thin.Surface_Set_Opaque_Region (Surface.My_Surface,
                                         Region.My_Region);
   end Set_Opaque_Region;

   procedure Set_Input_Region (Surface : Wayland_Client.Surface;
                               Region  : Wayland_Client.Region) is
   begin
      Wl_Thin.Surface_Set_Input_Region (Surface.My_Surface,
                                        Region.My_Region);

   end Set_Input_Region;

   procedure Commit (Surface : Wayland_Client.Surface) is
   begin
      Wl_Thin.Surface_Commit (Surface.My_Surface);
   end Commit;

   procedure Set_Buffer_Transform (Surface   : Wayland_Client.Surface;
                                   Transform : Integer) is
   begin
      Wl_Thin.Surface_Set_Buffer_Transform (Surface.My_Surface,
                                            Transform);
   end Set_Buffer_Transform;

   procedure Set_Buffer_Scale (Surface : Wayland_Client.Surface;
                               Scale   : Integer) is
   begin
      Wl_Thin.Surface_Set_Buffer_Scale (Surface.My_Surface,
                                        Scale);
   end Set_Buffer_Scale;

   procedure Damage_Buffer (Surface : Wayland_Client.Surface;
                            X       : Integer;
                            Y       : Integer;
                            Width   : Integer;
                            Height  : Integer) is
   begin
      Wl_Thin.Surface_Damage_Buffer
        (Surface.My_Surface, X, Y, Width, Height);
   end Damage_Buffer;

   procedure Destroy (Surface : in out Wayland_Client.Surface) is
   begin
      if Surface.My_Surface /= null then
         Wl_Thin.Surface_Destroy (Surface.My_Surface);
         Surface.My_Surface := null;
      end if;
   end Destroy;

   function Sync (Display : Wayland_Client.Display) return Callback is
   begin
      return Callback : Wayland_Client.Callback do
         Callback.My_Callback := Wl_Thin.Display_Sync (Display.My_Display);
      end return;
   end Sync;

   function Get_Version
     (Registry : Wayland_Client.Registry) return Unsigned_32 is
   begin
      return Wl_Thin.Registry_Get_Version (Registry.My_Registry);
   end Get_Version;

   function Has_Proxy (Callback : Wayland_Client.Callback) return Boolean is
      (Callback.My_Callback /= null);

   procedure Destroy (Callback : in out Wayland_Client.Callback) is
   begin
      if Callback.My_Callback /= null then
         Wl_Thin.Callback_Destroy (Callback.My_Callback);
         Callback.My_Callback := null;
      end if;
   end Destroy;

   function Get_Version
     (Callback : Wayland_Client.Callback) return Unsigned_32 is
   begin
      return Wl_Thin.Callback_Get_Version (Callback.My_Callback);
   end Get_Version;

   function Get_Version
     (Pointer : Wayland_Client.Pointer) return Unsigned_32 is
   begin
      return Wl_Thin.Pointer_Get_Version (Pointer.My_Pointer);
   end Get_Version;

   procedure Destroy (Pointer : in out Wayland_Client.Pointer) is
   begin
      if Pointer.My_Pointer /= null then
         Wl_Thin.Pointer_Destroy (Pointer.My_Pointer);
         Pointer.My_Pointer := null;
      end if;
   end Destroy;

   procedure Set_Cursor (Pointer   : Wayland_Client.Pointer;
                         Serial    : Unsigned_32;
                         Surface   : Wayland_Client.Surface;
                         Hotspot_X : Integer;
                         Hotspot_Y : Integer) is
   begin
      Wl_Thin.Pointer_Set_Cursor (Pointer.My_Pointer,
                                  Serial,
                                  Surface.My_Surface,
                                  Hotspot_X,
                                  Hotspot_Y);
   end Set_Cursor;

   procedure Release (Pointer : in out Wayland_Client.Pointer) is
   begin
      if Pointer.My_Pointer /= null then
         Wl_Thin.Pointer_Release (Pointer.My_Pointer);
         Pointer.My_Pointer := null;
      end if;
   end Release;

   function Get_Version
     (Keyboard : Wayland_Client.Keyboard) return Unsigned_32 is
   begin
      return Wl_Thin.Keyboard_Get_Version (Keyboard.My_Keyboard);
   end Get_Version;

   procedure Destroy (Keyboard : in out Wayland_Client.Keyboard) is
   begin
      if Keyboard.My_Keyboard /= null then
         Wl_Thin.Keyboard_Destroy (Keyboard.My_Keyboard);
         Keyboard.My_Keyboard := null;
      end if;
   end Destroy;

   procedure Release (Keyboard : in out Wayland_Client.Keyboard) is
   begin
      if Keyboard.My_Keyboard /= null then
         Wl_Thin.Keyboard_Release (Keyboard.My_Keyboard);
         Keyboard.My_Keyboard := null;
      end if;
   end Release;

   function Get_Version (Touch : Wayland_Client.Touch) return Unsigned_32 is
   begin
      return Wl_Thin.Touch_Get_Version (Touch.My_Touch);
   end Get_Version;

   procedure Destroy (Touch : in out Wayland_Client.Touch) is
   begin
      if Touch.My_Touch /= null then
         Wl_Thin.Touch_Destroy (Touch.My_Touch);
         Touch.My_Touch := null;
      end if;
   end Destroy;

   procedure Release (Touch : in out Wayland_Client.Touch) is
   begin
      if Touch.My_Touch /= null then
         Wl_Thin.Touch_Release (Touch.My_Touch);
         Touch.My_Touch := null;
      end if;
   end Release;

   function Get_Version
     (Output : Wayland_Client.Output) return Unsigned_32 is
   begin
      return Wl_Thin.Output_Get_Version (Output.My_Output);
   end Get_Version;

   procedure Destroy (Output : in out Wayland_Client.Output) is
   begin
      if Output.My_Output /= null then
         Wl_Thin.Output_Destroy (Output.My_Output);
         Output.My_Output := null;
      end if;
   end Destroy;

   procedure Release (Output : in out Wayland_Client.Output) is
   begin
      if Output.My_Output /= null then
         Wl_Thin.Output_Release (Output.My_Output);
         Output.My_Output := null;
      end if;
   end Release;

   function Get_Version
     (Region : Wayland_Client.Region) return Unsigned_32 is
   begin
      return Wl_Thin.Region_Get_Version (Region.My_Region);
   end Get_Version;

   procedure Destroy (Region : in out Wayland_Client.Region) is
   begin
      if Region.My_Region /= null then
         Wl_Thin.Region_Destroy (Region.My_Region);
         Region.My_Region := null;
      end if;
   end Destroy;

   procedure Add (Region : Wayland_Client.Region;
                  X      : Integer;
                  Y      : Integer;
                  Width  : Integer;
                  Height : Integer) is
   begin
      Wl_Thin.Region_Add (Region.My_Region, X, Y, Width, Height);
   end Add;

   procedure Subtract (Region : Wayland_Client.Region;
                       X      : Integer;
                       Y      : Integer;
                       Width  : Integer;
                       Height : Integer) is
   begin
      Wl_Thin.Region_Subtract (Region.My_Region, X, Y, Width, Height);
   end Subtract;

   function Get_Version
     (S : Wayland_Client.Subcompositor) return Unsigned_32 is
   begin
      return Wl_Thin.Subcompositor_Get_Version (S.My_Subcompositor);
   end Get_Version;

   procedure Destroy (S : in out Wayland_Client.Subcompositor) is
   begin
      if S.My_Subcompositor /= null then
         Wl_Thin.Subcompositor_Destroy (S.My_Subcompositor);
         S.My_Subcompositor := null;
      end if;
   end Destroy;

   procedure Get_Subsurface
     (Subcompositor : Wayland_Client.Subcompositor;
      Surface       : Wayland_Client.Surface;
      Parent        : Wayland_Client.Surface;
      Subsurface    : in out Wayland_Client.Subsurface) is
   begin
      Subsurface.My_Subsurface :=
        Wl_Thin.Subcompositor_Get_Subsurface
          (Subcompositor.My_Subcompositor,
           Surface.My_Surface,
           Parent.My_Surface);
   end Get_Subsurface;

   function Get_Version
     (Subsurface : Wayland_Client.Subsurface) return Unsigned_32 is
   begin
      return Wl_Thin.Subsurface_Get_Version (Subsurface.My_Subsurface);
   end Get_Version;

   procedure Destroy (Subsurface : in out Wayland_Client.Subsurface) is
   begin
      if Subsurface.My_Subsurface /= null then
         Wl_Thin.Subsurface_Destroy (Subsurface.My_Subsurface);
         Subsurface.My_Subsurface := null;
      end if;
   end Destroy;

   procedure Set_Position (Subsurface : Wayland_Client.Subsurface;
                           X          : Integer;
                           Y          : Integer) is
   begin
      Wl_Thin.Subsurface_Set_Position (Subsurface.My_Subsurface,
                                       X,
                                       Y);
   end Set_Position;

   procedure Place_Above (Subsurface : Wayland_Client.Subsurface;
                          Sibling    : Wayland_Client.Surface) is
   begin
      Wl_Thin.Subsurface_Place_Above (Subsurface.My_Subsurface,
                                      Sibling.My_Surface);
   end Place_Above;

   procedure Place_Below (Subsurface : Wayland_Client.Subsurface;
                          Sibling    : Wayland_Client.Surface) is
   begin
      Wl_Thin.Subsurface_Place_Below (Subsurface.My_Subsurface,
                                      Sibling.My_Surface);
   end Place_Below;

   procedure Set_Sync (Subsurface : Wayland_Client.Subsurface) is
   begin
      Wl_Thin.Subsurface_Set_Sync (Subsurface.My_Subsurface);
   end Set_Sync;

   procedure Set_Desync (Subsurface : Wayland_Client.Subsurface) is
   begin
      Wl_Thin.Subsurface_Set_Desync (Subsurface.My_Subsurface);
   end Set_Desync;

   function Get_Version (Source : Data_Source) return Unsigned_32 is
   begin
      return Wl_Thin.Data_Source_Get_Version (Source.My_Data_Source);
   end Get_Version;

   procedure Destroy (Source : in out Data_Source) is
   begin
      if Source.My_Data_Source /= null then
         Wl_Thin.Data_Source_Destroy (Source.My_Data_Source);
         Source.My_Data_Source := null;
      end if;
   end Destroy;

   procedure Offer (Source    : Data_Source;
                    Mime_Type : String) is
   begin
      Wl_Thin.Proxy_Marshal
        (Wl_Thin.Proxy_Ptr'(Source.My_Data_Source.all'Access),
         WL_DATA_SOURCE_OFFER,
         +Mime_Type);
   end Offer;

   procedure Set_Actions (Source      : Data_Source;
                          Dnd_Actions : Unsigned_32) is
   begin
      Wl_Thin.Data_Source_Set_Actions (Source.My_Data_Source,
                                       Dnd_Actions);
   end Set_Actions;

   function Get_Version (Device : Data_Device) return Unsigned_32 is
   begin
      return Wl_Thin.Data_Device_Get_Version (Device.My_Data_Device);
   end Get_Version;

   procedure Destroy (Device : in out Data_Device) is
   begin
      if Device.My_Data_Device /= null then
         Wl_Thin.Data_Device_Destroy (Device.My_Data_Device);
         Device.My_Data_Device := null;
      end if;
   end Destroy;

   procedure Start_Drag (Device : Data_Device;
                         Source : Data_Source;
                         Origin : Surface;
                         Icon   : Surface;
                         Serial : Unsigned_32) is
   begin
      Wl_Thin.Data_Device_Start_Drag (Device.My_Data_Device,
                                      Source.My_Data_Source,
                                      Origin.My_Surface,
                                      Icon.My_Surface,
                                      Serial);
   end Start_Drag;

   procedure Set_Selection (Device : Data_Device;
                            Source : Data_Source;
                            Serial : Unsigned_32) is
   begin
      Wl_Thin.Data_Device_Set_Selection (Device.My_Data_Device,
                                         Source.My_Data_Source,
                                         Serial);
   end Set_Selection;

   procedure Release (Device : in out Data_Device) is
   begin
      if Device.My_Data_Device /= null then
         Wl_Thin.Data_Device_Release (Device.My_Data_Device);
         Device.My_Data_Device := null;
      end if;
   end Release;

   function Get_Version (Manager : Data_Device_Manager) return Unsigned_32 is
   begin
      return Wl_Thin.Data_Device_Manager_Get_Version
        (Manager.My_Data_Device_Manager);
   end Get_Version;

   procedure Destroy (Manager : in out Data_Device_Manager) is
   begin
      if Manager.My_Data_Device_Manager /= null then
         Wl_Thin.Data_Device_Manager_Destroy (Manager.My_Data_Device_Manager);
         Manager.My_Data_Device_Manager := null;
      end if;
   end Destroy;

   procedure Create_Data_Source (Manager : Data_Device_Manager;
                                 Source  : in out Data_Source) is
   begin
      Source.My_Data_Source := Wl_Thin.Data_Device_Manager_Create_Data_Source
        (Manager.My_Data_Device_Manager);
   end Create_Data_Source;

   procedure Get_Data_Device (Manager : Data_Device_Manager;
                              Seat    : Wayland_Client.Seat;
                              Device  : in out Data_Device) is
   begin
      Device.My_Data_Device := Wl_Thin.Data_Device_Manager_Get_Data_Device
        (Manager.My_Data_Device_Manager, Seat.My_Seat);
   end Get_Data_Device;

end C_Binding.Linux.Wayland_Client;
