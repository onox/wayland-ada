with Interfaces.C.Strings;
with System;

-- Auto generated from Wayland.xml except for the Wayland Core parts
package Wl_Thin is

-- Begin core parts

   pragma Linker_Options ("-lwayland-client");
   -- Added this linker option here to avoid added it to each gpr file that with's
   -- this Ada binding to Wayland. If the wayland client lib changes its name it
   -- means there is only one place one needs to update.

   subtype Void_Ptr is System.Address;

   Default_Display_Name : aliased Interfaces.C.char_array := Interfaces.C.To_C ("wayland-0");

   type Proxy_T is limited private;

   type Proxy_Ptr is access all Proxy_T;

   type Display_Ptr;

   function Display_Connect (Name : Interfaces.C.Strings.char_array_access) return Display_Ptr;

   procedure Display_Disconnect (This : in out Display_Ptr);

   type Interface_T is limited record
      Name         : Interfaces.C.Strings.chars_ptr;
      Version      : Interfaces.C.int;
      Method_Count : Interfaces.C.int;
      Methods      : Void_Ptr; -- Can be improved upon.
      Event_Count  : Interfaces.C.int;
      Events       : Void_Ptr; -- Can be improved upon.
   end record with
      Convention => C_Pass_By_Copy;

   type Interface_Ptr is access all Interface_T;

   function Proxy_Marshal_Constructor
     (Proxy       : Proxy_Ptr;
      Opcode      : Interfaces.Unsigned_32;
      Interface_V : Interface_Ptr;
      New_Id      : Interfaces.Unsigned_32) return Proxy_Ptr with
      Convention    => C,
      Import        => True,
      External_Name => "wl_proxy_marshal_constructor";

   function Proxy_Marshal_Constructor
     (Proxy       : Proxy_Ptr;
      Opcode      : Interfaces.Unsigned_32;
      Interface_V : Interface_Ptr;
      New_Id      : Interfaces.Unsigned_32;
      Offset      : Integer;
      Width       : Integer;
      Height      : Integer;
      Stride      : Integer;
      Format      : Interfaces.Unsigned_32) return Proxy_Ptr with
      Convention    => C,
      Import        => True,
      External_Name => "wl_proxy_marshal_constructor";

   function Proxy_Marshal_Constructor_Versioned
     (Proxy          : Proxy_Ptr;
      Opcode         : Interfaces.Unsigned_32;
      Interface_V    : Interface_Ptr;
      New_Id_1       : Interfaces.Unsigned_32;
      Name           : Interfaces.Unsigned_32;
      Interface_Name : Interfaces.C.Strings.chars_ptr;
      New_Id_2       : Interfaces.Unsigned_32;
      Version        : Interfaces.Unsigned_32) return Proxy_Ptr with
      Convention    => C,
      Import        => True,
      External_Name => "wl_proxy_marshal_constructor_versioned";

   procedure Proxy_Marshal (Proxy : Proxy_Ptr; Opcode : Interfaces.Unsigned_32) with
      Convention    => C,
      Import        => True,
      External_Name => "wl_proxy_marshal";

   function Display_Dispatch (Display : Display_Ptr) return Interfaces.C.int with
      Import        => True,
      Convention    => C,
      External_Name => "wl_display_dispatch";

   function Display_Roundtrip (Display : Display_Ptr) return Interfaces.C.int with
      Import        => True,
      Convention    => C,
      External_Name => "wl_display_roundtrip";

   function Proxy_Marshal_Constructor
     (Proxy       : Proxy_Ptr;
      Opcode      : Interfaces.Unsigned_32;
      Interface_V : Interface_Ptr;
      New_Id      : Interfaces.Unsigned_32;
      Offset      : Void_Ptr) return Proxy_Ptr with
      Convention    => C,
      Import        => True,
      External_Name => "wl_proxy_marshal_constructor";

   procedure Proxy_Marshal (Proxy : Proxy_Ptr; Opcode : Interfaces.Unsigned_32; Arg_1 : Integer) with
      Convention    => C,
      Import        => True,
      External_Name => "wl_proxy_marshal";

   procedure Proxy_Marshal (Proxy : Proxy_Ptr; Opcode : Interfaces.Unsigned_32; Arg_1 : Interfaces.Unsigned_32) with
      Convention    => C,
      Import        => True,
      External_Name => "wl_proxy_marshal";

   procedure Proxy_Marshal
     (Proxy  : Proxy_Ptr;
      Opcode : Interfaces.Unsigned_32;
      Arg_1  : Interfaces.Unsigned_32;
      Arg_2  : Interfaces.C.Strings.chars_ptr) with
      Convention    => C,
      Import        => True,
      External_Name => "wl_proxy_marshal";

   procedure Proxy_Marshal
     (Proxy  : Proxy_Ptr;
      Opcode : Interfaces.Unsigned_32;
      Arg_1  : Interfaces.Unsigned_32;
      Arg_2  : Interfaces.Unsigned_32) with
      Convention    => C,
      Import        => True,
      External_Name => "wl_proxy_marshal";

   procedure Proxy_Marshal
     (Proxy  : Proxy_Ptr;
      Opcode : Interfaces.Unsigned_32;
      Arg_1  : Interfaces.C.Strings.chars_ptr) with
      Convention    => C,
      Import        => True,
      External_Name => "wl_proxy_marshal";

   procedure Proxy_Marshal
     (Proxy  : Proxy_Ptr;
      Opcode : Interfaces.Unsigned_32;
      Arg_1  : Interfaces.C.Strings.chars_ptr;
      Arg_2  : Integer) with
      Convention    => C,
      Import        => True,
      External_Name => "wl_proxy_marshal";

   procedure Proxy_Marshal (Proxy : Proxy_Ptr; Opcode : Interfaces.Unsigned_32; Arg_1 : Integer; Arg_2 : Integer) with
      Convention    => C,
      Import        => True,
      External_Name => "wl_proxy_marshal";

   procedure Proxy_Marshal (Proxy : Proxy_Ptr; Opcode : Interfaces.Unsigned_32; Arg_1 : Void_Ptr) with
      Convention    => C,
      Import        => True,
      External_Name => "wl_proxy_marshal";

   procedure Proxy_Marshal
     (Proxy  : Proxy_Ptr;
      Opcode : Interfaces.Unsigned_32;
      Arg_1  : Void_Ptr;
      Arg_4  : Interfaces.Unsigned_32) with
      Convention    => C,
      Import        => True,
      External_Name => "wl_proxy_marshal";

   procedure Proxy_Marshal
     (Proxy  : Proxy_Ptr;
      Opcode : Interfaces.Unsigned_32;
      Arg_1  : Void_Ptr;
      Arg_2  : Interfaces.Unsigned_32;
      Arg_3  : Interfaces.Unsigned_32) with
      Convention    => C,
      Import        => True,
      External_Name => "wl_proxy_marshal";

   procedure Proxy_Marshal
     (Proxy  : Proxy_Ptr;
      Opcode : Interfaces.Unsigned_32;
      Arg_1  : Void_Ptr;
      Arg_2  : Integer;
      Arg_3  : Integer) with
      Convention    => C,
      Import        => True,
      External_Name => "wl_proxy_marshal";

   procedure Proxy_Marshal
     (Proxy  : Proxy_Ptr;
      Opcode : Interfaces.Unsigned_32;
      Arg_1  : Interfaces.Unsigned_32;
      Arg_2  : Interfaces.Unsigned_32;
      Arg_3  : Void_Ptr) with
      Convention    => C,
      Import        => True,
      External_Name => "wl_proxy_marshal";

   procedure Proxy_Marshal
     (Proxy  : Proxy_Ptr;
      Opcode : Interfaces.Unsigned_32;
      Arg_1  : Integer;
      Arg_2  : Integer;
      Arg_3  : Integer;
      Arg_4  : Integer) with
      Convention    => C,
      Import        => True,
      External_Name => "wl_proxy_marshal";

   procedure Proxy_Marshal
     (Proxy  : Proxy_Ptr;
      Opcode : Interfaces.Unsigned_32;
      Arg_1  : Void_Ptr;
      Arg_2  : Integer;
      Arg_3  : Integer;
      Arg_4  : Interfaces.Unsigned_32) with
      Convention    => C,
      Import        => True,
      External_Name => "wl_proxy_marshal";

   procedure Proxy_Marshal
     (Proxy  : Proxy_Ptr;
      Opcode : Interfaces.Unsigned_32;
      Arg_1  : Void_Ptr;
      Arg_2  : Void_Ptr;
      Arg_3  : Void_Ptr;
      Arg_4  : Interfaces.Unsigned_32) with
      Convention    => C,
      Import        => True,
      External_Name => "wl_proxy_marshal";

   procedure Proxy_Marshal
     (Proxy  : Proxy_Ptr;
      Opcode : Interfaces.Unsigned_32;
      Arg_1  : Interfaces.Unsigned_32;
      Arg_2  : Void_Ptr;
      Arg_3  : Integer;
      Arg_4  : Integer) with
      Convention    => C,
      Import        => True,
      External_Name => "wl_proxy_marshal";

   procedure Proxy_Marshal
     (Proxy  : Proxy_Ptr;
      Opcode : Interfaces.Unsigned_32;
      Arg_1  : Void_Ptr;
      Arg_2  : Interfaces.Unsigned_32;
      Arg_3  : Void_Ptr;
      Arg_4  : Integer;
      Arg_5  : Integer;
      Arg_6  : Interfaces.Unsigned_32) with
      Convention    => C,
      Import        => True,
      External_Name => "wl_proxy_marshal";

   function Proxy_Marshal_Constructor
     (Proxy       : Proxy_Ptr;
      Opcode      : Interfaces.Unsigned_32;
      Interface_V : Interface_Ptr;
      New_Id      : Interfaces.Unsigned_32;
      Arg_1       : Integer;
      Arg_2       : Integer) return Proxy_Ptr with
      Convention    => C,
      Import        => True,
      External_Name => "wl_proxy_marshal_constructor";

   function Proxy_Marshal_Constructor
     (Proxy       : Proxy_Ptr;
      Opcode      : Interfaces.Unsigned_32;
      Interface_V : Interface_Ptr;
      New_Id      : Interfaces.Unsigned_32;
      Arg_1       : Void_Ptr;
      Arg_2       : Void_Ptr) return Proxy_Ptr with
      Convention    => C,
      Import        => True,
      External_Name => "wl_proxy_marshal_constructor";

      -- End core parts

   type Fixed_T is new Interfaces.Integer_32;

   type Wayland_Array_T is record
      Size  : Interfaces.Unsigned_32;
      Alloc : Interfaces.Unsigned_32;
      Data  : Void_Ptr;
   end record with
      Convention => C_Pass_By_Copy;

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

   Display_Interface : aliased Interface_T with
      Import        => True,
      Convention    => C,
      External_Name => "wl_display_interface";

   Registry_Interface : aliased Interface_T with
      Import        => True,
      Convention    => C,
      External_Name => "wl_registry_interface";

   Callback_Interface : aliased Interface_T with
      Import        => True,
      Convention    => C,
      External_Name => "wl_callback_interface";

   Compositor_Interface : aliased Interface_T with
      Import        => True,
      Convention    => C,
      External_Name => "wl_compositor_interface";

   Shm_Pool_Interface : aliased Interface_T with
      Import        => True,
      Convention    => C,
      External_Name => "wl_shm_pool_interface";

   Shm_Interface : aliased Interface_T with
      Import        => True,
      Convention    => C,
      External_Name => "wl_shm_interface";

   Buffer_Interface : aliased Interface_T with
      Import        => True,
      Convention    => C,
      External_Name => "wl_buffer_interface";

   Data_Offer_Interface : aliased Interface_T with
      Import        => True,
      Convention    => C,
      External_Name => "wl_data_offer_interface";

   Data_Source_Interface : aliased Interface_T with
      Import        => True,
      Convention    => C,
      External_Name => "wl_data_source_interface";

   Data_Device_Interface : aliased Interface_T with
      Import        => True,
      Convention    => C,
      External_Name => "wl_data_device_interface";

   Data_Device_Manager_Interface : aliased Interface_T with
      Import        => True,
      Convention    => C,
      External_Name => "wl_data_device_manager_interface";

   Shell_Interface : aliased Interface_T with
      Import        => True,
      Convention    => C,
      External_Name => "wl_shell_interface";

   Shell_Surface_Interface : aliased Interface_T with
      Import        => True,
      Convention    => C,
      External_Name => "wl_shell_surface_interface";

   Surface_Interface : aliased Interface_T with
      Import        => True,
      Convention    => C,
      External_Name => "wl_surface_interface";

   Seat_Interface : aliased Interface_T with
      Import        => True,
      Convention    => C,
      External_Name => "wl_seat_interface";

   Pointer_Interface : aliased Interface_T with
      Import        => True,
      Convention    => C,
      External_Name => "wl_pointer_interface";

   Keyboard_Interface : aliased Interface_T with
      Import        => True,
      Convention    => C,
      External_Name => "wl_keyboard_interface";

   Touch_Interface : aliased Interface_T with
      Import        => True,
      Convention    => C,
      External_Name => "wl_touch_interface";

   Output_Interface : aliased Interface_T with
      Import        => True,
      Convention    => C,
      External_Name => "wl_output_interface";

   Region_Interface : aliased Interface_T with
      Import        => True,
      Convention    => C,
      External_Name => "wl_region_interface";

   Subcompositor_Interface : aliased Interface_T with
      Import        => True,
      Convention    => C,
      External_Name => "wl_subcompositor_interface";

   Subsurface_Interface : aliased Interface_T with
      Import        => True,
      Convention    => C,
      External_Name => "wl_subsurface_interface";

   type Display_Ptr is new Proxy_Ptr;

   type Registry_Ptr is new Proxy_Ptr;

   type Callback_Ptr is new Proxy_Ptr;

   type Compositor_Ptr is new Proxy_Ptr;

   type Shm_Pool_Ptr is new Proxy_Ptr;

   type Shm_Ptr is new Proxy_Ptr;

   type Buffer_Ptr is new Proxy_Ptr;

   type Data_Offer_Ptr is new Proxy_Ptr;

   type Data_Source_Ptr is new Proxy_Ptr;

   type Data_Device_Ptr is new Proxy_Ptr;

   type Data_Device_Manager_Ptr is new Proxy_Ptr;

   type Shell_Ptr is new Proxy_Ptr;

   type Shell_Surface_Ptr is new Proxy_Ptr;

   type Surface_Ptr is new Proxy_Ptr;

   type Seat_Ptr is new Proxy_Ptr;

   type Pointer_Ptr is new Proxy_Ptr;

   type Keyboard_Ptr is new Proxy_Ptr;

   type Touch_Ptr is new Proxy_Ptr;

   type Output_Ptr is new Proxy_Ptr;

   type Region_Ptr is new Proxy_Ptr;

   type Subcompositor_Ptr is new Proxy_Ptr;

   type Subsurface_Ptr is new Proxy_Ptr;

   type Display_Error_T is new Interfaces.Unsigned_32;
-- server couldn't find object
   Display_Invalid_Object_Invalid_Object : constant Display_Error_T := 0;

-- method doesn't exist on the specified interface
   Display_Invalid_Method_Invalid_Method : constant Display_Error_T := 1;

-- server is out of memory
   Display_No_Memory_No_Memory : constant Display_Error_T := 2;

   type Display_Error_Subprogram_Ptr is access procedure
     (Data      : Void_Ptr;
      Display   : Display_Ptr;
      Object_Id : Void_Ptr;
      Code      : Interfaces.Unsigned_32;
      Message   : Interfaces.C.Strings.chars_ptr) with
      Convention => C;

   type Display_Delete_Id_Subprogram_Ptr is access procedure
     (Data    : Void_Ptr;
      Display : Display_Ptr;
      Id      : Interfaces.Unsigned_32) with
      Convention => C;

   type Display_Listener_T is record
      Error     : Display_Error_Subprogram_Ptr;
      Delete_Id : Display_Delete_Id_Subprogram_Ptr;
   end record with
      Convention => C_Pass_By_Copy;

   type Display_Listener_Ptr is access all Display_Listener_T;

   function Display_Add_Listener
     (Display  : Display_Ptr;
      Listener : Display_Listener_Ptr;
      Data     : Void_Ptr) return Interfaces.C.int;

   procedure Display_Set_User_Data (Display : Display_Ptr; Data : Void_Ptr);

   function Display_Get_User_Data (Display : Display_Ptr) return Void_Ptr;

   function Display_Get_Version (Display : Display_Ptr) return Interfaces.Unsigned_32;

   procedure Display_Destroy (Display : Display_Ptr);

-- The sync request asks the server to emit the 'done' event
-- on the returned wl_callback object.  Since requests are
-- handled in-order and events are delivered in-order, this can
-- be used as a barrier to ensure all previous requests and the
-- resulting events have been handled.
--
-- The object returned by this request will be destroyed by the
-- compositor after the callback is fired and as such the client must not
-- attempt to use it after that point.
--
-- The callback_data passed in the callback is the event serial.
   function Display_Sync (Display : Display_Ptr) return Callback_Ptr;

-- This request creates a registry object that allows the client
-- to list and bind the global objects available from the
-- compositor.
--
-- It should be noted that the server side resources consumed in
-- response to a get_registry request can only be released when the
-- client disconnects, not when the client side proxy is destroyed.
-- Therefore, clients should invoke get_registry as infrequently as
-- possible to avoid wasting memory.
   function Display_Get_Registry (Display : Display_Ptr) return Registry_Ptr;

   type Registry_Global_Subprogram_Ptr is access procedure
     (Data        : Void_Ptr;
      Registry    : Registry_Ptr;
      Name        : Interfaces.Unsigned_32;
      Interface_V : Interfaces.C.Strings.chars_ptr;
      Version     : Interfaces.Unsigned_32) with
      Convention => C;

   type Registry_Global_Remove_Subprogram_Ptr is access procedure
     (Data     : Void_Ptr;
      Registry : Registry_Ptr;
      Name     : Interfaces.Unsigned_32) with
      Convention => C;

   type Registry_Listener_T is record
      Global        : Registry_Global_Subprogram_Ptr;
      Global_Remove : Registry_Global_Remove_Subprogram_Ptr;
   end record with
      Convention => C_Pass_By_Copy;

   type Registry_Listener_Ptr is access all Registry_Listener_T;

   function Registry_Add_Listener
     (Registry : Registry_Ptr;
      Listener : Registry_Listener_Ptr;
      Data     : Void_Ptr) return Interfaces.C.int;

   procedure Registry_Set_User_Data (Registry : Registry_Ptr; Data : Void_Ptr);

   function Registry_Get_User_Data (Registry : Registry_Ptr) return Void_Ptr;

   function Registry_Get_Version (Registry : Registry_Ptr) return Interfaces.Unsigned_32;

   procedure Registry_Destroy (Registry : Registry_Ptr);

-- Binds a new, client-created object to the server using the
-- specified name as the identifier.
   function Registry_Bind
     (Registry    : Registry_Ptr;
      Name        : Interfaces.Unsigned_32;
      Interface_V : Interface_Ptr;
      New_Id      : Interfaces.Unsigned_32) return Proxy_Ptr;

   type Callback_Done_Subprogram_Ptr is access procedure
     (Data          : Void_Ptr;
      Callback      : Callback_Ptr;
      Callback_Data : Interfaces.Unsigned_32) with
      Convention => C;

   type Callback_Listener_T is record
      Done : Callback_Done_Subprogram_Ptr;
   end record with
      Convention => C_Pass_By_Copy;

   type Callback_Listener_Ptr is access all Callback_Listener_T;

   function Callback_Add_Listener
     (Callback : Callback_Ptr;
      Listener : Callback_Listener_Ptr;
      Data     : Void_Ptr) return Interfaces.C.int;

   procedure Callback_Set_User_Data (Callback : Callback_Ptr; Data : Void_Ptr);

   function Callback_Get_User_Data (Callback : Callback_Ptr) return Void_Ptr;

   function Callback_Get_Version (Callback : Callback_Ptr) return Interfaces.Unsigned_32;

   procedure Callback_Destroy (Callback : Callback_Ptr);

   procedure Compositor_Set_User_Data (Compositor : Compositor_Ptr; Data : Void_Ptr);

   function Compositor_Get_User_Data (Compositor : Compositor_Ptr) return Void_Ptr;

   function Compositor_Get_Version (Compositor : Compositor_Ptr) return Interfaces.Unsigned_32;

   procedure Compositor_Destroy (Compositor : Compositor_Ptr);

-- Ask the compositor to create a new surface.
   function Compositor_Create_Surface (Compositor : Compositor_Ptr) return Surface_Ptr;

-- Ask the compositor to create a new region.
   function Compositor_Create_Region (Compositor : Compositor_Ptr) return Region_Ptr;

   procedure Shm_Pool_Set_User_Data (Shm_Pool : Shm_Pool_Ptr; Data : Void_Ptr);

   function Shm_Pool_Get_User_Data (Shm_Pool : Shm_Pool_Ptr) return Void_Ptr;

   function Shm_Pool_Get_Version (Shm_Pool : Shm_Pool_Ptr) return Interfaces.Unsigned_32;

   procedure Shm_Pool_Destroy (Shm_Pool : Shm_Pool_Ptr);

-- Create a wl_buffer object from the pool.
--
-- The buffer is created offset bytes into the pool and has
-- width and height as specified.  The stride argument specifies
-- the number of bytes from the beginning of one row to the beginning
-- of the next.  The format is the pixel format of the buffer and
-- must be one of those advertised through the wl_shm.format event.
--
-- A buffer will keep a reference to the pool it was created from
-- so it is valid to destroy the pool immediately after creating
-- a buffer from it.
   function Shm_Pool_Create_Buffer
     (Shm_Pool : Shm_Pool_Ptr;
      Offset   : Integer;
      Width    : Integer;
      Height   : Integer;
      Stride   : Integer;
      Format   : Interfaces.Unsigned_32) return Buffer_Ptr;

   procedure Shm_Pool_Resize (Shm_Pool : Shm_Pool_Ptr; Size : Integer);

   type Shm_Error_T is new Interfaces.Unsigned_32;
-- buffer format is not known
   Shm_Invalid_Format_Invalid_Format : constant Shm_Error_T := 0;

-- invalid size or stride during pool or buffer creation
   Shm_Invalid_Stride_Invalid_Stride : constant Shm_Error_T := 1;

-- mmapping the file descriptor failed
   Shm_Invalid_Fd_Invalid_Fd : constant Shm_Error_T := 2;

   type Shm_Format_T is new Interfaces.Unsigned_32;
-- 32-bit ARGB format, [31:0] A:R:G:B 8:8:8:8 little endian
   Shm_Argb_8888_Argb_8888 : constant Shm_Format_T := 0;

-- 32-bit RGB format, [31:0] x:R:G:B 8:8:8:8 little endian
   Shm_Xrgb_8888_Xrgb_8888 : constant Shm_Format_T := 1;

-- 8-bit color index format, [7:0] C
   Shm_C_8_C_8 : constant Shm_Format_T := 538982467;

-- 8-bit RGB format, [7:0] R:G:B 3:3:2
   Shm_Rgb_332_Rgb_332 : constant Shm_Format_T := 943867730;

-- 8-bit BGR format, [7:0] B:G:R 2:3:3
   Shm_Bgr_233_Bgr_233 : constant Shm_Format_T := 944916290;

-- 16-bit xRGB format, [15:0] x:R:G:B 4:4:4:4 little endian
   Shm_Xrgb_4444_Xrgb_4444 : constant Shm_Format_T := 842093144;

-- 16-bit xBGR format, [15:0] x:B:G:R 4:4:4:4 little endian
   Shm_Xbgr_4444_Xbgr_4444 : constant Shm_Format_T := 842089048;

-- 16-bit RGBx format, [15:0] R:G:B:x 4:4:4:4 little endian
   Shm_Rgbx_4444_Rgbx_4444 : constant Shm_Format_T := 842094674;

-- 16-bit BGRx format, [15:0] B:G:R:x 4:4:4:4 little endian
   Shm_Bgrx_4444_Bgrx_4444 : constant Shm_Format_T := 842094658;

-- 16-bit ARGB format, [15:0] A:R:G:B 4:4:4:4 little endian
   Shm_Argb_4444_Argb_4444 : constant Shm_Format_T := 842093121;

-- 16-bit ABGR format, [15:0] A:B:G:R 4:4:4:4 little endian
   Shm_Abgr_4444_Abgr_4444 : constant Shm_Format_T := 842089025;

-- 16-bit RBGA format, [15:0] R:G:B:A 4:4:4:4 little endian
   Shm_Rgba_4444_Rgba_4444 : constant Shm_Format_T := 842088786;

-- 16-bit BGRA format, [15:0] B:G:R:A 4:4:4:4 little endian
   Shm_Bgra_4444_Bgra_4444 : constant Shm_Format_T := 842088770;

-- 16-bit xRGB format, [15:0] x:R:G:B 1:5:5:5 little endian
   Shm_Xrgb_1555_Xrgb_1555 : constant Shm_Format_T := 892424792;

-- 16-bit xBGR 1555 format, [15:0] x:B:G:R 1:5:5:5 little endian
   Shm_Xbgr_1555_Xbgr_1555 : constant Shm_Format_T := 892420696;

-- 16-bit RGBx 5551 format, [15:0] R:G:B:x 5:5:5:1 little endian
   Shm_Rgbx_5551_Rgbx_5551 : constant Shm_Format_T := 892426322;

-- 16-bit BGRx 5551 format, [15:0] B:G:R:x 5:5:5:1 little endian
   Shm_Bgrx_5551_Bgrx_5551 : constant Shm_Format_T := 892426306;

-- 16-bit ARGB 1555 format, [15:0] A:R:G:B 1:5:5:5 little endian
   Shm_Argb_1555_Argb_1555 : constant Shm_Format_T := 892424769;

-- 16-bit ABGR 1555 format, [15:0] A:B:G:R 1:5:5:5 little endian
   Shm_Abgr_1555_Abgr_1555 : constant Shm_Format_T := 892420673;

-- 16-bit RGBA 5551 format, [15:0] R:G:B:A 5:5:5:1 little endian
   Shm_Rgba_5551_Rgba_5551 : constant Shm_Format_T := 892420434;

-- 16-bit BGRA 5551 format, [15:0] B:G:R:A 5:5:5:1 little endian
   Shm_Bgra_5551_Bgra_5551 : constant Shm_Format_T := 892420418;

-- 16-bit RGB 565 format, [15:0] R:G:B 5:6:5 little endian
   Shm_Rgb_565_Rgb_565 : constant Shm_Format_T := 909199186;

-- 16-bit BGR 565 format, [15:0] B:G:R 5:6:5 little endian
   Shm_Bgr_565_Bgr_565 : constant Shm_Format_T := 909199170;

-- 24-bit RGB format, [23:0] R:G:B little endian
   Shm_Rgb_888_Rgb_888 : constant Shm_Format_T := 875710290;

-- 24-bit BGR format, [23:0] B:G:R little endian
   Shm_Bgr_888_Bgr_888 : constant Shm_Format_T := 875710274;

-- 32-bit xBGR format, [31:0] x:B:G:R 8:8:8:8 little endian
   Shm_Xbgr_8888_Xbgr_8888 : constant Shm_Format_T := 875709016;

-- 32-bit RGBx format, [31:0] R:G:B:x 8:8:8:8 little endian
   Shm_Rgbx_8888_Rgbx_8888 : constant Shm_Format_T := 875714642;

-- 32-bit BGRx format, [31:0] B:G:R:x 8:8:8:8 little endian
   Shm_Bgrx_8888_Bgrx_8888 : constant Shm_Format_T := 875714626;

-- 32-bit ABGR format, [31:0] A:B:G:R 8:8:8:8 little endian
   Shm_Abgr_8888_Abgr_8888 : constant Shm_Format_T := 875708993;

-- 32-bit RGBA format, [31:0] R:G:B:A 8:8:8:8 little endian
   Shm_Rgba_8888_Rgba_8888 : constant Shm_Format_T := 875708754;

-- 32-bit BGRA format, [31:0] B:G:R:A 8:8:8:8 little endian
   Shm_Bgra_8888_Bgra_8888 : constant Shm_Format_T := 875708738;

-- 32-bit xRGB format, [31:0] x:R:G:B 2:10:10:10 little endian
   Shm_Xrgb_2101010_Xrgb_2101010 : constant Shm_Format_T := 808669784;

-- 32-bit xBGR format, [31:0] x:B:G:R 2:10:10:10 little endian
   Shm_Xbgr_2101010_Xbgr_2101010 : constant Shm_Format_T := 808665688;

-- 32-bit RGBx format, [31:0] R:G:B:x 10:10:10:2 little endian
   Shm_Rgbx_1010102_Rgbx_1010102 : constant Shm_Format_T := 808671314;

-- 32-bit BGRx format, [31:0] B:G:R:x 10:10:10:2 little endian
   Shm_Bgrx_1010102_Bgrx_1010102 : constant Shm_Format_T := 808671298;

-- 32-bit ARGB format, [31:0] A:R:G:B 2:10:10:10 little endian
   Shm_Argb_2101010_Argb_2101010 : constant Shm_Format_T := 808669761;

-- 32-bit ABGR format, [31:0] A:B:G:R 2:10:10:10 little endian
   Shm_Abgr_2101010_Abgr_2101010 : constant Shm_Format_T := 808665665;

-- 32-bit RGBA format, [31:0] R:G:B:A 10:10:10:2 little endian
   Shm_Rgba_1010102_Rgba_1010102 : constant Shm_Format_T := 808665426;

-- 32-bit BGRA format, [31:0] B:G:R:A 10:10:10:2 little endian
   Shm_Bgra_1010102_Bgra_1010102 : constant Shm_Format_T := 808665410;

-- packed YCbCr format, [31:0] Cr0:Y1:Cb0:Y0 8:8:8:8 little endian
   Shm_Yuyv_Yuyv : constant Shm_Format_T := 1448695129;

-- packed YCbCr format, [31:0] Cb0:Y1:Cr0:Y0 8:8:8:8 little endian
   Shm_Yvyu_Yvyu : constant Shm_Format_T := 1431918169;

-- packed YCbCr format, [31:0] Y1:Cr0:Y0:Cb0 8:8:8:8 little endian
   Shm_Uyvy_Uyvy : constant Shm_Format_T := 1498831189;

-- packed YCbCr format, [31:0] Y1:Cb0:Y0:Cr0 8:8:8:8 little endian
   Shm_Vyuy_Vyuy : constant Shm_Format_T := 1498765654;

-- packed AYCbCr format, [31:0] A:Y:Cb:Cr 8:8:8:8 little endian
   Shm_Ayuv_Ayuv : constant Shm_Format_T := 1448433985;

-- 2 plane YCbCr Cr:Cb format, 2x2 subsampled Cr:Cb plane
   Shm_Nv_12_Nv_12 : constant Shm_Format_T := 842094158;

-- 2 plane YCbCr Cb:Cr format, 2x2 subsampled Cb:Cr plane
   Shm_Nv_21_Nv_21 : constant Shm_Format_T := 825382478;

-- 2 plane YCbCr Cr:Cb format, 2x1 subsampled Cr:Cb plane
   Shm_Nv_16_Nv_16 : constant Shm_Format_T := 909203022;

-- 2 plane YCbCr Cb:Cr format, 2x1 subsampled Cb:Cr plane
   Shm_Nv_61_Nv_61 : constant Shm_Format_T := 825644622;

-- 3 plane YCbCr format, 4x4 subsampled Cb (1) and Cr (2) planes
   Shm_Yuv_410_Yuv_410 : constant Shm_Format_T := 961959257;

-- 3 plane YCbCr format, 4x4 subsampled Cr (1) and Cb (2) planes
   Shm_Yvu_410_Yvu_410 : constant Shm_Format_T := 961893977;

-- 3 plane YCbCr format, 4x1 subsampled Cb (1) and Cr (2) planes
   Shm_Yuv_411_Yuv_411 : constant Shm_Format_T := 825316697;

-- 3 plane YCbCr format, 4x1 subsampled Cr (1) and Cb (2) planes
   Shm_Yvu_411_Yvu_411 : constant Shm_Format_T := 825316953;

-- 3 plane YCbCr format, 2x2 subsampled Cb (1) and Cr (2) planes
   Shm_Yuv_420_Yuv_420 : constant Shm_Format_T := 842093913;

-- 3 plane YCbCr format, 2x2 subsampled Cr (1) and Cb (2) planes
   Shm_Yvu_420_Yvu_420 : constant Shm_Format_T := 842094169;

-- 3 plane YCbCr format, 2x1 subsampled Cb (1) and Cr (2) planes
   Shm_Yuv_422_Yuv_422 : constant Shm_Format_T := 909202777;

-- 3 plane YCbCr format, 2x1 subsampled Cr (1) and Cb (2) planes
   Shm_Yvu_422_Yvu_422 : constant Shm_Format_T := 909203033;

-- 3 plane YCbCr format, non-subsampled Cb (1) and Cr (2) planes
   Shm_Yuv_444_Yuv_444 : constant Shm_Format_T := 875713881;

-- 3 plane YCbCr format, non-subsampled Cr (1) and Cb (2) planes
   Shm_Yvu_444_Yvu_444 : constant Shm_Format_T := 875714137;

   type Shm_Format_Subprogram_Ptr is access procedure
     (Data   : Void_Ptr;
      Shm    : Shm_Ptr;
      Format : Interfaces.Unsigned_32) with
      Convention => C;

   type Shm_Listener_T is record
      Format : Shm_Format_Subprogram_Ptr;
   end record with
      Convention => C_Pass_By_Copy;

   type Shm_Listener_Ptr is access all Shm_Listener_T;

   function Shm_Add_Listener (Shm : Shm_Ptr; Listener : Shm_Listener_Ptr; Data : Void_Ptr) return Interfaces.C.int;

   procedure Shm_Set_User_Data (Shm : Shm_Ptr; Data : Void_Ptr);

   function Shm_Get_User_Data (Shm : Shm_Ptr) return Void_Ptr;

   function Shm_Get_Version (Shm : Shm_Ptr) return Interfaces.Unsigned_32;

   procedure Shm_Destroy (Shm : Shm_Ptr);

-- Create a new wl_shm_pool object.
--
-- The pool can be used to create shared memory based buffer
-- objects.  The server will mmap size bytes of the passed file
-- descriptor, to use as backing memory for the pool.
   function Shm_Create_Pool (Shm : Shm_Ptr; Fd : Integer; Size : Integer) return Shm_Pool_Ptr;

   type Buffer_Release_Subprogram_Ptr is access procedure (Data : Void_Ptr; Buffer : Buffer_Ptr) with
      Convention => C;

   type Buffer_Listener_T is record
      Release : Buffer_Release_Subprogram_Ptr;
   end record with
      Convention => C_Pass_By_Copy;

   type Buffer_Listener_Ptr is access all Buffer_Listener_T;

   function Buffer_Add_Listener
     (Buffer   : Buffer_Ptr;
      Listener : Buffer_Listener_Ptr;
      Data     : Void_Ptr) return Interfaces.C.int;

   procedure Buffer_Set_User_Data (Buffer : Buffer_Ptr; Data : Void_Ptr);

   function Buffer_Get_User_Data (Buffer : Buffer_Ptr) return Void_Ptr;

   function Buffer_Get_Version (Buffer : Buffer_Ptr) return Interfaces.Unsigned_32;

   procedure Buffer_Destroy (Buffer : Buffer_Ptr);

   type Data_Offer_Error_T is new Interfaces.Unsigned_32;
-- finish request was called untimely
   Data_Offer_Invalid_Finish_Invalid_Finish : constant Data_Offer_Error_T := 0;

-- action mask contains invalid values
   Data_Offer_Invalid_Action_Mask_Invalid_Action_Mask : constant Data_Offer_Error_T := 1;

-- action argument has an invalid value
   Data_Offer_Invalid_Action_Invalid_Action : constant Data_Offer_Error_T := 2;

-- offer doesn't accept this request
   Data_Offer_Invalid_Offer_Invalid_Offer : constant Data_Offer_Error_T := 3;

   type Data_Offer_Offer_Subprogram_Ptr is access procedure
     (Data       : Void_Ptr;
      Data_Offer : Data_Offer_Ptr;
      Mime_Type  : Interfaces.C.Strings.chars_ptr) with
      Convention => C;

   type Data_Offer_Source_Actions_Subprogram_Ptr is access procedure
     (Data           : Void_Ptr;
      Data_Offer     : Data_Offer_Ptr;
      Source_Actions : Interfaces.Unsigned_32) with
      Convention => C;

   type Data_Offer_Action_Subprogram_Ptr is access procedure
     (Data       : Void_Ptr;
      Data_Offer : Data_Offer_Ptr;
      Dnd_Action : Interfaces.Unsigned_32) with
      Convention => C;

   type Data_Offer_Listener_T is record
      Offer          : Data_Offer_Offer_Subprogram_Ptr;
      Source_Actions : Data_Offer_Source_Actions_Subprogram_Ptr;
      Action         : Data_Offer_Action_Subprogram_Ptr;
   end record with
      Convention => C_Pass_By_Copy;

   type Data_Offer_Listener_Ptr is access all Data_Offer_Listener_T;

   function Data_Offer_Add_Listener
     (Data_Offer : Data_Offer_Ptr;
      Listener   : Data_Offer_Listener_Ptr;
      Data       : Void_Ptr) return Interfaces.C.int;

   procedure Data_Offer_Set_User_Data (Data_Offer : Data_Offer_Ptr; Data : Void_Ptr);

   function Data_Offer_Get_User_Data (Data_Offer : Data_Offer_Ptr) return Void_Ptr;

   function Data_Offer_Get_Version (Data_Offer : Data_Offer_Ptr) return Interfaces.Unsigned_32;

   procedure Data_Offer_Destroy (Data_Offer : Data_Offer_Ptr);

   procedure Data_Offer_Accept
     (Data_Offer : Data_Offer_Ptr;
      Serial     : Interfaces.Unsigned_32;
      Mime_Type  : Interfaces.C.Strings.chars_ptr);

   procedure Data_Offer_Receive (Data_Offer : Data_Offer_Ptr; Mime_Type : Interfaces.C.Strings.chars_ptr; Fd : Integer);

   procedure Data_Offer_Finish (Data_Offer : Data_Offer_Ptr);

   procedure Data_Offer_Set_Actions
     (Data_Offer       : Data_Offer_Ptr;
      Dnd_Actions      : Interfaces.Unsigned_32;
      Preferred_Action : Interfaces.Unsigned_32);

   type Data_Source_Error_T is new Interfaces.Unsigned_32;
-- action mask contains invalid values
   Data_Source_Invalid_Action_Mask_Invalid_Action_Mask : constant Data_Source_Error_T := 0;

-- source doesn't accept this request
   Data_Source_Invalid_Source_Invalid_Source : constant Data_Source_Error_T := 1;

   type Data_Source_Target_Subprogram_Ptr is access procedure
     (Data        : Void_Ptr;
      Data_Source : Data_Source_Ptr;
      Mime_Type   : Interfaces.C.Strings.chars_ptr) with
      Convention => C;

   type Data_Source_Send_Subprogram_Ptr is access procedure
     (Data        : Void_Ptr;
      Data_Source : Data_Source_Ptr;
      Mime_Type   : Interfaces.C.Strings.chars_ptr;
      Fd          : Integer) with
      Convention => C;

   type Data_Source_Cancelled_Subprogram_Ptr is access procedure (Data : Void_Ptr; Data_Source : Data_Source_Ptr) with
      Convention => C;

   type Data_Source_Dnd_Drop_Performed_Subprogram_Ptr is access procedure
     (Data        : Void_Ptr;
      Data_Source : Data_Source_Ptr) with
      Convention => C;

   type Data_Source_Dnd_Finished_Subprogram_Ptr is access procedure
     (Data        : Void_Ptr;
      Data_Source : Data_Source_Ptr) with
      Convention => C;

   type Data_Source_Action_Subprogram_Ptr is access procedure
     (Data        : Void_Ptr;
      Data_Source : Data_Source_Ptr;
      Dnd_Action  : Interfaces.Unsigned_32) with
      Convention => C;

   type Data_Source_Listener_T is record
      Target             : Data_Source_Target_Subprogram_Ptr;
      Send               : Data_Source_Send_Subprogram_Ptr;
      Cancelled          : Data_Source_Cancelled_Subprogram_Ptr;
      Dnd_Drop_Performed : Data_Source_Dnd_Drop_Performed_Subprogram_Ptr;
      Dnd_Finished       : Data_Source_Dnd_Finished_Subprogram_Ptr;
      Action             : Data_Source_Action_Subprogram_Ptr;
   end record with
      Convention => C_Pass_By_Copy;

   type Data_Source_Listener_Ptr is access all Data_Source_Listener_T;

   function Data_Source_Add_Listener
     (Data_Source : Data_Source_Ptr;
      Listener    : Data_Source_Listener_Ptr;
      Data        : Void_Ptr) return Interfaces.C.int;

   procedure Data_Source_Set_User_Data (Data_Source : Data_Source_Ptr; Data : Void_Ptr);

   function Data_Source_Get_User_Data (Data_Source : Data_Source_Ptr) return Void_Ptr;

   function Data_Source_Get_Version (Data_Source : Data_Source_Ptr) return Interfaces.Unsigned_32;

   procedure Data_Source_Destroy (Data_Source : Data_Source_Ptr);

   procedure Data_Source_Offer (Data_Source : Data_Source_Ptr; Mime_Type : Interfaces.C.Strings.chars_ptr);

   procedure Data_Source_Set_Actions (Data_Source : Data_Source_Ptr; Dnd_Actions : Interfaces.Unsigned_32);

   type Data_Device_Error_T is new Interfaces.Unsigned_32;
-- given wl_surface has another role
   Data_Device_Role_Role : constant Data_Device_Error_T := 0;

   type Data_Device_Data_Offer_Subprogram_Ptr is access procedure
     (Data        : Void_Ptr;
      Data_Device : Data_Device_Ptr;
      Id          : Interfaces.Unsigned_32) with
      Convention => C;

   type Data_Device_Enter_Subprogram_Ptr is access procedure
     (Data        : Void_Ptr;
      Data_Device : Data_Device_Ptr;
      Serial      : Interfaces.Unsigned_32;
      Surface     : Surface_Ptr;
      X           : Fixed_T;
      Y           : Fixed_T;
      Id          : Data_Offer_Ptr) with
      Convention => C;

   type Data_Device_Leave_Subprogram_Ptr is access procedure (Data : Void_Ptr; Data_Device : Data_Device_Ptr) with
      Convention => C;

   type Data_Device_Motion_Subprogram_Ptr is access procedure
     (Data        : Void_Ptr;
      Data_Device : Data_Device_Ptr;
      Time        : Interfaces.Unsigned_32;
      X           : Fixed_T;
      Y           : Fixed_T) with
      Convention => C;

   type Data_Device_Drop_Subprogram_Ptr is access procedure (Data : Void_Ptr; Data_Device : Data_Device_Ptr) with
      Convention => C;

   type Data_Device_Selection_Subprogram_Ptr is access procedure
     (Data        : Void_Ptr;
      Data_Device : Data_Device_Ptr;
      Id          : Data_Offer_Ptr) with
      Convention => C;

   type Data_Device_Listener_T is record
      Data_Offer : Data_Device_Data_Offer_Subprogram_Ptr;
      Enter      : Data_Device_Enter_Subprogram_Ptr;
      Leave      : Data_Device_Leave_Subprogram_Ptr;
      Motion     : Data_Device_Motion_Subprogram_Ptr;
      Drop       : Data_Device_Drop_Subprogram_Ptr;
      Selection  : Data_Device_Selection_Subprogram_Ptr;
   end record with
      Convention => C_Pass_By_Copy;

   type Data_Device_Listener_Ptr is access all Data_Device_Listener_T;

   function Data_Device_Add_Listener
     (Data_Device : Data_Device_Ptr;
      Listener    : Data_Device_Listener_Ptr;
      Data        : Void_Ptr) return Interfaces.C.int;

   procedure Data_Device_Set_User_Data (Data_Device : Data_Device_Ptr; Data : Void_Ptr);

   function Data_Device_Get_User_Data (Data_Device : Data_Device_Ptr) return Void_Ptr;

   function Data_Device_Get_Version (Data_Device : Data_Device_Ptr) return Interfaces.Unsigned_32;

   procedure Data_Device_Destroy (Data_Device : Data_Device_Ptr);

   procedure Data_Device_Start_Drag
     (Data_Device : Data_Device_Ptr;
      Source      : Data_Source_Ptr;
      Origin      : Surface_Ptr;
      Icon        : Surface_Ptr;
      Serial      : Interfaces.Unsigned_32);

   procedure Data_Device_Set_Selection
     (Data_Device : Data_Device_Ptr;
      Source      : Data_Source_Ptr;
      Serial      : Interfaces.Unsigned_32);

   procedure Data_Device_Release (Data_Device : Data_Device_Ptr);

   type Data_Device_Manager_Dnd_Action_T is new Interfaces.Unsigned_32;
-- no action
   Data_Device_Manager_None_None : constant Data_Device_Manager_Dnd_Action_T := 0;

-- copy action
   Data_Device_Manager_Copy_Copy : constant Data_Device_Manager_Dnd_Action_T := 1;

-- move action
   Data_Device_Manager_Move_Move : constant Data_Device_Manager_Dnd_Action_T := 2;

-- ask action
   Data_Device_Manager_Ask_Ask : constant Data_Device_Manager_Dnd_Action_T := 4;

   procedure Data_Device_Manager_Set_User_Data (Data_Device_Manager : Data_Device_Manager_Ptr; Data : Void_Ptr);

   function Data_Device_Manager_Get_User_Data (Data_Device_Manager : Data_Device_Manager_Ptr) return Void_Ptr;

   function Data_Device_Manager_Get_Version
     (Data_Device_Manager : Data_Device_Manager_Ptr) return Interfaces.Unsigned_32;

   procedure Data_Device_Manager_Destroy (Data_Device_Manager : Data_Device_Manager_Ptr);

-- Create a new data source.
   function Data_Device_Manager_Create_Data_Source
     (Data_Device_Manager : Data_Device_Manager_Ptr) return Data_Source_Ptr;

-- Create a new data device for a given seat.
   function Data_Device_Manager_Get_Data_Device
     (Data_Device_Manager : Data_Device_Manager_Ptr;
      Seat                : Seat_Ptr) return Data_Device_Ptr;

   type Shell_Error_T is new Interfaces.Unsigned_32;
-- given wl_surface has another role
   Shell_Role_Role : constant Shell_Error_T := 0;

   procedure Shell_Set_User_Data (Shell : Shell_Ptr; Data : Void_Ptr);

   function Shell_Get_User_Data (Shell : Shell_Ptr) return Void_Ptr;

   function Shell_Get_Version (Shell : Shell_Ptr) return Interfaces.Unsigned_32;

   procedure Shell_Destroy (Shell : Shell_Ptr);

-- Create a shell surface for an existing surface. This gives
-- the wl_surface the role of a shell surface. If the wl_surface
-- already has another role, it raises a protocol error.
--
-- Only one shell surface can be associated with a given surface.
   function Shell_Get_Shell_Surface (Shell : Shell_Ptr; Surface : Surface_Ptr) return Shell_Surface_Ptr;

   type Shell_Surface_Resize_T is new Interfaces.Unsigned_32;
-- no edge
   Shell_Surface_None_None : constant Shell_Surface_Resize_T := 0;

-- top edge
   Shell_Surface_Top_Top : constant Shell_Surface_Resize_T := 1;

-- bottom edge
   Shell_Surface_Bottom_Bottom : constant Shell_Surface_Resize_T := 2;

-- left edge
   Shell_Surface_Left_Left : constant Shell_Surface_Resize_T := 4;

-- top and left edges
   Shell_Surface_Top_Left_Top_Left : constant Shell_Surface_Resize_T := 5;

-- bottom and left edges
   Shell_Surface_Bottom_Left_Bottom_Left : constant Shell_Surface_Resize_T := 6;

-- right edge
   Shell_Surface_Right_Right : constant Shell_Surface_Resize_T := 8;

-- top and right edges
   Shell_Surface_Top_Right_Top_Right : constant Shell_Surface_Resize_T := 9;

-- bottom and right edges
   Shell_Surface_Bottom_Right_Bottom_Right : constant Shell_Surface_Resize_T := 10;

   type Shell_Surface_Transient_T is new Interfaces.Unsigned_32;
-- do not set keyboard focus
   Shell_Surface_Inactive_Inactive : constant Shell_Surface_Transient_T := 1;

   type Shell_Surface_Fullscreen_Method_T is new Interfaces.Unsigned_32;
-- no preference, apply default policy
   Shell_Surface_Default_Default : constant Shell_Surface_Fullscreen_Method_T := 0;

-- scale, preserve the surface's aspect ratio and center on output
   Shell_Surface_Scale_Scale : constant Shell_Surface_Fullscreen_Method_T := 1;

-- switch output mode to the smallest mode that can fit the surface, add black borders to compensate size mismatch
   Shell_Surface_Driver_Driver : constant Shell_Surface_Fullscreen_Method_T := 2;

-- no upscaling, center on output and add black borders to compensate size mismatch
   Shell_Surface_Fill_Fill : constant Shell_Surface_Fullscreen_Method_T := 3;

   type Shell_Surface_Ping_Subprogram_Ptr is access procedure
     (Data          : Void_Ptr;
      Shell_Surface : Shell_Surface_Ptr;
      Serial        : Interfaces.Unsigned_32) with
      Convention => C;

   type Shell_Surface_Configure_Subprogram_Ptr is access procedure
     (Data          : Void_Ptr;
      Shell_Surface : Shell_Surface_Ptr;
      Edges         : Interfaces.Unsigned_32;
      Width         : Integer;
      Height        : Integer) with
      Convention => C;

   type Shell_Surface_Popup_Done_Subprogram_Ptr is access procedure
     (Data          : Void_Ptr;
      Shell_Surface : Shell_Surface_Ptr) with
      Convention => C;

   type Shell_Surface_Listener_T is record
      Ping       : Shell_Surface_Ping_Subprogram_Ptr;
      Configure  : Shell_Surface_Configure_Subprogram_Ptr;
      Popup_Done : Shell_Surface_Popup_Done_Subprogram_Ptr;
   end record with
      Convention => C_Pass_By_Copy;

   type Shell_Surface_Listener_Ptr is access all Shell_Surface_Listener_T;

   function Shell_Surface_Add_Listener
     (Shell_Surface : Shell_Surface_Ptr;
      Listener      : Shell_Surface_Listener_Ptr;
      Data          : Void_Ptr) return Interfaces.C.int;

   procedure Shell_Surface_Set_User_Data (Shell_Surface : Shell_Surface_Ptr; Data : Void_Ptr);

   function Shell_Surface_Get_User_Data (Shell_Surface : Shell_Surface_Ptr) return Void_Ptr;

   function Shell_Surface_Get_Version (Shell_Surface : Shell_Surface_Ptr) return Interfaces.Unsigned_32;

   procedure Shell_Surface_Destroy (Shell_Surface : Shell_Surface_Ptr);

   procedure Shell_Surface_Pong (Shell_Surface : Shell_Surface_Ptr; Serial : Interfaces.Unsigned_32);

   procedure Shell_Surface_Move (Shell_Surface : Shell_Surface_Ptr; Seat : Seat_Ptr; Serial : Interfaces.Unsigned_32);

   procedure Shell_Surface_Resize
     (Shell_Surface : Shell_Surface_Ptr;
      Seat          : Seat_Ptr;
      Serial        : Interfaces.Unsigned_32;
      Edges         : Interfaces.Unsigned_32);

   procedure Shell_Surface_Set_Toplevel (Shell_Surface : Shell_Surface_Ptr);

   procedure Shell_Surface_Set_Transient
     (Shell_Surface : Shell_Surface_Ptr;
      Parent        : Surface_Ptr;
      X             : Integer;
      Y             : Integer;
      Flags         : Interfaces.Unsigned_32);

   procedure Shell_Surface_Set_Fullscreen
     (Shell_Surface : Shell_Surface_Ptr;
      Method        : Interfaces.Unsigned_32;
      Framerate     : Interfaces.Unsigned_32;
      Output        : Output_Ptr);

   procedure Shell_Surface_Set_Popup
     (Shell_Surface : Shell_Surface_Ptr;
      Seat          : Seat_Ptr;
      Serial        : Interfaces.Unsigned_32;
      Parent        : Surface_Ptr;
      X             : Integer;
      Y             : Integer;
      Flags         : Interfaces.Unsigned_32);

   procedure Shell_Surface_Set_Maximized (Shell_Surface : Shell_Surface_Ptr; Output : Output_Ptr);

   procedure Shell_Surface_Set_Title (Shell_Surface : Shell_Surface_Ptr; Title : Interfaces.C.Strings.chars_ptr);

   procedure Shell_Surface_Set_Class (Shell_Surface : Shell_Surface_Ptr; Class_V : Interfaces.C.Strings.chars_ptr);

   type Surface_Error_T is new Interfaces.Unsigned_32;
-- buffer scale value is invalid
   Surface_Invalid_Scale_Invalid_Scale : constant Surface_Error_T := 0;

-- buffer transform value is invalid
   Surface_Invalid_Transform_Invalid_Transform : constant Surface_Error_T := 1;

   type Surface_Enter_Subprogram_Ptr is access procedure
     (Data    : Void_Ptr;
      Surface : Surface_Ptr;
      Output  : Output_Ptr) with
      Convention => C;

   type Surface_Leave_Subprogram_Ptr is access procedure
     (Data    : Void_Ptr;
      Surface : Surface_Ptr;
      Output  : Output_Ptr) with
      Convention => C;

   type Surface_Listener_T is record
      Enter : Surface_Enter_Subprogram_Ptr;
      Leave : Surface_Leave_Subprogram_Ptr;
   end record with
      Convention => C_Pass_By_Copy;

   type Surface_Listener_Ptr is access all Surface_Listener_T;

   function Surface_Add_Listener
     (Surface  : Surface_Ptr;
      Listener : Surface_Listener_Ptr;
      Data     : Void_Ptr) return Interfaces.C.int;

   procedure Surface_Set_User_Data (Surface : Surface_Ptr; Data : Void_Ptr);

   function Surface_Get_User_Data (Surface : Surface_Ptr) return Void_Ptr;

   function Surface_Get_Version (Surface : Surface_Ptr) return Interfaces.Unsigned_32;

   procedure Surface_Destroy (Surface : Surface_Ptr);

   procedure Surface_Attach (Surface : Surface_Ptr; Buffer : Buffer_Ptr; X : Integer; Y : Integer);

   procedure Surface_Damage (Surface : Surface_Ptr; X : Integer; Y : Integer; Width : Integer; Height : Integer);

-- Request a notification when it is a good time to start drawing a new
-- frame, by creating a frame callback. This is useful for throttling
-- redrawing operations, and driving animations.
--
-- When a client is animating on a wl_surface, it can use the 'frame'
-- request to get notified when it is a good time to draw and commit the
-- next frame of animation. If the client commits an update earlier than
-- that, it is likely that some updates will not make it to the display,
-- and the client is wasting resources by drawing too often.
--
-- The frame request will take effect on the next wl_surface.commit.
-- The notification will only be posted for one frame unless
-- requested again. For a wl_surface, the notifications are posted in
-- the order the frame requests were committed.
--
-- The server must send the notifications so that a client
-- will not send excessive updates, while still allowing
-- the highest possible update rate for clients that wait for the reply
-- before drawing again. The server should give some time for the client
-- to draw and commit after sending the frame callback events to let it
-- hit the next output refresh.
--
-- A server should avoid signaling the frame callbacks if the
-- surface is not visible in any way, e.g. the surface is off-screen,
-- or completely obscured by other opaque surfaces.
--
-- The object returned by this request will be destroyed by the
-- compositor after the callback is fired and as such the client must not
-- attempt to use it after that point.
--
-- The callback_data passed in the callback is the current time, in
-- milliseconds, with an undefined base.
   function Surface_Frame (Surface : Surface_Ptr) return Callback_Ptr;

   procedure Surface_Set_Opaque_Region (Surface : Surface_Ptr; Region : Region_Ptr);

   procedure Surface_Set_Input_Region (Surface : Surface_Ptr; Region : Region_Ptr);

   procedure Surface_Commit (Surface : Surface_Ptr);

   procedure Surface_Set_Buffer_Transform (Surface : Surface_Ptr; Transform : Integer);

   procedure Surface_Set_Buffer_Scale (Surface : Surface_Ptr; Scale : Integer);

   procedure Surface_Damage_Buffer (Surface : Surface_Ptr; X : Integer; Y : Integer; Width : Integer; Height : Integer);

   type Seat_Capability_T is new Interfaces.Unsigned_32;
-- the seat has pointer devices
   Seat_Pointer_Pointer : constant Seat_Capability_T := 1;

-- the seat has one or more keyboards
   Seat_Keyboard_Keyboard : constant Seat_Capability_T := 2;

-- the seat has touch devices
   Seat_Touch_Touch : constant Seat_Capability_T := 4;

   type Seat_Capabilities_Subprogram_Ptr is access procedure
     (Data         : Void_Ptr;
      Seat         : Seat_Ptr;
      Capabilities : Interfaces.Unsigned_32) with
      Convention => C;

   type Seat_Name_Subprogram_Ptr is access procedure
     (Data : Void_Ptr;
      Seat : Seat_Ptr;
      Name : Interfaces.C.Strings.chars_ptr) with
      Convention => C;

   type Seat_Listener_T is record
      Capabilities : Seat_Capabilities_Subprogram_Ptr;
      Name         : Seat_Name_Subprogram_Ptr;
   end record with
      Convention => C_Pass_By_Copy;

   type Seat_Listener_Ptr is access all Seat_Listener_T;

   function Seat_Add_Listener (Seat : Seat_Ptr; Listener : Seat_Listener_Ptr; Data : Void_Ptr) return Interfaces.C.int;

   procedure Seat_Set_User_Data (Seat : Seat_Ptr; Data : Void_Ptr);

   function Seat_Get_User_Data (Seat : Seat_Ptr) return Void_Ptr;

   function Seat_Get_Version (Seat : Seat_Ptr) return Interfaces.Unsigned_32;

   procedure Seat_Destroy (Seat : Seat_Ptr);

-- The ID provided will be initialized to the wl_pointer interface
-- for this seat.
--
-- This request only takes effect if the seat has the pointer
-- capability, or has had the pointer capability in the past.
-- It is a protocol violation to issue this request on a seat that has
-- never had the pointer capability.
   function Seat_Get_Pointer (Seat : Seat_Ptr) return Pointer_Ptr;

-- The ID provided will be initialized to the wl_keyboard interface
-- for this seat.
--
-- This request only takes effect if the seat has the keyboard
-- capability, or has had the keyboard capability in the past.
-- It is a protocol violation to issue this request on a seat that has
-- never had the keyboard capability.
   function Seat_Get_Keyboard (Seat : Seat_Ptr) return Keyboard_Ptr;

-- The ID provided will be initialized to the wl_touch interface
-- for this seat.
--
-- This request only takes effect if the seat has the touch
-- capability, or has had the touch capability in the past.
-- It is a protocol violation to issue this request on a seat that has
-- never had the touch capability.
   function Seat_Get_Touch (Seat : Seat_Ptr) return Touch_Ptr;

   procedure Seat_Release (Seat : Seat_Ptr);

   type Pointer_Error_T is new Interfaces.Unsigned_32;
-- given wl_surface has another role
   Pointer_Role_Role : constant Pointer_Error_T := 0;

   type Pointer_Button_State_T is new Interfaces.Unsigned_32;
-- the button is not pressed
   Pointer_Released_Released : constant Pointer_Button_State_T := 0;

-- the button is pressed
   Pointer_Pressed_Pressed : constant Pointer_Button_State_T := 1;

   type Pointer_Axis_T is new Interfaces.Unsigned_32;
-- vertical axis
   Pointer_Vertical_Scroll_Vertical_Scroll : constant Pointer_Axis_T := 0;

-- horizontal axis
   Pointer_Horizontal_Scroll_Horizontal_Scroll : constant Pointer_Axis_T := 1;

   type Pointer_Axis_Source_T is new Interfaces.Unsigned_32;
-- a physical wheel rotation
   Pointer_Wheel_Wheel : constant Pointer_Axis_Source_T := 0;

-- finger on a touch surface
   Pointer_Finger_Finger : constant Pointer_Axis_Source_T := 1;

-- continuous coordinate space
   Pointer_Continuous_Continuous : constant Pointer_Axis_Source_T := 2;

-- a physical wheel tilt
   Pointer_Wheel_Tilt_Wheel_Tilt : constant Pointer_Axis_Source_T := 3;

   type Pointer_Enter_Subprogram_Ptr is access procedure
     (Data      : Void_Ptr;
      Pointer   : Pointer_Ptr;
      Serial    : Interfaces.Unsigned_32;
      Surface   : Surface_Ptr;
      Surface_X : Fixed_T;
      Surface_Y : Fixed_T) with
      Convention => C;

   type Pointer_Leave_Subprogram_Ptr is access procedure
     (Data    : Void_Ptr;
      Pointer : Pointer_Ptr;
      Serial  : Interfaces.Unsigned_32;
      Surface : Surface_Ptr) with
      Convention => C;

   type Pointer_Motion_Subprogram_Ptr is access procedure
     (Data      : Void_Ptr;
      Pointer   : Pointer_Ptr;
      Time      : Interfaces.Unsigned_32;
      Surface_X : Fixed_T;
      Surface_Y : Fixed_T) with
      Convention => C;

   type Pointer_Button_Subprogram_Ptr is access procedure
     (Data    : Void_Ptr;
      Pointer : Pointer_Ptr;
      Serial  : Interfaces.Unsigned_32;
      Time    : Interfaces.Unsigned_32;
      Button  : Interfaces.Unsigned_32;
      State   : Interfaces.Unsigned_32) with
      Convention => C;

   type Pointer_Axis_Subprogram_Ptr is access procedure
     (Data    : Void_Ptr;
      Pointer : Pointer_Ptr;
      Time    : Interfaces.Unsigned_32;
      Axis    : Interfaces.Unsigned_32;
      Value   : Fixed_T) with
      Convention => C;

   type Pointer_Frame_Subprogram_Ptr is access procedure (Data : Void_Ptr; Pointer : Pointer_Ptr) with
      Convention => C;

   type Pointer_Axis_Source_Subprogram_Ptr is access procedure
     (Data        : Void_Ptr;
      Pointer     : Pointer_Ptr;
      Axis_Source : Interfaces.Unsigned_32) with
      Convention => C;

   type Pointer_Axis_Stop_Subprogram_Ptr is access procedure
     (Data    : Void_Ptr;
      Pointer : Pointer_Ptr;
      Time    : Interfaces.Unsigned_32;
      Axis    : Interfaces.Unsigned_32) with
      Convention => C;

   type Pointer_Axis_Discrete_Subprogram_Ptr is access procedure
     (Data     : Void_Ptr;
      Pointer  : Pointer_Ptr;
      Axis     : Interfaces.Unsigned_32;
      Discrete : Integer) with
      Convention => C;

   type Pointer_Listener_T is record
      Enter         : Pointer_Enter_Subprogram_Ptr;
      Leave         : Pointer_Leave_Subprogram_Ptr;
      Motion        : Pointer_Motion_Subprogram_Ptr;
      Button        : Pointer_Button_Subprogram_Ptr;
      Axis          : Pointer_Axis_Subprogram_Ptr;
      Frame         : Pointer_Frame_Subprogram_Ptr;
      Axis_Source   : Pointer_Axis_Source_Subprogram_Ptr;
      Axis_Stop     : Pointer_Axis_Stop_Subprogram_Ptr;
      Axis_Discrete : Pointer_Axis_Discrete_Subprogram_Ptr;
   end record with
      Convention => C_Pass_By_Copy;

   type Pointer_Listener_Ptr is access all Pointer_Listener_T;

   function Pointer_Add_Listener
     (Pointer  : Pointer_Ptr;
      Listener : Pointer_Listener_Ptr;
      Data     : Void_Ptr) return Interfaces.C.int;

   procedure Pointer_Set_User_Data (Pointer : Pointer_Ptr; Data : Void_Ptr);

   function Pointer_Get_User_Data (Pointer : Pointer_Ptr) return Void_Ptr;

   function Pointer_Get_Version (Pointer : Pointer_Ptr) return Interfaces.Unsigned_32;

   procedure Pointer_Destroy (Pointer : Pointer_Ptr);

   procedure Pointer_Set_Cursor
     (Pointer   : Pointer_Ptr;
      Serial    : Interfaces.Unsigned_32;
      Surface   : Surface_Ptr;
      Hotspot_X : Integer;
      Hotspot_Y : Integer);

   procedure Pointer_Release (Pointer : Pointer_Ptr);

   type Keyboard_Keymap_Format_T is new Interfaces.Unsigned_32;
-- no keymap; client must understand how to interpret the raw keycode
   Keyboard_No_Keymap_No_Keymap : constant Keyboard_Keymap_Format_T := 0;

-- libxkbcommon compatible; to determine the xkb keycode, clients must add 8 to the key event keycode
   Keyboard_Xkb_V_1_Xkb_V_1 : constant Keyboard_Keymap_Format_T := 1;

   type Keyboard_Key_State_T is new Interfaces.Unsigned_32;
-- key is not pressed
   Keyboard_Released_Released : constant Keyboard_Key_State_T := 0;

-- key is pressed
   Keyboard_Pressed_Pressed : constant Keyboard_Key_State_T := 1;

   type Keyboard_Keymap_Subprogram_Ptr is access procedure
     (Data     : Void_Ptr;
      Keyboard : Keyboard_Ptr;
      Format   : Interfaces.Unsigned_32;
      Fd       : Integer;
      Size     : Interfaces.Unsigned_32) with
      Convention => C;

   type Keyboard_Enter_Subprogram_Ptr is access procedure
     (Data     : Void_Ptr;
      Keyboard : Keyboard_Ptr;
      Serial   : Interfaces.Unsigned_32;
      Surface  : Surface_Ptr;
      Keys     : Wayland_Array_T) with
      Convention => C;

   type Keyboard_Leave_Subprogram_Ptr is access procedure
     (Data     : Void_Ptr;
      Keyboard : Keyboard_Ptr;
      Serial   : Interfaces.Unsigned_32;
      Surface  : Surface_Ptr) with
      Convention => C;

   type Keyboard_Key_Subprogram_Ptr is access procedure
     (Data     : Void_Ptr;
      Keyboard : Keyboard_Ptr;
      Serial   : Interfaces.Unsigned_32;
      Time     : Interfaces.Unsigned_32;
      Key      : Interfaces.Unsigned_32;
      State    : Interfaces.Unsigned_32) with
      Convention => C;

   type Keyboard_Modifiers_Subprogram_Ptr is access procedure
     (Data           : Void_Ptr;
      Keyboard       : Keyboard_Ptr;
      Serial         : Interfaces.Unsigned_32;
      Mods_Depressed : Interfaces.Unsigned_32;
      Mods_Latched   : Interfaces.Unsigned_32;
      Mods_Locked    : Interfaces.Unsigned_32;
      Group          : Interfaces.Unsigned_32) with
      Convention => C;

   type Keyboard_Repeat_Info_Subprogram_Ptr is access procedure
     (Data     : Void_Ptr;
      Keyboard : Keyboard_Ptr;
      Rate     : Integer;
      Delay_V  : Integer) with
      Convention => C;

   type Keyboard_Listener_T is record
      Keymap      : Keyboard_Keymap_Subprogram_Ptr;
      Enter       : Keyboard_Enter_Subprogram_Ptr;
      Leave       : Keyboard_Leave_Subprogram_Ptr;
      Key         : Keyboard_Key_Subprogram_Ptr;
      Modifiers   : Keyboard_Modifiers_Subprogram_Ptr;
      Repeat_Info : Keyboard_Repeat_Info_Subprogram_Ptr;
   end record with
      Convention => C_Pass_By_Copy;

   type Keyboard_Listener_Ptr is access all Keyboard_Listener_T;

   function Keyboard_Add_Listener
     (Keyboard : Keyboard_Ptr;
      Listener : Keyboard_Listener_Ptr;
      Data     : Void_Ptr) return Interfaces.C.int;

   procedure Keyboard_Set_User_Data (Keyboard : Keyboard_Ptr; Data : Void_Ptr);

   function Keyboard_Get_User_Data (Keyboard : Keyboard_Ptr) return Void_Ptr;

   function Keyboard_Get_Version (Keyboard : Keyboard_Ptr) return Interfaces.Unsigned_32;

   procedure Keyboard_Destroy (Keyboard : Keyboard_Ptr);

   procedure Keyboard_Release (Keyboard : Keyboard_Ptr);

   type Touch_Down_Subprogram_Ptr is access procedure
     (Data    : Void_Ptr;
      Touch   : Touch_Ptr;
      Serial  : Interfaces.Unsigned_32;
      Time    : Interfaces.Unsigned_32;
      Surface : Surface_Ptr;
      Id      : Integer;
      X       : Fixed_T;
      Y       : Fixed_T) with
      Convention => C;

   type Touch_Up_Subprogram_Ptr is access procedure
     (Data   : Void_Ptr;
      Touch  : Touch_Ptr;
      Serial : Interfaces.Unsigned_32;
      Time   : Interfaces.Unsigned_32;
      Id     : Integer) with
      Convention => C;

   type Touch_Motion_Subprogram_Ptr is access procedure
     (Data  : Void_Ptr;
      Touch : Touch_Ptr;
      Time  : Interfaces.Unsigned_32;
      Id    : Integer;
      X     : Fixed_T;
      Y     : Fixed_T) with
      Convention => C;

   type Touch_Frame_Subprogram_Ptr is access procedure (Data : Void_Ptr; Touch : Touch_Ptr) with
      Convention => C;

   type Touch_Cancel_Subprogram_Ptr is access procedure (Data : Void_Ptr; Touch : Touch_Ptr) with
      Convention => C;

   type Touch_Shape_Subprogram_Ptr is access procedure
     (Data  : Void_Ptr;
      Touch : Touch_Ptr;
      Id    : Integer;
      Major : Fixed_T;
      Minor : Fixed_T) with
      Convention => C;

   type Touch_Orientation_Subprogram_Ptr is access procedure
     (Data        : Void_Ptr;
      Touch       : Touch_Ptr;
      Id          : Integer;
      Orientation : Fixed_T) with
      Convention => C;

   type Touch_Listener_T is record
      Down        : Touch_Down_Subprogram_Ptr;
      Up          : Touch_Up_Subprogram_Ptr;
      Motion      : Touch_Motion_Subprogram_Ptr;
      Frame       : Touch_Frame_Subprogram_Ptr;
      Cancel      : Touch_Cancel_Subprogram_Ptr;
      Shape       : Touch_Shape_Subprogram_Ptr;
      Orientation : Touch_Orientation_Subprogram_Ptr;
   end record with
      Convention => C_Pass_By_Copy;

   type Touch_Listener_Ptr is access all Touch_Listener_T;

   function Touch_Add_Listener
     (Touch    : Touch_Ptr;
      Listener : Touch_Listener_Ptr;
      Data     : Void_Ptr) return Interfaces.C.int;

   procedure Touch_Set_User_Data (Touch : Touch_Ptr; Data : Void_Ptr);

   function Touch_Get_User_Data (Touch : Touch_Ptr) return Void_Ptr;

   function Touch_Get_Version (Touch : Touch_Ptr) return Interfaces.Unsigned_32;

   procedure Touch_Destroy (Touch : Touch_Ptr);

   procedure Touch_Release (Touch : Touch_Ptr);

   type Output_Subpixel_T is new Interfaces.Unsigned_32;
-- unknown geometry
   Output_Unknown_Unknown : constant Output_Subpixel_T := 0;

-- no geometry
   Output_None_None : constant Output_Subpixel_T := 1;

-- horizontal RGB
   Output_Horizontal_Rgb_Horizontal_Rgb : constant Output_Subpixel_T := 2;

-- horizontal BGR
   Output_Horizontal_Bgr_Horizontal_Bgr : constant Output_Subpixel_T := 3;

-- vertical RGB
   Output_Vertical_Rgb_Vertical_Rgb : constant Output_Subpixel_T := 4;

-- vertical BGR
   Output_Vertical_Bgr_Vertical_Bgr : constant Output_Subpixel_T := 5;

   type Output_Transform_T is new Interfaces.Unsigned_32;
-- no transform
   Output_Normal_Normal : constant Output_Transform_T := 0;

-- 90 degrees counter-clockwise
   Output_90_90 : constant Output_Transform_T := 1;

-- 180 degrees counter-clockwise
   Output_180_180 : constant Output_Transform_T := 2;

-- 270 degrees counter-clockwise
   Output_270_270 : constant Output_Transform_T := 3;

-- 180 degree flip around a vertical axis
   Output_Flipped_Flipped : constant Output_Transform_T := 4;

-- flip and rotate 90 degrees counter-clockwise
   Output_Flipped_90_Flipped_90 : constant Output_Transform_T := 5;

-- flip and rotate 180 degrees counter-clockwise
   Output_Flipped_180_Flipped_180 : constant Output_Transform_T := 6;

-- flip and rotate 270 degrees counter-clockwise
   Output_Flipped_270_Flipped_270 : constant Output_Transform_T := 7;

   type Output_Mode_T is new Interfaces.Unsigned_32;
-- indicates this is the current mode
   Output_Current_Current : constant Output_Mode_T := 1;

-- indicates this is the preferred mode
   Output_Preferred_Preferred : constant Output_Mode_T := 2;

   type Output_Geometry_Subprogram_Ptr is access procedure
     (Data            : Void_Ptr;
      Output          : Output_Ptr;
      X               : Integer;
      Y               : Integer;
      Physical_Width  : Integer;
      Physical_Height : Integer;
      Subpixel        : Integer;
      Make            : Interfaces.C.Strings.chars_ptr;
      Model           : Interfaces.C.Strings.chars_ptr;
      Transform       : Integer) with
      Convention => C;

   type Output_Mode_Subprogram_Ptr is access procedure
     (Data    : Void_Ptr;
      Output  : Output_Ptr;
      Flags   : Interfaces.Unsigned_32;
      Width   : Integer;
      Height  : Integer;
      Refresh : Integer) with
      Convention => C;

   type Output_Done_Subprogram_Ptr is access procedure (Data : Void_Ptr; Output : Output_Ptr) with
      Convention => C;

   type Output_Scale_Subprogram_Ptr is access procedure (Data : Void_Ptr; Output : Output_Ptr; Factor : Integer) with
      Convention => C;

   type Output_Listener_T is record
      Geometry : Output_Geometry_Subprogram_Ptr;
      Mode     : Output_Mode_Subprogram_Ptr;
      Done     : Output_Done_Subprogram_Ptr;
      Scale    : Output_Scale_Subprogram_Ptr;
   end record with
      Convention => C_Pass_By_Copy;

   type Output_Listener_Ptr is access all Output_Listener_T;

   function Output_Add_Listener
     (Output   : Output_Ptr;
      Listener : Output_Listener_Ptr;
      Data     : Void_Ptr) return Interfaces.C.int;

   procedure Output_Set_User_Data (Output : Output_Ptr; Data : Void_Ptr);

   function Output_Get_User_Data (Output : Output_Ptr) return Void_Ptr;

   function Output_Get_Version (Output : Output_Ptr) return Interfaces.Unsigned_32;

   procedure Output_Destroy (Output : Output_Ptr);

   procedure Output_Release (Output : Output_Ptr);

   procedure Region_Set_User_Data (Region : Region_Ptr; Data : Void_Ptr);

   function Region_Get_User_Data (Region : Region_Ptr) return Void_Ptr;

   function Region_Get_Version (Region : Region_Ptr) return Interfaces.Unsigned_32;

   procedure Region_Destroy (Region : Region_Ptr);

   procedure Region_Add (Region : Region_Ptr; X : Integer; Y : Integer; Width : Integer; Height : Integer);

   procedure Region_Subtract (Region : Region_Ptr; X : Integer; Y : Integer; Width : Integer; Height : Integer);

   type Subcompositor_Error_T is new Interfaces.Unsigned_32;
-- the to-be sub-surface is invalid
   Subcompositor_Bad_Surface_Bad_Surface : constant Subcompositor_Error_T := 0;

   procedure Subcompositor_Set_User_Data (Subcompositor : Subcompositor_Ptr; Data : Void_Ptr);

   function Subcompositor_Get_User_Data (Subcompositor : Subcompositor_Ptr) return Void_Ptr;

   function Subcompositor_Get_Version (Subcompositor : Subcompositor_Ptr) return Interfaces.Unsigned_32;

   procedure Subcompositor_Destroy (Subcompositor : Subcompositor_Ptr);

-- Create a sub-surface interface for the given surface, and
-- associate it with the given parent surface. This turns a
-- plain wl_surface into a sub-surface.
--
-- The to-be sub-surface must not already have another role, and it
-- must not have an existing wl_subsurface object. Otherwise a protocol
-- error is raised.
--
-- Adding sub-surfaces to a parent is a double-buffered operation on the
-- parent (see wl_surface.commit). The effect of adding a sub-surface
-- becomes visible on the next time the state of the parent surface is
-- applied.
--
-- This request modifies the behaviour of wl_surface.commit request on
-- the sub-surface, see the documentation on wl_subsurface interface.
   function Subcompositor_Get_Subsurface
     (Subcompositor : Subcompositor_Ptr;
      Surface       : Surface_Ptr;
      Parent        : Surface_Ptr) return Subsurface_Ptr;

   type Subsurface_Error_T is new Interfaces.Unsigned_32;
-- wl_surface is not a sibling or the parent
   Subsurface_Bad_Surface_Bad_Surface : constant Subsurface_Error_T := 0;

   procedure Subsurface_Set_User_Data (Subsurface : Subsurface_Ptr; Data : Void_Ptr);

   function Subsurface_Get_User_Data (Subsurface : Subsurface_Ptr) return Void_Ptr;

   function Subsurface_Get_Version (Subsurface : Subsurface_Ptr) return Interfaces.Unsigned_32;

   procedure Subsurface_Destroy (Subsurface : Subsurface_Ptr);

   procedure Subsurface_Set_Position (Subsurface : Subsurface_Ptr; X : Integer; Y : Integer);

   procedure Subsurface_Place_Above (Subsurface : Subsurface_Ptr; Sibling : Surface_Ptr);

   procedure Subsurface_Place_Below (Subsurface : Subsurface_Ptr; Sibling : Surface_Ptr);

   procedure Subsurface_Set_Sync (Subsurface : Subsurface_Ptr);

   procedure Subsurface_Set_Desync (Subsurface : Subsurface_Ptr);

private

   type Proxy_T is limited null record;

end Wl_Thin;
