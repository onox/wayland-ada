private with Interfaces.C.Strings;
private with Wayland.Thin;

with C_Binding.Linux.Files;

package Wayland.Client is

   type Display;
   type Registry;
   type Callback;
   type Compositor;
   type Shm_Pool;
   type Shm;
   type Buffer;
   type Data_Offer;
   type Data_Source;
   type Data_Device;
   type Data_Device_Manager;
   type Shell;
   type Shell_Surface;
   type Surface;
   type Seat;
   type Pointer;
   type Keyboard;
   type Touch;
   type Output;
   type Region;
   type Subcompositor;
   type Subsurface;

   pragma Linker_Options ("-lwayland-client");
   --  Added this linker option here to avoid adding it
   --  to each gpr file that with's this Wayland Ada binding.

   type Call_Result_Code is (
                             Success,
                             Error
                            );

   type Fixed is new Integer;

   type Wayland_Array_T is record
      Size  : Unsigned_32;
      Alloc : Unsigned_32;
      Data  : Void_Ptr;
   end record with
      Convention => C_Pass_By_Copy;
      --   TODO: Remove the trailing _T from the name of this type

   type Interface_Type is tagged limited private;
   --  This type name ends with _Type because 'interface'
   --  is a reserved keyword in the Ada programming language.

   function Name (I : Interface_Type) return String
     with Global => null;

   Display_Interface : constant Interface_Type;

   Registry_Interface : constant Interface_Type;

   Callback_Interface : constant Interface_Type;

   Compositor_Interface : constant Interface_Type;

   Shm_Pool_Interface : constant Interface_Type;

   Shm_Interface : constant Interface_Type;

   Buffer_Interface : constant Interface_Type;

   Data_Offer_Interface : constant Interface_Type;

   Data_Source_Interface : constant Interface_Type;

   Data_Device_Interface : constant Interface_Type;

   Data_Device_Manager_Interface : constant Interface_Type;

   Shell_Interface : constant Interface_Type;

   Shell_Surface_Interface : constant Interface_Type;

   Surface_Interface : constant Interface_Type;

   Seat_Interface : constant Interface_Type;

   Pointer_Interface : constant Interface_Type;

   Keyboard_Interface : constant Interface_Type;

   Touch_Interface : constant Interface_Type;

   Output_Interface : constant Interface_Type;

   Region_Interface : constant Interface_Type;

   Subcompositor_Interface : constant Interface_Type;

   Subsurface_Interface : constant Interface_Type;

   Default_Display_Name : constant String := "wayland-0";

   Display_Error_Invalid_Object : constant Unsigned_32 := 0;
   --  server couldn't find object

   Display_Error_Invalid_Method : constant Unsigned_32 := 1;
   --  method doesn't exist on the specified interface

   Display_Error_No_Memory : constant Unsigned_32 := 2;
   --  server is out of memory

   Shm_Error_Invalid_Format : constant Unsigned_32 := 0;
   --  buffer format is not known

   Shm_Error_Invalid_Stride : constant Unsigned_32 := 1;
   --  invalid size or stride during pool or buffer creation

   Shm_Error_Invalid_Fd : constant Unsigned_32 := 2;
   --  mmapping the file descriptor failed

   type Shm_Format is new Unsigned_32;

   Shm_Format_Argb_8888 : constant Shm_Format := 0;
   --  32-bit ARGB format, [31:0] A:R:G:B 8:8:8:8 little endian

   Shm_Format_Xrgb_8888 : constant Shm_Format := 1;
   --  32-bit RGB format, [31:0] x:R:G:B 8:8:8:8 little endian

   Shm_Format_C_8 : constant Shm_Format := 538982467;
   --  8-bit color index format, [7:0] C

   Shm_Format_Rgb_332 : constant Shm_Format := 943867730;
   --  8-bit RGB format, [7:0] R:G:B 3:3:2

   Shm_Format_Bgr_233 : constant Shm_Format := 944916290;
   --  8-bit BGR format, [7:0] B:G:R 2:3:3

   Shm_Format_Xrgb_4444 : constant Shm_Format := 842093144;
   --  16-bit xRGB format, [15:0] x:R:G:B 4:4:4:4 little endian

   Shm_Format_Xbgr_4444 : constant Shm_Format := 842089048;
   --  16-bit xBGR format, [15:0] x:B:G:R 4:4:4:4 little endian

   Shm_Format_Rgbx_4444 : constant Shm_Format := 842094674;
   --  16-bit RGBx format, [15:0] R:G:B:x 4:4:4:4 little endian

   Shm_Format_Bgrx_4444 : constant Shm_Format := 842094658;
   --  16-bit BGRx format, [15:0] B:G:R:x 4:4:4:4 little endian

   Shm_Format_Argb_4444 : constant Shm_Format := 842093121;
   --  16-bit ARGB format, [15:0] A:R:G:B 4:4:4:4 little endian

   Shm_Format_Abgr_4444 : constant Shm_Format := 842089025;
   --  16-bit ABGR format, [15:0] A:B:G:R 4:4:4:4 little endian

   Shm_Format_Rgba_4444 : constant Shm_Format := 842088786;
   --  16-bit RBGA format, [15:0] R:G:B:A 4:4:4:4 little endian

   Shm_Format_Bgra_4444 : constant Shm_Format := 842088770;
   --  16-bit BGRA format, [15:0] B:G:R:A 4:4:4:4 little endian

   Shm_Format_Xrgb_1555 : constant Shm_Format := 892424792;
   --  16-bit xRGB format, [15:0] x:R:G:B 1:5:5:5 little endian

   Shm_Format_Xbgr_1555 : constant Shm_Format := 892420696;
   --  16-bit xBGR 1555 format, [15:0] x:B:G:R 1:5:5:5 little endian

   Shm_Format_Rgbx_5551 : constant Shm_Format := 892426322;
   --  16-bit RGBx 5551 format, [15:0] R:G:B:x 5:5:5:1 little endian

   Shm_Format_Bgrx_5551 : constant Shm_Format := 892426306;
   --  16-bit BGRx 5551 format, [15:0] B:G:R:x 5:5:5:1 little endian

   Shm_Format_Argb_1555 : constant Shm_Format := 892424769;
   --  16-bit ARGB 1555 format, [15:0] A:R:G:B 1:5:5:5 little endian

   Shm_Format_Abgr_1555 : constant Shm_Format := 892420673;
   --  16-bit ABGR 1555 format, [15:0] A:B:G:R 1:5:5:5 little endian

   Shm_Format_Rgba_5551 : constant Shm_Format := 892420434;
   --  16-bit RGBA 5551 format, [15:0] R:G:B:A 5:5:5:1 little endian

   Shm_Format_Bgra_5551 : constant Shm_Format := 892420418;
   --  16-bit BGRA 5551 format, [15:0] B:G:R:A 5:5:5:1 little endian

   Shm_Format_Rgb_565 : constant Shm_Format := 909199186;
   --  16-bit RGB 565 format, [15:0] R:G:B 5:6:5 little endian

   Shm_Format_Bgr_565 : constant Shm_Format := 909199170;
   --  16-bit BGR 565 format, [15:0] B:G:R 5:6:5 little endian

   Shm_Format_Rgb_888 : constant Shm_Format := 875710290;
   --  24-bit RGB format, [23:0] R:G:B little endian

   Shm_Format_Bgr_888 : constant Shm_Format := 875710274;
   --  24-bit BGR format, [23:0] B:G:R little endian

   Shm_Format_Xbgr_8888 : constant Shm_Format := 875709016;
   --  32-bit xBGR format, [31:0] x:B:G:R 8:8:8:8 little endian

   Shm_Format_Rgbx_8888 : constant Shm_Format := 875714642;
   --  32-bit RGBx format, [31:0] R:G:B:x 8:8:8:8 little endian

   Shm_Format_Bgrx_8888 : constant Shm_Format := 875714626;
   --  32-bit BGRx format, [31:0] B:G:R:x 8:8:8:8 little endian

   Shm_Format_Abgr_8888 : constant Shm_Format := 875708993;
   --  32-bit ABGR format, [31:0] A:B:G:R 8:8:8:8 little endian

   Shm_Format_Rgba_8888 : constant Shm_Format := 875708754;
   --  32-bit RGBA format, [31:0] R:G:B:A 8:8:8:8 little endian

   Shm_Format_Bgra_8888 : constant Shm_Format := 875708738;
   --  32-bit BGRA format, [31:0] B:G:R:A 8:8:8:8 little endian

   Shm_Format_Xrgb_2101010 : constant Shm_Format := 808669784;
   --  32-bit xRGB format, [31:0] x:R:G:B 2:10:10:10 little endian

   Shm_Format_Xbgr_2101010 : constant Shm_Format := 808665688;
   --  32-bit xBGR format, [31:0] x:B:G:R 2:10:10:10 little endian

   Shm_Format_Rgbx_1010102 : constant Shm_Format := 808671314;
   --  32-bit RGBx format, [31:0] R:G:B:x 10:10:10:2 little endian

   Shm_Format_Bgrx_1010102 : constant Shm_Format := 808671298;
   --  32-bit BGRx format, [31:0] B:G:R:x 10:10:10:2 little endian

   Shm_Format_Argb_2101010 : constant Shm_Format := 808669761;
   --  32-bit ARGB format, [31:0] A:R:G:B 2:10:10:10 little endian

   Shm_Format_Abgr_2101010 : constant Shm_Format := 808665665;
   --  32-bit ABGR format, [31:0] A:B:G:R 2:10:10:10 little endian

   Shm_Format_Rgba_1010102 : constant Shm_Format := 808665426;
   --  32-bit RGBA format, [31:0] R:G:B:A 10:10:10:2 little endian

   Shm_Format_Bgra_1010102 : constant Shm_Format := 808665410;
   --  32-bit BGRA format, [31:0] B:G:R:A 10:10:10:2 little endian

   Shm_Format_Yuyv : constant Shm_Format := 1448695129;
   --  packed YCbCr format, [31:0] Cr0:Y1:Cb0:Y0 8:8:8:8 little endian

   Shm_Format_Yvyu : constant Shm_Format := 1431918169;
   --  packed YCbCr format, [31:0] Cb0:Y1:Cr0:Y0 8:8:8:8 little endian

   Shm_Format_Uyvy : constant Shm_Format := 1498831189;
   --  packed YCbCr format, [31:0] Y1:Cr0:Y0:Cb0 8:8:8:8 little endian

   Shm_Format_Vyuy : constant Shm_Format := 1498765654;
   --  packed YCbCr format, [31:0] Y1:Cb0:Y0:Cr0 8:8:8:8 little endian

   Shm_Format_Ayuv : constant Shm_Format := 1448433985;
   --  packed AYCbCr format, [31:0] A:Y:Cb:Cr 8:8:8:8 little endian

   Shm_Format_Nv_12 : constant Shm_Format := 842094158;
   --  2 plane YCbCr Cr:Cb format, 2x2 subsampled Cr:Cb plane

   Shm_Format_Nv_21 : constant Shm_Format := 825382478;
   --  2 plane YCbCr Cb:Cr format, 2x2 subsampled Cb:Cr plane

   Shm_Format_Nv_16 : constant Shm_Format := 909203022;
   --  2 plane YCbCr Cr:Cb format, 2x1 subsampled Cr:Cb plane

   Shm_Format_Nv_61 : constant Shm_Format := 825644622;
   --  2 plane YCbCr Cb:Cr format, 2x1 subsampled Cb:Cr plane

   Shm_Format_Yuv_410 : constant Shm_Format := 961959257;
   --  3 plane YCbCr format, 4x4 subsampled Cb (1) and Cr (2) planes

   Shm_Format_Yvu_410 : constant Shm_Format := 961893977;
   --  3 plane YCbCr format, 4x4 subsampled Cr (1) and Cb (2) planes

   Shm_Format_Yuv_411 : constant Shm_Format := 825316697;
   --  3 plane YCbCr format, 4x1 subsampled Cb (1) and Cr (2) planes

   Shm_Format_Yvu_411 : constant Shm_Format := 825316953;
   --  3 plane YCbCr format, 4x1 subsampled Cr (1) and Cb (2) planes

   Shm_Format_Yuv_420 : constant Shm_Format := 842093913;
   --  3 plane YCbCr format, 2x2 subsampled Cb (1) and Cr (2) planes

   Shm_Format_Yvu_420 : constant Shm_Format := 842094169;
   --  3 plane YCbCr format, 2x2 subsampled Cr (1) and Cb (2) planes

   Shm_Format_Yuv_422 : constant Shm_Format := 909202777;
   --  3 plane YCbCr format, 2x1 subsampled Cb (1) and Cr (2) planes

   Shm_Format_Yvu_422 : constant Shm_Format := 909203033;
   --  3 plane YCbCr format, 2x1 subsampled Cr (1) and Cb (2) planes

   Shm_Format_Yuv_444 : constant Shm_Format := 875713881;
   --  3 plane YCbCr format, non-subsampled Cb (1) and Cr (2) planes

   Shm_Format_Yvu_444 : constant Shm_Format := 875714137;
   --  3 plane YCbCr format, non-subsampled Cr (1) and Cb (2) planes

   Data_Offer_Error_Invalid_Finish : constant Unsigned_32 := 0;
   --  finish request was called untimely

   Data_Offer_Error_Invalid_Action_Mask : constant Unsigned_32 := 1;
   --  action mask contains invalid values

   Data_Offer_Error_Invalid_Action : constant Unsigned_32 := 2;
   --  action argument has an invalid value

   Data_Offer_Error_Invalid_Offer : constant Unsigned_32 := 3;
   --  offer doesn't accept this request

   Data_Source_Error_Invalid_Action_Mask : constant Unsigned_32 := 0;
   --  action mask contains invalid values

   Data_Source_Error_Invalid_Source : constant Unsigned_32 := 1;
   --  source doesn't accept this request

   Data_Device_Error_Role : constant Unsigned_32 := 0;
   --  given wl_surface has another role

   Data_Device_Manager_Dnd_Action_None : constant Unsigned_32 := 0;
   --  no action

   Data_Device_Manager_Dnd_Action_Copy : constant Unsigned_32 := 1;
   --  copy action

   Data_Device_Manager_Dnd_Action_Move : constant Unsigned_32 := 2;
   --  move action

   Data_Device_Manager_Dnd_Action_Ask : constant Unsigned_32 := 4;
   --  ask action

   Shell_Error_Role : constant Unsigned_32 := 0;
   --  given wl_surface has another role

   Shell_Surface_Resize_None : constant Unsigned_32 := 0;
   --  no edge

   Shell_Surface_Resize_Top : constant Unsigned_32 := 1;
   --  top edge

   Shell_Surface_Resize_Bottom : constant Unsigned_32 := 2;
   --  bottom edge

   Shell_Surface_Resize_Left : constant Unsigned_32 := 4;
   --  left edge

   Shell_Surface_Resize_Top_Left : constant Unsigned_32 := 5;
   --  top and left edges

   Shell_Surface_Resize_Bottom_Left : constant Unsigned_32 := 6;
   --  bottom and left edges

   Shell_Surface_Resize_Right : constant Unsigned_32 := 8;
   --  right edge

   Shell_Surface_Resize_Top_Right : constant Unsigned_32 := 9;
   --  top and right edges

   Shell_Surface_Resize_Bottom_Right : constant Unsigned_32 := 10;
   --  bottom and right edges

   Shell_Surface_Transient_Inactive : constant Unsigned_32 := 1;
   --  do not set keyboard focus

   Shell_Surface_Fullscreen_Method_Default : constant Unsigned_32 := 0;
   --  no preference, apply default policy

   Shell_Surface_Fullscreen_Method_Scale : constant Unsigned_32 := 1;
   --  scale, preserve the surface's aspect ratio and center on output

   Shell_Surface_Fullscreen_Method_Driver : constant Unsigned_32 := 2;
   --  switch output mode to the smallest mode that can fit the surface,
   --  add black borders to compensate size mismatch

   Shell_Surface_Fullscreen_Method_Fill : constant Unsigned_32 := 3;
   --  no upscaling, center on output and add black borders to compensate size mismatch

   Surface_Error_Invalid_Scale : constant Unsigned_32 := 0;
   --  buffer scale value is invalid

   Surface_Error_Invalid_Transform : constant Unsigned_32 := 1;
   --  buffer transform value is invalid

   Seat_Capability_Pointer : constant Unsigned_32 := 1;
   --  the seat has pointer devices

   Seat_Capability_Keyboard : constant Unsigned_32 := 2;
   --  the seat has one or more keyboards

   Seat_Capability_Touch : constant Unsigned_32 := 4;
   --  the seat has touch devices

   Pointer_Error_Role : constant Unsigned_32 := 0;
   --  given wl_surface has another role

   Pointer_Button_State_Released : constant Unsigned_32 := 0;
   --  the button is not pressed

   Pointer_Button_State_Pressed : constant Unsigned_32 := 1;
   --  the button is pressed

   Pointer_Axis_Vertical_Scroll : constant Unsigned_32 := 0;
   --  vertical axis

   Pointer_Axis_Horizontal_Scroll : constant Unsigned_32 := 1;
   --  horizontal axis

   Pointer_Axis_Source_Wheel : constant Unsigned_32 := 0;
   --  a physical wheel rotation

   Pointer_Axis_Source_Finger : constant Unsigned_32 := 1;
   --  finger on a touch surface

   Pointer_Axis_Source_Continuous : constant Unsigned_32 := 2;
   --  continuous coordinate space

   Pointer_Axis_Source_Wheel_Tilt : constant Unsigned_32 := 3;
   --  a physical wheel tilt

   Keyboard_Keymap_Format_No_Keymap : constant Unsigned_32 := 0;
   --  no keymap; client must understand how to interpret the raw keycode

   Keyboard_Keymap_Format_Xkb_V_1 : constant Unsigned_32 := 1;
   --  libxkbcommon compatible; to determine the xkb keycode, clients must add 8 to the key event keycode

   Keyboard_Key_State_Released : constant Unsigned_32 := 0;
   --  key is not pressed

   Keyboard_Key_State_Pressed : constant Unsigned_32 := 1;
   --  key is pressed

   Output_Subpixel_Unknown : constant Unsigned_32 := 0;
   --  unknown geometry

   Output_Subpixel_None : constant Unsigned_32 := 1;
   --  no geometry

   Output_Subpixel_Horizontal_Rgb : constant Unsigned_32 := 2;
   --  horizontal RGB

   Output_Subpixel_Horizontal_Bgr : constant Unsigned_32 := 3;
   --  horizontal BGR

   Output_Subpixel_Vertical_Rgb : constant Unsigned_32 := 4;
   --  vertical RGB

   Output_Subpixel_Vertical_Bgr : constant Unsigned_32 := 5;
   --  vertical BGR

   Output_Transform_Normal : constant Unsigned_32 := 0;
   --  no transform

   Output_Transform_90 : constant Unsigned_32 := 1;
   --  90 degrees counter-clockwise

   Output_Transform_180 : constant Unsigned_32 := 2;
   --  180 degrees counter-clockwise

   Output_Transform_270 : constant Unsigned_32 := 3;
   --  270 degrees counter-clockwise

   Output_Transform_Flipped : constant Unsigned_32 := 4;
   --  180 degree flip around a vertical axis

   Output_Transform_Flipped_90 : constant Unsigned_32 := 5;
   --  flip and rotate 90 degrees counter-clockwise

   Output_Transform_Flipped_180 : constant Unsigned_32 := 6;
   --  flip and rotate 180 degrees counter-clockwise

   Output_Transform_Flipped_270 : constant Unsigned_32 := 7;
   --  flip and rotate 270 degrees counter-clockwise

   Output_Mode_Current : constant Unsigned_32 := 1;
   --  indicates this is the current mode

   Output_Mode_Preferred : constant Unsigned_32 := 2;
   --  indicates this is the preferred mode

   Subcompositor_Error_Bad_Surface : constant Unsigned_32 := 0;
   --  the to-be sub-surface is invalid

   Subsurface_Error_Bad_Surface : constant Unsigned_32 := 0;
   --  wl_surface is not a sibling or the parent

   type Subcompositor is tagged limited private;

   function Has_Proxy (Subcompositor : Wayland.Client.Subcompositor) return Boolean;

   procedure Destroy (S : in out Wayland.Client.Subcompositor) with
     Pre    => S.Has_Proxy,
     Post   => not S.Has_Proxy;

   function Get_Version (S : Wayland.Client.Subcompositor) return Unsigned_32 with
     Pre => S.Has_Proxy;

   procedure Get_Subsurface (Subcompositor : Wayland.Client.Subcompositor;
                             Surface       : Wayland.Client.Surface;
                             Parent        : Wayland.Client.Surface;
                             Subsurface    : in out Wayland.Client.Subsurface);
   --  Create a sub-surface interface for the given surface, and
   --  associate it with the given parent surface. This turns a
   --  plain wl_surface into a sub-surface.
   --
   --  The to-be sub-surface must not already have another role, and it
   --  must not have an existing wl_subsurface object. Otherwise a protocol
   --  error is raised.
   --
   --  Adding sub-surfaces to a parent is a double-buffered operation on the
   --  parent (see wl_surface.commit). The effect of adding a sub-surface
   --  becomes visible on the next time the state of the parent surface is
   --  applied.
   --
   --  This request modifies the behaviour of wl_surface.commit request on
   --  the sub-surface, see the documentation on wl_subsurface interface.

   type Compositor is tagged limited private;

   function Has_Proxy (Compositor : Wayland.Client.Compositor) return Boolean;

   procedure Get_Proxy (Compositor  : in out Wayland.Client.Compositor;
                        Registry    : Wayland.Client.Registry;
                        Id          : Unsigned_32;
                        Version     : Unsigned_32) with
     Pre => not Compositor.Has_Proxy and Has_Proxy (Registry);

   procedure Get_Surface_Proxy (Compositor : Wayland.Client.Compositor;
                                Surface    : in out Wayland.Client.Surface) with
     Pre => Compositor.Has_Proxy;
   --  Ask the compositor to create a new surface. When success, Surface has
   --  When success Surface.Has_Proxy = True,
   --  otherwise Surface.Has_Proxy = False.

   procedure Get_Region_Proxy (Compositor : Wayland.Client.Compositor;
                               Region     : in out Wayland.Client.Region) with
     Pre => Compositor.Has_Proxy;
   --  Ask the compositor to create a new region.
   --  When success Region.Has_Proxy = True, otherwise Region.Has_Proxy = False.

   procedure Destroy (Compositor : in out Wayland.Client.Compositor) with
     Pre  => Compositor.Has_Proxy,
     Post => not Compositor.Has_Proxy;

   type Shell is tagged limited private;

   function Has_Proxy (Shell : Wayland.Client.Shell) return Boolean;

   procedure Get_Proxy (Shell    : in out Wayland.Client.Shell;
                        Registry : Wayland.Client.Registry;
                        Id       : Unsigned_32;
                        Version  : Unsigned_32) with
     Pre => Has_Proxy (Registry);

   procedure Get_Shell_Surface
     (Shell         : Wayland.Client.Shell;
      Surface       : Wayland.Client.Surface;
      Shell_Surface : in out Wayland.Client.Shell_Surface) with
     Pre => Shell.Has_Proxy;
   --  Create a shell surface for an existing surface. This gives
   --  the Surface the role of a shell surface. If the Surface
   --  already has another role, it raises a protocol error.
   --
   --  Only one shell surface can be associated with a given surface.

   type Shell_Surface is tagged limited private;

   function Has_Proxy (Surface : Shell_Surface) return Boolean;

   function Get_Version (Surface : Shell_Surface) return Unsigned_32 with
     Pre => Surface.Has_Proxy;

   procedure Destroy (Surface : in out Shell_Surface) with
     Pre  => Surface.Has_Proxy,
     Post => not Surface.Has_Proxy;

   procedure Pong (Surface : Shell_Surface;
                   Serial  : Unsigned_32) with
     Pre => Surface.Has_Proxy;
   --  A client must respond to a ping event with a pong request or
   --  the client may be deemed unresponsive.

   procedure Move (Surface : Shell_Surface;
                   Seat    : Wayland.Client.Seat;
                   Serial  : Unsigned_32) with
     Pre => Surface.Has_Proxy;
   --  Start a pointer-driven move of the surface.
   --
   --  This request must be used in response to a button press event.
   --  The server may ignore move requests depending on the state of
   --  the surface (e.g. fullscreen or maximized).

   procedure Resize (Surface : Shell_Surface;
                     Seat    : Wayland.Client.Seat;
                     Serial  : Unsigned_32;
                     Edges   : Unsigned_32) with
     Pre => Surface.Has_Proxy;
   --  Start a pointer-driven resizing of the surface.
   --
   --  This request must be used in response to a button press event.
   --  The server may ignore resize requests depending on the state of
   --  the surface (e.g. fullscreen or maximized).

   procedure Set_Toplevel (Surface : Shell_Surface) with
     Pre => Surface.Has_Proxy;
   --  Map the surface as a toplevel surface.
   --
   --  A toplevel surface is not fullscreen, maximized or transient.

   procedure Set_Transient (Surface : Shell_Surface;
                            Parent  : Wayland.Client.Surface;
                            X       : Integer;
                            Y       : Integer;
                            Flags   : Unsigned_32) with
     Pre => Surface.Has_Proxy;
   --  Map the surface relative to an existing surface.
   --
   --  The x and y arguments specify the location of the upper left
   --  corner of the surface relative to the upper left corner of the
   --  parent surface, in surface-local coordinates.
   --
   --  The flags argument controls details of the transient behaviour.

   procedure Set_Fullscreen (Surface   : Shell_Surface;
                             Method    : Unsigned_32;
                             Framerate : Unsigned_32;
                             Output    : Wayland.Client.Output) with
     Pre => Surface.Has_Proxy;
   --  Map the surface as a fullscreen surface.
   --
   --  If an output parameter is given then the surface will be made
   --  fullscreen on that output. If the client does not specify the
   --  output then the compositor will apply its policy - usually
   --  choosing the output on which the surface has the biggest surface
   --  area.
   --
   --  The client may specify a method to resolve a size conflict
   --  between the output size and the surface size - this is provided
   --  through the method parameter.
   --
   --  The framerate parameter is used only when the method is set
   --  to "driver", to indicate the preferred framerate. A value of 0
   --  indicates that the client does not care about framerate.  The
   --  framerate is specified in mHz, that is framerate of 60000 is 60Hz.
   --
   --  A method of "scale" or "driver" implies a scaling operation of
   --  the surface, either via a direct scaling operation or a change of
   --  the output mode. This will override any kind of output scaling, so
   --  that mapping a surface with a buffer size equal to the mode can
   --  fill the screen independent of buffer_scale.
   --
   --  A method of "fill" means we don't scale up the buffer, however
   --  any output scale is applied. This means that you may run into
   --  an edge case where the application maps a buffer with the same
   --  size of the output mode but buffer_scale 1 (thus making a
   --  surface larger than the output). In this case it is allowed to
   --  downscale the results to fit the screen.
   --
   --  The compositor must reply to this request with a configure event
   --  with the dimensions for the output on which the surface will
   --  be made fullscreen.

   procedure Set_Popup (Surface : Shell_Surface;
                        Seat    : Wayland.Client.Seat;
                        Serial  : Unsigned_32;
                        Parent  : Wayland.Client.Surface;
                        X       : Integer;
                        Y       : Integer;
                        Flags   : Unsigned_32) with
     Pre => Surface.Has_Proxy;
   --  Map the surface as a popup.
   --
   --  A popup surface is a transient surface with an added pointer
   --  grab.
   --
   --  An existing implicit grab will be changed to owner-events mode,
   --  and the popup grab will continue after the implicit grab ends
   --  (i.e. releasing the mouse button does not cause the popup to
   --  be unmapped).
   --
   --  The popup grab continues until the window is destroyed or a
   --  mouse button is pressed in any other client's window. A click
   --  in any of the client's surfaces is reported as normal, however,
   --  clicks in other clients' surfaces will be discarded and trigger
   --  the callback.
   --
   --  The x and y arguments specify the location of the upper left
   --  corner of the surface relative to the upper left corner of the
   --  parent surface, in surface-local coordinates.

   procedure Set_Maximized (Surface : Shell_Surface;
                            Output  : Wayland.Client.Output) with
     Pre => Surface.Has_Proxy;
   --  Map the surface as a maximized surface.
   --
   --  If an output parameter is given then the surface will be
   --  maximized on that output. If the client does not specify the
   --  output then the compositor will apply its policy - usually
   --  choosing the output on which the surface has the biggest surface
   --  area.
   --
   --  The compositor will reply with a configure event telling
   --  the expected new surface size. The operation is completed
   --  on the next buffer attach to this surface.
   --
   --  A maximized surface typically fills the entire output it is
   --  bound to, except for desktop elements such as panels. This is
   --  the main difference between a maximized shell surface and a
   --  fullscreen shell surface.
   --
   --  The details depend on the compositor implementation.

   procedure Set_Title (Surface : Shell_Surface;
                        Title   : String) with
     Pre => Surface.Has_Proxy;
   --  Set a short title for the surface.
   --
   --  This string may be used to identify the surface in a task bar,
   --  window list, or other user interface elements provided by the
   --  compositor.
   --
   --  The string must be encoded in UTF-8.

   procedure Set_Class (Surface : Shell_Surface;
                        Class_V : String) with
     Pre => Surface.Has_Proxy;
   --  Set a class for the surface.
   --
   --  The surface class identifies the general class of applications
   --  to which the surface belongs. A common convention is to use the
   --  file name (or the full path if it is a non-standard location) of
   --  the application's .desktop file as the class.

   type Data_Device_Manager is tagged limited private;

   function Has_Proxy (Data_Device_Manager : Wayland.Client.Data_Device_Manager) return Boolean;

   procedure Destroy (Manager : in out Data_Device_Manager) with
     Pre    => Manager.Has_Proxy,
     Post   => not Manager.Has_Proxy;

   function Get_Version (Manager : Data_Device_Manager) return Unsigned_32 with
     Pre => Manager.Has_Proxy;

   procedure Create_Data_Source (Manager : Data_Device_Manager;
                                 Source  : in out Data_Source);
   --  Create a new data source.

   procedure Get_Data_Device (Manager : Data_Device_Manager;
                              Seat    : Wayland.Client.Seat;
                              Device  : in out Data_Device);
   --  Create a new data device for a given seat.

   type Seat is tagged limited private;

   function Has_Proxy (Seat : Wayland.Client.Seat) return Boolean;

   function Get_Version (Seat : Wayland.Client.Seat) return Unsigned_32 with
     Pre => Seat.Has_Proxy;

   procedure Get_Proxy (Seat     : in out Wayland.Client.Seat;
                        Registry : Wayland.Client.Registry;
                        Id       : Unsigned_32;
                        Version  : Unsigned_32) with
     Pre => Has_Proxy (Registry);

   procedure Get_Pointer (Seat    : Wayland.Client.Seat;
                          Pointer : in out Wayland.Client.Pointer) with
     Pre => Seat.Has_Proxy and not Has_Proxy (Pointer);
   --  This request only takes effect if the seat has the pointer
   --  capability, or has had the pointer capability in the past.
   --  It is a protocol violation to issue this request on a seat that has
   --  never had the pointer capability.

   procedure Get_Keyboard (Seat     : Wayland.Client.Seat;
                           Keyboard : in out Wayland.Client.Keyboard) with
     Pre => Seat.Has_Proxy and not Has_Proxy (Keyboard);
   --  This request only takes effect if the seat has the keyboard
   --  capability, or has had the keyboard capability in the past.
   --  It is a protocol violation to issue this request on a seat that has
   --  never had the keyboard capability.

   procedure Get_Touch (Seat  : Wayland.Client.Seat;
                        Touch : in out Wayland.Client.Touch) with
     Pre => Seat.Has_Proxy and not Has_Proxy (Touch);
   --  This request only takes effect if the seat has the touch
   --  capability, or has had the touch capability in the past.
   --  It is a protocol violation to issue this request on a seat that has
   --  never had the touch capability.

   procedure Release (Seat : in out Wayland.Client.Seat) with
     Pre => Seat.Has_Proxy;
   --  Using this request a client can tell the server that it is not going to
   --  use the seat object anymore.

   type Pointer is tagged limited private;

   function Has_Proxy (Pointer : Wayland.Client.Pointer) return Boolean;

   function Get_Version (Pointer : Wayland.Client.Pointer) return Unsigned_32 with
     Pre => Pointer.Has_Proxy;

   procedure Destroy (Pointer : in out Wayland.Client.Pointer) with
     Pre  => Pointer.Has_Proxy,
     Post => not Pointer.Has_Proxy;

   procedure Set_Cursor (Pointer   : Wayland.Client.Pointer;
                         Serial    : Unsigned_32;
                         Surface   : Wayland.Client.Surface;
                         Hotspot_X : Integer;
                         Hotspot_Y : Integer) with
     Pre => Pointer.Has_Proxy;
   --  Set the pointer surface, i.e., the surface that contains the
   --  pointer image (cursor). This request gives the surface the role
   --  of a cursor. If the surface already has another role, it raises
   --  a protocol error.
   --
   --  The cursor actually changes only if the pointer
   --  focus for this device is one of the requesting client's surfaces
   --  or the surface parameter is the current pointer surface. If
   --  there was a previous surface set with this request it is
   --  replaced. If surface is NULL, the pointer image is hidden.
   --
   --  The parameters hotspot_x and hotspot_y define the position of
   --  the pointer surface relative to the pointer location. Its
   --  top-left corner is always at (x, y) - (hotspot_x, hotspot_y),
   --  where (x, y) are the coordinates of the pointer location, in
   --  surface-local coordinates.
   --
   --  On surface.attach requests to the pointer surface, hotspot_x
   --  and hotspot_y are decremented by the x and y parameters
   --  passed to the request. Attach must be confirmed by
   --  wl_surface.commit as usual.
   --
   --  The hotspot can also be updated by passing the currently set
   --  pointer surface to this request with new values for hotspot_x
   --  and hotspot_y.
   --
   --  The current and pending input regions of the wl_surface are
   --  cleared, and wl_surface.set_input_region is ignored until the
   --  wl_surface is no longer used as the cursor. When the use as a
   --  cursor ends, the current and pending input regions become
   --  undefined, and the wl_surface is unmapped.

   procedure Release (Pointer : in out Wayland.Client.Pointer) with
     Pre  => Pointer.Has_Proxy,
     Post => not Pointer.Has_Proxy;
   --  Using this request a client can tell the server that it is not going to
   --  use the pointer object anymore.
   --
   --  This request destroys the pointer proxy object, so clients must not call
   --  wl_pointer_destroy() after using this request.

   type Shm is tagged limited private;

   function Has_Proxy (Shm : Wayland.Client.Shm) return Boolean;

   procedure Get_Proxy (Shm      : in out Wayland.Client.Shm;
                        Registry : Wayland.Client.Registry;
                        Id       : Unsigned_32;
                        Version  : Unsigned_32) with
     Pre => Has_Proxy (Registry);

   procedure Create_Pool (Shm             : Wayland.Client.Shm;
                          File_Descriptor : C_Binding.Linux.Files.File;
                          Size            : Integer;
                          Pool            : in out Shm_Pool);
   --  Create a new Shm_Pool object.
   --
   --  The pool can be used to create shared memory based buffer
   --  objects.  The server will mmap size bytes of the passed file
   --  descriptor, to use as backing memory for the pool.

   function Get_Version (Shm : Wayland.Client.Shm) return Unsigned_32 with
     Pre => Shm.Has_Proxy;

   procedure Destroy (Shm : in out Wayland.Client.Shm) with
     Pre  => Shm.Has_Proxy,
     Post => not Shm.Has_Proxy;

   type Shm_Pool is tagged limited private;
   --  Shared memory pool.

   function Has_Proxy (Pool : Wayland.Client.Shm_Pool) return Boolean;

   procedure Create_Buffer (Pool   : Wayland.Client.Shm_Pool;
                            Offset : Integer;
                            Width  : Integer;
                            Height : Integer;
                            Stride : Integer;
                            Format : Shm_Format;
                            Buffer : in out Wayland.Client.Buffer) with
     Pre => Pool.Has_Proxy;
   --  Create a Buffer object from the pool.
   --
   --  The buffer is created offset bytes into the pool and has
   --  width and height as specified.  The stride argument specifies
   --  the number of bytes from the beginning of one row to the beginning
   --  of the next.  The format is the pixel format of the buffer and
   --  must be one of the constants of type Shm_Format.
   --
   --  A buffer will keep a reference to the pool it was created from
   --  so it is valid to destroy the pool immediately after creating
   --  a buffer from it.

   procedure Resize (Pool : Wayland.Client.Shm_Pool;
                     Size : Integer) with
     Pre => Pool.Has_Proxy;
   --  This request will cause the server to remap the backing memory
   --  for the pool from the file descriptor passed when the pool was
   --  created, but using the new size.  This request can only be
   --  used to make the pool bigger.

   function Get_Version (Pool : Wayland.Client.Shm_Pool) return Unsigned_32 with
     Pre => Pool.Has_Proxy;

   procedure Destroy (Pool : in out Wayland.Client.Shm_Pool) with
     Pre  => Pool.Has_Proxy,
     Post => not Pool.Has_Proxy;

   type Data_Device is tagged limited private;

   function Has_Proxy (Data_Device : Wayland.Client.Data_Device) return Boolean;

   procedure Destroy (Device : in out Data_Device) with
     Pre    => Device.Has_Proxy,
     Post   => not Device.Has_Proxy;

   function Get_Version (Device : Data_Device) return Unsigned_32 with
     Pre => Device.Has_Proxy;

   procedure Start_Drag (Device : Data_Device;
                         Source : Data_Source;
                         Origin : Surface;
                         Icon   : Surface;
                         Serial : Unsigned_32) with
     Pre => Device.Has_Proxy and Has_Proxy (Source)
                             and Has_Proxy (Origin) and Has_Proxy (Icon);
   --  This request asks the compositor to start a drag-and-drop
   --  operation on behalf of the client.
   --
   --  The source argument is the data source that provides the data
   --  for the eventual data transfer. If source is NULL, enter, leave
   --  and motion events are sent only to the client that initiated the
   --  drag and the client is expected to handle the data passing
   --  internally.
   --
   --  The origin surface is the surface where the drag originates and
   --  the client must have an active implicit grab that matches the
   --  serial.
   --
   --  The icon surface is an optional (can be NULL) surface that
   --  provides an icon to be moved around with the cursor.  Initially,
   --  the top-left corner of the icon surface is placed at the cursor
   --  hotspot, but subsequent wl_surface.attach request can move the
   --  relative position. Attach requests must be confirmed with
   --  wl_surface.commit as usual. The icon surface is given the role of
   --  a drag-and-drop icon. If the icon surface already has another role,
   --  it raises a protocol error.
   --
   --  The current and pending input regions of the icon wl_surface are
   --  cleared, and wl_surface.set_input_region is ignored until the
   --  wl_surface is no longer used as the icon surface. When the use
   --  as an icon ends, the current and pending input regions become
   --  undefined, and the wl_surface is unmapped.

   procedure Set_Selection (Device : Data_Device;
                            Source : Data_Source;
                            Serial : Unsigned_32) with
     Pre => Device.Has_Proxy and Has_Proxy (Source);
   --  This request asks the compositor to set the selection
   --  to the data from the source on behalf of the client.
   --
   --  To unset the selection, set the source to NULL.

   procedure Release (Device : in out Data_Device) with
     Pre    => Device.Has_Proxy,
     Post   => not Device.Has_Proxy;
   --  This request destroys the data device.

   type Subsurface is tagged limited private;

   function Has_Proxy (Subsurface : Wayland.Client.Subsurface) return Boolean;

   procedure Destroy (Subsurface : in out Wayland.Client.Subsurface) with
     Pre    => Subsurface.Has_Proxy,
     Post   => not Subsurface.Has_Proxy;

   function Get_Version (Subsurface : Wayland.Client.Subsurface) return Unsigned_32 with
     Pre => Subsurface.Has_Proxy;

   procedure Set_Position (Subsurface : Wayland.Client.Subsurface;
                           X          : Integer;
                           Y          : Integer) with
     Pre => Subsurface.Has_Proxy;
   --  This schedules a sub-surface position change.
   --  The sub-surface will be moved so that its origin (top left
   --  corner pixel) will be at the location x, y of the parent surface
   --  coordinate system. The coordinates are not restricted to the parent
   --  surface area. Negative values are allowed.
   --
   --  The scheduled coordinates will take effect whenever the state of the
   --  parent surface is applied. When this happens depends on whether the
   --  parent surface is in synchronized mode or not. See
   --  wl_subsurface.set_sync and wl_subsurface.set_desync for details.
   --
   --  If more than one set_position request is invoked by the client before
   --  the commit of the parent surface, the position of a new request always
   --  replaces the scheduled position from any previous request.
   --
   --  The initial position is 0, 0.

   procedure Place_Above (Subsurface : Wayland.Client.Subsurface;
                          Sibling    : Wayland.Client.Surface) with
     Pre => Subsurface.Has_Proxy and Has_Proxy (Sibling);
   --  This sub-surface is taken from the stack, and put back just
   --  above the reference surface, changing the z-order of the sub-surfaces.
   --  The reference surface must be one of the sibling surfaces, or the
   --  parent surface. Using any other surface, including this sub-surface,
   --  will cause a protocol error.
   --
   --  The z-order is double-buffered. Requests are handled in order and
   --  applied immediately to a pending state. The final pending state is
   --  copied to the active state the next time the state of the parent
   --  surface is applied. When this happens depends on whether the parent
   --  surface is in synchronized mode or not. See wl_subsurface.set_sync and
   --  wl_subsurface.set_desync for details.
   --
   --  A new sub-surface is initially added as the top-most in the stack
   --  of its siblings and parent.

   procedure Place_Below (Subsurface : Wayland.Client.Subsurface;
                          Sibling    : Wayland.Client.Surface) with
     Pre => Subsurface.Has_Proxy and Has_Proxy (Sibling);
   --  The sub-surface is placed just below the reference surface.
   --  See wl_subsurface.place_above.

   procedure Set_Sync (Subsurface : Wayland.Client.Subsurface) with
     Pre => Subsurface.Has_Proxy;
   --  Change the commit behaviour of the sub-surface to synchronized
   --  mode, also described as the parent dependent mode.
   --
   --  In synchronized mode, wl_surface.commit on a sub-surface will
   --  accumulate the committed state in a cache, but the state will
   --  not be applied and hence will not change the compositor output.
   --  The cached state is applied to the sub-surface immediately after
   --  the parent surface's state is applied. This ensures atomic
   --  updates of the parent and all its synchronized sub-surfaces.
   --  Applying the cached state will invalidate the cache, so further
   --  parent surface commits do not (re-)apply old state.
   --
   --  See wl_subsurface for the recursive effect of this mode.

   procedure Set_Desync (Subsurface : Wayland.Client.Subsurface) with
     Pre => Subsurface.Has_Proxy;
   --  Change the commit behaviour of the sub-surface to desynchronized
   --  mode, also described as independent or freely running mode.
   --
   --  In desynchronized mode, wl_surface.commit on a sub-surface will
   --  apply the pending state directly, without caching, as happens
   --  normally with a wl_surface. Calling wl_surface.commit on the
   --  parent surface has no effect on the sub-surface's wl_surface
   --  state. This mode allows a sub-surface to be updated on its own.
   --
   --  If cached state exists when wl_surface.commit is called in
   --  desynchronized mode, the pending state is added to the cached
   --  state, and applied as a whole. This invalidates the cache.
   --
   --  Note: even if a sub-surface is set to desynchronized, a parent
   --  sub-surface may override it to behave as synchronized. For details,
   --  see wl_subsurface.
   --
   --  If a surface's parent surface behaves as desynchronized, then
   --  the cached state is applied on set_desync.

   type Surface is tagged limited private;

   function Has_Proxy (Surface : Wayland.Client.Surface) return Boolean;

   procedure Attach (Surface : Wayland.Client.Surface;
                     Buffer  : Wayland.Client.Buffer;
                     X       : Integer;
                     Y       : Integer) with
     Pre => Surface.Has_Proxy and Has_Proxy (Buffer);
   --  Set a buffer as the content of this surface.
   --
   --  The new size of the surface is calculated based on the buffer
   --  size transformed by the inverse buffer_transform and the
   --  inverse buffer_scale. This means that the supplied buffer
   --  must be an integer multiple of the buffer_scale.
   --
   --  The x and y arguments specify the location of the new pending
   --  buffer's upper left corner, relative to the current buffer's upper
   --  left corner, in surface-local coordinates. In other words, the
   --  x and y, combined with the new surface size define in which
   --  directions the surface's size changes.
   --
   --  Surface contents are double-buffered state, see wl_surface.commit.
   --
   --  The initial surface contents are void; there is no content.
   --  wl_surface.attach assigns the given wl_buffer as the pending
   --  wl_buffer. wl_surface.commit makes the pending wl_buffer the new
   --  surface contents, and the size of the surface becomes the size
   --  calculated from the wl_buffer, as described above. After commit,
   --  there is no pending buffer until the next attach.
   --
   --  Committing a pending wl_buffer allows the compositor to read the
   --  pixels in the wl_buffer. The compositor may access the pixels at
   --  any time after the wl_surface.commit request. When the compositor
   --  will not access the pixels anymore, it will send the
   --  wl_buffer.release event. Only after receiving wl_buffer.release,
   --  the client may reuse the wl_buffer. A wl_buffer that has been
   --  attached and then replaced by another attach instead of committed
   --  will not receive a release event, and is not used by the
   --  compositor.
   --
   --  Destroying the wl_buffer after wl_buffer.release does not change
   --  the surface contents. However, if the client destroys the
   --  wl_buffer before receiving the wl_buffer.release event, the surface
   --  contents become undefined immediately.
   --
   --  If wl_surface.attach is sent with a NULL wl_buffer, the
   --  following wl_surface.commit will remove the surface content.

   procedure Damage (Surface : Wayland.Client.Surface;
                     X       : Integer;
                     Y       : Integer;
                     Width   : Integer;
                     Height  : Integer) with
     Pre => Surface.Has_Proxy;
   --  This request is used to describe the regions where the pending
   --  buffer is different from the current surface contents, and where
   --  the surface therefore needs to be repainted. The compositor
   --  ignores the parts of the damage that fall outside of the surface.
   --
   --  Damage is double-buffered state, see wl_surface.commit.
   --
   --  The damage rectangle is specified in surface-local coordinates,
   --  where x and y specify the upper left corner of the damage rectangle.
   --
   --  The initial value for pending damage is empty: no damage.
   --  wl_surface.damage adds pending damage: the new pending damage
   --  is the union of old pending damage and the given rectangle.
   --
   --  wl_surface.commit assigns pending damage as the current damage,
   --  and clears pending damage. The server will clear the current
   --  damage as it repaints the surface.
   --
   --  Alternatively, damage can be posted with wl_surface.damage_buffer
   --  which uses buffer coordinates instead of surface coordinates,
   --  and is probably the preferred and intuitive way of doing this.

   function Frame (Surface : Wayland.Client.Surface) return Callback with
     Pre => Surface.Has_Proxy;
   --  Request a notification when it is a good time to start drawing a new
   --  frame, by creating a frame callback. This is useful for throttling
   --  redrawing operations, and driving animations.
   --
   --  When a client is animating on a wl_surface, it can use the 'frame'
   --  request to get notified when it is a good time to draw and commit the
   --  next frame of animation. If the client commits an update earlier than
   --  that, it is likely that some updates will not make it to the display,
   --  and the client is wasting resources by drawing too often.
   --
   --  The frame request will take effect on the next wl_surface.commit.
   --  The notification will only be posted for one frame unless
   --  requested again. For a wl_surface, the notifications are posted in
   --  the order the frame requests were committed.
   --
   --  The server must send the notifications so that a client
   --  will not send excessive updates, while still allowing
   --  the highest possible update rate for clients that wait for the reply
   --  before drawing again. The server should give some time for the client
   --  to draw and commit after sending the frame callback events to let it
   --  hit the next output refresh.
   --
   --  A server should avoid signaling the frame callbacks if the
   --  surface is not visible in any way, e.g. the surface is off-screen,
   --  or completely obscured by other opaque surfaces.
   --
   --  The object returned by this request will be destroyed by the
   --  compositor after the callback is fired and as such the client must not
   --  attempt to use it after that point.
   --
   --  The callback_data passed in the callback is the current time, in
   --  milliseconds, with an undefined base.

   procedure Set_Opaque_Region (Surface : Wayland.Client.Surface;
                                Region  : Wayland.Client.Region) with
     Pre => Surface.Has_Proxy;
   --  This request sets the region of the surface that contains
   --  opaque content.
   --
   --  The opaque region is an optimization hint for the compositor
   --  that lets it optimize the redrawing of content behind opaque
   --  regions.  Setting an opaque region is not required for correct
   --  behaviour, but marking transparent content as opaque will result
   --  in repaint artifacts.
   --
   --  The opaque region is specified in surface-local coordinates.
   --
   --  The compositor ignores the parts of the opaque region that fall
   --  outside of the surface.
   --
   --  Opaque region is double-buffered state, see wl_surface.commit.
   --
   --  wl_surface.set_opaque_region changes the pending opaque region.
   --  wl_surface.commit copies the pending region to the current region.
   --  Otherwise, the pending and current regions are never changed.
   --
   --  The initial value for an opaque region is empty. Setting the pending
   --  opaque region has copy semantics, and the wl_region object can be
   --  destroyed immediately. A NULL wl_region causes the pending opaque
   --  region to be set to empty.

   procedure Set_Input_Region (Surface : Wayland.Client.Surface;
                               Region  : Wayland.Client.Region) with
     Pre => Surface.Has_Proxy;
   --  This request sets the region of the surface that can receive
   --  pointer and touch events.
   --
   --  Input events happening outside of this region will try the next
   --  surface in the server surface stack. The compositor ignores the
   --  parts of the input region that fall outside of the surface.
   --
   --  The input region is specified in surface-local coordinates.
   --
   --  Input region is double-buffered state, see wl_surface.commit.
   --
   --  wl_surface.set_input_region changes the pending input region.
   --  wl_surface.commit copies the pending region to the current region.
   --  Otherwise the pending and current regions are never changed,
   --  except cursor and icon surfaces are special cases, see
   --  wl_pointer.set_cursor and wl_data_device.start_drag.
   --
   --  The initial value for an input region is infinite. That means the
   --  whole surface will accept input. Setting the pending input region
   --  has copy semantics, and the wl_region object can be destroyed
   --  immediately. A NULL wl_region causes the input region to be set
   --  to infinite.

   procedure Commit (Surface : Wayland.Client.Surface) with
     Pre => Surface.Has_Proxy;
   --  Surface state (input, opaque, and damage regions, attached buffers,
   --  etc.) is double-buffered. Protocol requests modify the pending state,
   --  as opposed to the current state in use by the compositor. A commit
   --  request atomically applies all pending state, replacing the current
   --  state. After commit, the new pending state is as documented for each
   --  related request.
   --
   --  On commit, a pending wl_buffer is applied first, and all other state
   --  second. This means that all coordinates in double-buffered state are
   --  relative to the new wl_buffer coming into use, except for
   --  wl_surface.attach itself. If there is no pending wl_buffer, the
   --  coordinates are relative to the current surface contents.
   --
   --  All requests that need a commit to become effective are documented
   --  to affect double-buffered state.
   --
   --  Other interfaces may add further double-buffered surface state.

   procedure Set_Buffer_Transform (Surface   : Wayland.Client.Surface;
                                   Transform : Integer) with
     Pre => Surface.Has_Proxy;
   --  This request sets an optional transformation on how the compositor
   --  interprets the contents of the buffer attached to the surface. The
   --  accepted values for the transform parameter are the values for
   --  wl_output.transform.
   --
   --  Buffer transform is double-buffered state, see wl_surface.commit.
   --
   --  A newly created surface has its buffer transformation set to normal.
   --
   --  wl_surface.set_buffer_transform changes the pending buffer
   --  transformation. wl_surface.commit copies the pending buffer
   --  transformation to the current one. Otherwise, the pending and current
   --  values are never changed.
   --
   --  The purpose of this request is to allow clients to render content
   --  according to the output transform, thus permitting the compositor to
   --  use certain optimizations even if the display is rotated. Using
   --  hardware overlays and scanning out a client buffer for fullscreen
   --  surfaces are examples of such optimizations. Those optimizations are
   --  highly dependent on the compositor implementation, so the use of this
   --  request should be considered on a case-by-case basis.
   --
   --  Note that if the transform value includes 90 or 270 degree rotation,
   --  the width of the buffer will become the surface height and the height
   --  of the buffer will become the surface width.
   --
   --  If transform is not one of the values from the
   --  wl_output.transform enum the invalid_transform protocol error
   --  is raised.

   procedure Set_Buffer_Scale (Surface : Wayland.Client.Surface;
                               Scale   : Integer) with
     Pre => Surface.Has_Proxy;
   --  This request sets an optional scaling factor on how the compositor
   --  interprets the contents of the buffer attached to the window.
   --
   --  Buffer scale is double-buffered state, see wl_surface.commit.
   --
   --  A newly created surface has its buffer scale set to 1.
   --
   --  wl_surface.set_buffer_scale changes the pending buffer scale.
   --  wl_surface.commit copies the pending buffer scale to the current one.
   --  Otherwise, the pending and current values are never changed.
   --
   --  The purpose of this request is to allow clients to supply higher
   --  resolution buffer data for use on high resolution outputs. It is
   --  intended that you pick the same buffer scale as the scale of the
   --  output that the surface is displayed on. This means the compositor
   --  can avoid scaling when rendering the surface on that output.
   --
   --  Note that if the scale is larger than 1, then you have to attach
   --  a buffer that is larger (by a factor of scale in each dimension)
   --  than the desired surface size.
   --
   --  If scale is not positive the invalid_scale protocol error is
   --  raised.

   procedure Damage_Buffer (Surface : Wayland.Client.Surface;
                            X       : Integer;
                            Y       : Integer;
                            Width   : Integer;
                            Height  : Integer) with
     Pre => Surface.Has_Proxy;
   --  This request is used to describe the regions where the pending
   --  buffer is different from the current surface contents, and where
   --  the surface therefore needs to be repainted. The compositor
   --  ignores the parts of the damage that fall outside of the surface.
   --
   --  Damage is double-buffered state, see wl_surface.commit.
   --
   --  The damage rectangle is specified in buffer coordinates,
   --  where x and y specify the upper left corner of the damage rectangle.
   --
   --  The initial value for pending damage is empty: no damage.
   --  wl_surface.damage_buffer adds pending damage: the new pending
   --  damage is the union of old pending damage and the given rectangle.
   --
   --  wl_surface.commit assigns pending damage as the current damage,
   --  and clears pending damage. The server will clear the current
   --  damage as it repaints the surface.
   --
   --  This request differs from wl_surface.damage in only one way - it
   --  takes damage in buffer coordinates instead of surface-local
   --  coordinates. While this generally is more intuitive than surface
   --  coordinates, it is especially desirable when using wp_viewport
   --  or when a drawing library (like EGL) is unaware of buffer scale
   --  and buffer transform.
   --
   --  Note: Because buffer transformation changes and damage requests may
   --  be interleaved in the protocol stream, it is impossible to determine
   --  the actual mapping between surface and buffer damage until
   --  wl_surface.commit time. Therefore, compositors wishing to take both
   --  kinds of damage into account will have to accumulate damage from the
   --  two requests separately and only transform from one to the other
   --  after receiving the wl_surface.commit.

   procedure Destroy (Surface : in out Wayland.Client.Surface) with
     Pre  => Surface.Has_Proxy,
     Post => not Surface.Has_Proxy;

   type Buffer is tagged limited private;

   function Has_Proxy (Buffer : Wayland.Client.Buffer) return Boolean;

   function Get_Version (Buffer : Wayland.Client.Buffer) return Unsigned_32 with
     Pre => Buffer.Has_Proxy;

   procedure Destroy (Buffer : in out Wayland.Client.Buffer) with
     Pre  => Buffer.Has_Proxy,
     Post => not Buffer.Has_Proxy;

   type Display is tagged limited private with
     Default_Initial_Condition => not Display.Is_Connected;

   function Is_Connected (Display : Wayland.Client.Display) return Boolean;

   procedure Connect (Display : in out Wayland.Client.Display;
                      Name    : String := Default_Display_Name) with
     Pre => not Display.Is_Connected;
   --  Attempts connecting with the Wayland server.

   type Check_For_Events_Status is (
                                    Events_Need_Processing,
                                    No_Events,
                                    Error
                                    );

   function Check_For_Events (Display : Wayland.Client.Display;
                              Timeout : Integer) return Check_For_Events_Status;
   --  The timeout is given in milliseconds.

   function Dispatch (Display : Wayland.Client.Display) return Integer with
     Pre => Display.Is_Connected;
   --  Process incoming events.

   procedure Dispatch (Display : Wayland.Client.Display) with
     Pre => Display.Is_Connected;
   --  Process incoming events. Ignores error code. To be removed?

   function Dispatch_Pending
     (Display : Wayland.Client.Display) return Integer with
     Pre => Display.Is_Connected;
   --  Dispatch default queue events without reading from
   --  the display file descriptor.
   --
   --  This function dispatches events on the main event queue.
   --  It does not attempt to read the display fd and simply returns zero
   --  if the main queue is empty, i.e., it doesn't block.
   --
   --  Returns the number of dispatched events or -1 on failure

   function Prepare_Read
     (Display : Wayland.Client.Display) return Integer with
     Pre => Display.Is_Connected;
   --  Prepare to read events from the display's file descriptor

   function Read_Events
     (Display : Wayland.Client.Display) return Call_Result_Code with
     Pre => Display.Is_Connected;
   --  Returns 0 on success or -1 on error.

   procedure Cancel_Read (Display : Wayland.Client.Display) with
     Pre => Display.Is_Connected;
   --  Cancel read intention on display's fd.

   function Roundtrip (Display : Wayland.Client.Display) return Integer with
     Pre => Display.Is_Connected;

   procedure Roundtrip (Display : Wayland.Client.Display) with
     Pre => Display.Is_Connected;

   procedure Disconnect (Display : in out Wayland.Client.Display) with
     Pre  => Display.Is_Connected,
     Post => not Display.Is_Connected;

   function Get_Version (Display : Wayland.Client.Display) return Unsigned_32 with
     Pre => Display.Is_Connected;

   procedure Get_Registry (Display  : Wayland.Client.Display;
                           Registry : in out Wayland.Client.Registry) with
     Pre => Display.Is_Connected and not Has_Proxy (Registry);
   --  This request to the compositor (Wayland Server)
   --  creates a registry proxy object that allows the client
   --  to list and get a hold of proxy objects for
   --  the global objects available from the compositor.
   --
   --  It should be noted that the server side resources consumed in
   --  response to a Get_Registry_Proxy request can only be released when the
   --  client disconnects, not when the client side registry proxy is destroyed.
   --  Therefore, clients should invoke Get_Registry_Proxy as infrequently as
   --  possible to avoid wasting memory.

   function Sync (Display : Wayland.Client.Display) return Callback with
     Pre => Display.Is_Connected;
   --  The sync request asks the server to emit the 'done' event
   --  on the returned wl_callback object.  Since requests are
   --  handled in-order and events are delivered in-order, this can
   --  be used as a barrier to ensure all previous requests and the
   --  resulting events have been handled.
   --
   --  The object returned by this request will be destroyed by the
   --  compositor after the callback is fired and as such the client must not
   --  attempt to use it after that point.
   --
   --  The callback_data passed in the callback is the event serial.

   type Registry is tagged limited private;

   function Has_Proxy (Registry : Wayland.Client.Registry) return Boolean;

   procedure Destroy (Registry : in out Wayland.Client.Registry) with
     Pre  => Registry.Has_Proxy,
     Post => not Registry.Has_Proxy;

   function Get_Version (Registry : Wayland.Client.Registry) return Unsigned_32 with
     Pre => Registry.Has_Proxy;

   type Callback is tagged limited private;

   function Has_Proxy (Callback : Wayland.Client.Callback) return Boolean;

   procedure Destroy (Callback : in out Wayland.Client.Callback) with
     Pre    => Callback.Has_Proxy,
     Post   => not Callback.Has_Proxy;

   function Get_Version (Callback : Wayland.Client.Callback) return Unsigned_32 with
     Pre => Callback.Has_Proxy;

   type Data_Offer is tagged limited private;
   --  Provides drag and drop functionality in a Wayland application

   function Has_Proxy (Offer : Data_Offer) return Boolean;

   procedure Destroy (Offer : in out Data_Offer) with
     Pre    => Offer.Has_Proxy,
     Post   => not Offer.Has_Proxy;

   function Get_Version (Offer : Data_Offer) return Unsigned_32 with
     Pre => Offer.Has_Proxy;

   procedure Do_Accept (Offer     : Data_Offer;
                        Serial    : Unsigned_32;
                        Mime_Type : String) with
     Pre => Offer.Has_Proxy;
   --  Indicate that the client can accept the given mime type.

   procedure Do_Not_Accept (Offer  : Data_Offer;
                            Serial : Unsigned_32) with
     Pre => Offer.Has_Proxy;
   --  Indicate that the client does not accept the given mime type.

   procedure Receive (Offer           : Data_Offer;
                      Mime_Type       : String;
                      File_Descriptor : Integer) with
     Pre => Offer.Has_Proxy;
   --  To transfer the offered data, the client issues this request
   --  and indicates the mime type it wants to receive.  The transfer
   --  happens through the passed file descriptor (typically created
   --  with the pipe system call).  The source client writes the data
   --  in the mime type representation requested and then closes the
   --  file descriptor.
   --
   --  The receiving client reads from the read end of the pipe until
   --  EOF and then closes its end, at which point the transfer is
   --  complete.
   --
   --  This request may happen multiple times for different mime types,
   --  both before and after wl_data_device.drop. Drag-and-drop destination
   --  clients may preemptively fetch data or examine it more closely to
   --  determine acceptance.

   procedure Finish (Offer : Data_Offer) with
     Pre => Offer.Has_Proxy;
   --  Notifies the compositor that the drag destination successfully
   --  finished the drag-and-drop operation.
   --
   --  Upon receiving this request, the compositor will emit
   --  wl_data_source.dnd_finished on the drag source client.
   --
   --  It is a client error to perform other requests than
   --  wl_data_offer.destroy after this one. It is also an error to perform
   --  this request after a NULL mime type has been set in
   --  wl_data_offer.accept or no action was received through
   --  wl_data_offer.action.

   procedure Set_Actions (Offer            : Data_Offer;
                          Dnd_Actions      : Unsigned_32;
                          Preferred_Action : Unsigned_32) with
     Pre => Offer.Has_Proxy;
   --  Sets the actions that the destination side client supports for
   --  this operation. This request may trigger the emission of
   --  wl_data_source.action and wl_data_offer.action events if the compositor
   --  needs to change the selected action.
   --
   --  This request can be called multiple times throughout the
   --  drag-and-drop operation, typically in response to wl_data_device.enter
   --  or wl_data_device.motion events.
   --
   --  This request determines the final result of the drag-and-drop
   --  operation. If the end result is that no action is accepted,
   --  the drag source will receive wl_drag_source.cancelled.
   --
   --  The dnd_actions argument must contain only values expressed in the
   --  wl_data_device_manager.dnd_actions enum, and the preferred_action
   --  argument must only contain one of those values set, otherwise it
   --  will result in a protocol error.
   --
   --  While managing an "ask" action, the destination drag-and-drop client
   --  may perform further wl_data_offer.receive requests, and is expected
   --  to perform one last wl_data_offer.set_actions request with a preferred
   --  action other than "ask" (and optionally wl_data_offer.accept) before
   --  requesting wl_data_offer.finish, in order to convey the action selected
   --  by the user. If the preferred action is not in the
   --  wl_data_offer.source_actions mask, an error will be raised.
   --
   --  If the "ask" action is dismissed (e.g. user cancellation), the client
   --  is expected to perform wl_data_offer.destroy right away.
   --
   --  This request can only be made on drag-and-drop offers, a protocol error
   --  will be raised otherwise.

   type Data_Source is tagged limited private;

   function Has_Proxy (Data_Source : Wayland.Client.Data_Source) return Boolean;

   procedure Destroy (Source : in out Data_Source) with
     Pre    => Source.Has_Proxy,
     Post   => not Source.Has_Proxy;

   function Get_Version (Source : Data_Source) return Unsigned_32 with
     Pre => Source.Has_Proxy;

   procedure Offer (Source    : Data_Source;
                    Mime_Type : String) with
     Pre => Source.Has_Proxy;
   --  This request adds a mime type to the set of mime types
   --  advertised to targets.  Can be called several times to offer
   --  multiple types.

   procedure Set_Actions (Source      : Data_Source;
                          Dnd_Actions : Unsigned_32) with
     Pre => Source.Has_Proxy;
   --  Sets the actions that the source side client supports for this
   --  operation. This request may trigger wl_data_source.action and
   --  wl_data_offer.action events if the compositor needs to change the
   --  selected action.
   --
   --  The dnd_actions argument must contain only values expressed in the
   --  wl_data_device_manager.dnd_actions enum, otherwise it will result
   --  in a protocol error.
   --
   --  This request must be made once only, and can only be made on sources
   --  used in drag-and-drop, so it must be performed before
   --  wl_data_device.start_drag. Attempting to use the source other than
   --  for drag-and-drop will raise a protocol error.

   type Keyboard is tagged limited private;

   function Has_Proxy (Keyboard : Wayland.Client.Keyboard) return Boolean;

   procedure Destroy (Keyboard : in out Wayland.Client.Keyboard) with
     Pre    => Keyboard.Has_Proxy,
     Post   => not Keyboard.Has_Proxy;

   function Get_Version (Keyboard : Wayland.Client.Keyboard) return Unsigned_32 with
     Pre => Keyboard.Has_Proxy;

   procedure Release (Keyboard : in out Wayland.Client.Keyboard) with
     Pre    => Keyboard.Has_Proxy,
     Post   => not Keyboard.Has_Proxy;

   type Touch is tagged limited private;

   function Has_Proxy (Touch : Wayland.Client.Touch) return Boolean;

   procedure Destroy (Touch : in out Wayland.Client.Touch) with
     Pre    => Touch.Has_Proxy,
     Post   => not Touch.Has_Proxy;

   function Get_Version (Touch : Wayland.Client.Touch) return Unsigned_32 with
     Pre => Touch.Has_Proxy;

   procedure Release (Touch : in out Wayland.Client.Touch) with
     Pre    => Touch.Has_Proxy,
     Post   => not Touch.Has_Proxy;

   type Output is tagged limited private;

   function Has_Proxy (Output : Wayland.Client.Output) return Boolean;

   procedure Destroy (Output : in out Wayland.Client.Output) with
     Pre    => Output.Has_Proxy,
     Post   => not Output.Has_Proxy;

   function Get_Version (Output : Wayland.Client.Output) return Unsigned_32 with
     Pre => Output.Has_Proxy;

   procedure Release (Output : in out Wayland.Client.Output) with
     Pre    => Output.Has_Proxy,
     Post   => not Output.Has_Proxy;
   --  Using this request a client can tell the server that it is not going to
   --  use the output object anymore.

   type Region is tagged limited private;

   function Has_Proxy (Region : Wayland.Client.Region) return Boolean;

   procedure Destroy (Region : in out Wayland.Client.Region) with
     Pre    => Region.Has_Proxy,
     Post   => not Region.Has_Proxy;

   function Get_Version (Region : Wayland.Client.Region) return Unsigned_32 with
     Pre => Region.Has_Proxy;

   procedure Add (Region : Wayland.Client.Region;
                  X      : Integer;
                  Y      : Integer;
                  Width  : Integer;
                  Height : Integer) with
     Pre => Region.Has_Proxy;
   --  Add the specified rectangle to the region.

   procedure Subtract (Region : Wayland.Client.Region;
                       X      : Integer;
                       Y      : Integer;
                       Width  : Integer;
                       Height : Integer) with
     Pre => Region.Has_Proxy;
   --  Subtract the specified rectangle from the region.

   generic
      type Data_Type is limited private;
      type Data_Ptr is access all Data_Type;

      with procedure Error (Data      : not null Data_Ptr;
                            Display   : Wayland.Client.Display;
                            Object_Id : Void_Ptr;
                            Code      : Unsigned_32;
                            Message   : String);
      --  Should really Object_Id really be exposed here? This part
      --  of the API can potentially be improved upon.

      with procedure Delete_Id (Data    : not null Data_Ptr;
                                Display : Wayland.Client.Display;
                                Id      : Unsigned_32);

   package Display_Events is

      function Subscribe (Display : in out Wayland.Client.Display;
                          Data    : not null Data_Ptr) return Call_Result_Code;

   end Display_Events;

   generic
      type Data_Type is limited private;
      type Data_Ptr is access all Data_Type;

      with procedure Global_Object_Added (Data     : not null Data_Ptr;
                                          Registry : Wayland.Client.Registry;
                                          Id       : Unsigned_32;
                                          Name     : String;
                                          Version  : Unsigned_32);

      with procedure Global_Object_Removed (Data     : not null Data_Ptr;
                                            Registry : Wayland.Client.Registry;
                                            Id       : Unsigned_32);
   package Registry_Events is

      function Subscribe (Registry : in out Wayland.Client.Registry;
                          Data     : not null Data_Ptr) return Call_Result_Code;
      --  Starts subcription to global objects addded and removed events.
      --  To stop subscription, call Registry.Destroy.

   end Registry_Events;

   generic
      type Data_Type is limited private;
      type Data_Ptr is access all Data_Type;

      with procedure Done (Data          : not null Data_Ptr;
                           Callback      : Wayland.Client.Callback;
                           Callback_Data : Unsigned_32);

   package Callback_Events is

      function Subscribe (Callback : in out Wayland.Client.Callback;
                          Data     : not null Data_Ptr) return Call_Result_Code;

   end Callback_Events;

   generic
      type Data_Type is limited private;
      type Data_Ptr is access all Data_Type;

      with procedure Format (Data   : not null Data_Ptr;
                             Shm    : Wayland.Client.Shm;
                             Format : Unsigned_32);
   package Shm_Events is

      function Subscribe (Shm  : in out Wayland.Client.Shm;
                          Data : not null Data_Ptr) return Call_Result_Code;

   end Shm_Events;

   generic
      type Data_Type is limited private;
      type Data_Ptr is access all Data_Type;

      with procedure Release (Data   : not null Data_Ptr;
                              Buffer : Wayland.Client.Buffer);

   package Buffer_Events is

      function Subscribe (Buffer : in out Wayland.Client.Buffer;
                          Data   : not null Data_Ptr) return Call_Result_Code;

   end Buffer_Events;

   generic
      type Data_Type is limited private;
      type Data_Ptr is access all Data_Type;

      with procedure Offer (Data       : not null Data_Ptr;
                            Data_Offer : Wayland.Client.Data_Offer;
                            Mime_Type  : String);

      with procedure Source_Actions (Data           : not null Data_Ptr;
                                     Data_Offer     : Wayland.Client.Data_Offer;
                                     Source_Actions : Unsigned_32);

      with procedure Action (Data       : not null Data_Ptr;
                             Data_Offer : Wayland.Client.Data_Offer;
                             Dnd_Action : Unsigned_32);

   package Data_Offer_Events is

      function Subscribe
        (Data_Offer : in out Wayland.Client.Data_Offer;
         Data       : not null Data_Ptr) return Call_Result_Code;

   end Data_Offer_Events;

   generic
      type Data_Type is limited private;
      type Data_Ptr is access all Data_Type;

      with procedure Target (Data        : not null Data_Ptr;
                             Data_Source : Wayland.Client.Data_Source;
                             Mime_Type   : String);

      with procedure Send (Data        : not null Data_Ptr;
                           Data_Source : Wayland.Client.Data_Source;
                           Mime_Type   : String;
                           Fd          : Integer);

      with procedure Cancelled (Data        : not null Data_Ptr;
                                Data_Source : Wayland.Client.Data_Source);

      with procedure Dnd_Drop_Performed
        (Data        : not null Data_Ptr;
         Data_Source : Wayland.Client.Data_Source);

      with procedure Dnd_Finished (Data        : not null Data_Ptr;
                                   Data_Source : Wayland.Client.Data_Source);

      with procedure Action (Data        : not null Data_Ptr;
                             Data_Source : Wayland.Client.Data_Source;
                             Dnd_Action  : Unsigned_32);

   package Data_Source_Events is

      function Subscribe
        (Data_Source : in out Wayland.Client.Data_Source;
         Data        : not null Data_Ptr) return Call_Result_Code;

   end Data_Source_Events;

   generic
      type Data_Type is limited private;
      type Data_Ptr is access all Data_Type;

      with procedure Data_Offer (Data        : not null Data_Ptr;
                                 Data_Device : Wayland.Client.Data_Device;
                                 Id          : Unsigned_32);

      with procedure Enter (Data        : not null Data_Ptr;
                            Data_Device : Wayland.Client.Data_Device;
                            Serial      : Unsigned_32;
                            Surface     : Wayland.Client.Surface;
                            X           : Fixed;
                            Y           : Fixed;
                            Id          : Wayland.Client.Data_Offer);

      with procedure Leave (Data        : not null Data_Ptr;
                            Data_Device : Wayland.Client.Data_Device);

      with procedure Motion (Data        : not null Data_Ptr;
                             Data_Device : Wayland.Client.Data_Device;
                             Time        : Unsigned_32;
                             X           : Fixed;
                             Y           : Fixed);

      with procedure Drop (Data        : not null Data_Ptr;
                           Data_Device : Wayland.Client.Data_Device);

      with procedure Selection (Data        : not null Data_Ptr;
                                Data_Device : Wayland.Client.Data_Device;
                                Id          : Wayland.Client.Data_Offer);

   package Data_Device_Events is

      function Subscribe
        (Data_Device : in out Wayland.Client.Data_Device;
         Data        : not null Data_Ptr) return Call_Result_Code;

   end Data_Device_Events;

   generic
      type Data_Type is limited private;
      type Data_Ptr is access all Data_Type;

      with procedure Shell_Surface_Ping
        (Data    : not null Data_Ptr;
         Surface : Shell_Surface;
         Serial  : Unsigned_32);

      with procedure Shell_Surface_Configure
        (Data    : not null Data_Ptr;
         Surface : Shell_Surface;
         Edges   : Unsigned_32;
         Width   : Integer;
         Height  : Integer);

      with procedure Shell_Surface_Popup_Done
        (Data    : not null Data_Ptr;
         Surface : Shell_Surface);

   package Shell_Surface_Events is

      function Subscribe (Surface : in out Shell_Surface;
                          Data    : not null Data_Ptr) return Call_Result_Code;

   end Shell_Surface_Events;

   generic
      type Data_Type is limited private;
      type Data_Ptr is access all Data_Type;

      with procedure Enter
        (Data    : not null Data_Ptr;
         Surface : Wayland.Client.Surface;
         Output  : Wayland.Client.Output);

      with procedure Leave
        (Data    : not null Data_Ptr;
         Surface : Wayland.Client.Surface;
         Output  : Wayland.Client.Output);

   package Surface_Events is

      function Subscribe
        (Surface : in out Wayland.Client.Surface;
         Data    : not null Data_Ptr) return Call_Result_Code;

   end Surface_Events;

   generic
      type Data_Type is limited private;
      type Data_Ptr is access all Data_Type;

      with procedure Seat_Capabilities
        (Data         : not null Data_Ptr;
         Seat         : Wayland.Client.Seat;
         Capabilities : Unsigned_32);

      with procedure Seat_Name
        (Data : not null Data_Ptr;
         Seat : Wayland.Client.Seat;
         Name : String);

   package Seat_Events is

      function Subscribe (Seat : in out Wayland.Client.Seat;
                          Data : not null Data_Ptr) return Call_Result_Code;

   end Seat_Events;

   generic
      type Data_Type is limited private;
      type Data_Ptr is access all Data_Type;

      with procedure Pointer_Enter
        (Data      : not null Data_Ptr;
         Pointer   : Wayland.Client.Pointer;
         Serial    : Unsigned_32;
         Surface   : Wayland.Client.Surface;
         Surface_X : Wayland.Client.Fixed;
         Surface_Y : Wayland.Client.Fixed);

      with procedure Pointer_Leave
        (Data    : not null Data_Ptr;
         Pointer : Wayland.Client.Pointer;
         Serial  : Unsigned_32;
         Surface : Wayland.Client.Surface);

      with procedure Pointer_Motion
        (Data      : not null Data_Ptr;
         Pointer   : Wayland.Client.Pointer;
         Time      : Unsigned_32;
         Surface_X : Wayland.Client.Fixed;
         Surface_Y : Wayland.Client.Fixed);

      with procedure Pointer_Button
        (Data    : not null Data_Ptr;
         Pointer : Wayland.Client.Pointer;
         Serial  : Unsigned_32;
         Time    : Unsigned_32;
         Button  : Unsigned_32;
         State   : Unsigned_32);

      with procedure Pointer_Axis
        (Data    : not null Data_Ptr;
         Pointer : Wayland.Client.Pointer;
         Time    : Unsigned_32;
         Axis    : Unsigned_32;
         Value   : Wayland.Client.Fixed);

      with procedure Pointer_Frame (Data    : not null Data_Ptr;
                                    Pointer : Wayland.Client.Pointer);

      with procedure Pointer_Axis_Source
        (Data        : not null Data_Ptr;
         Pointer     : Wayland.Client.Pointer;
         Axis_Source : Unsigned_32);

      with procedure Pointer_Axis_Stop
        (Data    : not null Data_Ptr;
         Pointer : Wayland.Client.Pointer;
         Time    : Unsigned_32;
         Axis    : Unsigned_32);

      with procedure Pointer_Axis_Discrete
        (Data     : not null Data_Ptr;
         Pointer  : Wayland.Client.Pointer;
         Axis     : Unsigned_32;
         Discrete : Integer);

   package Pointer_Events is

      function Subscribe (Pointer : in out Wayland.Client.Pointer;
                          Data    : not null Data_Ptr) return Call_Result_Code;

   end Pointer_Events;
   --  Pointer Axis Events are for example scroll wheel rotation

   generic
      type Data_Type is limited private;
      type Data_Ptr is access all Data_Type;

      with procedure Keymap (Data     : not null Data_Ptr;
                             Keyboard : Wayland.Client.Keyboard;
                             Format   : Unsigned_32;
                             Fd       : Integer;
                             Size     : Unsigned_32);

      with procedure Enter (Data     : not null Data_Ptr;
                            Keyboard : Wayland.Client.Keyboard;
                            Serial   : Unsigned_32;
                            Surface  : Wayland.Client.Surface;
                            Keys     : Wayland_Array_T);

      with procedure Leave (Data     : not null Data_Ptr;
                            Keyboard : Wayland.Client.Keyboard;
                            Serial   : Unsigned_32;
                            Surface  : Wayland.Client.Surface);

      with procedure Key (Data     : not null Data_Ptr;
                          Keyboard : Wayland.Client.Keyboard;
                          Serial   : Unsigned_32;
                          Time     : Unsigned_32;
                          Key      : Unsigned_32;
                          State    : Unsigned_32);

      with procedure Modifiers (Data           : not null Data_Ptr;
                                Keyboard       : Wayland.Client.Keyboard;
                                Serial         : Unsigned_32;
                                Mods_Depressed : Unsigned_32;
                                Mods_Latched   : Unsigned_32;
                                Mods_Locked    : Unsigned_32;
                                Group          : Unsigned_32);

      with procedure Repeat_Info (Data     : not null Data_Ptr;
                                  Keyboard : Wayland.Client.Keyboard;
                                  Rate     : Integer;
                                  Delay_V  : Integer);

   package Keyboard_Events is

      function Subscribe (Keyboard : in out Wayland.Client.Keyboard;
                          Data     : not null Data_Ptr) return Call_Result_Code;

   end Keyboard_Events;

   generic
      type Data_Type is limited private;
      type Data_Ptr is access all Data_Type;

      with procedure Down (Data    : not null Data_Ptr;
                           Touch   : Wayland.Client.Touch;
                           Serial  : Unsigned_32;
                           Time    : Unsigned_32;
                           Surface : Wayland.Client.Surface;
                           Id      : Integer;
                           X       : Fixed;
                           Y       : Fixed);

      with procedure Up (Data   : not null Data_Ptr;
                         Touch  : Wayland.Client.Touch;
                         Serial : Unsigned_32;
                         Time   : Unsigned_32;
                         Id     : Integer);

      with procedure Motion (Data  : not null Data_Ptr;
                             Touch : Wayland.Client.Touch;
                             Time  : Unsigned_32;
                             Id    : Integer;
                             X     : Fixed;
                             Y     : Fixed);

      with procedure Frame (Data  : not null Data_Ptr;
                            Touch : Wayland.Client.Touch);

      with procedure Cancel (Data  : not null Data_Ptr;
                             Touch : Wayland.Client.Touch);

      with procedure Shape (Data  : not null Data_Ptr;
                            Touch : Wayland.Client.Touch;
                            Id    : Integer;
                            Major : Fixed;
                            Minor : Fixed);

      with procedure Orientation (Data        : not null Data_Ptr;
                                  Touch       : Wayland.Client.Touch;
                                  Id          : Integer;
                                  Orientation : Fixed);
   package Touch_Events is

      function Subscribe (Touch : in out Wayland.Client.Touch;
                          Data  : not null Data_Ptr) return Call_Result_Code;

   end Touch_Events;

   generic
      type Data_Type is limited private;
      type Data_Ptr is access all Data_Type;

      with procedure Geometry (Data            : not null Data_Ptr;
                               Output          : Wayland.Client.Output;
                               X               : Integer;
                               Y               : Integer;
                               Physical_Width  : Integer;
                               Physical_Height : Integer;
                               Subpixel        : Integer;
                               Make            : String;
                               Model           : String;
                               Transform       : Integer);

      with procedure Mode (Data    : not null Data_Ptr;
                           Output  : Wayland.Client.Output;
                           Flags   : Unsigned_32;
                           Width   : Integer;
                           Height  : Integer;
                           Refresh : Integer);

      with procedure Done (Data   : not null Data_Ptr;
                           Output : Wayland.Client.Output);

      with procedure Scale (Data   : not null Data_Ptr;
                            Output : Wayland.Client.Output;
                            Factor : Integer);

   package Output_Events is

      function Subscribe (Output : in out Wayland.Client.Output;
                          Data   : not null Data_Ptr) return Call_Result_Code;

   end Output_Events;

   type Display_Ptr is access all Display;
   type Registry_Ptr is access all Registry;
   type Callback_Ptr is access all Callback;
   type Compositor_Ptr is access all Compositor;
   type Shm_Pool_Ptr is access all Shm_Pool;
   type Shm_Ptr is access all Shm;
   type Buffer_Ptr is access all Buffer;
   type Data_Offer_Ptr is access all Data_Offer;
   type Data_Source_Ptr is access all Data_Source;
   type Data_Device_Ptr is access all Data_Device;
   type Data_Device_Manager_Ptr is access all Data_Device_Manager;
   type Shell_Ptr is access all Shell;
   type Shell_Surface_Ptr is access all Shell_Surface;
   type Surface_Ptr is access all Surface;
   type Seat_Ptr is access all Seat;
   type Pointer_Ptr is access all Pointer;
   type Keyboard_Ptr is access all Keyboard;
   type Touch_Ptr is access all Touch;
   type Output_Ptr is access all Output;
   type Region_Ptr is access all Region;
   type Subcompositor_Ptr is access all Subcompositor;
   type Subsurface_Ptr is access all Subsurface;

private

   subtype char_array is Interfaces.C.char_array;

   subtype chars_ptr is Interfaces.C.Strings.chars_ptr;

   function Value
     (C : chars_ptr) return String renames Interfaces.C.Strings.Value;

   package Wl_Thin renames Wayland.Thin;
   --  FIXME Remove renaming

   use type Wl_Thin.Display_Ptr;
   use type Wl_Thin.Registry_Ptr;
   use type Wl_Thin.Callback_Ptr;
   use type Wl_Thin.Compositor_Ptr;
   use type Wl_Thin.Shm_Pool_Ptr;
   use type Wl_Thin.Shm_Ptr;
   use type Wl_Thin.Buffer_Ptr;
   use type Wl_Thin.Data_Offer_Ptr;
   use type Wl_Thin.Data_Source_Ptr;
   use type Wl_Thin.Data_Device_Ptr;
   use type Wl_Thin.Data_Device_Manager_Ptr;
   use type Wl_Thin.Shell_Ptr;
   use type Wl_Thin.Shell_Surface_Ptr;
   use type Wl_Thin.Surface_Ptr;
   use type Wl_Thin.Seat_Ptr;
   use type Wl_Thin.Pointer_Ptr;
   use type Wl_Thin.Keyboard_Ptr;
   use type Wl_Thin.Touch_Ptr;
   use type Wl_Thin.Output_Ptr;
   use type Wl_Thin.Region_Ptr;
   use type Wl_Thin.Subcompositor_Ptr;
   use type Wl_Thin.Subsurface_Ptr;

   type Display is tagged limited record
      My_Display : Wl_Thin.Display_Ptr;
      My_Fd      : Integer;
   end record;

   function Is_Connected (Display : Wayland.Client.Display) return Boolean is
     (Display.My_Display /= null);

   type Registry is tagged limited record
      My_Registry : Wl_Thin.Registry_Ptr;
   end record;

   function Has_Proxy (Registry : Wayland.Client.Registry) return Boolean is
     (Registry.My_Registry /= null);

   type Compositor is tagged limited record
      My_Compositor : Wl_Thin.Compositor_Ptr;
   end record;

   function Has_Proxy (Compositor : Wayland.Client.Compositor) return Boolean is
     (Compositor.My_Compositor /= null);

   type Pointer is tagged limited record
      My_Pointer : Wl_Thin.Pointer_Ptr;
   end record;

   function Has_Proxy (Pointer : Wayland.Client.Pointer) return Boolean is
     (Pointer.My_Pointer /= null);

   type Seat is tagged limited record
      My_Seat : Wl_Thin.Seat_Ptr;
   end record;

   function Has_Proxy (Seat : Wayland.Client.Seat) return Boolean is
     (Seat.My_Seat /= null);

   type Shell is tagged limited record
      My_Shell : Wl_Thin.Shell_Ptr;
   end record;

   function Has_Proxy (Shell : Wayland.Client.Shell) return Boolean is
     (Shell.My_Shell /= null);

   type Shm is tagged limited record
      My_Shm : Wl_Thin.Shm_Ptr;
   end record;

   function Has_Proxy (Shm : Wayland.Client.Shm) return Boolean is (Shm.My_Shm /= null);

   type Shm_Pool is tagged limited record
      My_Shm_Pool : Wl_Thin.Shm_Pool_Ptr;
   end record;

   function Has_Proxy (Pool : Shm_Pool) return Boolean is
     (Pool.My_Shm_Pool /= null);

   type Buffer is tagged limited record
      My_Buffer : Wl_Thin.Buffer_Ptr;
   end record;

   function Has_Proxy (Buffer : Wayland.Client.Buffer) return Boolean is
     (Buffer.My_Buffer /= null);

   type Surface is tagged limited record
      My_Surface : Wl_Thin.Surface_Ptr;
   end record;

   function Has_Proxy (Surface : Wayland.Client.Surface) return Boolean is
     (Surface.My_Surface /= null);

   type Shell_Surface is tagged limited record
      My_Shell_Surface : Wl_Thin.Shell_Surface_Ptr;
   end record;

   function Has_Proxy (Surface : Shell_Surface) return Boolean is
     (Surface.My_Shell_Surface /= null);

   type Callback is tagged limited record
      My_Callback : Wl_Thin.Callback_Ptr;
   end record;

   type Data_Offer is tagged limited record
      My_Data_Offer : Wl_Thin.Data_Offer_Ptr;
   end record;

   type Data_Source is tagged limited record
      My_Data_Source : Wl_Thin.Data_Source_Ptr;
   end record;

   function Has_Proxy (Data_Source : Wayland.Client.Data_Source) return Boolean is
     (Data_Source.My_Data_Source /= null);

   type Data_Device is tagged limited record
      My_Data_Device : Wl_Thin.Data_Device_Ptr;
   end record;

   function Has_Proxy (Data_Device : Wayland.Client.Data_Device) return Boolean is
     (Data_Device.My_Data_Device /= null);

   type Data_Device_Manager is tagged limited record
      My_Data_Device_Manager : Wl_Thin.Data_Device_Manager_Ptr;
   end record;

   function Has_Proxy (Data_Device_Manager : Wayland.Client.Data_Device_Manager) return Boolean is
     (Data_Device_Manager.My_Data_Device_Manager /= null);

   type Keyboard is tagged limited record
      My_Keyboard : Wl_Thin.Keyboard_Ptr;
   end record;

   function Has_Proxy (Keyboard : Wayland.Client.Keyboard) return Boolean is
     (Keyboard.My_Keyboard /= null);

   type Touch is tagged limited record
      My_Touch : Wl_Thin.Touch_Ptr;
   end record;

   function Has_Proxy (Touch : Wayland.Client.Touch) return Boolean is
     (Touch.My_Touch /= null);

   type Output is tagged limited record
      My_Output : Wl_Thin.Output_Ptr;
   end record;

   function Has_Proxy (Output : Wayland.Client.Output) return Boolean is
     (Output.My_Output /= null);

   type Region is tagged limited record
      My_Region : Wl_Thin.Region_Ptr;
   end record;

   function Has_Proxy (Region : Wayland.Client.Region) return Boolean is
     (Region.My_Region /= null);

   type Subcompositor is tagged limited record
      My_Subcompositor : Wl_Thin.Subcompositor_Ptr;
   end record;

   function Has_Proxy (Subcompositor : Wayland.Client.Subcompositor) return Boolean is
     (Subcompositor.My_Subcompositor /= null);

   type Subsurface is tagged limited record
      My_Subsurface : Wl_Thin.Subsurface_Ptr;
   end record;

   function Has_Proxy (Subsurface : Wayland.Client.Subsurface) return Boolean is
     (Subsurface.My_Subsurface /= null);

   type Interface_Type is tagged limited record
      My_Interface : not null Wl_Thin.Interface_Ptr;
   end record;

   function Name (I : Interface_Type) return String is
     (Value (I.My_Interface.Name));

   Display_Interface : constant Interface_Type :=
     (My_Interface => Wl_Thin.Display_Interface'Access);

   Registry_Interface : constant Interface_Type :=
     (My_Interface => Wl_Thin.Registry_Interface'Access);

   Callback_Interface : constant Interface_Type :=
     (My_Interface => Wl_Thin.Callback_Interface'Access);

   Compositor_Interface : constant Interface_Type :=
     (My_Interface => Wl_Thin.Compositor_Interface'Access);

   Shm_Pool_Interface : constant Interface_Type :=
     (My_Interface => Wl_Thin.Shm_Pool_Interface'Access);

   Shm_Interface : constant Interface_Type :=
     (My_Interface => Wl_Thin.Shm_Interface'Access);

   Buffer_Interface : constant Interface_Type :=
     (My_Interface => Wl_Thin.Buffer_Interface'Access);

   Data_Offer_Interface : constant Interface_Type :=
     (My_Interface => Wl_Thin.Data_Offer_Interface'Access);

   Data_Source_Interface : constant Interface_Type :=
     (My_Interface => Wl_Thin.Data_Source_Interface'Access);

   Data_Device_Interface : constant Interface_Type :=
     (My_Interface => Wl_Thin.Data_Device_Interface'Access);

   Data_Device_Manager_Interface : constant Interface_Type :=
     (My_Interface => Wl_Thin.Data_Device_Manager_Interface'Access);

   Shell_Interface : constant Interface_Type :=
     (My_Interface => Wl_Thin.Shell_Interface'Access);

   Shell_Surface_Interface : constant Interface_Type :=
     (My_Interface => Wl_Thin.Shell_Surface_Interface'Access);

   Surface_Interface : constant Interface_Type :=
     (My_Interface => Wl_Thin.Surface_Interface'Access);

   Seat_Interface : constant Interface_Type :=
     (My_Interface => Wl_Thin.Seat_Interface'Access);

   Pointer_Interface : constant Interface_Type :=
     (My_Interface => Wl_Thin.Pointer_Interface'Access);

   Keyboard_Interface : constant Interface_Type :=
     (My_Interface => Wl_Thin.Keyboard_Interface'Access);

   Touch_Interface : constant Interface_Type :=
     (My_Interface => Wl_Thin.Touch_Interface'Access);

   Output_Interface : constant Interface_Type :=
     (My_Interface => Wl_Thin.Output_Interface'Access);

   Region_Interface : constant Interface_Type :=
     (My_Interface => Wl_Thin.Region_Interface'Access);

   Subcompositor_Interface : constant Interface_Type :=
     (My_Interface => Wl_Thin.Subcompositor_Interface'Access);

   Subsurface_Interface : constant Interface_Type :=
     (My_Interface => Wl_Thin.Subsurface_Interface'Access);

end Wayland.Client;
