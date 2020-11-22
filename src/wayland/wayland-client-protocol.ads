private with Interfaces.C.Strings;
private with Wayland.Client.Thin;

with C_Binding.Linux.Files;

package Wayland.Client.Protocol is
   pragma Preelaborate;

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

   type Call_Result_Code is (Success, Error);

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

   function Has_Proxy (Subcompositor : Protocol.Subcompositor) return Boolean;

   procedure Destroy (S : in out Protocol.Subcompositor) with
     Pre    => S.Has_Proxy,
     Post   => not S.Has_Proxy;

   function Get_Version (S : Protocol.Subcompositor) return Unsigned_32 with
     Pre => S.Has_Proxy;

   procedure Get_Subsurface (Subcompositor : Protocol.Subcompositor;
                             Surface       : Protocol.Surface;
                             Parent        : Protocol.Surface;
                             Subsurface    : in out Protocol.Subsurface);

   type Compositor is tagged limited private;

   function Has_Proxy (Compositor : Protocol.Compositor) return Boolean;

   procedure Get_Proxy (Compositor  : in out Protocol.Compositor;
                        Registry    : Protocol.Registry;
                        Id          : Unsigned_32;
                        Version     : Unsigned_32) with
     Pre => not Compositor.Has_Proxy and Has_Proxy (Registry);

   procedure Get_Surface_Proxy (Compositor : Protocol.Compositor;
                                Surface    : in out Protocol.Surface) with
     Pre => Compositor.Has_Proxy;
   --  Ask the compositor to create a new surface. When success, Surface has
   --  When success Surface.Has_Proxy = True,
   --  otherwise Surface.Has_Proxy = False.

   procedure Get_Region_Proxy (Compositor : Protocol.Compositor;
                               Region     : in out Protocol.Region) with
     Pre => Compositor.Has_Proxy;
   --  Ask the compositor to create a new region.
   --  When success Region.Has_Proxy = True, otherwise Region.Has_Proxy = False.

   procedure Destroy (Compositor : in out Protocol.Compositor) with
     Pre  => Compositor.Has_Proxy,
     Post => not Compositor.Has_Proxy;

   type Shell is tagged limited private;

   function Has_Proxy (Shell : Protocol.Shell) return Boolean;

   procedure Get_Proxy (Shell    : in out Protocol.Shell;
                        Registry : Protocol.Registry;
                        Id       : Unsigned_32;
                        Version  : Unsigned_32) with
     Pre => Has_Proxy (Registry);

   procedure Get_Shell_Surface
     (Shell         : Protocol.Shell;
      Surface       : Protocol.Surface;
      Shell_Surface : in out Protocol.Shell_Surface) with
     Pre => Shell.Has_Proxy;

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

   procedure Move (Surface : Shell_Surface;
                   Seat    : Protocol.Seat;
                   Serial  : Unsigned_32) with
     Pre => Surface.Has_Proxy;

   procedure Resize (Surface : Shell_Surface;
                     Seat    : Protocol.Seat;
                     Serial  : Unsigned_32;
                     Edges   : Unsigned_32) with
     Pre => Surface.Has_Proxy;

   procedure Set_Toplevel (Surface : Shell_Surface) with
     Pre => Surface.Has_Proxy;

   procedure Set_Transient (Surface : Shell_Surface;
                            Parent  : Protocol.Surface;
                            X       : Integer;
                            Y       : Integer;
                            Flags   : Unsigned_32) with
     Pre => Surface.Has_Proxy;

   procedure Set_Fullscreen (Surface   : Shell_Surface;
                             Method    : Unsigned_32;
                             Framerate : Unsigned_32;
                             Output    : Protocol.Output) with
     Pre => Surface.Has_Proxy;

   procedure Set_Popup (Surface : Shell_Surface;
                        Seat    : Protocol.Seat;
                        Serial  : Unsigned_32;
                        Parent  : Protocol.Surface;
                        X       : Integer;
                        Y       : Integer;
                        Flags   : Unsigned_32) with
     Pre => Surface.Has_Proxy;

   procedure Set_Maximized (Surface : Shell_Surface;
                            Output  : Protocol.Output) with
     Pre => Surface.Has_Proxy;

   procedure Set_Title (Surface : Shell_Surface;
                        Title   : String) with
     Pre => Surface.Has_Proxy;

   procedure Set_Class (Surface : Shell_Surface;
                        Class_V : String) with
     Pre => Surface.Has_Proxy;

   type Data_Device_Manager is tagged limited private;

   function Has_Proxy (Data_Device_Manager : Protocol.Data_Device_Manager) return Boolean;

   procedure Destroy (Manager : in out Data_Device_Manager) with
     Pre    => Manager.Has_Proxy,
     Post   => not Manager.Has_Proxy;

   function Get_Version (Manager : Data_Device_Manager) return Unsigned_32 with
     Pre => Manager.Has_Proxy;

   procedure Create_Data_Source (Manager : Data_Device_Manager;
                                 Source  : in out Data_Source);
   --  Create a new data source.

   procedure Get_Data_Device (Manager : Data_Device_Manager;
                              Seat    : Protocol.Seat;
                              Device  : in out Data_Device);
   --  Create a new data device for a given seat.

   type Seat is tagged limited private;

   function Has_Proxy (Seat : Protocol.Seat) return Boolean;

   function Get_Version (Seat : Protocol.Seat) return Unsigned_32 with
     Pre => Seat.Has_Proxy;

   procedure Get_Proxy (Seat     : in out Protocol.Seat;
                        Registry : Protocol.Registry;
                        Id       : Unsigned_32;
                        Version  : Unsigned_32) with
     Pre => Has_Proxy (Registry);

   procedure Get_Pointer (Seat    : Protocol.Seat;
                          Pointer : in out Protocol.Pointer) with
     Pre => Seat.Has_Proxy and not Has_Proxy (Pointer);

   procedure Get_Keyboard (Seat     : Protocol.Seat;
                           Keyboard : in out Protocol.Keyboard) with
     Pre => Seat.Has_Proxy and not Has_Proxy (Keyboard);

   procedure Get_Touch (Seat  : Protocol.Seat;
                        Touch : in out Protocol.Touch) with
     Pre => Seat.Has_Proxy and not Has_Proxy (Touch);

   procedure Release (Seat : in out Protocol.Seat) with
     Pre => Seat.Has_Proxy;

   type Pointer is tagged limited private;

   function Has_Proxy (Pointer : Protocol.Pointer) return Boolean;

   function Get_Version (Pointer : Protocol.Pointer) return Unsigned_32 with
     Pre => Pointer.Has_Proxy;

   procedure Destroy (Pointer : in out Protocol.Pointer) with
     Pre  => Pointer.Has_Proxy,
     Post => not Pointer.Has_Proxy;

   procedure Set_Cursor (Pointer   : Protocol.Pointer;
                         Serial    : Unsigned_32;
                         Surface   : Protocol.Surface;
                         Hotspot_X : Integer;
                         Hotspot_Y : Integer) with
     Pre => Pointer.Has_Proxy;

   procedure Release (Pointer : in out Protocol.Pointer) with
     Pre  => Pointer.Has_Proxy,
     Post => not Pointer.Has_Proxy;

   type Shm is tagged limited private;

   function Has_Proxy (Shm : Protocol.Shm) return Boolean;

   procedure Get_Proxy (Shm      : in out Protocol.Shm;
                        Registry : Protocol.Registry;
                        Id       : Unsigned_32;
                        Version  : Unsigned_32) with
     Pre => Has_Proxy (Registry);

   procedure Create_Pool (Shm             : Protocol.Shm;
                          File_Descriptor : C_Binding.Linux.Files.File;
                          Size            : Integer;
                          Pool            : in out Shm_Pool);

   function Get_Version (Shm : Protocol.Shm) return Unsigned_32 with
     Pre => Shm.Has_Proxy;

   procedure Destroy (Shm : in out Protocol.Shm) with
     Pre  => Shm.Has_Proxy,
     Post => not Shm.Has_Proxy;

   type Shm_Pool is tagged limited private;

   function Has_Proxy (Pool : Protocol.Shm_Pool) return Boolean;

   procedure Create_Buffer (Pool   : Protocol.Shm_Pool;
                            Offset : Integer;
                            Width  : Integer;
                            Height : Integer;
                            Stride : Integer;
                            Format : Shm_Format;
                            Buffer : in out Protocol.Buffer) with
     Pre => Pool.Has_Proxy;

   procedure Resize (Pool : Protocol.Shm_Pool;
                     Size : Integer) with
     Pre => Pool.Has_Proxy;

   function Get_Version (Pool : Protocol.Shm_Pool) return Unsigned_32 with
     Pre => Pool.Has_Proxy;

   procedure Destroy (Pool : in out Protocol.Shm_Pool) with
     Pre  => Pool.Has_Proxy,
     Post => not Pool.Has_Proxy;

   type Data_Device is tagged limited private;

   function Has_Proxy (Data_Device : Protocol.Data_Device) return Boolean;

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

   procedure Set_Selection (Device : Data_Device;
                            Source : Data_Source;
                            Serial : Unsigned_32) with
     Pre => Device.Has_Proxy and Has_Proxy (Source);

   procedure Release (Device : in out Data_Device) with
     Pre    => Device.Has_Proxy,
     Post   => not Device.Has_Proxy;

   type Subsurface is tagged limited private;

   function Has_Proxy (Subsurface : Protocol.Subsurface) return Boolean;

   procedure Destroy (Subsurface : in out Protocol.Subsurface) with
     Pre    => Subsurface.Has_Proxy,
     Post   => not Subsurface.Has_Proxy;

   function Get_Version (Subsurface : Protocol.Subsurface) return Unsigned_32 with
     Pre => Subsurface.Has_Proxy;

   procedure Set_Position (Subsurface : Protocol.Subsurface;
                           X          : Integer;
                           Y          : Integer) with
     Pre => Subsurface.Has_Proxy;

   procedure Place_Above (Subsurface : Protocol.Subsurface;
                          Sibling    : Protocol.Surface) with
     Pre => Subsurface.Has_Proxy and Has_Proxy (Sibling);

   procedure Place_Below (Subsurface : Protocol.Subsurface;
                          Sibling    : Protocol.Surface) with
     Pre => Subsurface.Has_Proxy and Has_Proxy (Sibling);

   procedure Set_Sync (Subsurface : Protocol.Subsurface) with
     Pre => Subsurface.Has_Proxy;

   procedure Set_Desync (Subsurface : Protocol.Subsurface) with
     Pre => Subsurface.Has_Proxy;

   type Surface is tagged limited private;

   function Has_Proxy (Surface : Protocol.Surface) return Boolean;

   procedure Attach (Surface : Protocol.Surface;
                     Buffer  : Protocol.Buffer;
                     X       : Integer;
                     Y       : Integer) with
     Pre => Surface.Has_Proxy and Has_Proxy (Buffer);

   procedure Damage (Surface : Protocol.Surface;
                     X       : Integer;
                     Y       : Integer;
                     Width   : Integer;
                     Height  : Integer) with
     Pre => Surface.Has_Proxy;

   function Frame (Surface : Protocol.Surface) return Callback with
     Pre => Surface.Has_Proxy;

   procedure Set_Opaque_Region (Surface : Protocol.Surface;
                                Region  : Protocol.Region) with
     Pre => Surface.Has_Proxy;

   procedure Set_Input_Region (Surface : Protocol.Surface;
                               Region  : Protocol.Region) with
     Pre => Surface.Has_Proxy;

   procedure Commit (Surface : Protocol.Surface) with
     Pre => Surface.Has_Proxy;

   procedure Set_Buffer_Transform (Surface   : Protocol.Surface;
                                   Transform : Integer) with
     Pre => Surface.Has_Proxy;

   procedure Set_Buffer_Scale (Surface : Protocol.Surface;
                               Scale   : Integer) with
     Pre => Surface.Has_Proxy;

   procedure Damage_Buffer (Surface : Protocol.Surface;
                            X       : Integer;
                            Y       : Integer;
                            Width   : Integer;
                            Height  : Integer) with
     Pre => Surface.Has_Proxy;

   procedure Destroy (Surface : in out Protocol.Surface) with
     Pre  => Surface.Has_Proxy,
     Post => not Surface.Has_Proxy;

   type Buffer is tagged limited private;

   function Has_Proxy (Buffer : Protocol.Buffer) return Boolean;

   function Get_Version (Buffer : Protocol.Buffer) return Unsigned_32 with
     Pre => Buffer.Has_Proxy;

   procedure Destroy (Buffer : in out Protocol.Buffer) with
     Pre  => Buffer.Has_Proxy,
     Post => not Buffer.Has_Proxy;

   type Display is tagged limited private with
     Default_Initial_Condition => not Display.Is_Connected;

   function Is_Connected (Display : Protocol.Display) return Boolean;

   procedure Connect (Display : in out Protocol.Display;
                      Name    : String := Default_Display_Name) with
     Pre => not Display.Is_Connected;
   --  Attempts connecting with the Wayland server.

   type Check_For_Events_Status is (Events_Need_Processing, No_Events, Error);

   function Check_For_Events (Display : Protocol.Display;
                              Timeout : Integer) return Check_For_Events_Status;
   --  The timeout is given in milliseconds.

   function Dispatch (Display : Protocol.Display) return Integer with
     Pre => Display.Is_Connected;
   --  Process incoming events.

   procedure Dispatch (Display : Protocol.Display) with
     Pre => Display.Is_Connected;
   --  Process incoming events. Ignores error code. To be removed?

   function Dispatch_Pending
     (Display : Protocol.Display) return Integer with
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
     (Display : Protocol.Display) return Integer with
     Pre => Display.Is_Connected;
   --  Prepare to read events from the display's file descriptor

   function Read_Events
     (Display : Protocol.Display) return Call_Result_Code with
     Pre => Display.Is_Connected;
   --  Returns 0 on success or -1 on error.

   procedure Cancel_Read (Display : Protocol.Display) with
     Pre => Display.Is_Connected;
   --  Cancel read intention on display's fd.

   function Roundtrip (Display : Protocol.Display) return Integer with
     Pre => Display.Is_Connected;

   procedure Roundtrip (Display : Protocol.Display) with
     Pre => Display.Is_Connected;

   procedure Disconnect (Display : in out Protocol.Display) with
     Pre  => Display.Is_Connected,
     Post => not Display.Is_Connected;

   function Get_Version (Display : Protocol.Display) return Unsigned_32 with
     Pre => Display.Is_Connected;

   procedure Get_Registry (Display  : Protocol.Display;
                           Registry : in out Protocol.Registry) with
     Pre => Display.Is_Connected and not Has_Proxy (Registry);

   function Sync (Display : Protocol.Display) return Callback with
     Pre => Display.Is_Connected;

   type Registry is tagged limited private;

   function Has_Proxy (Registry : Protocol.Registry) return Boolean;

   procedure Destroy (Registry : in out Protocol.Registry) with
     Pre  => Registry.Has_Proxy,
     Post => not Registry.Has_Proxy;

   function Get_Version (Registry : Protocol.Registry) return Unsigned_32 with
     Pre => Registry.Has_Proxy;

   type Callback is tagged limited private;

   function Has_Proxy (Callback : Protocol.Callback) return Boolean;

   procedure Destroy (Callback : in out Protocol.Callback) with
     Pre    => Callback.Has_Proxy,
     Post   => not Callback.Has_Proxy;

   function Get_Version (Callback : Protocol.Callback) return Unsigned_32 with
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

   procedure Finish (Offer : Data_Offer) with
     Pre => Offer.Has_Proxy;

   procedure Set_Actions (Offer            : Data_Offer;
                          Dnd_Actions      : Unsigned_32;
                          Preferred_Action : Unsigned_32) with
     Pre => Offer.Has_Proxy;

   type Data_Source is tagged limited private;

   function Has_Proxy (Data_Source : Protocol.Data_Source) return Boolean;

   procedure Destroy (Source : in out Data_Source) with
     Pre    => Source.Has_Proxy,
     Post   => not Source.Has_Proxy;

   function Get_Version (Source : Data_Source) return Unsigned_32 with
     Pre => Source.Has_Proxy;

   procedure Offer (Source    : Data_Source;
                    Mime_Type : String) with
     Pre => Source.Has_Proxy;

   procedure Set_Actions (Source      : Data_Source;
                          Dnd_Actions : Unsigned_32) with
     Pre => Source.Has_Proxy;

   type Keyboard is tagged limited private;

   function Has_Proxy (Keyboard : Protocol.Keyboard) return Boolean;

   procedure Destroy (Keyboard : in out Protocol.Keyboard) with
     Pre    => Keyboard.Has_Proxy,
     Post   => not Keyboard.Has_Proxy;

   function Get_Version (Keyboard : Protocol.Keyboard) return Unsigned_32 with
     Pre => Keyboard.Has_Proxy;

   procedure Release (Keyboard : in out Protocol.Keyboard) with
     Pre    => Keyboard.Has_Proxy,
     Post   => not Keyboard.Has_Proxy;

   type Touch is tagged limited private;

   function Has_Proxy (Touch : Protocol.Touch) return Boolean;

   procedure Destroy (Touch : in out Protocol.Touch) with
     Pre    => Touch.Has_Proxy,
     Post   => not Touch.Has_Proxy;

   function Get_Version (Touch : Protocol.Touch) return Unsigned_32 with
     Pre => Touch.Has_Proxy;

   procedure Release (Touch : in out Protocol.Touch) with
     Pre    => Touch.Has_Proxy,
     Post   => not Touch.Has_Proxy;

   type Output is tagged limited private;

   function Has_Proxy (Output : Protocol.Output) return Boolean;

   procedure Destroy (Output : in out Protocol.Output) with
     Pre    => Output.Has_Proxy,
     Post   => not Output.Has_Proxy;

   function Get_Version (Output : Protocol.Output) return Unsigned_32 with
     Pre => Output.Has_Proxy;

   procedure Release (Output : in out Protocol.Output) with
     Pre    => Output.Has_Proxy,
     Post   => not Output.Has_Proxy;

   type Region is tagged limited private;

   function Has_Proxy (Region : Protocol.Region) return Boolean;

   procedure Destroy (Region : in out Protocol.Region) with
     Pre    => Region.Has_Proxy,
     Post   => not Region.Has_Proxy;

   function Get_Version (Region : Protocol.Region) return Unsigned_32 with
     Pre => Region.Has_Proxy;

   procedure Add (Region : Protocol.Region;
                  X      : Integer;
                  Y      : Integer;
                  Width  : Integer;
                  Height : Integer) with
     Pre => Region.Has_Proxy;

   procedure Subtract (Region : Protocol.Region;
                       X      : Integer;
                       Y      : Integer;
                       Width  : Integer;
                       Height : Integer) with
     Pre => Region.Has_Proxy;

   generic
      type Data_Type is limited private;
      type Data_Ptr is access all Data_Type;

      with procedure Error (Data      : not null Data_Ptr;
                            Display   : Protocol.Display;
                            Object_Id : Void_Ptr;
                            Code      : Unsigned_32;
                            Message   : String);
      --  TODO Should really Object_Id really be exposed here? This part
      --  of the API can potentially be improved upon.

      with procedure Delete_Id (Data    : not null Data_Ptr;
                                Display : Protocol.Display;
                                Id      : Unsigned_32);

   package Display_Events is

      function Subscribe (Display : in out Protocol.Display;
                          Data    : not null Data_Ptr) return Call_Result_Code;

   end Display_Events;

   generic
      type Data_Type is limited private;
      type Data_Ptr is access all Data_Type;

      with procedure Global_Object_Added (Data     : not null Data_Ptr;
                                          Registry : Protocol.Registry;
                                          Id       : Unsigned_32;
                                          Name     : String;
                                          Version  : Unsigned_32);

      with procedure Global_Object_Removed (Data     : not null Data_Ptr;
                                            Registry : Protocol.Registry;
                                            Id       : Unsigned_32);
   package Registry_Events is

      function Subscribe (Registry : in out Protocol.Registry;
                          Data     : not null Data_Ptr) return Call_Result_Code;
      --  Starts subcription to global objects addded and removed events.
      --  To stop subscription, call Registry.Destroy.

   end Registry_Events;

   generic
      type Data_Type is limited private;
      type Data_Ptr is access all Data_Type;

      with procedure Done (Data          : not null Data_Ptr;
                           Callback      : Protocol.Callback;
                           Callback_Data : Unsigned_32);

   package Callback_Events is

      function Subscribe (Callback : in out Protocol.Callback;
                          Data     : not null Data_Ptr) return Call_Result_Code;

   end Callback_Events;

   generic
      type Data_Type is limited private;
      type Data_Ptr is access all Data_Type;

      with procedure Format (Data   : not null Data_Ptr;
                             Shm    : Protocol.Shm;
                             Format : Unsigned_32);
   package Shm_Events is

      function Subscribe (Shm  : in out Protocol.Shm;
                          Data : not null Data_Ptr) return Call_Result_Code;

   end Shm_Events;

   generic
      type Data_Type is limited private;
      type Data_Ptr is access all Data_Type;

      with procedure Release (Data   : not null Data_Ptr;
                              Buffer : Protocol.Buffer);

   package Buffer_Events is

      function Subscribe (Buffer : in out Protocol.Buffer;
                          Data   : not null Data_Ptr) return Call_Result_Code;

   end Buffer_Events;

   generic
      type Data_Type is limited private;
      type Data_Ptr is access all Data_Type;

      with procedure Offer (Data       : not null Data_Ptr;
                            Data_Offer : Protocol.Data_Offer;
                            Mime_Type  : String);

      with procedure Source_Actions (Data           : not null Data_Ptr;
                                     Data_Offer     : Protocol.Data_Offer;
                                     Source_Actions : Unsigned_32);

      with procedure Action (Data       : not null Data_Ptr;
                             Data_Offer : Protocol.Data_Offer;
                             Dnd_Action : Unsigned_32);

   package Data_Offer_Events is

      function Subscribe
        (Data_Offer : in out Protocol.Data_Offer;
         Data       : not null Data_Ptr) return Call_Result_Code;

   end Data_Offer_Events;

   generic
      type Data_Type is limited private;
      type Data_Ptr is access all Data_Type;

      with procedure Target (Data        : not null Data_Ptr;
                             Data_Source : Protocol.Data_Source;
                             Mime_Type   : String);

      with procedure Send (Data        : not null Data_Ptr;
                           Data_Source : Protocol.Data_Source;
                           Mime_Type   : String;
                           Fd          : Integer);

      with procedure Cancelled (Data        : not null Data_Ptr;
                                Data_Source : Protocol.Data_Source);

      with procedure Dnd_Drop_Performed
        (Data        : not null Data_Ptr;
         Data_Source : Protocol.Data_Source);

      with procedure Dnd_Finished (Data        : not null Data_Ptr;
                                   Data_Source : Protocol.Data_Source);

      with procedure Action (Data        : not null Data_Ptr;
                             Data_Source : Protocol.Data_Source;
                             Dnd_Action  : Unsigned_32);

   package Data_Source_Events is

      function Subscribe
        (Data_Source : in out Protocol.Data_Source;
         Data        : not null Data_Ptr) return Call_Result_Code;

   end Data_Source_Events;

   generic
      type Data_Type is limited private;
      type Data_Ptr is access all Data_Type;

      with procedure Data_Offer (Data        : not null Data_Ptr;
                                 Data_Device : Protocol.Data_Device;
                                 Id          : Unsigned_32);

      with procedure Enter (Data        : not null Data_Ptr;
                            Data_Device : Protocol.Data_Device;
                            Serial      : Unsigned_32;
                            Surface     : Protocol.Surface;
                            X           : Fixed;
                            Y           : Fixed;
                            Id          : Protocol.Data_Offer);

      with procedure Leave (Data        : not null Data_Ptr;
                            Data_Device : Protocol.Data_Device);

      with procedure Motion (Data        : not null Data_Ptr;
                             Data_Device : Protocol.Data_Device;
                             Time        : Unsigned_32;
                             X           : Fixed;
                             Y           : Fixed);

      with procedure Drop (Data        : not null Data_Ptr;
                           Data_Device : Protocol.Data_Device);

      with procedure Selection (Data        : not null Data_Ptr;
                                Data_Device : Protocol.Data_Device;
                                Id          : Protocol.Data_Offer);

   package Data_Device_Events is

      function Subscribe
        (Data_Device : in out Protocol.Data_Device;
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
         Surface : Protocol.Surface;
         Output  : Protocol.Output);

      with procedure Leave
        (Data    : not null Data_Ptr;
         Surface : Protocol.Surface;
         Output  : Protocol.Output);

   package Surface_Events is

      function Subscribe
        (Surface : in out Protocol.Surface;
         Data    : not null Data_Ptr) return Call_Result_Code;

   end Surface_Events;

   generic
      type Data_Type is limited private;
      type Data_Ptr is access all Data_Type;

      with procedure Seat_Capabilities
        (Data         : not null Data_Ptr;
         Seat         : Protocol.Seat;
         Capabilities : Unsigned_32);

      with procedure Seat_Name
        (Data : not null Data_Ptr;
         Seat : Protocol.Seat;
         Name : String);

   package Seat_Events is

      function Subscribe (Seat : in out Protocol.Seat;
                          Data : not null Data_Ptr) return Call_Result_Code;

   end Seat_Events;

   generic
      type Data_Type is limited private;
      type Data_Ptr is access all Data_Type;

      with procedure Pointer_Enter
        (Data      : not null Data_Ptr;
         Pointer   : Protocol.Pointer;
         Serial    : Unsigned_32;
         Surface   : Protocol.Surface;
         Surface_X : Fixed;
         Surface_Y : Fixed);

      with procedure Pointer_Leave
        (Data    : not null Data_Ptr;
         Pointer : Protocol.Pointer;
         Serial  : Unsigned_32;
         Surface : Protocol.Surface);

      with procedure Pointer_Motion
        (Data      : not null Data_Ptr;
         Pointer   : Protocol.Pointer;
         Time      : Unsigned_32;
         Surface_X : Fixed;
         Surface_Y : Fixed);

      with procedure Pointer_Button
        (Data    : not null Data_Ptr;
         Pointer : Protocol.Pointer;
         Serial  : Unsigned_32;
         Time    : Unsigned_32;
         Button  : Unsigned_32;
         State   : Unsigned_32);

      with procedure Pointer_Axis
        (Data    : not null Data_Ptr;
         Pointer : Protocol.Pointer;
         Time    : Unsigned_32;
         Axis    : Unsigned_32;
         Value   : Fixed);

      with procedure Pointer_Frame (Data    : not null Data_Ptr;
                                    Pointer : Protocol.Pointer);

      with procedure Pointer_Axis_Source
        (Data        : not null Data_Ptr;
         Pointer     : Protocol.Pointer;
         Axis_Source : Unsigned_32);

      with procedure Pointer_Axis_Stop
        (Data    : not null Data_Ptr;
         Pointer : Protocol.Pointer;
         Time    : Unsigned_32;
         Axis    : Unsigned_32);

      with procedure Pointer_Axis_Discrete
        (Data     : not null Data_Ptr;
         Pointer  : Protocol.Pointer;
         Axis     : Unsigned_32;
         Discrete : Integer);

   package Pointer_Events is

      function Subscribe (Pointer : in out Protocol.Pointer;
                          Data    : not null Data_Ptr) return Call_Result_Code;

   end Pointer_Events;
   --  Pointer Axis Events are for example scroll wheel rotation

   generic
      type Data_Type is limited private;
      type Data_Ptr is access all Data_Type;

      with procedure Keymap (Data     : not null Data_Ptr;
                             Keyboard : Protocol.Keyboard;
                             Format   : Unsigned_32;
                             Fd       : Integer;
                             Size     : Unsigned_32);

      with procedure Enter (Data     : not null Data_Ptr;
                            Keyboard : Protocol.Keyboard;
                            Serial   : Unsigned_32;
                            Surface  : Protocol.Surface;
                            Keys     : Wayland_Array_T);

      with procedure Leave (Data     : not null Data_Ptr;
                            Keyboard : Protocol.Keyboard;
                            Serial   : Unsigned_32;
                            Surface  : Protocol.Surface);

      with procedure Key (Data     : not null Data_Ptr;
                          Keyboard : Protocol.Keyboard;
                          Serial   : Unsigned_32;
                          Time     : Unsigned_32;
                          Key      : Unsigned_32;
                          State    : Unsigned_32);

      with procedure Modifiers (Data           : not null Data_Ptr;
                                Keyboard       : Protocol.Keyboard;
                                Serial         : Unsigned_32;
                                Mods_Depressed : Unsigned_32;
                                Mods_Latched   : Unsigned_32;
                                Mods_Locked    : Unsigned_32;
                                Group          : Unsigned_32);

      with procedure Repeat_Info (Data     : not null Data_Ptr;
                                  Keyboard : Protocol.Keyboard;
                                  Rate     : Integer;
                                  Delay_V  : Integer);

   package Keyboard_Events is

      function Subscribe (Keyboard : in out Protocol.Keyboard;
                          Data     : not null Data_Ptr) return Call_Result_Code;

   end Keyboard_Events;

   generic
      type Data_Type is limited private;
      type Data_Ptr is access all Data_Type;

      with procedure Down (Data    : not null Data_Ptr;
                           Touch   : Protocol.Touch;
                           Serial  : Unsigned_32;
                           Time    : Unsigned_32;
                           Surface : Protocol.Surface;
                           Id      : Integer;
                           X       : Fixed;
                           Y       : Fixed);

      with procedure Up (Data   : not null Data_Ptr;
                         Touch  : Protocol.Touch;
                         Serial : Unsigned_32;
                         Time   : Unsigned_32;
                         Id     : Integer);

      with procedure Motion (Data  : not null Data_Ptr;
                             Touch : Protocol.Touch;
                             Time  : Unsigned_32;
                             Id    : Integer;
                             X     : Fixed;
                             Y     : Fixed);

      with procedure Frame (Data  : not null Data_Ptr;
                            Touch : Protocol.Touch);

      with procedure Cancel (Data  : not null Data_Ptr;
                             Touch : Protocol.Touch);

      with procedure Shape (Data  : not null Data_Ptr;
                            Touch : Protocol.Touch;
                            Id    : Integer;
                            Major : Fixed;
                            Minor : Fixed);

      with procedure Orientation (Data        : not null Data_Ptr;
                                  Touch       : Protocol.Touch;
                                  Id          : Integer;
                                  Orientation : Fixed);
   package Touch_Events is

      function Subscribe (Touch : in out Protocol.Touch;
                          Data  : not null Data_Ptr) return Call_Result_Code;

   end Touch_Events;

   generic
      type Data_Type is limited private;
      type Data_Ptr is access all Data_Type;

      with procedure Geometry (Data            : not null Data_Ptr;
                               Output          : Protocol.Output;
                               X               : Integer;
                               Y               : Integer;
                               Physical_Width  : Integer;
                               Physical_Height : Integer;
                               Subpixel        : Integer;
                               Make            : String;
                               Model           : String;
                               Transform       : Integer);

      with procedure Mode (Data    : not null Data_Ptr;
                           Output  : Protocol.Output;
                           Flags   : Unsigned_32;
                           Width   : Integer;
                           Height  : Integer;
                           Refresh : Integer);

      with procedure Done (Data   : not null Data_Ptr;
                           Output : Protocol.Output);

      with procedure Scale (Data   : not null Data_Ptr;
                            Output : Protocol.Output;
                            Factor : Integer);

   package Output_Events is

      function Subscribe (Output : in out Protocol.Output;
                          Data   : not null Data_Ptr) return Call_Result_Code;

   end Output_Events;

private

   subtype char_array is Interfaces.C.char_array;

   subtype chars_ptr is Interfaces.C.Strings.chars_ptr;

   function Value
     (C : chars_ptr) return String renames Interfaces.C.Strings.Value;

   use type Thin.Display_Ptr;
   use type Thin.Registry_Ptr;
   use type Thin.Callback_Ptr;
   use type Thin.Compositor_Ptr;
   use type Thin.Shm_Pool_Ptr;
   use type Thin.Shm_Ptr;
   use type Thin.Buffer_Ptr;
   use type Thin.Data_Offer_Ptr;
   use type Thin.Data_Source_Ptr;
   use type Thin.Data_Device_Ptr;
   use type Thin.Data_Device_Manager_Ptr;
   use type Thin.Shell_Ptr;
   use type Thin.Shell_Surface_Ptr;
   use type Thin.Surface_Ptr;
   use type Thin.Seat_Ptr;
   use type Thin.Pointer_Ptr;
   use type Thin.Keyboard_Ptr;
   use type Thin.Touch_Ptr;
   use type Thin.Output_Ptr;
   use type Thin.Region_Ptr;
   use type Thin.Subcompositor_Ptr;
   use type Thin.Subsurface_Ptr;

   type Display is tagged limited record
      My_Display : Thin.Display_Ptr;
      My_Fd      : Integer;
   end record;

   function Is_Connected (Display : Protocol.Display) return Boolean is
     (Display.My_Display /= null);

   type Registry is tagged limited record
      My_Registry : Thin.Registry_Ptr;
   end record;

   function Has_Proxy (Registry : Protocol.Registry) return Boolean is
     (Registry.My_Registry /= null);

   type Compositor is tagged limited record
      My_Compositor : Thin.Compositor_Ptr;
   end record;

   function Has_Proxy (Compositor : Protocol.Compositor) return Boolean is
     (Compositor.My_Compositor /= null);

   type Pointer is tagged limited record
      My_Pointer : Thin.Pointer_Ptr;
   end record;

   function Has_Proxy (Pointer : Protocol.Pointer) return Boolean is
     (Pointer.My_Pointer /= null);

   type Seat is tagged limited record
      My_Seat : Thin.Seat_Ptr;
   end record;

   function Has_Proxy (Seat : Protocol.Seat) return Boolean is
     (Seat.My_Seat /= null);

   type Shell is tagged limited record
      My_Shell : Thin.Shell_Ptr;
   end record;

   function Has_Proxy (Shell : Protocol.Shell) return Boolean is
     (Shell.My_Shell /= null);

   type Shm is tagged limited record
      My_Shm : Thin.Shm_Ptr;
   end record;

   function Has_Proxy (Shm : Protocol.Shm) return Boolean is (Shm.My_Shm /= null);

   type Shm_Pool is tagged limited record
      My_Shm_Pool : Thin.Shm_Pool_Ptr;
   end record;

   function Has_Proxy (Pool : Shm_Pool) return Boolean is
     (Pool.My_Shm_Pool /= null);

   type Buffer is tagged limited record
      My_Buffer : Thin.Buffer_Ptr;
   end record;

   function Has_Proxy (Buffer : Protocol.Buffer) return Boolean is
     (Buffer.My_Buffer /= null);

   type Surface is tagged limited record
      My_Surface : Thin.Surface_Ptr;
   end record;

   function Has_Proxy (Surface : Protocol.Surface) return Boolean is
     (Surface.My_Surface /= null);

   type Shell_Surface is tagged limited record
      My_Shell_Surface : Thin.Shell_Surface_Ptr;
   end record;

   function Has_Proxy (Surface : Shell_Surface) return Boolean is
     (Surface.My_Shell_Surface /= null);

   type Callback is tagged limited record
      My_Callback : Thin.Callback_Ptr;
   end record;

   type Data_Offer is tagged limited record
      My_Data_Offer : Thin.Data_Offer_Ptr;
   end record;

   type Data_Source is tagged limited record
      My_Data_Source : Thin.Data_Source_Ptr;
   end record;

   function Has_Proxy (Data_Source : Protocol.Data_Source) return Boolean is
     (Data_Source.My_Data_Source /= null);

   type Data_Device is tagged limited record
      My_Data_Device : Thin.Data_Device_Ptr;
   end record;

   function Has_Proxy (Data_Device : Protocol.Data_Device) return Boolean is
     (Data_Device.My_Data_Device /= null);

   type Data_Device_Manager is tagged limited record
      My_Data_Device_Manager : Thin.Data_Device_Manager_Ptr;
   end record;

   function Has_Proxy (Data_Device_Manager : Protocol.Data_Device_Manager) return Boolean is
     (Data_Device_Manager.My_Data_Device_Manager /= null);

   type Keyboard is tagged limited record
      My_Keyboard : Thin.Keyboard_Ptr;
   end record;

   function Has_Proxy (Keyboard : Protocol.Keyboard) return Boolean is
     (Keyboard.My_Keyboard /= null);

   type Touch is tagged limited record
      My_Touch : Thin.Touch_Ptr;
   end record;

   function Has_Proxy (Touch : Protocol.Touch) return Boolean is
     (Touch.My_Touch /= null);

   type Output is tagged limited record
      My_Output : Thin.Output_Ptr;
   end record;

   function Has_Proxy (Output : Protocol.Output) return Boolean is
     (Output.My_Output /= null);

   type Region is tagged limited record
      My_Region : Thin.Region_Ptr;
   end record;

   function Has_Proxy (Region : Protocol.Region) return Boolean is
     (Region.My_Region /= null);

   type Subcompositor is tagged limited record
      My_Subcompositor : Thin.Subcompositor_Ptr;
   end record;

   function Has_Proxy (Subcompositor : Protocol.Subcompositor) return Boolean is
     (Subcompositor.My_Subcompositor /= null);

   type Subsurface is tagged limited record
      My_Subsurface : Thin.Subsurface_Ptr;
   end record;

   function Has_Proxy (Subsurface : Protocol.Subsurface) return Boolean is
     (Subsurface.My_Subsurface /= null);

   type Interface_Type is tagged limited record
      My_Interface : not null Thin.Interface_Ptr;
   end record;

   function Name (I : Interface_Type) return String is
     (Value (I.My_Interface.Name));

   Display_Interface : constant Interface_Type :=
     (My_Interface => Thin.Display_Interface'Access);

   Registry_Interface : constant Interface_Type :=
     (My_Interface => Thin.Registry_Interface'Access);

   Callback_Interface : constant Interface_Type :=
     (My_Interface => Thin.Callback_Interface'Access);

   Compositor_Interface : constant Interface_Type :=
     (My_Interface => Thin.Compositor_Interface'Access);

   Shm_Pool_Interface : constant Interface_Type :=
     (My_Interface => Thin.Shm_Pool_Interface'Access);

   Shm_Interface : constant Interface_Type :=
     (My_Interface => Thin.Shm_Interface'Access);

   Buffer_Interface : constant Interface_Type :=
     (My_Interface => Thin.Buffer_Interface'Access);

   Data_Offer_Interface : constant Interface_Type :=
     (My_Interface => Thin.Data_Offer_Interface'Access);

   Data_Source_Interface : constant Interface_Type :=
     (My_Interface => Thin.Data_Source_Interface'Access);

   Data_Device_Interface : constant Interface_Type :=
     (My_Interface => Thin.Data_Device_Interface'Access);

   Data_Device_Manager_Interface : constant Interface_Type :=
     (My_Interface => Thin.Data_Device_Manager_Interface'Access);

   Shell_Interface : constant Interface_Type :=
     (My_Interface => Thin.Shell_Interface'Access);

   Shell_Surface_Interface : constant Interface_Type :=
     (My_Interface => Thin.Shell_Surface_Interface'Access);

   Surface_Interface : constant Interface_Type :=
     (My_Interface => Thin.Surface_Interface'Access);

   Seat_Interface : constant Interface_Type :=
     (My_Interface => Thin.Seat_Interface'Access);

   Pointer_Interface : constant Interface_Type :=
     (My_Interface => Thin.Pointer_Interface'Access);

   Keyboard_Interface : constant Interface_Type :=
     (My_Interface => Thin.Keyboard_Interface'Access);

   Touch_Interface : constant Interface_Type :=
     (My_Interface => Thin.Touch_Interface'Access);

   Output_Interface : constant Interface_Type :=
     (My_Interface => Thin.Output_Interface'Access);

   Region_Interface : constant Interface_Type :=
     (My_Interface => Thin.Region_Interface'Access);

   Subcompositor_Interface : constant Interface_Type :=
     (My_Interface => Thin.Subcompositor_Interface'Access);

   Subsurface_Interface : constant Interface_Type :=
     (My_Interface => Thin.Subsurface_Interface'Access);

end Wayland.Client.Protocol;
