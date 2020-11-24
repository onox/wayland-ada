package Wayland.Client.Enums is
   pragma Preelaborate;

   type Display_Error is
     (Invalid_Object,
      --  server couldn't find object
      Invalid_Method,
      --  method doesn't exist on the specified interface
      No_Memory);
      --  server is out of memory

   type Shm_Error is
     (Invalid_Format,
      --  buffer format is not known
      Invalid_Stride,
      --  invalid size or stride during pool or buffer creation
      Invalid_Fd);
      --  mmapping the file descriptor failed

   type Shm_Format is
     (Argb_8888,
      --  32-bit ARGB format, [31:0] A:R:G:B 8:8:8:8 little endian
      Xrgb_8888,
      --  32-bit RGB format, [31:0] x:R:G:B 8:8:8:8 little endian
      C_8,
      --  8-bit color index format, [7:0] C
      Rgb_332,
      --  8-bit RGB format, [7:0] R:G:B 3:3:2
      Bgr_233,
      --  8-bit BGR format, [7:0] B:G:R 2:3:3
      Xrgb_4444,
      --  16-bit xRGB format, [15:0] x:R:G:B 4:4:4:4 little endian
      Xbgr_4444,
      --  16-bit xBGR format, [15:0] x:B:G:R 4:4:4:4 little endian
      Rgbx_4444,
      --  16-bit RGBx format, [15:0] R:G:B:x 4:4:4:4 little endian
      Bgrx_4444,
      --  16-bit BGRx format, [15:0] B:G:R:x 4:4:4:4 little endian
      Argb_4444,
      --  16-bit ARGB format, [15:0] A:R:G:B 4:4:4:4 little endian
      Abgr_4444,
      --  16-bit ABGR format, [15:0] A:B:G:R 4:4:4:4 little endian
      Rgba_4444,
      --  16-bit RBGA format, [15:0] R:G:B:A 4:4:4:4 little endian
      Bgra_4444,
      --  16-bit BGRA format, [15:0] B:G:R:A 4:4:4:4 little endian
      Xrgb_1555,
      --  16-bit xRGB format, [15:0] x:R:G:B 1:5:5:5 little endian
      Xbgr_1555,
      --  16-bit xBGR 1555 format, [15:0] x:B:G:R 1:5:5:5 little endian
      Rgbx_5551,
      --  16-bit RGBx 5551 format, [15:0] R:G:B:x 5:5:5:1 little endian
      Bgrx_5551,
      --  16-bit BGRx 5551 format, [15:0] B:G:R:x 5:5:5:1 little endian
      Argb_1555,
      --  16-bit ARGB 1555 format, [15:0] A:R:G:B 1:5:5:5 little endian
      Abgr_1555,
      --  16-bit ABGR 1555 format, [15:0] A:B:G:R 1:5:5:5 little endian
      Rgba_5551,
      --  16-bit RGBA 5551 format, [15:0] R:G:B:A 5:5:5:1 little endian
      Bgra_5551,
      --  16-bit BGRA 5551 format, [15:0] B:G:R:A 5:5:5:1 little endian
      Rgb_565,
      --  16-bit RGB 565 format, [15:0] R:G:B 5:6:5 little endian
      Bgr_565,
      --  16-bit BGR 565 format, [15:0] B:G:R 5:6:5 little endian
      Rgb_888,
      --  24-bit RGB format, [23:0] R:G:B little endian
      Bgr_888,
      --  24-bit BGR format, [23:0] B:G:R little endian
      Xbgr_8888,
      --  32-bit xBGR format, [31:0] x:B:G:R 8:8:8:8 little endian
      Rgbx_8888,
      --  32-bit RGBx format, [31:0] R:G:B:x 8:8:8:8 little endian
      Bgrx_8888,
      --  32-bit BGRx format, [31:0] B:G:R:x 8:8:8:8 little endian
      Abgr_8888,
      --  32-bit ABGR format, [31:0] A:B:G:R 8:8:8:8 little endian
      Rgba_8888,
      --  32-bit RGBA format, [31:0] R:G:B:A 8:8:8:8 little endian
      Bgra_8888,
      --  32-bit BGRA format, [31:0] B:G:R:A 8:8:8:8 little endian
      Xrgb_2101010,
      --  32-bit xRGB format, [31:0] x:R:G:B 2:10:10:10 little endian
      Xbgr_2101010,
      --  32-bit xBGR format, [31:0] x:B:G:R 2:10:10:10 little endian
      Rgbx_1010102,
      --  32-bit RGBx format, [31:0] R:G:B:x 10:10:10:2 little endian
      Bgrx_1010102,
      --  32-bit BGRx format, [31:0] B:G:R:x 10:10:10:2 little endian
      Argb_2101010,
      --  32-bit ARGB format, [31:0] A:R:G:B 2:10:10:10 little endian
      Abgr_2101010,
      --  32-bit ABGR format, [31:0] A:B:G:R 2:10:10:10 little endian
      Rgba_1010102,
      --  32-bit RGBA format, [31:0] R:G:B:A 10:10:10:2 little endian
      Bgra_1010102,
      --  32-bit BGRA format, [31:0] B:G:R:A 10:10:10:2 little endian
      Yuyv,
      --  packed YCbCr format, [31:0] Cr0:Y1:Cb0:Y0 8:8:8:8 little endian
      Yvyu,
      --  packed YCbCr format, [31:0] Cb0:Y1:Cr0:Y0 8:8:8:8 little endian
      Uyvy,
      --  packed YCbCr format, [31:0] Y1:Cr0:Y0:Cb0 8:8:8:8 little endian
      Vyuy,
      --  packed YCbCr format, [31:0] Y1:Cb0:Y0:Cr0 8:8:8:8 little endian
      Ayuv,
      --  packed AYCbCr format, [31:0] A:Y:Cb:Cr 8:8:8:8 little endian
      Nv_12,
      --  2 plane YCbCr Cr:Cb format, 2x2 subsampled Cr:Cb plane
      Nv_21,
      --  2 plane YCbCr Cb:Cr format, 2x2 subsampled Cb:Cr plane
      Nv_16,
      --  2 plane YCbCr Cr:Cb format, 2x1 subsampled Cr:Cb plane
      Nv_61,
      --  2 plane YCbCr Cb:Cr format, 2x1 subsampled Cb:Cr plane
      Yuv_410,
      --  3 plane YCbCr format, 4x4 subsampled Cb (1) and Cr (2) planes
      Yvu_410,
      --  3 plane YCbCr format, 4x4 subsampled Cr (1) and Cb (2) planes
      Yuv_411,
      --  3 plane YCbCr format, 4x1 subsampled Cb (1) and Cr (2) planes
      Yvu_411,
      --  3 plane YCbCr format, 4x1 subsampled Cr (1) and Cb (2) planes
      Yuv_420,
      --  3 plane YCbCr format, 2x2 subsampled Cb (1) and Cr (2) planes
      Yvu_420,
      --  3 plane YCbCr format, 2x2 subsampled Cr (1) and Cb (2) planes
      Yuv_422,
      --  3 plane YCbCr format, 2x1 subsampled Cb (1) and Cr (2) planes
      Yvu_422,
      --  3 plane YCbCr format, 2x1 subsampled Cr (1) and Cb (2) planes
      Yuv_444,
      --  3 plane YCbCr format, non-subsampled Cb (1) and Cr (2) planes
      Yvu_444);
      --  3 plane YCbCr format, non-subsampled Cr (1) and Cb (2) planes

   type Data_Offer_Error is
     (Invalid_Finish,
      --  finish request was called untimely
      Invalid_Action_Mask,
      --  action mask contains invalid values
      Invalid_Action,
      --  action argument has an invalid value
      Invalid_Offer);
      --  offer doesn't accept this request

   type Data_Source_Error is
     (Invalid_Action_Mask,
      --  action mask contains invalid values
      Invalid_Source);
      --  source doesn't accept this request

   type Data_Device_Error is
     (Role);
      --  given wl_surface has another role

   type Data_Device_Manager_Dnd_Action is record
      Copy : Boolean := False;
      --  copy action
      Move : Boolean := False;
      --  move action
      Ask  : Boolean := False;
      --  ask action
   end record;

   type Surface_Error is
     (Invalid_Scale,
      --  buffer scale value is invalid
      Invalid_Transform);
      --  buffer transform value is invalid

   type Seat_Capability is record
      Pointer  : Boolean := False;
      --  the seat has pointer devices
      Keyboard : Boolean := False;
      --  the seat has one or more keyboards
      Touch    : Boolean := False;
      --  the seat has touch devices
   end record;

   type Pointer_Error is
     (Role);
      --  given wl_surface has another role

   type Pointer_Button_State is
     (Released,
      --  the button is not pressed
      Pressed);
      --  the button is pressed

   type Pointer_Axis is
     (Vertical_Scroll,
      --  vertical axis
      Horizontal_Scroll);
      --  horizontal axis

   type Pointer_Axis_Source is
     (Wheel,
      --  a physical wheel rotation
      Finger,
      --  finger on a touch surface
      Continuous,
      --  continuous coordinate space
      Wheel_Tilt);
      --  a physical wheel tilt

   type Keyboard_Keymap_Format is
     (No_Keymap,
      --  no keymap; client must understand how to interpret the raw keycode
      Xkb_V_1);
      --  libxkbcommon compatible; to determine the xkb keycode, clients must add 8 to the key event keycode

   type Keyboard_Key_State is
     (Released,
      --  key is not pressed
      Pressed);
      --  key is pressed

   type Output_Subpixel is
     (Unknown,
      --  unknown geometry
      None,
      --  no geometry
      Horizontal_Rgb,
      --  horizontal RGB
      Horizontal_Bgr,
      --  horizontal BGR
      Vertical_Rgb,
      --  vertical RGB
      Vertical_Bgr);
      --  vertical BGR

   type Output_Transform is
     (Normal,
      --  no transform
      Rotate_90,
      --  90 degrees counter-clockwise
      Rotate_180,
      --  180 degrees counter-clockwise
      Rotate_270,
      --  270 degrees counter-clockwise
      Flipped,
      --  180 degree flip around a vertical axis
      Flipped_90,
      --  flip and rotate 90 degrees counter-clockwise
      Flipped_180,
      --  flip and rotate 180 degrees counter-clockwise
      Flipped_270);
      --  flip and rotate 270 degrees counter-clockwise

   type Output_Mode is record
      Current   : Boolean := False;
      --  indicates this is the current mode
      Preferred : Boolean := False;
      --  indicates this is the preferred mode
   end record;

   type Subcompositor_Error is
     (Bad_Surface);
      --  the to-be sub-surface is invalid

   type Subsurface_Error is
     (Bad_Surface);
      --  wl_surface is not a sibling or the parent


private

   for Display_Error use
     (Invalid_Object => 0,
      Invalid_Method => 1,
      No_Memory      => 2);
   for Display_Error'Size use Unsigned_32'Size;

   for Shm_Error use
     (Invalid_Format => 0,
      Invalid_Stride => 1,
      Invalid_Fd     => 2);
   for Shm_Error'Size use Unsigned_32'Size;

   for Shm_Format use
     (Argb_8888    => 0,
      Xrgb_8888    => 1,
      C_8          => 538982467,
      Rgb_332      => 943867730,
      Bgr_233      => 944916290,
      Xrgb_4444    => 842093144,
      Xbgr_4444    => 842089048,
      Rgbx_4444    => 842094674,
      Bgrx_4444    => 842094658,
      Argb_4444    => 842093121,
      Abgr_4444    => 842089025,
      Rgba_4444    => 842088786,
      Bgra_4444    => 842088770,
      Xrgb_1555    => 892424792,
      Xbgr_1555    => 892420696,
      Rgbx_5551    => 892426322,
      Bgrx_5551    => 892426306,
      Argb_1555    => 892424769,
      Abgr_1555    => 892420673,
      Rgba_5551    => 892420434,
      Bgra_5551    => 892420418,
      Rgb_565      => 909199186,
      Bgr_565      => 909199170,
      Rgb_888      => 875710290,
      Bgr_888      => 875710274,
      Xbgr_8888    => 875709016,
      Rgbx_8888    => 875714642,
      Bgrx_8888    => 875714626,
      Abgr_8888    => 875708993,
      Rgba_8888    => 875708754,
      Bgra_8888    => 875708738,
      Xrgb_2101010 => 808669784,
      Xbgr_2101010 => 808665688,
      Rgbx_1010102 => 808671314,
      Bgrx_1010102 => 808671298,
      Argb_2101010 => 808669761,
      Abgr_2101010 => 808665665,
      Rgba_1010102 => 808665426,
      Bgra_1010102 => 808665410,
      Yuyv         => 1448695129,
      Yvyu         => 1431918169,
      Uyvy         => 1498831189,
      Vyuy         => 1498765654,
      Ayuv         => 1448433985,
      Nv_12        => 842094158,
      Nv_21        => 825382478,
      Nv_16        => 909203022,
      Nv_61        => 825644622,
      Yuv_410      => 961959257,
      Yvu_410      => 961893977,
      Yuv_411      => 825316697,
      Yvu_411      => 825316953,
      Yuv_420      => 842093913,
      Yvu_420      => 842094169,
      Yuv_422      => 909202777,
      Yvu_422      => 909203033,
      Yuv_444      => 875713881,
      Yvu_444      => 875714137);
   for Shm_Format'Size use Unsigned_32'Size;

   for Data_Offer_Error use
     (Invalid_Finish      => 0,
      Invalid_Action_Mask => 1,
      Invalid_Action      => 2,
      Invalid_Offer       => 3);
   for Data_Offer_Error'Size use Unsigned_32'Size;

   for Data_Source_Error use
     (Invalid_Action_Mask => 0,
      Invalid_Source      => 1);
   for Data_Source_Error'Size use Unsigned_32'Size;

   for Data_Device_Error use
     (Role => 0);
   for Data_Device_Error'Size use Unsigned_32'Size;

   for Data_Device_Manager_Dnd_Action use record
      Copy at 0 range 0 .. 0;
      Move at 0 range 1 .. 1;
      Ask  at 0 range 2 .. 2;
   end record;
   for Data_Device_Manager_Dnd_Action'Size use Unsigned_32'Size;

   for Surface_Error use
     (Invalid_Scale     => 0,
      Invalid_Transform => 1);
   for Surface_Error'Size use Unsigned_32'Size;

   for Seat_Capability use record
      Pointer  at 0 range 0 .. 0;
      Keyboard at 0 range 1 .. 1;
      Touch    at 0 range 2 .. 2;
   end record;
   for Seat_Capability'Size use Unsigned_32'Size;

   for Pointer_Error use
     (Role => 0);
   for Pointer_Error'Size use Unsigned_32'Size;

   for Pointer_Button_State use
     (Released => 0,
      Pressed  => 1);
   for Pointer_Button_State'Size use Unsigned_32'Size;

   for Pointer_Axis use
     (Vertical_Scroll   => 0,
      Horizontal_Scroll => 1);
   for Pointer_Axis'Size use Unsigned_32'Size;

   for Pointer_Axis_Source use
     (Wheel      => 0,
      Finger     => 1,
      Continuous => 2,
      Wheel_Tilt => 3);
   for Pointer_Axis_Source'Size use Unsigned_32'Size;

   for Keyboard_Keymap_Format use
     (No_Keymap => 0,
      Xkb_V_1   => 1);
   for Keyboard_Keymap_Format'Size use Unsigned_32'Size;

   for Keyboard_Key_State use
     (Released => 0,
      Pressed  => 1);
   for Keyboard_Key_State'Size use Unsigned_32'Size;

   for Output_Subpixel use
     (Unknown        => 0,
      None           => 1,
      Horizontal_Rgb => 2,
      Horizontal_Bgr => 3,
      Vertical_Rgb   => 4,
      Vertical_Bgr   => 5);
   for Output_Subpixel'Size use Unsigned_32'Size;

   for Output_Transform use
     (Normal      => 0,
      Rotate_90   => 1,
      Rotate_180  => 2,
      Rotate_270  => 3,
      Flipped     => 4,
      Flipped_90  => 5,
      Flipped_180 => 6,
      Flipped_270 => 7);
   for Output_Transform'Size use Unsigned_32'Size;

   for Output_Mode use record
      Current   at 0 range 0 .. 0;
      Preferred at 0 range 1 .. 1;
   end record;
   for Output_Mode'Size use Unsigned_32'Size;

   for Subcompositor_Error use
     (Bad_Surface => 0);
   for Subcompositor_Error'Size use Unsigned_32'Size;

   for Subsurface_Error use
     (Bad_Surface => 0);
   for Subsurface_Error'Size use Unsigned_32'Size;

end Wayland.Client.Enums;
