with Wayland.Client.Constants;

use Wayland.Client.Constants;

--  Mostly auto generated from Wayland.xml
package body Wayland.Client.Thin is

   use type Proxy_Ptr;

   function Display_Connect (Name : C_String) return Display_Ptr is
   begin
      return Wayland.API.Display_Connect (Name);
   end Display_Connect;

   procedure Display_Disconnect (This : in out Display_Ptr) is
      use type Wayland.API.Display_Ptr;
   begin
      if This /= null then
         Wayland.API.Display_Disconnect (This);
         This := null;
      end if;
   end Display_Disconnect;

   function Display_Add_Listener
     (Display  : Display_Ptr;
      Listener : Display_Listener_Ptr;
      Data     : Void_Ptr) return Interfaces.C.int is
   begin
      return Wayland.API.Proxy_Add_Listener (Display.all, Listener.all'Address, Data);
   end Display_Add_Listener;

   procedure Display_Set_User_Data (Display : Display_Ptr; Data : Void_Ptr) is
   begin
      Wayland.API.Proxy_Set_User_Data (Display.all, Data);
   end Display_Set_User_Data;

   function Display_Get_User_Data (Display : Display_Ptr) return Void_Ptr is
   begin
      return Wayland.API.Proxy_Get_User_Data (Display.all);
   end Display_Get_User_Data;

   function Display_Get_Version (Display : Display_Ptr) return Unsigned_32 is
   begin
      return Wayland.API.Proxy_Get_Version (Display.all);
   end Display_Get_Version;

   procedure Display_Destroy (Display : Display_Ptr) is
   begin
      Wayland.API.Proxy_Destroy (Display.all);
   end Display_Destroy;

   function Display_Sync (Display : Display_Ptr) return Callback_Ptr is
      P : constant Proxy_Ptr :=
        Wayland.API.Proxy_Marshal_Constructor
          (Display.all,
           Constants.Display_Sync,
           Callback_Interface'Access,
           0);
   begin
      return (if P /= null then P.all'Access else null);
   end Display_Sync;

   function Display_Get_Registry (Display : Display_Ptr) return Registry_Ptr is
      P : constant Proxy_Ptr :=
        Wayland.API.Proxy_Marshal_Constructor
          (Display.all,
           Constants.Display_Get_Registry,
           Registry_Interface'Access,
           0);
   begin
      return (if P /= null then P.all'Access else null);
   end Display_Get_Registry;

   function Registry_Add_Listener
     (Registry : Registry_Ptr;
      Listener : Registry_Listener_Ptr;
      Data     : Void_Ptr) return Interfaces.C.int is
   begin
      return Wayland.API.Proxy_Add_Listener (Registry.all, Listener.all'Address, Data);
   end Registry_Add_Listener;

   procedure Registry_Set_User_Data (Registry : Registry_Ptr; Data : Void_Ptr) is
   begin
      Wayland.API.Proxy_Set_User_Data (Registry.all, Data);
   end Registry_Set_User_Data;

   function Registry_Get_User_Data (Registry : Registry_Ptr) return Void_Ptr is
   begin
      return Wayland.API.Proxy_Get_User_Data (Registry.all);
   end Registry_Get_User_Data;

   function Registry_Get_Version (Registry : Registry_Ptr) return Unsigned_32 is
   begin
      return Wayland.API.Proxy_Get_Version (Registry.all);
   end Registry_Get_Version;

   procedure Registry_Destroy (Registry : Registry_Ptr) is
   begin
      Wayland.API.Proxy_Destroy (Registry.all);
   end Registry_Destroy;

   function Registry_Bind
     (Registry    : Registry_Ptr;
      Name        : Unsigned_32;
      Interface_V : Interface_Ptr;
      New_Id      : Unsigned_32) return Proxy_Ptr is
   begin
      return Wayland.API.Proxy_Marshal_Constructor_Versioned
        (Registry.all,
         Constants.Registry_Bind,
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
      Data     : Void_Ptr) return Interfaces.C.int is
   begin
      return Wayland.API.Proxy_Add_Listener (Callback.all, Listener.all'Address, Data);
   end Callback_Add_Listener;

   procedure Callback_Set_User_Data (Callback : Callback_Ptr; Data : Void_Ptr) is
   begin
      Wayland.API.Proxy_Set_User_Data (Callback.all, Data);
   end Callback_Set_User_Data;

   function Callback_Get_User_Data (Callback : Callback_Ptr) return Void_Ptr is
   begin
      return Wayland.API.Proxy_Get_User_Data (Callback.all);
   end Callback_Get_User_Data;

   function Callback_Get_Version (Callback : Callback_Ptr) return Unsigned_32 is
   begin
      return Wayland.API.Proxy_Get_Version (Callback.all);
   end Callback_Get_Version;

   procedure Callback_Destroy (Callback : Callback_Ptr) is
   begin
      Wayland.API.Proxy_Destroy (Callback.all);
   end Callback_Destroy;

   procedure Compositor_Set_User_Data (Compositor : Compositor_Ptr; Data : Void_Ptr) is
   begin
      Wayland.API.Proxy_Set_User_Data (Compositor.all, Data);
   end Compositor_Set_User_Data;

   function Compositor_Get_User_Data (Compositor : Compositor_Ptr) return Void_Ptr is
   begin
      return Wayland.API.Proxy_Get_User_Data (Compositor.all);
   end Compositor_Get_User_Data;

   function Compositor_Get_Version (Compositor : Compositor_Ptr) return Unsigned_32 is
   begin
      return Wayland.API.Proxy_Get_Version (Compositor.all);
   end Compositor_Get_Version;

   procedure Compositor_Destroy (Compositor : Compositor_Ptr) is
   begin
      Wayland.API.Proxy_Destroy (Compositor.all);
   end Compositor_Destroy;

   function Compositor_Create_Surface (Compositor : Compositor_Ptr) return Surface_Ptr is
      P : constant Proxy_Ptr :=
        Wayland.API.Proxy_Marshal_Constructor
          (Compositor.all,
           Constants.Compositor_Create_Surface,
           Surface_Interface'Access,
           0);
   begin
      return (if P /= null then P.all'Access else null);
   end Compositor_Create_Surface;

   function Compositor_Create_Region (Compositor : Compositor_Ptr) return Region_Ptr is
      P : constant Proxy_Ptr :=
        Wayland.API.Proxy_Marshal_Constructor
          (Compositor.all,
           Constants.Compositor_Create_Region,
           Region_Interface'Access,
           0);
   begin
      return (if P /= null then P.all'Access else null);
   end Compositor_Create_Region;

   procedure Shm_Pool_Set_User_Data (Shm_Pool : Shm_Pool_Ptr; Data : Void_Ptr) is
   begin
      Wayland.API.Proxy_Set_User_Data (Shm_Pool.all, Data);
   end Shm_Pool_Set_User_Data;

   function Shm_Pool_Get_User_Data (Shm_Pool : Shm_Pool_Ptr) return Void_Ptr is
   begin
      return Wayland.API.Proxy_Get_User_Data (Shm_Pool.all);
   end Shm_Pool_Get_User_Data;

   function Shm_Pool_Get_Version (Shm_Pool : Shm_Pool_Ptr) return Unsigned_32 is
   begin
      return Wayland.API.Proxy_Get_Version (Shm_Pool.all);
   end Shm_Pool_Get_Version;

   procedure Shm_Pool_Destroy (Shm_Pool : Shm_Pool_Ptr) is
   begin
      Wayland.API.Proxy_Marshal (Shm_Pool.all, Constants.Shm_Pool_Destroy);

      Wayland.API.Proxy_Destroy (Shm_Pool.all);
   end Shm_Pool_Destroy;

   function Shm_Pool_Create_Buffer
     (Shm_Pool : Shm_Pool_Ptr;
      Offset   : Integer;
      Width    : Integer;
      Height   : Integer;
      Stride   : Integer;
      Format   : Unsigned_32) return Buffer_Ptr
   is
      P : constant Proxy_Ptr :=
        Wayland.API.Proxy_Marshal_Constructor
          (Shm_Pool.all,
           Constants.Shm_Pool_Create_Buffer,
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

   procedure Shm_Pool_Resize (Shm_Pool : Shm_Pool_Ptr; Size : Integer) is
   begin
      Wayland.API.Proxy_Marshal (Shm_Pool.all, Constants.Shm_Pool_Resize, Size);
   end Shm_Pool_Resize;

   function Shm_Add_Listener
     (Shm      : Shm_Ptr;
      Listener : Shm_Listener_Ptr;
      Data     : Void_Ptr) return Interfaces.C.int is
   begin
      return Wayland.API.Proxy_Add_Listener (Shm.all, Listener.all'Address, Data);
   end Shm_Add_Listener;

   procedure Shm_Set_User_Data (Shm : Shm_Ptr; Data : Void_Ptr) is
   begin
      Wayland.API.Proxy_Set_User_Data (Shm.all, Data);
   end Shm_Set_User_Data;

   function Shm_Get_User_Data (Shm : Shm_Ptr) return Void_Ptr is
   begin
      return Wayland.API.Proxy_Get_User_Data (Shm.all);
   end Shm_Get_User_Data;

   function Shm_Get_Version (Shm : Shm_Ptr) return Unsigned_32 is
   begin
      return Wayland.API.Proxy_Get_Version (Shm.all);
   end Shm_Get_Version;

   procedure Shm_Destroy (Shm : Shm_Ptr) is
   begin
      Wayland.API.Proxy_Destroy (Shm.all);
   end Shm_Destroy;

   function Shm_Create_Pool (Shm : Shm_Ptr; Fd : Integer; Size : Integer) return Shm_Pool_Ptr is
      P : constant Proxy_Ptr :=
        Wayland.API.Proxy_Marshal_Constructor
          (Shm.all,
           Constants.Shm_Create_Pool,
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
      Data     : Void_Ptr) return Interfaces.C.int is
   begin
      return Wayland.API.Proxy_Add_Listener (Buffer.all, Listener.all'Address, Data);
   end Buffer_Add_Listener;

   procedure Buffer_Set_User_Data (Buffer : Buffer_Ptr; Data : Void_Ptr) is
   begin
      Wayland.API.Proxy_Set_User_Data (Buffer.all, Data);
   end Buffer_Set_User_Data;

   function Buffer_Get_User_Data (Buffer : Buffer_Ptr) return Void_Ptr is
   begin
      return Wayland.API.Proxy_Get_User_Data (Buffer.all);
   end Buffer_Get_User_Data;

   function Buffer_Get_Version (Buffer : Buffer_Ptr) return Unsigned_32 is
   begin
      return Wayland.API.Proxy_Get_Version (Buffer.all);
   end Buffer_Get_Version;

   procedure Buffer_Destroy (Buffer : Buffer_Ptr) is
   begin
      Wayland.API.Proxy_Marshal (Buffer.all, Constants.Buffer_Destroy);

      Wayland.API.Proxy_Destroy (Buffer.all);
   end Buffer_Destroy;

   function Data_Offer_Add_Listener
     (Data_Offer : Data_Offer_Ptr;
      Listener   : Data_Offer_Listener_Ptr;
      Data       : Void_Ptr) return Interfaces.C.int is
   begin
      return Wayland.API.Proxy_Add_Listener (Data_Offer.all, Listener.all'Address, Data);
   end Data_Offer_Add_Listener;

   procedure Data_Offer_Set_User_Data (Data_Offer : Data_Offer_Ptr; Data : Void_Ptr) is
   begin
      Wayland.API.Proxy_Set_User_Data (Data_Offer.all, Data);
   end Data_Offer_Set_User_Data;

   function Data_Offer_Get_User_Data (Data_Offer : Data_Offer_Ptr) return Void_Ptr is
   begin
      return Wayland.API.Proxy_Get_User_Data (Data_Offer.all);
   end Data_Offer_Get_User_Data;

   function Data_Offer_Get_Version (Data_Offer : Data_Offer_Ptr) return Unsigned_32 is
   begin
      return Wayland.API.Proxy_Get_Version (Data_Offer.all);
   end Data_Offer_Get_Version;

   procedure Data_Offer_Destroy (Data_Offer : Data_Offer_Ptr) is
   begin
      Wayland.API.Proxy_Marshal (Data_Offer.all, Constants.Data_Offer_Destroy);

      Wayland.API.Proxy_Destroy (Data_Offer.all);
   end Data_Offer_Destroy;

   procedure Data_Offer_Accept
     (Data_Offer : Data_Offer_Ptr;
      Serial     : Unsigned_32;
      Mime_Type  : Chars_Ptr) is
   begin
      Wayland.API.Proxy_Marshal
        (Data_Offer.all,
         Constants.Data_Offer_Accept,
         Serial,
         Mime_Type);
   end Data_Offer_Accept;

   procedure Data_Offer_Receive
     (Data_Offer : Data_Offer_Ptr;
      Mime_Type  : C_String;
      Fd         : Integer) is
   begin
      Wayland.API.Proxy_Marshal
        (Data_Offer.all,
         Constants.Data_Offer_Receive,
         Mime_Type,
         Fd);
   end Data_Offer_Receive;

   procedure Data_Offer_Finish (Data_Offer : Data_Offer_Ptr) is
   begin
      Wayland.API.Proxy_Marshal (Data_Offer.all, Constants.Data_Offer_Finish);
   end Data_Offer_Finish;

   procedure Data_Offer_Set_Actions
     (Data_Offer       : Data_Offer_Ptr;
      Dnd_Actions      : Unsigned_32;
      Preferred_Action : Unsigned_32) is
   begin
      Wayland.API.Proxy_Marshal
        (Data_Offer.all,
         Constants.Data_Offer_Set_Actions,
         Dnd_Actions,
         Preferred_Action);
   end Data_Offer_Set_Actions;

   function Data_Source_Add_Listener
     (Data_Source : Data_Source_Ptr;
      Listener    : Data_Source_Listener_Ptr;
      Data        : Void_Ptr) return Interfaces.C.int is
   begin
      return Wayland.API.Proxy_Add_Listener (Data_Source.all, Listener.all'Address, Data);
   end Data_Source_Add_Listener;

   procedure Data_Source_Set_User_Data (Data_Source : Data_Source_Ptr; Data : Void_Ptr) is
   begin
      Wayland.API.Proxy_Set_User_Data (Data_Source.all, Data);
   end Data_Source_Set_User_Data;

   function Data_Source_Get_User_Data (Data_Source : Data_Source_Ptr) return Void_Ptr is
   begin
      return Wayland.API.Proxy_Get_User_Data (Data_Source.all);
   end Data_Source_Get_User_Data;

   function Data_Source_Get_Version (Data_Source : Data_Source_Ptr) return Unsigned_32 is
   begin
      return Wayland.API.Proxy_Get_Version (Data_Source.all);
   end Data_Source_Get_Version;

   procedure Data_Source_Destroy (Data_Source : Data_Source_Ptr) is
   begin
      Wayland.API.Proxy_Marshal (Data_Source.all, Constants.Data_Source_Destroy);

      Wayland.API.Proxy_Destroy (Data_Source.all);
   end Data_Source_Destroy;

   procedure Data_Source_Offer (Data_Source : Data_Source_Ptr; Mime_Type : Chars_Ptr) is
   begin
      Wayland.API.Proxy_Marshal
        (Data_Source.all,
         Constants.Data_Source_Offer,
         Mime_Type);
   end Data_Source_Offer;

   procedure Data_Source_Set_Actions (Data_Source : Data_Source_Ptr; Dnd_Actions : Unsigned_32) is
   begin
      Wayland.API.Proxy_Marshal
        (Data_Source.all,
         Constants.Data_Source_Set_Actions,
         Dnd_Actions);
   end Data_Source_Set_Actions;

   function Data_Device_Add_Listener
     (Data_Device : Data_Device_Ptr;
      Listener    : Data_Device_Listener_Ptr;
      Data        : Void_Ptr) return Interfaces.C.int is
   begin
      return Wayland.API.Proxy_Add_Listener (Data_Device.all, Listener.all'Address, Data);
   end Data_Device_Add_Listener;

   procedure Data_Device_Set_User_Data (Data_Device : Data_Device_Ptr; Data : Void_Ptr) is
   begin
      Wayland.API.Proxy_Set_User_Data (Data_Device.all, Data);
   end Data_Device_Set_User_Data;

   function Data_Device_Get_User_Data (Data_Device : Data_Device_Ptr) return Void_Ptr is
   begin
      return Wayland.API.Proxy_Get_User_Data (Data_Device.all);
   end Data_Device_Get_User_Data;

   function Data_Device_Get_Version (Data_Device : Data_Device_Ptr) return Unsigned_32 is
   begin
      return Wayland.API.Proxy_Get_Version (Data_Device.all);
   end Data_Device_Get_Version;

   procedure Data_Device_Destroy (Data_Device : Data_Device_Ptr) is
   begin
      Wayland.API.Proxy_Destroy (Data_Device.all);
   end Data_Device_Destroy;

   procedure Data_Device_Start_Drag
     (Data_Device : Data_Device_Ptr;
      Source      : Data_Source_Ptr;
      Origin      : Surface_Ptr;
      Icon        : Surface_Ptr;
      Serial      : Unsigned_32) is
   begin
      Wayland.API.Proxy_Marshal
        (Data_Device.all,
         Constants.Data_Device_Start_Drag,
         Source.all'Address,
         Origin.all'Address,
         Icon.all'Address,
         Serial);
   end Data_Device_Start_Drag;

   procedure Data_Device_Set_Selection
     (Data_Device : Data_Device_Ptr;
      Source      : Data_Source_Ptr;
      Serial      : Unsigned_32) is
   begin
      Wayland.API.Proxy_Marshal
        (Data_Device.all,
         Constants.Data_Device_Set_Selection,
         Source.all'Address,
         Serial);
   end Data_Device_Set_Selection;

   procedure Data_Device_Release (Data_Device : Data_Device_Ptr) is
   begin
      Wayland.API.Proxy_Marshal (Data_Device.all, Constants.Data_Device_Release);
   end Data_Device_Release;

   procedure Data_Device_Manager_Set_User_Data
     (Data_Device_Manager : Data_Device_Manager_Ptr;
      Data                : Void_Ptr) is
   begin
      Wayland.API.Proxy_Set_User_Data (Data_Device_Manager.all, Data);
   end Data_Device_Manager_Set_User_Data;

   function Data_Device_Manager_Get_User_Data
     (Data_Device_Manager : Data_Device_Manager_Ptr) return Void_Ptr is
   begin
      return Wayland.API.Proxy_Get_User_Data (Data_Device_Manager.all);
   end Data_Device_Manager_Get_User_Data;

   function Data_Device_Manager_Get_Version
     (Data_Device_Manager : Data_Device_Manager_Ptr) return Unsigned_32 is
   begin
      return Wayland.API.Proxy_Get_Version (Data_Device_Manager.all);
   end Data_Device_Manager_Get_Version;

   procedure Data_Device_Manager_Destroy (Data_Device_Manager : Data_Device_Manager_Ptr) is
   begin
      Wayland.API.Proxy_Destroy (Data_Device_Manager.all);
   end Data_Device_Manager_Destroy;

   function Data_Device_Manager_Create_Data_Source
     (Data_Device_Manager : Data_Device_Manager_Ptr) return Data_Source_Ptr
   is
      P : constant Proxy_Ptr :=
        Wayland.API.Proxy_Marshal_Constructor
          (Data_Device_Manager.all,
           Constants.Data_Device_Manager_Create_Data_Source,
           Data_Source_Interface'Access,
           0);
   begin
      return (if P /= null then P.all'Access else null);
   end Data_Device_Manager_Create_Data_Source;

   function Data_Device_Manager_Get_Data_Device
     (Data_Device_Manager : Data_Device_Manager_Ptr;
      Seat                : Seat_Ptr) return Data_Device_Ptr
   is
      P : constant Proxy_Ptr :=
        Wayland.API.Proxy_Marshal_Constructor
          (Data_Device_Manager.all,
           Constants.Data_Device_Manager_Get_Data_Device,
           Data_Device_Interface'Access,
           0,
           Seat.all'Address);
   begin
      return (if P /= null then P.all'Access else null);
   end Data_Device_Manager_Get_Data_Device;

   procedure Shell_Set_User_Data (Shell : Shell_Ptr; Data : Void_Ptr) is
   begin
      Wayland.API.Proxy_Set_User_Data (Shell.all, Data);
   end Shell_Set_User_Data;

   function Shell_Get_User_Data (Shell : Shell_Ptr) return Void_Ptr is
   begin
      return Wayland.API.Proxy_Get_User_Data (Shell.all);
   end Shell_Get_User_Data;

   function Shell_Get_Version (Shell : Shell_Ptr) return Unsigned_32 is
   begin
      return Wayland.API.Proxy_Get_Version (Shell.all);
   end Shell_Get_Version;

   procedure Shell_Destroy (Shell : Shell_Ptr) is
   begin
      Wayland.API.Proxy_Destroy (Shell.all);
   end Shell_Destroy;

   function Shell_Get_Shell_Surface
     (Shell   : Shell_Ptr;
      Surface : Surface_Ptr) return Shell_Surface_Ptr
   is
      P : constant Proxy_Ptr :=
        Wayland.API.Proxy_Marshal_Constructor
          (Shell.all,
           Constants.Shell_Get_Shell_Surface,
           Shell_Surface_Interface'Access,
           0,
           Surface.all'Address);
   begin
      return (if P /= null then P.all'Access else null);
   end Shell_Get_Shell_Surface;

   function Shell_Surface_Add_Listener
     (Shell_Surface : Shell_Surface_Ptr;
      Listener      : Shell_Surface_Listener_Ptr;
      Data          : Void_Ptr) return Interfaces.C.int is
   begin
      return Wayland.API.Proxy_Add_Listener (Shell_Surface.all, Listener.all'Address, Data);
   end Shell_Surface_Add_Listener;

   procedure Shell_Surface_Set_User_Data (Shell_Surface : Shell_Surface_Ptr; Data : Void_Ptr) is
   begin
      Wayland.API.Proxy_Set_User_Data (Shell_Surface.all, Data);
   end Shell_Surface_Set_User_Data;

   function Shell_Surface_Get_User_Data (Shell_Surface : Shell_Surface_Ptr) return Void_Ptr is
   begin
      return Wayland.API.Proxy_Get_User_Data (Shell_Surface.all);
   end Shell_Surface_Get_User_Data;

   function Shell_Surface_Get_Version (Shell_Surface : Shell_Surface_Ptr) return Unsigned_32 is
   begin
      return Wayland.API.Proxy_Get_Version (Shell_Surface.all);
   end Shell_Surface_Get_Version;

   procedure Shell_Surface_Destroy (Shell_Surface : Shell_Surface_Ptr) is
   begin
      Wayland.API.Proxy_Destroy (Shell_Surface.all);
   end Shell_Surface_Destroy;

   procedure Shell_Surface_Pong (Shell_Surface : Shell_Surface_Ptr; Serial : Unsigned_32) is
   begin
      Wayland.API.Proxy_Marshal
        (Shell_Surface.all,
         Constants.Shell_Surface_Pong,
         Serial);
   end Shell_Surface_Pong;

   procedure Shell_Surface_Move
     (Shell_Surface : Shell_Surface_Ptr;
      Seat          : Seat_Ptr;
      Serial        : Unsigned_32) is
   begin
      Wayland.API.Proxy_Marshal
        (Shell_Surface.all,
         Constants.Shell_Surface_Move,
         Seat.all'Address,
         Serial);
   end Shell_Surface_Move;

   procedure Shell_Surface_Resize
     (Shell_Surface : Shell_Surface_Ptr;
      Seat          : Seat_Ptr;
      Serial        : Unsigned_32;
      Edges         : Unsigned_32) is
   begin
      Wayland.API.Proxy_Marshal
        (Shell_Surface.all,
         Constants.Shell_Surface_Resize,
         Seat.all'Address,
         Serial,
         Edges);
   end Shell_Surface_Resize;

   procedure Shell_Surface_Set_Toplevel (Shell_Surface : Shell_Surface_Ptr) is
   begin
      Wayland.API.Proxy_Marshal (Shell_Surface.all, Constants.Shell_Surface_Set_Toplevel);
   end Shell_Surface_Set_Toplevel;

   procedure Shell_Surface_Set_Transient
     (Shell_Surface : Shell_Surface_Ptr;
      Parent        : Surface_Ptr;
      X             : Integer;
      Y             : Integer;
      Flags         : Unsigned_32) is
   begin
      Wayland.API.Proxy_Marshal
        (Shell_Surface.all,
         Constants.Shell_Surface_Set_Transient,
         Parent.all'Address,
         X,
         Y,
         Flags);
   end Shell_Surface_Set_Transient;

   procedure Shell_Surface_Set_Fullscreen
     (Shell_Surface : Shell_Surface_Ptr;
      Method        : Unsigned_32;
      Framerate     : Unsigned_32;
      Output        : Output_Ptr) is
   begin
      Wayland.API.Proxy_Marshal
        (Shell_Surface.all,
         Constants.Shell_Surface_Set_Fullscreen,
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
      Wayland.API.Proxy_Marshal
        (Shell_Surface.all,
         Constants.Shell_Surface_Set_Popup,
         Seat.all'Address,
         Serial,
         Parent.all'Address,
         X,
         Y,
         Flags);
   end Shell_Surface_Set_Popup;

   procedure Shell_Surface_Set_Maximized
     (Shell_Surface : Shell_Surface_Ptr;
      Output        : Output_Ptr) is
   begin
      Wayland.API.Proxy_Marshal
        (Shell_Surface.all,
         Constants.Shell_Surface_Set_Maximized,
         Output.all'Address);
   end Shell_Surface_Set_Maximized;

   procedure Shell_Surface_Set_Title
     (Shell_Surface : Shell_Surface_Ptr;
      Title         : Chars_Ptr) is
   begin
      Wayland.API.Proxy_Marshal
        (Shell_Surface.all,
         Constants.Shell_Surface_Set_Title,
         Title);
   end Shell_Surface_Set_Title;

   procedure Shell_Surface_Set_Class
     (Shell_Surface : Shell_Surface_Ptr;
      Class_V       : Chars_Ptr) is
   begin
      Wayland.API.Proxy_Marshal
        (Shell_Surface.all,
         Constants.Shell_Surface_Set_Class,
         Class_V);
   end Shell_Surface_Set_Class;

   function Surface_Add_Listener
     (Surface  : Surface_Ptr;
      Listener : Surface_Listener_Ptr;
      Data     : Void_Ptr) return Interfaces.C.int is
   begin
      return Wayland.API.Proxy_Add_Listener (Surface.all, Listener.all'Address, Data);
   end Surface_Add_Listener;

   procedure Surface_Set_User_Data (Surface : Surface_Ptr; Data : Void_Ptr) is
   begin
      Wayland.API.Proxy_Set_User_Data (Surface.all, Data);
   end Surface_Set_User_Data;

   function Surface_Get_User_Data (Surface : Surface_Ptr) return Void_Ptr is
   begin
      return Wayland.API.Proxy_Get_User_Data (Surface.all);
   end Surface_Get_User_Data;

   function Surface_Get_Version (Surface : Surface_Ptr) return Unsigned_32 is
   begin
      return Wayland.API.Proxy_Get_Version (Surface.all);
   end Surface_Get_Version;

   procedure Surface_Destroy (Surface : Surface_Ptr) is
   begin
      Wayland.API.Proxy_Marshal (Surface.all, Constants.Surface_Destroy);

      Wayland.API.Proxy_Destroy (Surface.all);
   end Surface_Destroy;

   procedure Surface_Attach
     (Surface : Surface_Ptr;
      Buffer  : Buffer_Ptr;
      X       : Integer;
      Y       : Integer) is
   begin
      Wayland.API.Proxy_Marshal
        (Surface.all,
         Constants.Surface_Attach,
         Buffer.all'Address,
         X,
         Y);
   end Surface_Attach;

   procedure Surface_Damage
     (Surface : Surface_Ptr;
      X       : Integer;
      Y       : Integer;
      Width   : Integer;
      Height  : Integer) is
   begin
      Wayland.API.Proxy_Marshal
        (Surface.all,
         Constants.Surface_Damage,
         X,
         Y,
         Width,
         Height);
   end Surface_Damage;

   function Surface_Frame (Surface : Surface_Ptr) return Callback_Ptr is
      P : constant Proxy_Ptr :=
        Wayland.API.Proxy_Marshal_Constructor
          (Surface.all,
           Constants.Surface_Frame,
           Callback_Interface'Access,
           0);
   begin
      return (if P /= null then P.all'Access else null);
   end Surface_Frame;

   procedure Surface_Set_Opaque_Region (Surface : Surface_Ptr; Region : Region_Ptr) is
   begin
      Wayland.API.Proxy_Marshal
        (Surface.all,
         Constants.Surface_Set_Opaque_Region,
         Region.all'Address);
   end Surface_Set_Opaque_Region;

   procedure Surface_Set_Input_Region (Surface : Surface_Ptr; Region : Region_Ptr) is
   begin
      Wayland.API.Proxy_Marshal
        (Surface.all,
         Constants.Surface_Set_Input_Region,
         Region.all'Address);
   end Surface_Set_Input_Region;

   procedure Surface_Commit (Surface : Surface_Ptr) is
   begin
      Wayland.API.Proxy_Marshal (Surface.all, Constants.Surface_Commit);
   end Surface_Commit;

   procedure Surface_Set_Buffer_Transform (Surface : Surface_Ptr; Transform : Integer) is
   begin
      Wayland.API.Proxy_Marshal
        (Surface.all,
         Constants.Surface_Set_Buffer_Transform,
         Transform);
   end Surface_Set_Buffer_Transform;

   procedure Surface_Set_Buffer_Scale (Surface : Surface_Ptr; Scale : Integer) is
   begin
      Wayland.API.Proxy_Marshal
        (Surface.all,
         Constants.Surface_Set_Buffer_Scale,
         Scale);
   end Surface_Set_Buffer_Scale;

   procedure Surface_Damage_Buffer
     (Surface : Surface_Ptr;
      X       : Integer;
      Y       : Integer;
      Width   : Integer;
      Height  : Integer) is
   begin
      Wayland.API.Proxy_Marshal
        (Surface.all,
         Constants.Surface_Damage_Buffer,
         X,
         Y,
         Width,
         Height);
   end Surface_Damage_Buffer;

   function Seat_Add_Listener
     (Seat     : Seat_Ptr;
      Listener : Seat_Listener_Ptr;
      Data     : Void_Ptr) return Interfaces.C.int is
   begin
      return Wayland.API.Proxy_Add_Listener (Seat.all, Listener.all'Address, Data);
   end Seat_Add_Listener;

   procedure Seat_Set_User_Data (Seat : Seat_Ptr; Data : Void_Ptr) is
   begin
      Wayland.API.Proxy_Set_User_Data (Seat.all, Data);
   end Seat_Set_User_Data;

   function Seat_Get_User_Data (Seat : Seat_Ptr) return Void_Ptr is
   begin
      return Wayland.API.Proxy_Get_User_Data (Seat.all);
   end Seat_Get_User_Data;

   function Seat_Get_Version (Seat : Seat_Ptr) return Unsigned_32 is
   begin
      return Wayland.API.Proxy_Get_Version (Seat.all);
   end Seat_Get_Version;

   procedure Seat_Destroy (Seat : Seat_Ptr) is
   begin
      Wayland.API.Proxy_Destroy (Seat.all);
   end Seat_Destroy;

   function Seat_Get_Pointer (Seat : Seat_Ptr) return Pointer_Ptr is
      P : constant Proxy_Ptr :=
        Wayland.API.Proxy_Marshal_Constructor
          (Seat.all,
           Constants.Seat_Get_Pointer,
           Pointer_Interface'Access,
           0);
   begin
      return (if P /= null then P.all'Access else null);
   end Seat_Get_Pointer;

   function Seat_Get_Keyboard (Seat : Seat_Ptr) return Keyboard_Ptr is
      P : constant Proxy_Ptr :=
        Wayland.API.Proxy_Marshal_Constructor
          (Seat.all,
           Constants.Seat_Get_Keyboard,
           Keyboard_Interface'Access,
           0);
   begin
      return (if P /= null then P.all'Access else null);
   end Seat_Get_Keyboard;

   function Seat_Get_Touch (Seat : Seat_Ptr) return Touch_Ptr is
      P : constant Proxy_Ptr :=
        Wayland.API.Proxy_Marshal_Constructor
          (Seat.all,
           Constants.Seat_Get_Touch,
           Touch_Interface'Access,
           0);
   begin
      return (if P /= null then P.all'Access else null);
   end Seat_Get_Touch;

   procedure Seat_Release (Seat : Seat_Ptr) is
   begin
      Wayland.API.Proxy_Marshal (Seat.all, Constants.Seat_Release);
   end Seat_Release;

   function Pointer_Add_Listener
     (Pointer  : Pointer_Ptr;
      Listener : Pointer_Listener_Ptr;
      Data     : Void_Ptr) return Interfaces.C.int is
   begin
      return Wayland.API.Proxy_Add_Listener (Pointer.all, Listener.all'Address, Data);
   end Pointer_Add_Listener;

   procedure Pointer_Set_User_Data (Pointer : Pointer_Ptr; Data : Void_Ptr) is
   begin
      Wayland.API.Proxy_Set_User_Data (Pointer.all, Data);
   end Pointer_Set_User_Data;

   function Pointer_Get_User_Data (Pointer : Pointer_Ptr) return Void_Ptr is
   begin
      return Wayland.API.Proxy_Get_User_Data (Pointer.all);
   end Pointer_Get_User_Data;

   function Pointer_Get_Version (Pointer : Pointer_Ptr) return Unsigned_32 is
   begin
      return Wayland.API.Proxy_Get_Version (Pointer.all);
   end Pointer_Get_Version;

   procedure Pointer_Destroy (Pointer : Pointer_Ptr) is
   begin
      Wayland.API.Proxy_Destroy (Pointer.all);
   end Pointer_Destroy;

   procedure Pointer_Set_Cursor
     (Pointer   : Pointer_Ptr;
      Serial    : Unsigned_32;
      Surface   : Surface_Ptr;
      Hotspot_X : Integer;
      Hotspot_Y : Integer) is
   begin
      Wayland.API.Proxy_Marshal
        (Pointer.all,
         Constants.Pointer_Set_Cursor,
         Serial,
         Surface.all'Address,
         Hotspot_X,
         Hotspot_Y);
   end Pointer_Set_Cursor;

   procedure Pointer_Release (Pointer : Pointer_Ptr) is
   begin
      Wayland.API.Proxy_Marshal (Pointer.all, Constants.Pointer_Release);
   end Pointer_Release;

   function Keyboard_Add_Listener
     (Keyboard : Keyboard_Ptr;
      Listener : Keyboard_Listener_Ptr;
      Data     : Void_Ptr) return Interfaces.C.int is
   begin
      return Wayland.API.Proxy_Add_Listener (Keyboard.all, Listener.all'Address, Data);
   end Keyboard_Add_Listener;

   procedure Keyboard_Set_User_Data (Keyboard : Keyboard_Ptr; Data : Void_Ptr) is
   begin
      Wayland.API.Proxy_Set_User_Data (Keyboard.all, Data);
   end Keyboard_Set_User_Data;

   function Keyboard_Get_User_Data (Keyboard : Keyboard_Ptr) return Void_Ptr is
   begin
      return Wayland.API.Proxy_Get_User_Data (Keyboard.all);
   end Keyboard_Get_User_Data;

   function Keyboard_Get_Version (Keyboard : Keyboard_Ptr) return Unsigned_32 is
   begin
      return Wayland.API.Proxy_Get_Version (Keyboard.all);
   end Keyboard_Get_Version;

   procedure Keyboard_Destroy (Keyboard : Keyboard_Ptr) is
   begin
      Wayland.API.Proxy_Destroy (Keyboard.all);
   end Keyboard_Destroy;

   procedure Keyboard_Release (Keyboard : Keyboard_Ptr) is
   begin
      Wayland.API.Proxy_Marshal (Keyboard.all, Constants.Keyboard_Release);
   end Keyboard_Release;

   function Touch_Add_Listener
     (Touch    : Touch_Ptr;
      Listener : Touch_Listener_Ptr;
      Data     : Void_Ptr) return Interfaces.C.int is
   begin
      return Wayland.API.Proxy_Add_Listener (Touch.all, Listener.all'Address, Data);
   end Touch_Add_Listener;

   procedure Touch_Set_User_Data (Touch : Touch_Ptr; Data : Void_Ptr) is
   begin
      Wayland.API.Proxy_Set_User_Data (Touch.all, Data);
   end Touch_Set_User_Data;

   function Touch_Get_User_Data (Touch : Touch_Ptr) return Void_Ptr is
   begin
      return Wayland.API.Proxy_Get_User_Data (Touch.all);
   end Touch_Get_User_Data;

   function Touch_Get_Version (Touch : Touch_Ptr) return Unsigned_32 is
   begin
      return Wayland.API.Proxy_Get_Version (Touch.all);
   end Touch_Get_Version;

   procedure Touch_Destroy (Touch : Touch_Ptr) is
   begin
      Wayland.API.Proxy_Destroy (Touch.all);
   end Touch_Destroy;

   procedure Touch_Release (Touch : Touch_Ptr) is
   begin
      Wayland.API.Proxy_Marshal (Touch.all, Constants.Touch_Release);
   end Touch_Release;

   function Output_Add_Listener
     (Output   : Output_Ptr;
      Listener : Output_Listener_Ptr;
      Data     : Void_Ptr) return Interfaces.C.int is
   begin
      return Wayland.API.Proxy_Add_Listener (Output.all, Listener.all'Address, Data);
   end Output_Add_Listener;

   procedure Output_Set_User_Data (Output : Output_Ptr; Data : Void_Ptr) is
   begin
      Wayland.API.Proxy_Set_User_Data (Output.all, Data);
   end Output_Set_User_Data;

   function Output_Get_User_Data (Output : Output_Ptr) return Void_Ptr is
   begin
      return Wayland.API.Proxy_Get_User_Data (Output.all);
   end Output_Get_User_Data;

   function Output_Get_Version (Output : Output_Ptr) return Unsigned_32 is
   begin
      return Wayland.API.Proxy_Get_Version (Output.all);
   end Output_Get_Version;

   procedure Output_Destroy (Output : Output_Ptr) is
   begin
      Wayland.API.Proxy_Destroy (Output.all);
   end Output_Destroy;

   procedure Output_Release (Output : Output_Ptr) is
   begin
      Wayland.API.Proxy_Marshal (Output.all, Constants.Output_Release);
   end Output_Release;

   procedure Region_Set_User_Data (Region : Region_Ptr; Data : Void_Ptr) is
   begin
      Wayland.API.Proxy_Set_User_Data (Region.all, Data);
   end Region_Set_User_Data;

   function Region_Get_User_Data (Region : Region_Ptr) return Void_Ptr is
   begin
      return Wayland.API.Proxy_Get_User_Data (Region.all);
   end Region_Get_User_Data;

   function Region_Get_Version (Region : Region_Ptr) return Unsigned_32 is
   begin
      return Wayland.API.Proxy_Get_Version (Region.all);
   end Region_Get_Version;

   procedure Region_Destroy (Region : Region_Ptr) is
   begin
      Wayland.API.Proxy_Marshal (Region.all, Constants.Region_Destroy);

      Wayland.API.Proxy_Destroy (Region.all);
   end Region_Destroy;

   procedure Region_Add
     (Region : Region_Ptr;
      X      : Integer;
      Y      : Integer;
      Width  : Integer;
      Height : Integer) is
   begin
      Wayland.API.Proxy_Marshal
        (Region.all,
         Constants.Region_Add,
         X,
         Y,
         Width,
         Height);
   end Region_Add;

   procedure Region_Subtract
     (Region : Region_Ptr;
      X      : Integer;
      Y      : Integer;
      Width  : Integer;
      Height : Integer) is
   begin
      Wayland.API.Proxy_Marshal
        (Region.all,
         Constants.Region_Subtract,
         X,
         Y,
         Width,
         Height);
   end Region_Subtract;

   procedure Subcompositor_Set_User_Data (Subcompositor : Subcompositor_Ptr; Data : Void_Ptr) is
   begin
      Wayland.API.Proxy_Set_User_Data (Subcompositor.all, Data);
   end Subcompositor_Set_User_Data;

   function Subcompositor_Get_User_Data (Subcompositor : Subcompositor_Ptr) return Void_Ptr is
   begin
      return Wayland.API.Proxy_Get_User_Data (Subcompositor.all);
   end Subcompositor_Get_User_Data;

   function Subcompositor_Get_Version (Subcompositor : Subcompositor_Ptr) return Unsigned_32 is
   begin
      return Wayland.API.Proxy_Get_Version (Subcompositor.all);
   end Subcompositor_Get_Version;

   procedure Subcompositor_Destroy (Subcompositor : Subcompositor_Ptr) is
   begin
      Wayland.API.Proxy_Marshal (Subcompositor.all, Constants.Subcompositor_Destroy);

      Wayland.API.Proxy_Destroy (Subcompositor.all);
   end Subcompositor_Destroy;

   function Subcompositor_Get_Subsurface
     (Subcompositor : Subcompositor_Ptr;
      Surface       : Surface_Ptr;
      Parent        : Surface_Ptr) return Subsurface_Ptr
   is
      P : constant Proxy_Ptr :=
        Wayland.API.Proxy_Marshal_Constructor
          (Subcompositor.all,
           Constants.Subcompositor_Get_Subsurface,
           Subsurface_Interface'Access,
           0,
           Surface.all'Address,
           Parent.all'Address);
   begin
      return (if P /= null then P.all'Access else null);
   end Subcompositor_Get_Subsurface;

   procedure Subsurface_Set_User_Data (Subsurface : Subsurface_Ptr; Data : Void_Ptr) is
   begin
      Wayland.API.Proxy_Set_User_Data (Subsurface.all, Data);
   end Subsurface_Set_User_Data;

   function Subsurface_Get_User_Data (Subsurface : Subsurface_Ptr) return Void_Ptr is
   begin
      return Wayland.API.Proxy_Get_User_Data (Subsurface.all);
   end Subsurface_Get_User_Data;

   function Subsurface_Get_Version (Subsurface : Subsurface_Ptr) return Unsigned_32 is
   begin
      return Wayland.API.Proxy_Get_Version (Subsurface.all);
   end Subsurface_Get_Version;

   procedure Subsurface_Destroy (Subsurface : Subsurface_Ptr) is
   begin
      Wayland.API.Proxy_Marshal (Subsurface.all, Constants.Subsurface_Destroy);

      Wayland.API.Proxy_Destroy (Subsurface.all);
   end Subsurface_Destroy;

   procedure Subsurface_Set_Position
     (Subsurface : Subsurface_Ptr;
      X          : Integer;
      Y          : Integer) is
   begin
      Wayland.API.Proxy_Marshal
        (Subsurface.all,
         Constants.Subsurface_Set_Position,
         X,
         Y);
   end Subsurface_Set_Position;

   procedure Subsurface_Place_Above
     (Subsurface : Subsurface_Ptr;
      Sibling    : Surface_Ptr) is
   begin
      Wayland.API.Proxy_Marshal
        (Subsurface.all,
         Constants.Subsurface_Place_Above,
         Sibling.all'Address);
   end Subsurface_Place_Above;

   procedure Subsurface_Place_Below
     (Subsurface : Subsurface_Ptr;
      Sibling    : Surface_Ptr) is
   begin
      Wayland.API.Proxy_Marshal
        (Subsurface.all,
         Constants.Subsurface_Place_Below,
         Sibling.all'Address);
   end Subsurface_Place_Below;

   procedure Subsurface_Set_Sync (Subsurface : Subsurface_Ptr) is
   begin
      Wayland.API.Proxy_Marshal (Subsurface.all, Constants.Subsurface_Set_Sync);
   end Subsurface_Set_Sync;

   procedure Subsurface_Set_Desync (Subsurface : Subsurface_Ptr) is
   begin
      Wayland.API.Proxy_Marshal (Subsurface.all, Constants.Subsurface_Set_Desync);
   end Subsurface_Set_Desync;

end Wayland.Client.Thin;
