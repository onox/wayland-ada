private with Interfaces.C.Strings;
private with Wayland.Client.Thin;

with Wayland.Client.Enums;

with C_Binding.Linux.Files;

package Wayland.Client.Protocol is
   pragma Preelaborate;

   use Wayland.Client.Enums;

   type Display is tagged limited private;
   type Registry is tagged limited private;
   type Callback is tagged limited private;
   type Compositor is tagged limited private;
   type Shm_Pool is tagged limited private;
   type Shm is tagged limited private;
   type Buffer is tagged limited private;
   type Data_Offer is tagged limited private;
   type Data_Source is tagged limited private;
   type Data_Device is tagged limited private;
   type Data_Device_Manager is tagged limited private;
   type Surface is tagged limited private;
   type Seat is tagged limited private;
   type Pointer is tagged limited private;
   type Keyboard is tagged limited private;
   type Touch is tagged limited private;
   type Output is tagged limited private;
   type Region is tagged limited private;
   type Subcompositor is tagged limited private;
   type Subsurface is tagged limited private;

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
   Surface_Interface : constant Interface_Type;
   Seat_Interface : constant Interface_Type;
   Pointer_Interface : constant Interface_Type;
   Keyboard_Interface : constant Interface_Type;
   Touch_Interface : constant Interface_Type;
   Output_Interface : constant Interface_Type;
   Region_Interface : constant Interface_Type;
   Subcompositor_Interface : constant Interface_Type;
   Subsurface_Interface : constant Interface_Type;

   function Has_Proxy (Object : Display) return Boolean
     with Global => null;

   function Get_Version (Object : Display) return Unsigned_32
     with Pre => Object.Has_Proxy;

   function Is_Connected (Object : Display) return Boolean renames Has_Proxy;

   procedure Connect (Object : in out Display)
     with Pre => not Object.Is_Connected;
   --  Attempts connecting with the Wayland server

   type Check_For_Events_Status is (Events_Need_Processing, No_Events, Error);

   function Check_For_Events
     (Object  : Display;
      Timeout : Integer) return Check_For_Events_Status;
   --  The timeout is given in milliseconds

   function Dispatch (Object : Display) return Integer
     with Pre => Object.Is_Connected;
   --  Process incoming events

   procedure Dispatch (Object : Display)
     with Pre => Object.Is_Connected;
   --  Process incoming events. Ignores error code. TODO To be removed?

   function Dispatch_Pending (Object : Display) return Integer
     with Pre => Object.Is_Connected;
   --  Dispatch default queue events without reading from
   --  the display file descriptor
   --
   --  This function dispatches events on the main event queue.
   --  It does not attempt to read the display fd and simply returns zero
   --  if the main queue is empty, i.e., it doesn't block.
   --
   --  Returns the number of dispatched events or -1 on failure

   function Prepare_Read (Object : Display) return Integer
     with Pre => Object.Is_Connected;
   --  Prepare to read events from the display's file descriptor

   function Read_Events (Object : Display) return Call_Result_Code
     with Pre => Object.Is_Connected;

   procedure Cancel_Read (Object : Display)
     with Pre => Object.Is_Connected;
   --  Cancel read intention on display's file descriptor

   function Roundtrip (Object : Display) return Integer
     with Pre => Object.Is_Connected;

   procedure Roundtrip (Object : Display)
     with Pre => Object.Is_Connected;

   procedure Disconnect (Object : in out Display)
     with Pre  => Object.Is_Connected,
          Post => not Object.Is_Connected;

   procedure Get_Registry
     (Object   : Display;
      Registry : in out Protocol.Registry'Class)
   with Pre => Object.Is_Connected and not Registry.Has_Proxy;

   function Sync (Object : Display) return Callback'Class
     with Pre => Object.Is_Connected;

   function Has_Proxy (Object : Registry) return Boolean
     with Global => null;

   procedure Destroy (Object : in out Registry)
     with Pre  => Object.Has_Proxy,
          Post => not Object.Has_Proxy;

   function Get_Version (Object : Registry) return Unsigned_32
     with Pre => Object.Has_Proxy;

   function Has_Proxy (Object : Callback) return Boolean
     with Global => null;

   procedure Destroy (Object : in out Callback)
     with Pre  => Object.Has_Proxy,
          Post => not Object.Has_Proxy;

   function Get_Version (Object : Callback) return Unsigned_32
     with Pre => Object.Has_Proxy;

   function Has_Proxy (Object : Compositor) return Boolean
     with Global => null;

   procedure Destroy (Object : in out Compositor)
     with Pre  => Object.Has_Proxy,
          Post => not Object.Has_Proxy;

   function Get_Version (Object : Compositor) return Unsigned_32
     with Pre => Object.Has_Proxy;

   procedure Bind (Object   : in out Compositor;
                   Registry : Protocol.Registry'Class;
                   Id       : Unsigned_32;
                   Version  : Unsigned_32)
     with Pre => not Object.Has_Proxy and Registry.Has_Proxy;

   procedure Create_Surface (Object  : Compositor;
                             Surface : in out Protocol.Surface'Class)
     with Pre => Object.Has_Proxy;

   procedure Create_Region (Object : Compositor;
                            Region : in out Protocol.Region'Class)
     with Pre => Object.Has_Proxy;

   function Has_Proxy (Object : Shm_Pool) return Boolean
     with Global => null;

   procedure Destroy (Object : in out Shm_Pool)
     with Pre  => Object.Has_Proxy,
          Post => not Object.Has_Proxy;

   function Get_Version (Object : Shm_Pool) return Unsigned_32
     with Pre => Object.Has_Proxy;

   procedure Create_Buffer (Object : Shm_Pool;
                            Offset : Natural;
                            Width  : Natural;
                            Height : Natural;
                            Stride : Natural;
                            Format : Shm_Format;
                            Buffer : in out Protocol.Buffer'Class)
     with Pre => Object.Has_Proxy;

   procedure Resize (Object : Shm_Pool;
                     Size   : Positive)
     with Pre => Object.Has_Proxy;

   function Has_Proxy (Object : Shm) return Boolean
     with Global => null;

   procedure Destroy (Object : in out Shm)
     with Pre  => Object.Has_Proxy,
          Post => not Object.Has_Proxy;

   function Get_Version (Object : Shm) return Unsigned_32
     with Pre => Object.Has_Proxy;

   procedure Bind (Object   : in out Shm;
                   Registry : Protocol.Registry'Class;
                   Id       : Unsigned_32;
                   Version  : Unsigned_32)
     with Pre => not Object.Has_Proxy and Registry.Has_Proxy;

   procedure Create_Pool (Object          : Shm;
                          File_Descriptor : C_Binding.Linux.Files.File;
                          Size            : Positive;
                          Pool            : in out Shm_Pool'Class);

   function Has_Proxy (Object : Buffer) return Boolean
     with Global => null;

   procedure Destroy (Object : in out Buffer)
     with Pre  => Object.Has_Proxy,
          Post => not Object.Has_Proxy;

   function Get_Version (Object : Buffer) return Unsigned_32
     with Pre => Object.Has_Proxy;

   function Has_Proxy (Object : Data_Offer) return Boolean
     with Global => null;

   procedure Destroy (Object : in out Data_Offer)
     with Pre  => Object.Has_Proxy,
          Post => not Object.Has_Proxy;

   function Get_Version (Object : Data_Offer) return Unsigned_32
     with Pre => Object.Has_Proxy;

   procedure Do_Accept (Object    : Data_Offer;
                        Serial    : Unsigned_32;
                        Mime_Type : String)
     with Pre => Object.Has_Proxy;

   procedure Do_Not_Accept (Object : Data_Offer;
                            Serial : Unsigned_32)
     with Pre => Object.Has_Proxy;

   procedure Receive (Object          : Data_Offer;
                      Mime_Type       : String;
                      File_Descriptor : Integer)
     with Pre => Object.Has_Proxy;

   procedure Finish (Object : Data_Offer)
     with Pre => Object.Has_Proxy;

   procedure Set_Actions (Object           : Data_Offer;
                          Dnd_Actions      : Unsigned_32;
                          Preferred_Action : Unsigned_32)
     with Pre => Object.Has_Proxy;

   function Has_Proxy (Object : Data_Source) return Boolean
     with Global => null;

   procedure Destroy (Object : in out Data_Source)
     with Pre  => Object.Has_Proxy,
          Post => not Object.Has_Proxy;

   function Get_Version (Object : Data_Source) return Unsigned_32
     with Pre => Object.Has_Proxy;

   procedure Offer (Object    : Data_Source;
                    Mime_Type : String)
     with Pre => Object.Has_Proxy;

   procedure Set_Actions (Object      : Data_Source;
                          Dnd_Actions : Unsigned_32)
     with Pre => Object.Has_Proxy;

   function Has_Proxy (Object : Data_Device) return Boolean
     with Global => null;

   procedure Destroy (Object : in out Data_Device)
     with Pre  => Object.Has_Proxy,
          Post => not Object.Has_Proxy;

   function Get_Version (Object : Data_Device) return Unsigned_32
     with Pre => Object.Has_Proxy;

   procedure Release (Object : in out Data_Device)
     with Pre  => Object.Has_Proxy,
          Post => not Object.Has_Proxy;

   procedure Start_Drag (Object : Data_Device;
                         Source : Data_Source'Class;
                         Origin : Surface'Class;
                         Icon   : Surface'Class;
                         Serial : Unsigned_32)
     with Pre => Object.Has_Proxy and Source.Has_Proxy and Origin.Has_Proxy and Icon.Has_Proxy;

   procedure Set_Selection (Object : Data_Device;
                            Source : Data_Source'Class;
                            Serial : Unsigned_32)
     with Pre => Object.Has_Proxy and Source.Has_Proxy;

   function Has_Proxy (Object : Data_Device_Manager) return Boolean
     with Global => null;

   procedure Destroy (Object : in out Data_Device_Manager)
     with Pre  => Object.Has_Proxy,
          Post => not Object.Has_Proxy;

   function Get_Version (Object : Data_Device_Manager) return Unsigned_32
     with Pre => Object.Has_Proxy;

   procedure Create_Data_Source (Object : Data_Device_Manager;
                                 Source : in out Data_Source'Class)
     with Pre => Object.Has_Proxy;

   procedure Get_Data_Device (Object : Data_Device_Manager;
                              Seat   : Protocol.Seat'Class;
                              Device : in out Data_Device'Class)
     with Pre => Object.Has_Proxy;

   function Has_Proxy (Object : Surface) return Boolean
     with Global => null;

   procedure Destroy (Object : in out Surface)
     with Pre  => Object.Has_Proxy,
          Post => not Object.Has_Proxy;

   function Get_Version (Object : Surface) return Unsigned_32
     with Pre => Object.Has_Proxy;

   procedure Attach (Object : Surface;
                     Buffer : Protocol.Buffer'Class;
                     X, Y   : Integer)
     with Pre => Object.Has_Proxy and Buffer.Has_Proxy;

   procedure Damage (Object : Surface;
                     X, Y   : Integer;
                     Width  : Natural;
                     Height : Natural)
     with Pre => Object.Has_Proxy;

   function Frame (Object : Surface) return Callback'Class
     with Pre => Object.Has_Proxy;

   procedure Set_Opaque_Region (Object : Surface;
                                Region : Protocol.Region'Class)
     with Pre => Object.Has_Proxy;

   procedure Set_Input_Region (Object : Surface;
                               Region : Protocol.Region'Class)
     with Pre => Object.Has_Proxy;

   procedure Commit (Object : Surface)
     with Pre => Object.Has_Proxy;

   procedure Set_Buffer_Transform (Object    : Surface;
                                   Transform : Output_Transform)
     with Pre => Object.Has_Proxy;

   procedure Set_Buffer_Scale (Object : Surface;
                               Scale  : Positive)
     with Pre => Object.Has_Proxy;

   procedure Damage_Buffer (Object : Surface;
                            X, Y   : Integer;
                            Width  : Natural;
                            Height : Natural)
     with Pre => Object.Has_Proxy;

   function Has_Proxy (Object : Seat) return Boolean
     with Global => null;

   procedure Destroy (Object : in out Seat)
     with Pre  => Object.Has_Proxy,
          Post => not Object.Has_Proxy;

   function Get_Version (Object : Seat) return Unsigned_32
     with Pre => Object.Has_Proxy;

   procedure Release (Object : in out Seat)
     with Pre  => Object.Has_Proxy,
          Post => not Object.Has_Proxy;

   procedure Bind (Object   : in out Seat;
                   Registry : Protocol.Registry'Class;
                   Id       : Unsigned_32;
                   Version  : Unsigned_32)
     with Pre => not Object.Has_Proxy and Registry.Has_Proxy;

   procedure Get_Pointer (Object  : Seat;
                          Pointer : in out Protocol.Pointer'Class)
     with Pre => Object.Has_Proxy and not Pointer.Has_Proxy;

   procedure Get_Keyboard (Object   : Seat;
                           Keyboard : in out Protocol.Keyboard'Class)
     with Pre => Object.Has_Proxy and not Keyboard.Has_Proxy;

   procedure Get_Touch (Object : Seat;
                        Touch  : in out Protocol.Touch'Class)
     with Pre => Object.Has_Proxy and not Touch.Has_Proxy;

   function Has_Proxy (Object : Pointer) return Boolean
     with Global => null;

   procedure Destroy (Object : in out Pointer)
     with Pre  => Object.Has_Proxy,
          Post => not Object.Has_Proxy;

   function Get_Version (Object : Pointer) return Unsigned_32
     with Pre => Object.Has_Proxy;

   procedure Release (Object : in out Pointer)
     with Pre  => Object.Has_Proxy,
          Post => not Object.Has_Proxy;

   procedure Set_Cursor (Object    : Pointer;
                         Serial    : Unsigned_32;
                         Surface   : Protocol.Surface'Class;
                         Hotspot_X : Integer;
                         Hotspot_Y : Integer)
     with Pre => Object.Has_Proxy;

   function Has_Proxy (Object : Keyboard) return Boolean
     with Global => null;

   procedure Destroy (Object : in out Keyboard)
     with Pre  => Object.Has_Proxy,
          Post => not Object.Has_Proxy;

   function Get_Version (Object : Keyboard) return Unsigned_32
     with Pre => Object.Has_Proxy;

   procedure Release (Object : in out Keyboard)
     with Pre  => Object.Has_Proxy,
          Post => not Object.Has_Proxy;

   function Has_Proxy (Object : Touch) return Boolean
     with Global => null;

   procedure Destroy (Object : in out Touch)
     with Pre  => Object.Has_Proxy,
          Post => not Object.Has_Proxy;

   function Get_Version (Object : Touch) return Unsigned_32
     with Pre => Object.Has_Proxy;

   procedure Release (Object : in out Touch)
     with Pre  => Object.Has_Proxy,
          Post => not Object.Has_Proxy;

   function Has_Proxy (Object : Output) return Boolean
     with Global => null;

   procedure Destroy (Object : in out Output)
     with Pre  => Object.Has_Proxy,
          Post => not Object.Has_Proxy;

   function Get_Version (Object : Output) return Unsigned_32
     with Pre => Object.Has_Proxy;

   procedure Release (Object : in out Output)
     with Pre  => Object.Has_Proxy,
          Post => not Object.Has_Proxy;

   function Has_Proxy (Object : Region) return Boolean
     with Global => null;

   procedure Destroy (Object : in out Region)
     with Pre  => Object.Has_Proxy,
          Post => not Object.Has_Proxy;

   function Get_Version (Object : Region) return Unsigned_32
     with Pre => Object.Has_Proxy;

   procedure Add (Object : Region;
                  X, Y   : Integer;
                  Width  : Natural;
                  Height : Natural)
     with Pre => Object.Has_Proxy;

   procedure Subtract (Object : Region;
                       X, Y   : Integer;
                       Width  : Natural;
                       Height : Natural)
     with Pre => Object.Has_Proxy;

   function Has_Proxy (Object : Subcompositor) return Boolean
     with Global => null;

   procedure Destroy (Object : in out Subcompositor)
     with Pre  => Object.Has_Proxy,
          Post => not Object.Has_Proxy;

   function Get_Version (Object : Subcompositor) return Unsigned_32
     with Pre => Object.Has_Proxy;

   procedure Get_Subsurface (Object     : Subcompositor;
                             Surface    : Protocol.Surface'Class;
                             Parent     : Protocol.Surface'Class;
                             Subsurface : in out Protocol.Subsurface'Class)
     with Pre => Object.Has_Proxy and Surface.Has_Proxy and Parent.Has_Proxy;

   function Has_Proxy (Object : Subsurface) return Boolean
     with Global => null;

   procedure Destroy (Object : in out Subsurface)
     with Pre  => Object.Has_Proxy,
          Post => not Object.Has_Proxy;

   function Get_Version (Object : Subsurface) return Unsigned_32
     with Pre => Object.Has_Proxy;

   procedure Set_Position (Object : Subsurface; X, Y : Integer)
     with Pre => Object.Has_Proxy;

   procedure Place_Above (Object  : Subsurface;
                          Sibling : Surface'Class)
     with Pre => Object.Has_Proxy and Sibling.Has_Proxy;

   procedure Place_Below (Object  : Subsurface;
                          Sibling : Surface'Class)
     with Pre => Object.Has_Proxy and Sibling.Has_Proxy;

   procedure Set_Sync (Object : Subsurface)
     with Pre => Object.Has_Proxy;

   procedure Set_Desync (Object : Subsurface)
     with Pre => Object.Has_Proxy;

   generic
      type Data_Type is limited private;
      type Data_Ptr is access all Data_Type;

      with procedure Error
        (Data      : not null Data_Ptr;
         Display   : Protocol.Display;
         Object_Id : Void_Ptr;
         Code      : Unsigned_32;
         Message   : String);
      --  TODO Should really Object_Id really be exposed here? This part
      --  of the API can potentially be improved upon.

      with procedure Delete_Id
        (Data    : not null Data_Ptr;
         Display : Protocol.Display;
         Id      : Unsigned_32);
   package Display_Events is

      function Subscribe
        (Object : in out Display;
         Data   : not null Data_Ptr) return Call_Result_Code;

   end Display_Events;

   generic
      type Data_Type is limited private;
      type Data_Ptr is access all Data_Type;

      with procedure Global_Object_Added
        (Data     : not null Data_Ptr;
         Registry : Protocol.Registry;
         Id       : Unsigned_32;
         Name     : String;
         Version  : Unsigned_32);

      with procedure Global_Object_Removed
        (Data     : not null Data_Ptr;
         Registry : Protocol.Registry;
         Id       : Unsigned_32);
   package Registry_Events is

      function Subscribe
        (Object : in out Registry;
         Data   : not null Data_Ptr) return Call_Result_Code;

   end Registry_Events;

   generic
      type Data_Type is limited private;
      type Data_Ptr is access all Data_Type;

      with procedure Done
        (Data          : not null Data_Ptr;
         Callback      : Protocol.Callback;
         Callback_Data : Unsigned_32);
   package Callback_Events is

      function Subscribe
        (Object : in out Callback;
         Data   : not null Data_Ptr) return Call_Result_Code;

   end Callback_Events;

   generic
      type Data_Type is limited private;
      type Data_Ptr is access all Data_Type;

      with procedure Format
        (Data   : not null Data_Ptr;
         Shm    : Protocol.Shm;
         Format : Shm_Format);
   package Shm_Events is

      function Subscribe
        (Object : in out Shm;
         Data   : not null Data_Ptr) return Call_Result_Code;

   end Shm_Events;

   generic
      type Data_Type is limited private;
      type Data_Ptr is access all Data_Type;

      with procedure Release
        (Data   : not null Data_Ptr;
         Buffer : Protocol.Buffer);
   package Buffer_Events is

      function Subscribe
        (Object : in out Buffer;
         Data   : not null Data_Ptr) return Call_Result_Code;

   end Buffer_Events;

   generic
      type Data_Type is limited private;
      type Data_Ptr is access all Data_Type;

      with procedure Offer
        (Data       : not null Data_Ptr;
         Data_Offer : Protocol.Data_Offer;
         Mime_Type  : String);

      with procedure Source_Actions
        (Data           : not null Data_Ptr;
         Data_Offer     : Protocol.Data_Offer;
         Source_Actions : Unsigned_32);

      with procedure Action
        (Data       : not null Data_Ptr;
         Data_Offer : Protocol.Data_Offer;
         Dnd_Action : Unsigned_32);
   package Data_Offer_Events is

      function Subscribe
        (Object : in out Data_Offer;
         Data   : not null Data_Ptr) return Call_Result_Code;

   end Data_Offer_Events;

   generic
      type Data_Type is limited private;
      type Data_Ptr is access all Data_Type;

      with procedure Target
        (Data        : not null Data_Ptr;
         Data_Source : Protocol.Data_Source;
         Mime_Type   : String);

      with procedure Send
        (Data        : not null Data_Ptr;
         Data_Source : Protocol.Data_Source;
         Mime_Type   : String;
         Fd          : Integer);

      with procedure Cancelled
        (Data        : not null Data_Ptr;
         Data_Source : Protocol.Data_Source);

      with procedure Dnd_Drop_Performed
        (Data        : not null Data_Ptr;
         Data_Source : Protocol.Data_Source);

      with procedure Dnd_Finished
        (Data        : not null Data_Ptr;
         Data_Source : Protocol.Data_Source);

      with procedure Action
        (Data        : not null Data_Ptr;
         Data_Source : Protocol.Data_Source;
         Dnd_Action  : Unsigned_32);
   package Data_Source_Events is

      function Subscribe
        (Object : in out Data_Source;
         Data   : not null Data_Ptr) return Call_Result_Code;

   end Data_Source_Events;

   generic
      type Data_Type is limited private;
      type Data_Ptr is access all Data_Type;

      with procedure Data_Offer
        (Data        : not null Data_Ptr;
         Data_Device : Protocol.Data_Device;
         Id          : Unsigned_32);

      with procedure Enter
        (Data        : not null Data_Ptr;
         Data_Device : Protocol.Data_Device;
         Serial      : Unsigned_32;
         Surface     : Protocol.Surface;
         X, Y        : Fixed;
         Id          : Protocol.Data_Offer);

      with procedure Leave
        (Data        : not null Data_Ptr;
         Data_Device : Protocol.Data_Device);

      with procedure Motion
        (Data        : not null Data_Ptr;
         Data_Device : Protocol.Data_Device;
         Time        : Unsigned_32;
         X, Y        : Fixed);

      with procedure Drop
        (Data        : not null Data_Ptr;
         Data_Device : Protocol.Data_Device);

      with procedure Selection
        (Data        : not null Data_Ptr;
         Data_Device : Protocol.Data_Device;
         Id          : Protocol.Data_Offer);
   package Data_Device_Events is

      function Subscribe
        (Object : in out Data_Device;
         Data   : not null Data_Ptr) return Call_Result_Code;

   end Data_Device_Events;

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
        (Object : in out Surface;
         Data   : not null Data_Ptr) return Call_Result_Code;

   end Surface_Events;

   generic
      type Data_Type is limited private;
      type Data_Ptr is access all Data_Type;

      with procedure Seat_Capabilities
        (Data         : not null Data_Ptr;
         Seat         : Protocol.Seat;
         Capabilities : Seat_Capability);

      with procedure Seat_Name
        (Data : not null Data_Ptr;
         Seat : Protocol.Seat;
         Name : String);
   package Seat_Events is

      function Subscribe
        (Object : in out Seat;
         Data   : not null Data_Ptr) return Call_Result_Code;

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
         State   : Pointer_Button_State);

      with procedure Pointer_Scroll
        (Data    : not null Data_Ptr;
         Pointer : Protocol.Pointer;
         Time    : Unsigned_32;
         Axis    : Pointer_Axis;
         Value   : Fixed);

      with procedure Pointer_Frame
        (Data    : not null Data_Ptr;
         Pointer : Protocol.Pointer);

      with procedure Pointer_Scroll_Source
        (Data        : not null Data_Ptr;
         Pointer     : Protocol.Pointer;
         Axis_Source : Pointer_Axis_Source);

      with procedure Pointer_Scroll_Stop
        (Data    : not null Data_Ptr;
         Pointer : Protocol.Pointer;
         Time    : Unsigned_32;
         Axis    : Pointer_Axis);

      with procedure Pointer_Scroll_Discrete
        (Data     : not null Data_Ptr;
         Pointer  : Protocol.Pointer;
         Axis     : Pointer_Axis;
         Discrete : Integer);
   package Pointer_Events is

      function Subscribe
        (Object : in out Pointer;
         Data   : not null Data_Ptr) return Call_Result_Code;

   end Pointer_Events;

   generic
      type Data_Type is limited private;
      type Data_Ptr is access all Data_Type;

      with procedure Keymap
        (Data     : not null Data_Ptr;
         Keyboard : Protocol.Keyboard;
         Format   : Keyboard_Keymap_Format;
         Fd       : Integer;
         Size     : Unsigned_32);

      with procedure Enter
        (Data     : not null Data_Ptr;
         Keyboard : Protocol.Keyboard;
         Serial   : Unsigned_32;
         Surface  : Protocol.Surface;
         Keys     : Wayland_Array_T);

      with procedure Leave
        (Data     : not null Data_Ptr;
         Keyboard : Protocol.Keyboard;
         Serial   : Unsigned_32;
         Surface  : Protocol.Surface);

      with procedure Key
        (Data     : not null Data_Ptr;
         Keyboard : Protocol.Keyboard;
         Serial   : Unsigned_32;
         Time     : Unsigned_32;
         Key      : Unsigned_32;
         State    : Keyboard_Key_State);

      with procedure Modifiers
        (Data           : not null Data_Ptr;
         Keyboard       : Protocol.Keyboard;
         Serial         : Unsigned_32;
         Mods_Depressed : Unsigned_32;
         Mods_Latched   : Unsigned_32;
         Mods_Locked    : Unsigned_32;
         Group          : Unsigned_32);

      with procedure Repeat_Info
        (Data     : not null Data_Ptr;
         Keyboard : Protocol.Keyboard;
         Rate     : Integer;
         Delay_V  : Integer);
   package Keyboard_Events is

      function Subscribe
        (Object : in out Keyboard;
         Data   : not null Data_Ptr) return Call_Result_Code;

   end Keyboard_Events;

   generic
      type Data_Type is limited private;
      type Data_Ptr is access all Data_Type;

      with procedure Down
        (Data    : not null Data_Ptr;
         Touch   : Protocol.Touch;
         Serial  : Unsigned_32;
         Time    : Unsigned_32;
         Surface : Protocol.Surface;
         Id      : Integer;
         X, Y    : Fixed);

      with procedure Up
        (Data   : not null Data_Ptr;
         Touch  : Protocol.Touch;
         Serial : Unsigned_32;
         Time   : Unsigned_32;
         Id     : Integer);

      with procedure Motion
        (Data  : not null Data_Ptr;
         Touch : Protocol.Touch;
         Time  : Unsigned_32;
         Id    : Integer;
         X, Y  : Fixed);

      with procedure Frame
        (Data  : not null Data_Ptr;
         Touch : Protocol.Touch);

      with procedure Cancel
        (Data  : not null Data_Ptr;
         Touch : Protocol.Touch);

      with procedure Shape
        (Data  : not null Data_Ptr;
         Touch : Protocol.Touch;
         Id    : Integer;
         Major : Fixed;
         Minor : Fixed);

      with procedure Orientation
        (Data        : not null Data_Ptr;
         Touch       : Protocol.Touch;
         Id          : Integer;
         Orientation : Fixed);
   package Touch_Events is

      function Subscribe
        (Object : in out Touch;
         Data   : not null Data_Ptr) return Call_Result_Code;

   end Touch_Events;

   generic
      type Data_Type is limited private;
      type Data_Ptr is access all Data_Type;

      with procedure Geometry
        (Data            : not null Data_Ptr;
         Output          : Protocol.Output;
         X, Y            : Integer;
         Physical_Width  : Integer;
         Physical_Height : Integer;
         Subpixel        : Output_Subpixel;
         Make            : String;
         Model           : String;
         Transform       : Output_Transform);

      with procedure Mode
        (Data    : not null Data_Ptr;
         Output  : Protocol.Output;
         Flags   : Output_Mode;
         Width   : Integer;
         Height  : Integer;
         Refresh : Integer);

      with procedure Done
        (Data   : not null Data_Ptr;
         Output : Protocol.Output);

      with procedure Scale
        (Data   : not null Data_Ptr;
         Output : Protocol.Output;
         Factor : Integer);
   package Output_Events is

      function Subscribe
        (Object : in out Output;
         Data   : not null Data_Ptr) return Call_Result_Code;

   end Output_Events;

private

   subtype char_array is Interfaces.C.char_array;

   subtype chars_ptr is Interfaces.C.Strings.chars_ptr;

   function Value (C : chars_ptr) return String renames Interfaces.C.Strings.Value;

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
      Proxy : Thin.Display_Ptr;
   end record;

   function Has_Proxy (Object : Display) return Boolean is
     (Object.Proxy /= null);

   type Registry is tagged limited record
      Proxy : Thin.Registry_Ptr;
   end record;

   function Has_Proxy (Object : Registry) return Boolean is
     (Object.Proxy /= null);

   type Callback is tagged limited record
      Proxy : Thin.Callback_Ptr;
   end record;

   function Has_Proxy (Object : Callback) return Boolean is
     (Object.Proxy /= null);

   type Compositor is tagged limited record
      Proxy : Thin.Compositor_Ptr;
   end record;

   function Has_Proxy (Object : Compositor) return Boolean is
     (Object.Proxy /= null);

   type Shm_Pool is tagged limited record
      Proxy : Thin.Shm_Pool_Ptr;
   end record;

   function Has_Proxy (Object : Shm_Pool) return Boolean is
     (Object.Proxy /= null);

   type Shm is tagged limited record
      Proxy : Thin.Shm_Ptr;
   end record;

   function Has_Proxy (Object : Shm) return Boolean is
     (Object.Proxy /= null);

   type Buffer is tagged limited record
      Proxy : Thin.Buffer_Ptr;
   end record;

   function Has_Proxy (Object : Buffer) return Boolean is
     (Object.Proxy /= null);

   type Data_Offer is tagged limited record
      Proxy : Thin.Data_Offer_Ptr;
   end record;

   function Has_Proxy (Object : Data_Offer) return Boolean is
     (Object.Proxy /= null);

   type Data_Source is tagged limited record
      Proxy : Thin.Data_Source_Ptr;
   end record;

   function Has_Proxy (Object : Data_Source) return Boolean is
     (Object.Proxy /= null);

   type Data_Device is tagged limited record
      Proxy : Thin.Data_Device_Ptr;
   end record;

   function Has_Proxy (Object : Data_Device) return Boolean is
     (Object.Proxy /= null);

   type Data_Device_Manager is tagged limited record
      Proxy : Thin.Data_Device_Manager_Ptr;
   end record;

   function Has_Proxy (Object : Data_Device_Manager) return Boolean is
     (Object.Proxy /= null);

   type Surface is tagged limited record
      Proxy : Thin.Surface_Ptr;
   end record;

   function Has_Proxy (Object : Surface) return Boolean is
     (Object.Proxy /= null);

   type Seat is tagged limited record
      Proxy : Thin.Seat_Ptr;
   end record;

   function Has_Proxy (Object : Seat) return Boolean is
     (Object.Proxy /= null);

   type Pointer is tagged limited record
      Proxy : Thin.Pointer_Ptr;
   end record;

   function Has_Proxy (Object : Pointer) return Boolean is
     (Object.Proxy /= null);

   type Keyboard is tagged limited record
      Proxy : Thin.Keyboard_Ptr;
   end record;

   function Has_Proxy (Object : Keyboard) return Boolean is
     (Object.Proxy /= null);

   type Touch is tagged limited record
      Proxy : Thin.Touch_Ptr;
   end record;

   function Has_Proxy (Object : Touch) return Boolean is
     (Object.Proxy /= null);

   type Output is tagged limited record
      Proxy : Thin.Output_Ptr;
   end record;

   function Has_Proxy (Object : Output) return Boolean is
     (Object.Proxy /= null);

   type Region is tagged limited record
      Proxy : Thin.Region_Ptr;
   end record;

   function Has_Proxy (Object : Region) return Boolean is
     (Object.Proxy /= null);

   type Subcompositor is tagged limited record
      Proxy : Thin.Subcompositor_Ptr;
   end record;

   function Has_Proxy (Object : Subcompositor) return Boolean is
     (Object.Proxy /= null);

   type Subsurface is tagged limited record
      Proxy : Thin.Subsurface_Ptr;
   end record;

   function Has_Proxy (Object : Subsurface) return Boolean is
     (Object.Proxy /= null);

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
