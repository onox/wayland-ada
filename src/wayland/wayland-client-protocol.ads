private with Interfaces.C.Strings;
private with Wayland.Client.Thin;

with Wayland.Client.Enums;

with C_Binding.Linux.Files;

package Wayland.Client.Protocol is
   pragma Preelaborate;

   use Wayland.Client.Enums;

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

   type Subcompositor is tagged limited private;

   function Has_Proxy (Object : Subcompositor) return Boolean
     with Global => null;

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

   function Has_Proxy (Object : Compositor) return Boolean
     with Global => null;

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

   type Data_Device_Manager is tagged limited private;

   function Has_Proxy (Object : Data_Device_Manager) return Boolean
     with Global => null;

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

   function Has_Proxy (Object : Seat) return Boolean
     with Global => null;

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

   function Has_Proxy (Object : Pointer) return Boolean
     with Global => null;

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

   function Has_Proxy (Object : Shm) return Boolean
     with Global => null;

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

   function Has_Proxy (Object : Shm_Pool) return Boolean
     with Global => null;

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

   function Has_Proxy (Object : Data_Device) return Boolean
     with Global => null;

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

   function Has_Proxy (Object : Subsurface) return Boolean
     with Global => null;

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

   function Has_Proxy (Object : Surface) return Boolean
     with Global => null;

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
                                   Transform : Output_Transform) with
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

   function Has_Proxy (Object : Buffer) return Boolean
     with Global => null;

   function Get_Version (Buffer : Protocol.Buffer) return Unsigned_32 with
     Pre => Buffer.Has_Proxy;

   procedure Destroy (Buffer : in out Protocol.Buffer) with
     Pre  => Buffer.Has_Proxy,
     Post => not Buffer.Has_Proxy;

   type Display is tagged limited private with
     Default_Initial_Condition => not Display.Is_Connected;

   function Has_Proxy (Object : Display) return Boolean
     with Global => null;

   function Is_Connected (Object : Display) return Boolean renames Has_Proxy;

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

   function Has_Proxy (Object : Registry) return Boolean
     with Global => null;

   procedure Destroy (Registry : in out Protocol.Registry) with
     Pre  => Registry.Has_Proxy,
     Post => not Registry.Has_Proxy;

   function Get_Version (Registry : Protocol.Registry) return Unsigned_32 with
     Pre => Registry.Has_Proxy;

   type Callback is tagged limited private;

   function Has_Proxy (Object : Callback) return Boolean
     with Global => null;

   procedure Destroy (Callback : in out Protocol.Callback) with
     Pre    => Callback.Has_Proxy,
     Post   => not Callback.Has_Proxy;

   function Get_Version (Callback : Protocol.Callback) return Unsigned_32 with
     Pre => Callback.Has_Proxy;

   type Data_Offer is tagged limited private;
   --  Provides drag and drop functionality in a Wayland application

   function Has_Proxy (Object : Data_Offer) return Boolean
     with Global => null;

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

   function Has_Proxy (Object : Data_Source) return Boolean
     with Global => null;

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

   function Has_Proxy (Object : Keyboard) return Boolean
     with Global => null;

   procedure Destroy (Keyboard : in out Protocol.Keyboard) with
     Pre    => Keyboard.Has_Proxy,
     Post   => not Keyboard.Has_Proxy;

   function Get_Version (Keyboard : Protocol.Keyboard) return Unsigned_32 with
     Pre => Keyboard.Has_Proxy;

   procedure Release (Keyboard : in out Protocol.Keyboard) with
     Pre    => Keyboard.Has_Proxy,
     Post   => not Keyboard.Has_Proxy;

   type Touch is tagged limited private;

   function Has_Proxy (Object : Touch) return Boolean
     with Global => null;

   procedure Destroy (Touch : in out Protocol.Touch) with
     Pre    => Touch.Has_Proxy,
     Post   => not Touch.Has_Proxy;

   function Get_Version (Touch : Protocol.Touch) return Unsigned_32 with
     Pre => Touch.Has_Proxy;

   procedure Release (Touch : in out Protocol.Touch) with
     Pre    => Touch.Has_Proxy,
     Post   => not Touch.Has_Proxy;

   type Output is tagged limited private;

   function Has_Proxy (Object : Output) return Boolean
     with Global => null;

   procedure Destroy (Output : in out Protocol.Output) with
     Pre    => Output.Has_Proxy,
     Post   => not Output.Has_Proxy;

   function Get_Version (Output : Protocol.Output) return Unsigned_32 with
     Pre => Output.Has_Proxy;

   procedure Release (Output : in out Protocol.Output) with
     Pre    => Output.Has_Proxy,
     Post   => not Output.Has_Proxy;

   type Region is tagged limited private;

   function Has_Proxy (Object : Region) return Boolean
     with Global => null;

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
                             Format : Shm_Format);
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
         Capabilities : Seat_Capability);

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

      function Subscribe (Pointer : in out Protocol.Pointer;
                          Data    : not null Data_Ptr) return Call_Result_Code;

   end Pointer_Events;
   --  Pointer Axis Events are for example scroll wheel rotation

   generic
      type Data_Type is limited private;
      type Data_Ptr is access all Data_Type;

      with procedure Keymap (Data     : not null Data_Ptr;
                             Keyboard : Protocol.Keyboard;
                             Format   : Keyboard_Keymap_Format;
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
                          State    : Keyboard_Key_State);

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
                               Subpixel        : Output_Subpixel;
                               Make            : String;
                               Model           : String;
                               Transform       : Output_Transform);

      with procedure Mode (Data    : not null Data_Ptr;
                           Output  : Protocol.Output;
                           Flags   : Output_Mode;
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
      My_Fd : Integer;
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
