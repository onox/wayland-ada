with Interfaces.C.Strings;

with Wayland.API;

--  Mostly auto generated from /usr/share/wayland/wayland.xml
private package Wayland.Client.Thin is
   pragma Preelaborate;

   subtype chars_ptr is Interfaces.C.Strings.chars_ptr;

   --  Begin core parts

   subtype Interface_T is Wayland.API.Interface_T;

   subtype Proxy_Ptr     is Wayland.API.Proxy_Ptr;
   subtype Display_Ptr   is Wayland.API.Display_Ptr;
   subtype Interface_Ptr is Wayland.API.Interface_Ptr;

   function Display_Connect (Name : C_String) return Display_Ptr;

   procedure Display_Disconnect (This : in out Display_Ptr);

   --  End core parts

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

--   type Display_Ptr is new Proxy_Ptr;

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

   type Display_Error_Subprogram_Ptr is access procedure
     (Data      : Void_Ptr;
      Display   : Display_Ptr;
      Object_Id : Void_Ptr;
      Code      : Unsigned_32;
      Message   : chars_ptr)
   with Convention => C;

   type Display_Delete_Id_Subprogram_Ptr is access procedure
     (Data    : Void_Ptr;
      Display : Display_Ptr;
      Id      : Unsigned_32)
   with Convention => C;

   type Display_Listener_T is record
      Error     : Display_Error_Subprogram_Ptr;
      Delete_Id : Display_Delete_Id_Subprogram_Ptr;
   end record
     with Convention => C_Pass_By_Copy;

   type Display_Listener_Ptr is access all Display_Listener_T;

   function Display_Add_Listener
     (Display  : Display_Ptr;
      Listener : Display_Listener_Ptr;
      Data     : Void_Ptr) return Interfaces.C.int;

   procedure Display_Set_User_Data
     (Display : Display_Ptr;
      Data    : Void_Ptr);

   function Display_Get_User_Data (Display : Display_Ptr) return Void_Ptr;

   function Display_Get_Version (Display : Display_Ptr) return Unsigned_32;

   procedure Display_Destroy (Display : Display_Ptr);

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
   function Display_Sync (Display : Display_Ptr) return Callback_Ptr;

   --  This request creates a registry object that allows the client
   --  to list and bind the global objects available from the
   --  compositor.
   --
   --  It should be noted that the server side resources consumed in
   --  response to a get_registry request can only be released when the
   --  client disconnects, not when the client side proxy is destroyed.
   --  Therefore, clients should invoke get_registry as infrequently as
   --  possible to avoid wasting memory.
   function Display_Get_Registry (Display : Display_Ptr) return Registry_Ptr;

   type Registry_Global_Subprogram_Ptr is access procedure
     (Data        : Void_Ptr;
      Registry    : Registry_Ptr;
      Name        : Unsigned_32;
      Interface_V : chars_ptr;
      Version     : Unsigned_32)
   with Convention => C;

   type Registry_Global_Remove_Subprogram_Ptr is access procedure
     (Data     : Void_Ptr;
      Registry : Registry_Ptr;
      Name     : Unsigned_32)
   with Convention => C;

   type Registry_Listener_T is record
      Global        : Registry_Global_Subprogram_Ptr;
      Global_Remove : Registry_Global_Remove_Subprogram_Ptr;
   end record
     with Convention => C_Pass_By_Copy;

   type Registry_Listener_Ptr is access all Registry_Listener_T;

   function Registry_Add_Listener
     (Registry : Registry_Ptr;
      Listener : Registry_Listener_Ptr;
      Data     : Void_Ptr) return Interfaces.C.int;

   procedure Registry_Set_User_Data
     (Registry : Registry_Ptr;
      Data     : Void_Ptr);

   function Registry_Get_User_Data (Registry : Registry_Ptr) return Void_Ptr;

   function Registry_Get_Version (Registry : Registry_Ptr) return Unsigned_32;

   procedure Registry_Destroy (Registry : Registry_Ptr);

   --  Binds a new, client-created object to the server using the
   --  specified name as the identifier.
   function Registry_Bind
     (Registry    : Registry_Ptr;
      Name        : Unsigned_32;
      Interface_V : Interface_Ptr;
      New_Id      : Unsigned_32) return Proxy_Ptr;

   type Callback_Done_Subprogram_Ptr is access procedure
     (Data          : Void_Ptr;
      Callback      : Callback_Ptr;
      Callback_Data : Unsigned_32)
   with Convention => C;

   type Callback_Listener_T is record
      Done : Callback_Done_Subprogram_Ptr;
   end record
     with Convention => C_Pass_By_Copy;

   type Callback_Listener_Ptr is access all Callback_Listener_T;

   function Callback_Add_Listener
     (Callback : Callback_Ptr;
      Listener : Callback_Listener_Ptr;
      Data     : Void_Ptr) return Interfaces.C.int;

   procedure Callback_Set_User_Data
     (Callback : Callback_Ptr;
      Data     : Void_Ptr);

   function Callback_Get_User_Data (Callback : Callback_Ptr) return Void_Ptr;

   function Callback_Get_Version (Callback : Callback_Ptr) return Unsigned_32;

   procedure Callback_Destroy (Callback : Callback_Ptr);

   procedure Compositor_Set_User_Data
     (Compositor : Compositor_Ptr;
      Data       : Void_Ptr);

   function Compositor_Get_User_Data (Compositor : Compositor_Ptr) return Void_Ptr;

   function Compositor_Get_Version (Compositor : Compositor_Ptr) return Unsigned_32;

   procedure Compositor_Destroy (Compositor : Compositor_Ptr);

   --  Ask the compositor to create a new surface.
   function Compositor_Create_Surface (Compositor : Compositor_Ptr) return Surface_Ptr;

   --  Ask the compositor to create a new region.
   function Compositor_Create_Region (Compositor : Compositor_Ptr) return Region_Ptr;

   procedure Shm_Pool_Set_User_Data
     (Shm_Pool : Shm_Pool_Ptr;
      Data     : Void_Ptr);

   function Shm_Pool_Get_User_Data (Shm_Pool : Shm_Pool_Ptr) return Void_Ptr;

   function Shm_Pool_Get_Version (Shm_Pool : Shm_Pool_Ptr) return Unsigned_32;

   procedure Shm_Pool_Destroy (Shm_Pool : Shm_Pool_Ptr);

   --  Create a wl_buffer object from the pool.
   --
   --  The buffer is created offset bytes into the pool and has
   --  width and height as specified.  The stride argument specifies
   --  the number of bytes from the beginning of one row to the beginning
   --  of the next.  The format is the pixel format of the buffer and
   --  must be one of those advertised through the wl_shm.format event.
   --
   --  A buffer will keep a reference to the pool it was created from
   --  so it is valid to destroy the pool immediately after creating
   --  a buffer from it.
   function Shm_Pool_Create_Buffer
     (Shm_Pool : Shm_Pool_Ptr;
      Offset   : Integer;
      Width    : Integer;
      Height   : Integer;
      Stride   : Integer;
      Format   : Unsigned_32) return Buffer_Ptr;

   --  This request will cause the server to remap the backing memory
   --  for the pool from the file descriptor passed when the pool was
   --  created, but using the new size.  This request can only be
   --  used to make the pool bigger.
   procedure Shm_Pool_Resize
     (Shm_Pool : Shm_Pool_Ptr;
      Size     : Integer);

   type Shm_Format_Subprogram_Ptr is access procedure
     (Data   : Void_Ptr;
      Shm    : Shm_Ptr;
      Format : Unsigned_32)
   with Convention => C;

   type Shm_Listener_T is record
      Format : Shm_Format_Subprogram_Ptr;
   end record
     with Convention => C_Pass_By_Copy;

   type Shm_Listener_Ptr is access all Shm_Listener_T;

   function Shm_Add_Listener
     (Shm      : Shm_Ptr;
      Listener : Shm_Listener_Ptr;
      Data     : Void_Ptr) return Interfaces.C.int;

   procedure Shm_Set_User_Data
     (Shm  : Shm_Ptr;
      Data : Void_Ptr);

   function Shm_Get_User_Data (Shm : Shm_Ptr) return Void_Ptr;

   function Shm_Get_Version (Shm : Shm_Ptr) return Unsigned_32;

   procedure Shm_Destroy (Shm : Shm_Ptr);

   --  Create a new wl_shm_pool object.
   --
   --  The pool can be used to create shared memory based buffer
   --  objects.  The server will mmap size bytes of the passed file
   --  descriptor, to use as backing memory for the pool.
   function Shm_Create_Pool
     (Shm  : Shm_Ptr;
      Fd   : Integer;
      Size : Integer) return Shm_Pool_Ptr;

   type Buffer_Release_Subprogram_Ptr is access procedure
     (Data   : Void_Ptr;
      Buffer : Buffer_Ptr)
   with Convention => C;

   type Buffer_Listener_T is record
      Release : Buffer_Release_Subprogram_Ptr;
   end record
     with Convention => C_Pass_By_Copy;

   type Buffer_Listener_Ptr is access all Buffer_Listener_T;

   function Buffer_Add_Listener
     (Buffer   : Buffer_Ptr;
      Listener : Buffer_Listener_Ptr;
      Data     : Void_Ptr) return Interfaces.C.int;

   procedure Buffer_Set_User_Data
     (Buffer : Buffer_Ptr;
      Data   : Void_Ptr);

   function Buffer_Get_User_Data (Buffer : Buffer_Ptr) return Void_Ptr;

   function Buffer_Get_Version (Buffer : Buffer_Ptr) return Unsigned_32;

   procedure Buffer_Destroy (Buffer : Buffer_Ptr);

   type Data_Offer_Offer_Subprogram_Ptr is access procedure
     (Data       : Void_Ptr;
      Data_Offer : Data_Offer_Ptr;
      Mime_Type  : chars_ptr)
   with Convention => C;

   type Data_Offer_Source_Actions_Subprogram_Ptr is access procedure
     (Data           : Void_Ptr;
      Data_Offer     : Data_Offer_Ptr;
      Source_Actions : Unsigned_32)
   with Convention => C;

   type Data_Offer_Action_Subprogram_Ptr is access procedure
     (Data       : Void_Ptr;
      Data_Offer : Data_Offer_Ptr;
      Dnd_Action : Unsigned_32)
   with Convention => C;

   type Data_Offer_Listener_T is record
      Offer          : Data_Offer_Offer_Subprogram_Ptr;
      Source_Actions : Data_Offer_Source_Actions_Subprogram_Ptr;
      Action         : Data_Offer_Action_Subprogram_Ptr;
   end record
     with Convention => C_Pass_By_Copy;

   type Data_Offer_Listener_Ptr is access all Data_Offer_Listener_T;

   function Data_Offer_Add_Listener
     (Data_Offer : Data_Offer_Ptr;
      Listener   : Data_Offer_Listener_Ptr;
      Data       : Void_Ptr) return Interfaces.C.int;

   procedure Data_Offer_Set_User_Data
     (Data_Offer : Data_Offer_Ptr;
      Data       : Void_Ptr);

   function Data_Offer_Get_User_Data (Data_Offer : Data_Offer_Ptr) return Void_Ptr;

   function Data_Offer_Get_Version (Data_Offer : Data_Offer_Ptr) return Unsigned_32;

   procedure Data_Offer_Destroy (Data_Offer : Data_Offer_Ptr);

   --  Indicate that the client can accept the given mime type, or
   --  NULL for not accepted.
   --
   --  For objects of version 2 or older, this request is used by the
   --  client to give feedback whether the client can receive the given
   --  mime type, or NULL if none is accepted; the feedback does not
   --  determine whether the drag-and-drop operation succeeds or not.
   --
   --  For objects of version 3 or newer, this request determines the
   --  final result of the drag-and-drop operation. If the end result
   --  is that no mime types were accepted, the drag-and-drop operation
   --  will be cancelled and the corresponding drag source will receive
   --  wl_data_source.cancelled. Clients may still use this event in
   --  conjunction with wl_data_source.action for feedback.
   procedure Data_Offer_Accept
     (Data_Offer : Data_Offer_Ptr;
      Serial     : Unsigned_32;
      Mime_Type  : chars_ptr);

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
   procedure Data_Offer_Receive
     (Data_Offer : Data_Offer_Ptr;
      Mime_Type  : C_String;
      Fd         : Integer);

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
   procedure Data_Offer_Finish (Data_Offer : Data_Offer_Ptr);

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
   procedure Data_Offer_Set_Actions
     (Data_Offer       : Data_Offer_Ptr;
      Dnd_Actions      : Unsigned_32;
      Preferred_Action : Unsigned_32);

   type Data_Source_Target_Subprogram_Ptr is access procedure
     (Data        : Void_Ptr;
      Data_Source : Data_Source_Ptr;
      Mime_Type   : chars_ptr)
   with Convention => C;

   type Data_Source_Send_Subprogram_Ptr is access procedure
     (Data        : Void_Ptr;
      Data_Source : Data_Source_Ptr;
      Mime_Type   : chars_ptr;
      Fd          : Integer)
   with Convention => C;

   type Data_Source_Cancelled_Subprogram_Ptr is access procedure
     (Data        : Void_Ptr;
      Data_Source : Data_Source_Ptr)
   with Convention => C;

   type Data_Source_Dnd_Drop_Performed_Subprogram_Ptr is access procedure
     (Data        : Void_Ptr;
      Data_Source : Data_Source_Ptr)
   with Convention => C;

   type Data_Source_Dnd_Finished_Subprogram_Ptr is access procedure
     (Data        : Void_Ptr;
      Data_Source : Data_Source_Ptr)
   with Convention => C;

   type Data_Source_Action_Subprogram_Ptr is access procedure
     (Data        : Void_Ptr;
      Data_Source : Data_Source_Ptr;
      Dnd_Action  : Unsigned_32)
   with Convention => C;

   type Data_Source_Listener_T is record
      Target             : Data_Source_Target_Subprogram_Ptr;
      Send               : Data_Source_Send_Subprogram_Ptr;
      Cancelled          : Data_Source_Cancelled_Subprogram_Ptr;
      Dnd_Drop_Performed : Data_Source_Dnd_Drop_Performed_Subprogram_Ptr;
      Dnd_Finished       : Data_Source_Dnd_Finished_Subprogram_Ptr;
      Action             : Data_Source_Action_Subprogram_Ptr;
   end record
     with Convention => C_Pass_By_Copy;

   type Data_Source_Listener_Ptr is access all Data_Source_Listener_T;

   function Data_Source_Add_Listener
     (Data_Source : Data_Source_Ptr;
      Listener    : Data_Source_Listener_Ptr;
      Data        : Void_Ptr) return Interfaces.C.int;

   procedure Data_Source_Set_User_Data
     (Data_Source : Data_Source_Ptr;
      Data        : Void_Ptr);

   function Data_Source_Get_User_Data (Data_Source : Data_Source_Ptr) return Void_Ptr;

   function Data_Source_Get_Version (Data_Source : Data_Source_Ptr) return Unsigned_32;

   procedure Data_Source_Destroy (Data_Source : Data_Source_Ptr);

   --  This request adds a mime type to the set of mime types
   --  advertised to targets.  Can be called several times to offer
   --  multiple types.
   procedure Data_Source_Offer
     (Data_Source : Data_Source_Ptr;
      Mime_Type   : chars_ptr);

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
   procedure Data_Source_Set_Actions
     (Data_Source : Data_Source_Ptr;
      Dnd_Actions : Unsigned_32);

   type Data_Device_Data_Offer_Subprogram_Ptr is access procedure
     (Data        : Void_Ptr;
      Data_Device : Data_Device_Ptr;
      Id          : Unsigned_32)
   with Convention => C;

   type Data_Device_Enter_Subprogram_Ptr is access procedure
     (Data        : Void_Ptr;
      Data_Device : Data_Device_Ptr;
      Serial      : Unsigned_32;
      Surface     : Surface_Ptr;
      X           : Fixed;
      Y           : Fixed;
      Id          : Data_Offer_Ptr)
   with Convention => C;

   type Data_Device_Leave_Subprogram_Ptr is access procedure
     (Data        : Void_Ptr;
      Data_Device : Data_Device_Ptr)
   with Convention => C;

   type Data_Device_Motion_Subprogram_Ptr is access procedure
     (Data        : Void_Ptr;
      Data_Device : Data_Device_Ptr;
      Time        : Unsigned_32;
      X           : Fixed;
      Y           : Fixed)
   with Convention => C;

   type Data_Device_Drop_Subprogram_Ptr is access procedure
     (Data        : Void_Ptr;
      Data_Device : Data_Device_Ptr)
   with Convention => C;

   type Data_Device_Selection_Subprogram_Ptr is access procedure
     (Data        : Void_Ptr;
      Data_Device : Data_Device_Ptr;
      Id          : Data_Offer_Ptr)
   with Convention => C;

   type Data_Device_Listener_T is record
      Data_Offer : Data_Device_Data_Offer_Subprogram_Ptr;
      Enter      : Data_Device_Enter_Subprogram_Ptr;
      Leave      : Data_Device_Leave_Subprogram_Ptr;
      Motion     : Data_Device_Motion_Subprogram_Ptr;
      Drop       : Data_Device_Drop_Subprogram_Ptr;
      Selection  : Data_Device_Selection_Subprogram_Ptr;
   end record
     with Convention => C_Pass_By_Copy;

   type Data_Device_Listener_Ptr is access all Data_Device_Listener_T;

   function Data_Device_Add_Listener
     (Data_Device : Data_Device_Ptr;
      Listener    : Data_Device_Listener_Ptr;
      Data        : Void_Ptr) return Interfaces.C.int;

   procedure Data_Device_Set_User_Data
     (Data_Device : Data_Device_Ptr;
      Data        : Void_Ptr);

   function Data_Device_Get_User_Data (Data_Device : Data_Device_Ptr) return Void_Ptr;

   function Data_Device_Get_Version (Data_Device : Data_Device_Ptr) return Unsigned_32;

   procedure Data_Device_Destroy (Data_Device : Data_Device_Ptr);

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
   procedure Data_Device_Start_Drag
     (Data_Device : Data_Device_Ptr;
      Source      : Data_Source_Ptr;
      Origin      : Surface_Ptr;
      Icon        : Surface_Ptr;
      Serial      : Unsigned_32);

   --  This request asks the compositor to set the selection
   --  to the data from the source on behalf of the client.
   --
   --  To unset the selection, set the source to NULL.
   procedure Data_Device_Set_Selection
     (Data_Device : Data_Device_Ptr;
      Source      : Data_Source_Ptr;
      Serial      : Unsigned_32);

   --  This request destroys the data device.
   procedure Data_Device_Release (Data_Device : Data_Device_Ptr);

   procedure Data_Device_Manager_Set_User_Data
     (Data_Device_Manager : Data_Device_Manager_Ptr;
      Data                : Void_Ptr);

   function Data_Device_Manager_Get_User_Data
     (Data_Device_Manager : Data_Device_Manager_Ptr) return Void_Ptr;

   function Data_Device_Manager_Get_Version
     (Data_Device_Manager : Data_Device_Manager_Ptr) return Unsigned_32;

   procedure Data_Device_Manager_Destroy (Data_Device_Manager : Data_Device_Manager_Ptr);

   --  Create a new data source.
   function Data_Device_Manager_Create_Data_Source
     (Data_Device_Manager : Data_Device_Manager_Ptr) return Data_Source_Ptr;

   --  Create a new data device for a given seat.
   function Data_Device_Manager_Get_Data_Device
     (Data_Device_Manager : Data_Device_Manager_Ptr;
      Seat                : Seat_Ptr) return Data_Device_Ptr;

   procedure Shell_Set_User_Data
     (Shell : Shell_Ptr;
      Data  : Void_Ptr);

   function Shell_Get_User_Data (Shell : Shell_Ptr) return Void_Ptr;

   function Shell_Get_Version (Shell : Shell_Ptr) return Unsigned_32;

   procedure Shell_Destroy (Shell : Shell_Ptr);

   --  Create a shell surface for an existing surface. This gives
   --  the wl_surface the role of a shell surface. If the wl_surface
   --  already has another role, it raises a protocol error.
   --
   --  Only one shell surface can be associated with a given surface.
   function Shell_Get_Shell_Surface
     (Shell   : Shell_Ptr;
      Surface : Surface_Ptr) return Shell_Surface_Ptr;

   type Shell_Surface_Ping_Subprogram_Ptr is access procedure
     (Data          : Void_Ptr;
      Shell_Surface : Shell_Surface_Ptr;
      Serial        : Unsigned_32)
   with Convention => C;

   type Shell_Surface_Configure_Subprogram_Ptr is access procedure
     (Data          : Void_Ptr;
      Shell_Surface : Shell_Surface_Ptr;
      Edges         : Unsigned_32;
      Width         : Integer;
      Height        : Integer)
   with Convention => C;

   type Shell_Surface_Popup_Done_Subprogram_Ptr is access procedure
     (Data          : Void_Ptr;
      Shell_Surface : Shell_Surface_Ptr)
   with Convention => C;

   type Shell_Surface_Listener_T is record
      Ping       : Shell_Surface_Ping_Subprogram_Ptr;
      Configure  : Shell_Surface_Configure_Subprogram_Ptr;
      Popup_Done : Shell_Surface_Popup_Done_Subprogram_Ptr;
   end record
     with Convention => C_Pass_By_Copy;

   type Shell_Surface_Listener_Ptr is access all Shell_Surface_Listener_T;

   function Shell_Surface_Add_Listener
     (Shell_Surface : Shell_Surface_Ptr;
      Listener      : Shell_Surface_Listener_Ptr;
      Data          : Void_Ptr) return Interfaces.C.int;

   procedure Shell_Surface_Set_User_Data
     (Shell_Surface : Shell_Surface_Ptr;
      Data          : Void_Ptr);

   function Shell_Surface_Get_User_Data (Shell_Surface : Shell_Surface_Ptr) return Void_Ptr;

   function Shell_Surface_Get_Version (Shell_Surface : Shell_Surface_Ptr) return Unsigned_32;

   procedure Shell_Surface_Destroy (Shell_Surface : Shell_Surface_Ptr);

   --  A client must respond to a ping event with a pong request or
   --  the client may be deemed unresponsive.
   procedure Shell_Surface_Pong
     (Shell_Surface : Shell_Surface_Ptr;
      Serial        : Unsigned_32);

   --  Start a pointer-driven move of the surface.
   --
   --  This request must be used in response to a button press event.
   --  The server may ignore move requests depending on the state of
   --  the surface (e.g. fullscreen or maximized).
   procedure Shell_Surface_Move
     (Shell_Surface : Shell_Surface_Ptr;
      Seat          : Seat_Ptr;
      Serial        : Unsigned_32);

   --  Start a pointer-driven resizing of the surface.
   --
   --  This request must be used in response to a button press event.
   --  The server may ignore resize requests depending on the state of
   --  the surface (e.g. fullscreen or maximized).
   procedure Shell_Surface_Resize
     (Shell_Surface : Shell_Surface_Ptr;
      Seat          : Seat_Ptr;
      Serial        : Unsigned_32;
      Edges         : Unsigned_32);

   --  Map the surface as a toplevel surface.
   --
   --  A toplevel surface is not fullscreen, maximized or transient.
   procedure Shell_Surface_Set_Toplevel (Shell_Surface : Shell_Surface_Ptr);

   --  Map the surface relative to an existing surface.
   --
   --  The x and y arguments specify the location of the upper left
   --  corner of the surface relative to the upper left corner of the
   --  parent surface, in surface-local coordinates.
   --
   --  The flags argument controls details of the transient behaviour.
   procedure Shell_Surface_Set_Transient
     (Shell_Surface : Shell_Surface_Ptr;
      Parent        : Surface_Ptr;
      X             : Integer;
      Y             : Integer;
      Flags         : Unsigned_32);

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
   procedure Shell_Surface_Set_Fullscreen
     (Shell_Surface : Shell_Surface_Ptr;
      Method        : Unsigned_32;
      Framerate     : Unsigned_32;
      Output        : Output_Ptr);

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
   procedure Shell_Surface_Set_Popup
     (Shell_Surface : Shell_Surface_Ptr;
      Seat          : Seat_Ptr;
      Serial        : Unsigned_32;
      Parent        : Surface_Ptr;
      X             : Integer;
      Y             : Integer;
      Flags         : Unsigned_32);

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
   procedure Shell_Surface_Set_Maximized
     (Shell_Surface : Shell_Surface_Ptr;
      Output        : Output_Ptr);

   --  Set a short title for the surface.
   --
   --  This string may be used to identify the surface in a task bar,
   --  window list, or other user interface elements provided by the
   --  compositor.
   --
   --  The string must be encoded in UTF-8.
   procedure Shell_Surface_Set_Title
     (Shell_Surface : Shell_Surface_Ptr;
      Title         : chars_ptr);

   --  Set a class for the surface.
   --
   --  The surface class identifies the general class of applications
   --  to which the surface belongs. A common convention is to use the
   --  file name (or the full path if it is a non-standard location) of
   --  the application's .desktop file as the class.
   procedure Shell_Surface_Set_Class
     (Shell_Surface : Shell_Surface_Ptr;
      Class_V       : chars_ptr);

   type Surface_Enter_Subprogram_Ptr is access procedure
     (Data    : Void_Ptr;
      Surface : Surface_Ptr;
      Output  : Output_Ptr)
   with Convention => C;

   type Surface_Leave_Subprogram_Ptr is access procedure
     (Data    : Void_Ptr;
      Surface : Surface_Ptr;
      Output  : Output_Ptr)
   with Convention => C;

   type Surface_Listener_T is record
      Enter : Surface_Enter_Subprogram_Ptr;
      Leave : Surface_Leave_Subprogram_Ptr;
   end record
     with Convention => C_Pass_By_Copy;

   type Surface_Listener_Ptr is access all Surface_Listener_T;

   function Surface_Add_Listener
     (Surface  : Surface_Ptr;
      Listener : Surface_Listener_Ptr;
      Data     : Void_Ptr) return Interfaces.C.int;

   procedure Surface_Set_User_Data
     (Surface : Surface_Ptr;
      Data    : Void_Ptr);

   function Surface_Get_User_Data (Surface : Surface_Ptr) return Void_Ptr;

   function Surface_Get_Version (Surface : Surface_Ptr) return Unsigned_32;

   procedure Surface_Destroy (Surface : Surface_Ptr);

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
   procedure Surface_Attach
     (Surface : Surface_Ptr;
      Buffer  : Buffer_Ptr;
      X       : Integer;
      Y       : Integer);

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
   procedure Surface_Damage
     (Surface : Surface_Ptr;
      X       : Integer;
      Y       : Integer;
      Width   : Integer;
      Height  : Integer);

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
   function Surface_Frame (Surface : Surface_Ptr) return Callback_Ptr;

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
   procedure Surface_Set_Opaque_Region
     (Surface : Surface_Ptr;
      Region  : Region_Ptr);

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
   procedure Surface_Set_Input_Region
     (Surface : Surface_Ptr;
      Region  : Region_Ptr);

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
   procedure Surface_Commit (Surface : Surface_Ptr);

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
   procedure Surface_Set_Buffer_Transform
     (Surface   : Surface_Ptr;
      Transform : Integer);

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
   procedure Surface_Set_Buffer_Scale
     (Surface : Surface_Ptr;
      Scale   : Integer);

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
   procedure Surface_Damage_Buffer
     (Surface : Surface_Ptr;
      X       : Integer;
      Y       : Integer;
      Width   : Integer;
      Height  : Integer);

   type Seat_Capabilities_Subprogram_Ptr is access procedure
     (Data         : Void_Ptr;
      Seat         : Seat_Ptr;
      Capabilities : Unsigned_32)
   with Convention => C;

   type Seat_Name_Subprogram_Ptr is access procedure
     (Data : Void_Ptr;
      Seat : Seat_Ptr;
      Name : chars_ptr)
   with Convention => C;

   type Seat_Listener_T is record
      Capabilities : Seat_Capabilities_Subprogram_Ptr;
      Name         : Seat_Name_Subprogram_Ptr;
   end record
     with Convention => C_Pass_By_Copy;

   type Seat_Listener_Ptr is access all Seat_Listener_T;

   function Seat_Add_Listener
     (Seat     : Seat_Ptr;
      Listener : Seat_Listener_Ptr;
      Data     : Void_Ptr) return Interfaces.C.int;

   procedure Seat_Set_User_Data
     (Seat : Seat_Ptr;
      Data : Void_Ptr);

   function Seat_Get_User_Data (Seat : Seat_Ptr) return Void_Ptr;

   function Seat_Get_Version (Seat : Seat_Ptr) return Unsigned_32;

   procedure Seat_Destroy (Seat : Seat_Ptr);

   --  The ID provided will be initialized to the wl_pointer interface
   --  for this seat.
   --
   --  This request only takes effect if the seat has the pointer
   --  capability, or has had the pointer capability in the past.
   --  It is a protocol violation to issue this request on a seat that has
   --  never had the pointer capability.
   function Seat_Get_Pointer (Seat : Seat_Ptr) return Pointer_Ptr;

   --  The ID provided will be initialized to the wl_keyboard interface
   --  for this seat.
   --
   --  This request only takes effect if the seat has the keyboard
   --  capability, or has had the keyboard capability in the past.
   --  It is a protocol violation to issue this request on a seat that has
   --  never had the keyboard capability.
   function Seat_Get_Keyboard (Seat : Seat_Ptr) return Keyboard_Ptr;

   --  The ID provided will be initialized to the wl_touch interface
   --  for this seat.
   --
   --  This request only takes effect if the seat has the touch
   --  capability, or has had the touch capability in the past.
   --  It is a protocol violation to issue this request on a seat that has
   --  never had the touch capability.
   function Seat_Get_Touch (Seat : Seat_Ptr) return Touch_Ptr;

   --  Using this request a client can tell the server that it is not going to
   --  use the seat object anymore.
   procedure Seat_Release (Seat : Seat_Ptr);

   type Pointer_Enter_Subprogram_Ptr is access procedure
     (Data      : Void_Ptr;
      Pointer   : Pointer_Ptr;
      Serial    : Unsigned_32;
      Surface   : Surface_Ptr;
      Surface_X : Fixed;
      Surface_Y : Fixed)
   with Convention => C;

   type Pointer_Leave_Subprogram_Ptr is access procedure
     (Data    : Void_Ptr;
      Pointer : Pointer_Ptr;
      Serial  : Unsigned_32;
      Surface : Surface_Ptr)
   with Convention => C;

   type Pointer_Motion_Subprogram_Ptr is access procedure
     (Data      : Void_Ptr;
      Pointer   : Pointer_Ptr;
      Time      : Unsigned_32;
      Surface_X : Fixed;
      Surface_Y : Fixed)
   with Convention => C;

   type Pointer_Button_Subprogram_Ptr is access procedure
     (Data    : Void_Ptr;
      Pointer : Pointer_Ptr;
      Serial  : Unsigned_32;
      Time    : Unsigned_32;
      Button  : Unsigned_32;
      State   : Unsigned_32)
   with Convention => C;

   type Pointer_Axis_Subprogram_Ptr is access procedure
     (Data    : Void_Ptr;
      Pointer : Pointer_Ptr;
      Time    : Unsigned_32;
      Axis    : Unsigned_32;
      Value   : Fixed)
   with Convention => C;

   type Pointer_Frame_Subprogram_Ptr is access procedure
     (Data    : Void_Ptr;
      Pointer : Pointer_Ptr)
   with Convention => C;

   type Pointer_Axis_Source_Subprogram_Ptr is access procedure
     (Data        : Void_Ptr;
      Pointer     : Pointer_Ptr;
      Axis_Source : Unsigned_32)
   with Convention => C;

   type Pointer_Axis_Stop_Subprogram_Ptr is access procedure
     (Data    : Void_Ptr;
      Pointer : Pointer_Ptr;
      Time    : Unsigned_32;
      Axis    : Unsigned_32)
   with Convention => C;

   type Pointer_Axis_Discrete_Subprogram_Ptr is access procedure
     (Data     : Void_Ptr;
      Pointer  : Pointer_Ptr;
      Axis     : Unsigned_32;
      Discrete : Integer)
   with Convention => C;

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
   end record
     with Convention => C_Pass_By_Copy;

   type Pointer_Listener_Ptr is access all Pointer_Listener_T;

   function Pointer_Add_Listener
     (Pointer  : Pointer_Ptr;
      Listener : Pointer_Listener_Ptr;
      Data     : Void_Ptr) return Interfaces.C.int;

   procedure Pointer_Set_User_Data
     (Pointer : Pointer_Ptr;
      Data    : Void_Ptr);

   function Pointer_Get_User_Data (Pointer : Pointer_Ptr) return Void_Ptr;

   function Pointer_Get_Version (Pointer : Pointer_Ptr) return Unsigned_32;

   procedure Pointer_Destroy (Pointer : Pointer_Ptr);

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
   procedure Pointer_Set_Cursor
     (Pointer   : Pointer_Ptr;
      Serial    : Unsigned_32;
      Surface   : Surface_Ptr;
      Hotspot_X : Integer;
      Hotspot_Y : Integer);

   --  Using this request a client can tell the server that it is not going to
   --  use the pointer object anymore.
   --
   --  This request destroys the pointer proxy object, so clients must not call
   --  wl_pointer_destroy() after using this request.
   procedure Pointer_Release (Pointer : Pointer_Ptr);

   type Keyboard_Keymap_Subprogram_Ptr is access procedure
     (Data     : Void_Ptr;
      Keyboard : Keyboard_Ptr;
      Format   : Unsigned_32;
      Fd       : Integer;
      Size     : Unsigned_32)
   with Convention => C;

   type Keyboard_Enter_Subprogram_Ptr is access procedure
     (Data     : Void_Ptr;
      Keyboard : Keyboard_Ptr;
      Serial   : Unsigned_32;
      Surface  : Surface_Ptr;
      Keys     : Wayland_Array_T)
   with Convention => C;

   type Keyboard_Leave_Subprogram_Ptr is access procedure
     (Data     : Void_Ptr;
      Keyboard : Keyboard_Ptr;
      Serial   : Unsigned_32;
      Surface  : Surface_Ptr)
   with Convention => C;

   type Keyboard_Key_Subprogram_Ptr is access procedure
     (Data     : Void_Ptr;
      Keyboard : Keyboard_Ptr;
      Serial   : Unsigned_32;
      Time     : Unsigned_32;
      Key      : Unsigned_32;
      State    : Unsigned_32)
   with Convention => C;

   type Keyboard_Modifiers_Subprogram_Ptr is access procedure
     (Data           : Void_Ptr;
      Keyboard       : Keyboard_Ptr;
      Serial         : Unsigned_32;
      Mods_Depressed : Unsigned_32;
      Mods_Latched   : Unsigned_32;
      Mods_Locked    : Unsigned_32;
      Group          : Unsigned_32)
   with Convention => C;

   type Keyboard_Repeat_Info_Subprogram_Ptr is access procedure
     (Data     : Void_Ptr;
      Keyboard : Keyboard_Ptr;
      Rate     : Integer;
      Delay_V  : Integer)
   with Convention => C;

   type Keyboard_Listener_T is record
      Keymap      : Keyboard_Keymap_Subprogram_Ptr;
      Enter       : Keyboard_Enter_Subprogram_Ptr;
      Leave       : Keyboard_Leave_Subprogram_Ptr;
      Key         : Keyboard_Key_Subprogram_Ptr;
      Modifiers   : Keyboard_Modifiers_Subprogram_Ptr;
      Repeat_Info : Keyboard_Repeat_Info_Subprogram_Ptr;
   end record
     with Convention => C_Pass_By_Copy;

   type Keyboard_Listener_Ptr is access all Keyboard_Listener_T;

   function Keyboard_Add_Listener
     (Keyboard : Keyboard_Ptr;
      Listener : Keyboard_Listener_Ptr;
      Data     : Void_Ptr) return Interfaces.C.int;

   procedure Keyboard_Set_User_Data
     (Keyboard : Keyboard_Ptr;
      Data     : Void_Ptr);

   function Keyboard_Get_User_Data (Keyboard : Keyboard_Ptr) return Void_Ptr;

   function Keyboard_Get_Version (Keyboard : Keyboard_Ptr) return Unsigned_32;

   procedure Keyboard_Destroy (Keyboard : Keyboard_Ptr);

   procedure Keyboard_Release (Keyboard : Keyboard_Ptr);

   type Touch_Down_Subprogram_Ptr is access procedure
     (Data    : Void_Ptr;
      Touch   : Touch_Ptr;
      Serial  : Unsigned_32;
      Time    : Unsigned_32;
      Surface : Surface_Ptr;
      Id      : Integer;
      X       : Fixed;
      Y       : Fixed)
   with Convention => C;

   type Touch_Up_Subprogram_Ptr is access procedure
     (Data   : Void_Ptr;
      Touch  : Touch_Ptr;
      Serial : Unsigned_32;
      Time   : Unsigned_32;
      Id     : Integer)
   with Convention => C;

   type Touch_Motion_Subprogram_Ptr is access procedure
     (Data  : Void_Ptr;
      Touch : Touch_Ptr;
      Time  : Unsigned_32;
      Id    : Integer;
      X     : Fixed;
      Y     : Fixed)
   with Convention => C;

   type Touch_Frame_Subprogram_Ptr is access procedure
     (Data  : Void_Ptr;
      Touch : Touch_Ptr)
   with Convention => C;

   type Touch_Cancel_Subprogram_Ptr is access procedure
     (Data  : Void_Ptr;
      Touch : Touch_Ptr)
   with Convention => C;

   type Touch_Shape_Subprogram_Ptr is access procedure
     (Data  : Void_Ptr;
      Touch : Touch_Ptr;
      Id    : Integer;
      Major : Fixed;
      Minor : Fixed)
   with Convention => C;

   type Touch_Orientation_Subprogram_Ptr is access procedure
     (Data        : Void_Ptr;
      Touch       : Touch_Ptr;
      Id          : Integer;
      Orientation : Fixed)
   with Convention => C;

   type Touch_Listener_T is record
      Down        : Touch_Down_Subprogram_Ptr;
      Up          : Touch_Up_Subprogram_Ptr;
      Motion      : Touch_Motion_Subprogram_Ptr;
      Frame       : Touch_Frame_Subprogram_Ptr;
      Cancel      : Touch_Cancel_Subprogram_Ptr;
      Shape       : Touch_Shape_Subprogram_Ptr;
      Orientation : Touch_Orientation_Subprogram_Ptr;
   end record
     with Convention => C_Pass_By_Copy;

   type Touch_Listener_Ptr is access all Touch_Listener_T;

   function Touch_Add_Listener
     (Touch    : Touch_Ptr;
      Listener : Touch_Listener_Ptr;
      Data     : Void_Ptr) return Interfaces.C.int;

   procedure Touch_Set_User_Data
     (Touch : Touch_Ptr;
      Data  : Void_Ptr);

   function Touch_Get_User_Data (Touch : Touch_Ptr) return Void_Ptr;

   function Touch_Get_Version (Touch : Touch_Ptr) return Unsigned_32;

   procedure Touch_Destroy (Touch : Touch_Ptr);

   procedure Touch_Release (Touch : Touch_Ptr);

   type Output_Geometry_Subprogram_Ptr is access procedure
     (Data            : Void_Ptr;
      Output          : Output_Ptr;
      X               : Integer;
      Y               : Integer;
      Physical_Width  : Integer;
      Physical_Height : Integer;
      Subpixel        : Integer;
      Make            : chars_ptr;
      Model           : chars_ptr;
      Transform       : Integer)
   with Convention => C;

   type Output_Mode_Subprogram_Ptr is access procedure
     (Data    : Void_Ptr;
      Output  : Output_Ptr;
      Flags   : Unsigned_32;
      Width   : Integer;
      Height  : Integer;
      Refresh : Integer)
   with Convention => C;

   type Output_Done_Subprogram_Ptr is access procedure
     (Data   : Void_Ptr;
      Output : Output_Ptr)
   with Convention => C;

   type Output_Scale_Subprogram_Ptr is access procedure
     (Data   : Void_Ptr;
      Output : Output_Ptr;
      Factor : Integer)
   with Convention => C;

   type Output_Listener_T is record
      Geometry : Output_Geometry_Subprogram_Ptr;
      Mode     : Output_Mode_Subprogram_Ptr;
      Done     : Output_Done_Subprogram_Ptr;
      Scale    : Output_Scale_Subprogram_Ptr;
   end record
     with Convention => C_Pass_By_Copy;

   type Output_Listener_Ptr is access all Output_Listener_T;

   function Output_Add_Listener
     (Output   : Output_Ptr;
      Listener : Output_Listener_Ptr;
      Data     : Void_Ptr) return Interfaces.C.int;

   procedure Output_Set_User_Data
     (Output : Output_Ptr;
      Data   : Void_Ptr);

   function Output_Get_User_Data (Output : Output_Ptr) return Void_Ptr;

   function Output_Get_Version (Output : Output_Ptr) return Unsigned_32;

   procedure Output_Destroy (Output : Output_Ptr);

   --  Using this request a client can tell the server that it is not going to
   --  use the output object anymore.
   procedure Output_Release (Output : Output_Ptr);

   procedure Region_Set_User_Data
     (Region : Region_Ptr;
      Data   : Void_Ptr);

   function Region_Get_User_Data (Region : Region_Ptr) return Void_Ptr;

   function Region_Get_Version (Region : Region_Ptr) return Unsigned_32;

   procedure Region_Destroy (Region : Region_Ptr);

   --  Add the specified rectangle to the region.
   procedure Region_Add
     (Region : Region_Ptr;
      X      : Integer;
      Y      : Integer;
      Width  : Integer;
      Height : Integer);

   --  Subtract the specified rectangle from the region.
   procedure Region_Subtract
     (Region : Region_Ptr;
      X      : Integer;
      Y      : Integer;
      Width  : Integer;
      Height : Integer);

   procedure Subcompositor_Set_User_Data
     (Subcompositor : Subcompositor_Ptr;
      Data          : Void_Ptr);

   function Subcompositor_Get_User_Data (Subcompositor : Subcompositor_Ptr) return Void_Ptr;

   function Subcompositor_Get_Version (Subcompositor : Subcompositor_Ptr) return Unsigned_32;

   procedure Subcompositor_Destroy (Subcompositor : Subcompositor_Ptr);

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
   function Subcompositor_Get_Subsurface
     (Subcompositor : Subcompositor_Ptr;
      Surface       : Surface_Ptr;
      Parent        : Surface_Ptr) return Subsurface_Ptr;

   procedure Subsurface_Set_User_Data
     (Subsurface : Subsurface_Ptr;
      Data       : Void_Ptr);

   function Subsurface_Get_User_Data (Subsurface : Subsurface_Ptr) return Void_Ptr;

   function Subsurface_Get_Version (Subsurface : Subsurface_Ptr) return Unsigned_32;

   procedure Subsurface_Destroy (Subsurface : Subsurface_Ptr);

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
   procedure Subsurface_Set_Position
     (Subsurface : Subsurface_Ptr;
      X          : Integer;
      Y          : Integer);

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
   procedure Subsurface_Place_Above
     (Subsurface : Subsurface_Ptr;
      Sibling    : Surface_Ptr);

   --  The sub-surface is placed just below the reference surface.
   --  See wl_subsurface.place_above.
   procedure Subsurface_Place_Below
     (Subsurface : Subsurface_Ptr;
      Sibling    : Surface_Ptr);

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
   procedure Subsurface_Set_Sync (Subsurface : Subsurface_Ptr);

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
   procedure Subsurface_Set_Desync (Subsurface : Subsurface_Ptr);

end Wayland.Client.Thin;
