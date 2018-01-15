with Interfaces.C.Strings;
with System;

private with Ada.Finalization;

package Wl is

   pragma Linker_Options ("-lwayland-client");
   -- Added this linker option here to avoid added it to each gpr file that with's
   -- this Ada binding to Wayland. If the wayland client lib changes its name it
   -- means there is only one place one needs to update.

   subtype Void_Ptr is System.Address;

   Default_Display_Name : aliased Interfaces.C.char_array := Interfaces.C.To_C ("wayland-0");

   type Proxy_T is limited private;

   type Proxy_Ptr is access all Proxy_T;

   type Display_Ptr is new Proxy_Ptr;

   function Display_Connect (Name : Interfaces.C.Strings.char_array_access) return Display_Ptr;

   procedure Display_Disconnect (This : in out Display_Ptr);


   type Registry_Ptr is new Proxy_Ptr;

   function Display_Get_Registry (Display : Display_Ptr) return Registry_Ptr;

   procedure Registry_Destroy (Registry : in out Registry_Ptr);

   type Interface_T is limited private;

   type Interface_Ptr is access all Interface_T;

   function Proxy_Marshal_Constructor (Proxy       : Proxy_Ptr;
                                       Opcode      : Interfaces.Unsigned_32;
                                       Interface_V : Interface_Ptr;
                                       New_Id      : Interfaces.Unsigned_32) return Proxy_Ptr with
     Convention    => C,
     Import        => True,
     External_Name => "wl_proxy_marshal_constructor";



--  struct wl_registry_listener {
--  	/**
--  	 * announce global object
--  	 *
--  	 * Notify the client of global objects.
--  	 */
--  	void (*global)(void *data,
--  		       struct wl_registry *wl_registry,
--  		       uint32_t name,
--  		       const char *interface,
--  		       uint32_t version);
--  	/**
--  	 * announce removal of global object
--  	 */
--  	void (*global_remove)(void *data,
--  			      struct wl_registry *wl_registry,
--  			      uint32_t name);
--  };

   type Global_Subprogram_Ptr is access procedure (Data        : Wl.Void_Ptr;
                                                   Registry    : Wl.Registry_Ptr;
                                                   Name        : Interfaces.Unsigned_32;
                                                   Interface_V : Interfaces.C.Strings.chars_ptr;
                                                   Version     : Interfaces.Unsigned_32) with
     Convention => C;

   type Global_Remove_Subprogram_Ptr is access procedure (Data        : Wl.Void_Ptr;
                                                          Registry    : Wl.Registry_Ptr;
                                                          Name        : Interfaces.Unsigned_32) with
     Convention => C;

   type Registry_Listener_T is record
      Global        : Global_Subprogram_Ptr;
      Global_Remove : Global_Remove_Subprogram_Ptr;
   end record with
     Convention => C_Pass_By_Copy;

   type Registry_Listener_Ptr is access all Registry_Listener_T;

   function Display_Dispatch (Display : Display_Ptr) return Interfaces.C.int;  -- /usr/include/wayland-client-core.h:213
   pragma Import (C, Display_Dispatch, "wl_display_dispatch");

   function Display_Roundtrip (Display : Display_Ptr) return Interfaces.C.int;  -- /usr/include/wayland-client-core.h:242
   pragma Import (C, Display_Roundtrip, "wl_display_roundtrip");

   function Registry_Add_Listener (Registry : Registry_Ptr;
                                   Listener : Registry_Listener_Ptr;
                                   Data     : Wl.Void_Ptr) return Interfaces.C.int;

private

   type Proxy_T is limited null record;

   type Interface_T is limited null record;

end Wl;
