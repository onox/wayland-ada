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

   Failed_To_Connect_Exception : exception;

   type Display_T is limited private;

   function Display_Connect (Name : Interfaces.C.Strings.char_array_access) return Display_T;




   type Registry_T;

   function Display_Get_Registry (Display : Display_T) return Registry_T;

   type Registry_T is limited private;

   type Registry_Ptr is access all Registry_T;

   type Interface_T is limited private;

   type Interface_Ptr is access all Interface_T;

   type Proxy_T is limited private;

   type Proxy_Ptr is access all Proxy_T;

   function Proxy_Marshal_Constructor (Proxy       : Proxy_Ptr;
                                       Opcode      : Interfaces.Unsigned_32;
                                       Interface_V : Interface_Ptr;
                                       New_Id      : Interfaces.Unsigned_32) return Proxy_Ptr with
     Convention    => C,
     Import        => True,
     External_Name => "wl_proxy_marshal_constructor";

--     type Global_Subprogram_Ptr is access procedure (Data     : Wl.Void_Ptr;
--                                                     Registry : Wl.Registry_Ptr);

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
--
--  static inline int
--  wl_registry_add_listener(struct wl_registry *wl_registry,
--  			 const struct wl_registry_listener *listener, void *data)
--  {
--  	return wl_proxy_add_listener((struct wl_proxy *) wl_registry,
--  				     (void (**)(void)) listener, data);
--  }

private

   type Display_T is new Ada.Finalization.Limited_Controlled with record
      My_Proxy : Proxy_Ptr := null;
   end record;

   overriding
   procedure Finalize (This : in out Display_T);

   type Registry_T is new Ada.Finalization.Limited_Controlled with record
      My_Proxy : Proxy_Ptr := null;
   end record;

   overriding
   procedure Finalize (This : in out Registry_T);

   type Proxy_T is limited null record;

   type Interface_T is limited null record;

end Wl;
