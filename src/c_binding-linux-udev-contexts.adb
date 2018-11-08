with C_Binding.Linux.Udev.Monitors;

package body C_Binding.Linux.Udev.Contexts is

--     function Udev_Ref (Udev : Udev_Ptr) return Udev_Ptr;
--     pragma Import (C, Udev_Ref, "udev_ref");
   --  Acquire a udev context object.
   --  Returns the argument that it was passed, unmodified.

   function Udev_Unref (Udev : Udev_Ptr) return Udev_Ptr;
   pragma Import (C, Udev_Unref, "udev_unref");
   --  Release a udev context object.
   --  Always returns null.

   function Udev_New return Udev_Ptr;
   pragma Import (C, Udev_New, "udev_new");
   --  Create a udev context object
   --  On success, returns a pointer to the allocated udev context.
   --  On failure, null is returned.

--     procedure Udev_Set_Log_Fn
--       (Arg1 : System.Address; Arg2 : access procedure
--          (Arg1 : System.Address;
--           Arg2 : Int;
--           Arg3 : Interfaces.C.Strings.Chars_Ptr;
--           Arg4 : Int;
--           Arg5 : Interfaces.C.Strings.Chars_Ptr;
--           Arg6 : Interfaces.C.Strings.Chars_Ptr;
--           Arg7 : access System.Address));
--     pragma Import (C, Udev_Set_Log_Fn, "udev_set_log_fn");
--
--     function Udev_Get_Log_Priority (Arg1 : System.Address) return Int;
--     pragma Import (C, Udev_Get_Log_Priority, "udev_get_log_priority");
--
--     procedure Udev_Set_Log_Priority (Arg1 : System.Address; Arg2 : Int);
--     pragma Import (C, Udev_Set_Log_Priority, "udev_set_log_priority");

--     function Udev_Get_Userdata
--       (Arg1 : System.Address) return System.Address;
--     pragma Import (C, Udev_Get_Userdata, "udev_get_userdata");
--
--     procedure Udev_Set_Userdata
--       (Arg1 : System.Address;
--        Arg2 : System.Address);
--     pragma Import (C, Udev_Set_Userdata, "udev_set_userdata");

   function Udev_Monitor_New_From_Netlink
     (Arg1 : Udev_Ptr;
      Arg2 : C_String) return Udev_Monitor_Ptr;
   pragma Import
     (C, Udev_Monitor_New_From_Netlink, "udev_monitor_new_from_netlink");
   --  Create a udev monitor object.
   --  On success, returns a pointer to the allocated udev monitor.
   --  On failure, null is returned.

   procedure Create
     (Context : out Contexts.Context) is
   begin
      Context.My_Ptr := Udev_New;
   end Create;

   procedure Delete (Context : in out Contexts.Context) is
   begin
      Context.My_Ptr := Udev_Unref (Context.My_Ptr);

      Context.My_Ptr := null;
      --  Is unnecessary, but static code analyzers cannot know
      --  Udev_Unref (..) always returns null.
   end Delete;

   procedure New_From_Netlink (Context : Contexts.Context;
                               Name    : String;
                               Monitor : out Monitors.Monitor) is
   begin
      Monitor_Base (Monitor).My_Ptr
        := Udev_Monitor_New_From_Netlink (Context.My_Ptr, +Name);
   end New_From_Netlink;

end C_Binding.Linux.Udev.Contexts;
