with C_Binding.Linux.Udev.Monitors;

package body C_Binding.Linux.Udev.Contexts is

   procedure Create
     (Context : out Contexts.Context) is
   begin
      Context.My_Ptr := Thin.Udev_New;
   end Create;

   procedure Delete (Context : in out Contexts.Context) is
   begin
      Context.My_Ptr := Thin.Udev_Unref (Context.My_Ptr);

      Context.My_Ptr := null;
      --  Is unnecessary, but static code analyzers cannot know
      --  Thin.Udev_Unref (..) always returns null.
   end Delete;

   procedure New_From_Netlink (Context : Contexts.Context;
                               Name    : String;
                               Monitor : out Monitors.Monitor) is
   begin
      Monitor_Base (Monitor).My_Ptr
        := Thin.Udev_Monitor_New_From_Netlink (Context.My_Ptr, +Name);
   end New_From_Netlink;

end C_Binding.Linux.Udev.Contexts;
