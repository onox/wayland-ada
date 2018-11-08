limited with C_Binding.Linux.Udev.Monitors;

package C_Binding.Linux.Udev.Contexts is

   type Context is new Context_Base with null record;

   procedure Create (Context : out Contexts.Context);

   function Exists (Context : Contexts.Context) return Boolean;

   procedure Delete (Context : in out Contexts.Context) with
     Pre  => Context.Exists,
     Post => not Context.Exists;

   procedure New_From_Netlink (Context : Contexts.Context;
                               Name    : String;
                               Monitor : out Monitors.Monitor);

private

   function Exists (Context : Contexts.Context) return Boolean is
     (Context.My_Ptr /= null);

end C_Binding.Linux.Udev.Contexts;
