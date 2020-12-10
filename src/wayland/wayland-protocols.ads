private with Interfaces.C.Strings;

private with Wayland.API;

package Wayland.Protocols is
   pragma Preelaborate;

   type Interface_Type is tagged limited private;

   function Name (Object : Interface_Type) return String;

   type Secret_Proxy is limited private;

private

   type Secret_Proxy is new Wayland.API.Proxy_Ptr;

   type Interface_Type is tagged limited record
      My_Interface : Wayland.API.Interface_Ptr;
   end record;

   function Name (Object : Interface_Type) return String is
     (Interfaces.C.Strings.Value (Interfaces.C.Strings.To_Chars_Ptr
        (Object.My_Interface.Name)));

end Wayland.Protocols;
