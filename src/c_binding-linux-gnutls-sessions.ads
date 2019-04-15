package C_Binding.Linux.GnuTLS.Sessions is

   type Session is limited private;

private

   type Opaque_Session is null record;
   type Opaque_Session_Ptr is access all Opaque_Session;

   type Session is limited record
      My_Session : aliased Opaque_Session_Ptr;
   end record;

end C_Binding.Linux.GnuTLS.Sessions;
