private with Interfaces.C.Strings;

package C_Binding.Linux.GnuTLS is

   pragma Linker_Options ("-lgnutls");

   subtype String_Result is C_Binding.String_Result;

   function Check_Version (Version : String) return String_Result;

   function Get_Version return String;

   generic
      with procedure Handle_Success;
      with procedure Handle_Failure;
   procedure Initialize_GnuTLS;

   type Credentials_Base is limited private;

private

   type Opaque_Certificate_Credentials is null record;
   type Opaque_Certificate_Credentials_Ptr is
     access all Opaque_Certificate_Credentials;

   type Credentials_Base is limited record
      My_Credentials : aliased Opaque_Certificate_Credentials_Ptr;
   end record;

   function C_Check_Version
     (
      Req_Version : Interfaces.C.char_array
     ) return Interfaces.C.Strings.chars_ptr with
       Import        => True,
       Convention    => C,
       External_Name => "gnutls_check_version";
   --  req_version : version string to compare with, or NULL.
   --
   --  Returns : Check that the version of the library is at minimum
   --  the one given as a string in req_version and return the actual
   --  version string of the library; return NULL if the condition is not met.
   --  If NULL is passed to this function no check is done and only
   --  the version string is returned.

   function C_Check_Version
     (
      Req_Version : Interfaces.C.Strings.chars_ptr
     ) return Interfaces.C.Strings.chars_ptr with
       Import        => True,
       Convention    => C,
       External_Name => "gnutls_check_version";

   function C_Global_Init return Interfaces.C.int with
     Import        => True,
     Convention    => C,
     External_Name => "gnutls_global_init";
   --  This function initializes the global data to defaults.
   --  In order to free resources you may call gnutls_global_deinit()
   --  when gnutls usage is no longer needed.
   --
   --  Note that this function will also initialize the underlying
   --  crypto backend, if it has not been initialized before.
   --
   --  This function increments a global counter, so that
   --  gnutls_global_deinit() only releases resources when it has been called
   --  as many times as gnutls_global_init(). This is useful when GnuTLS is
   --  used by more than one library in an application. This function can be
   --  called many times, but will only do something the first time.
   --
   --  Note! This function is not thread safe. If two threads call this
   --  function simultaneously, they can cause a race between checking
   --  the global counter and incrementing it, causing both threads to execute
   --  the library initialization code. That would lead to a memory leak.
   --  To handle this, your application could invoke this function after
   --  aquiring a thread mutex. To ignore the potential memory leak
   --  is also an option.
   --
   --  Returns : On success, GNUTLS_E_SUCCESS (0) is returned,
   --  otherwise a negative error code is returned.

   procedure C_Global_Deinit with
     Import        => True,
     Convention    => C,
     External_Name => "gnutls_global_deinit";
   --  This function deinitializes the global data, that were initialized
   --  using gnutls_global_init().
   --
   --  Note! This function is not thread safe. See the discussion for
   --  gnutls_global_init() for more information.

   GNUTLS_E_SUCCESS               : constant := 0;
   GNUTLS_E_UNIMPLEMENTED_FEATURE : constant := -1250;

end C_Binding.Linux.GnuTLS;
