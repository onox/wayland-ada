--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2018 - 2019 Joakim Strandberg <joakim@mequinox.se>
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.

package body C_Binding.Linux.GnuTLS.Certificate_Credentials is

   procedure Allocate_Credentials is
      Result : constant Interfaces.C.int
        := C_Certificate_Allocate_Credentials (This.My_Credentials'Access);
   begin
      if Result = GNUTLS_E_SUCCESS then
         begin
            Handle_Success;
            C_Certificate_Free_Credentials (This.My_Credentials);
         exception
            when Error : others =>
               C_Certificate_Free_Credentials (This.My_Credentials);
               raise;
         end;
      else
         Handle_Failure;
      end if;
   end Allocate_Credentials;

   function Set_Session_Credentials
     (This    : Credentials;
      Session : Sessions.Session) return Success_Flag
   is
      Result : constant Interfaces.C.int
        := C_Credentials_Set
          (Session.My_Session,
           Certificate_Credential,
           This.My_Credentials);
   begin
      if Result = GNUTLS_E_SUCCESS then
         return Success;
      else
         return Failure;
      end if;
   end Set_Session_Credentials;

end C_Binding.Linux.GnuTLS.Certificate_Credentials;
