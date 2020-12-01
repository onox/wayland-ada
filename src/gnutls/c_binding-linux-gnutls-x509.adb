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

with Ada.Text_IO;

package body C_Binding.Linux.GnuTLS.X509 is

   function Add_The_Systems_Default_Trusted_CAs
     (
      Credentials : Certificate_Credentials.Credentials
     ) return Add_The_Systems_Default_Trusted_CAs_Result
   is
      Result : constant Interfaces.C.int
        := C_Certificate_Set_x509_System_Trust (Credentials.My_Credentials);
   begin
      case Result is
         when Interfaces.C.int'First .. -1251 =>
            return (Kind_Id => Add_Systems_Trusted_CAs_Failure);
         when GNUTLS_E_UNIMPLEMENTED_FEATURE =>
            return (Kind_Id => Add_Systems_Trusted_CAs_Unsupported);
         when -1249 .. -1 =>
            return (Kind_Id => Add_Systems_Trusted_CAs_Failure);
         when 0 .. Interfaces.C.int'Last =>
            return
              (Kind_Id => Add_Systems_Trusted_CAs_Success,
               Certificates_Count => Processed_Certificates_Count (Result));
      end case;
   end Add_The_Systems_Default_Trusted_CAs;

   function Set_Key_File
     (Credentials      : Certificate_Credentials.Credentials;
      Certificate_File : String;
      Key_File         : String;
      Format           : Certificate_Format) return Success_Flag
   is
      Result : constant Interfaces.C.int
        := C_Certificate_Set_x509_Key_File
          (Credentials.My_Credentials,
           Interfaces.C.To_C (Certificate_File),
           Interfaces.C.To_C (Key_File),
           Format);
   begin
      if Result = GNUTLS_E_SUCCESS then
         return Success;
      else
         return Failure;
      end if;
   end Set_Key_File;

   function Set_Trust_CAs_File
     (Credentials : Certificate_Credentials.Credentials;
      Name        : String;
      Format      : Certificate_Format) return Set_Trust_CAs_File_Result
   is
      Result : constant Interfaces.C.int
        := C_Set_X509_Trust_File
          (Credentials.My_Credentials,
           Interfaces.C.To_C (Name),
           Format);
   begin
      if Result >= 0 then
         return (Kind_Id                      => Set_Trust_CAs_File_Success,
                 Processed_Certificates_Count => Natural (Result));
      else
         return (Kind_Id => Set_Trust_CAs_File_Failure);
      end if;
   end Set_Trust_CAs_File;

   function Set_CRL_File
     (Credentials : Certificate_Credentials.Credentials;
      Name        : String;
      Format      : Certificate_Format) return Set_CRL_File_Result
   is
      Result : constant Interfaces.C.int
        := C_Set_X509_CRL_File
          (Credentials.My_Credentials,
           Interfaces.C.To_C (Name),
           Format);
   begin
      if Result >= 0 then
         return (Kind_Id                      => Set_CRL_File_Success,
                 Processed_Certificates_Count => Natural (Result));
      else
         Ada.Text_IO.Put_Line ("f:" & Result'Img);
         return (Kind_Id => Set_CRL_File_Failure);
      end if;
   end Set_CRL_File;

end C_Binding.Linux.GnuTLS.X509;
