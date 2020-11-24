with System.Address_To_Access_Conversions;

with Interfaces.C;

with C_Binding.Linux;

with Wayland.API;
with Wayland.Client.Constants;

package body Wayland.Client.Protocol is

   subtype int is Interfaces.C.int;

   use type int;

   use type Thin.Proxy_Ptr;

   subtype Registry_Global_Subprogram_Ptr is Thin.Registry_Global_Subprogram_Ptr;

   subtype Registry_Global_Remove_Subprogram_Ptr is Thin.Registry_Global_Remove_Subprogram_Ptr;

   subtype Registry_Listener_T is Thin.Registry_Listener_T;

   subtype Registry_Listener_Ptr is Thin.Registry_Listener_Ptr;

   package body Display_Events is

      package Conversion is new
        System.Address_To_Access_Conversions (Data_Type);

      procedure Internal_Error (Data      : Void_Ptr;
                                Display   : Thin.Display_Ptr;
                                Object_Id : Void_Ptr;
                                Code      : Unsigned_32;
                                Message   : chars_ptr) with
        Convention => C;

      procedure Internal_Delete_Id (Data    : Void_Ptr;
                                    Display : Thin.Display_Ptr;
                                    Id      : Unsigned_32) with
        Convention => C;

      procedure Internal_Error (Data      : Void_Ptr;
                                Display   : Thin.Display_Ptr;
                                Object_Id : Void_Ptr;
                                Code      : Unsigned_32;
                                Message   : chars_ptr)
      is
         D : constant Protocol.Display
           := (
               My_Display                  => Display,
               My_Fd                       =>
                 Wayland.API.Display_Get_File_Descriptor (Display)
              );
         M : constant String := Interfaces.C.Strings.Value (Message);
      begin
         Error (Data_Ptr (Conversion.To_Pointer (Data)),
                D,
                Object_Id,
                Code,
                M);
      end Internal_Error;

      procedure Internal_Delete_Id (Data    : Void_Ptr;
                                    Display : Thin.Display_Ptr;
                                    Id      : Unsigned_32)
      is
         D : constant Protocol.Display
           := (
               My_Display                  => Display,
               My_Fd                       =>
                 Wayland.API.Display_Get_File_Descriptor (Display)
              );
      begin
         Delete_Id (Data_Ptr (Conversion.To_Pointer (Data)),
                    D,
                    Id);
      end Internal_Delete_Id;

      Listener : aliased Thin.Display_Listener_T
        := (
            Error     => Internal_Error'Unrestricted_Access,
            Delete_Id => Internal_Delete_Id'Unrestricted_Access
           );

      function Subscribe
        (Display : in out Protocol.Display;
         Data    : not null Data_Ptr) return Call_Result_Code
      is
         I : int;
      begin
         I := Thin.Display_Add_Listener (Display.My_Display,
                                            Listener'Access,
                                            Data.all'Address);
         if I = 0 then
            return Success;
         else
            return Error;
         end if;
      end Subscribe;

   end Display_Events;

   package body Registry_Events is

      package Conversion is new
        System.Address_To_Access_Conversions (Data_Type);

      procedure Internal_Object_Added
        (Data        : Void_Ptr;
         Registry    : Thin.Registry_Ptr;
         Id          : Unsigned_32;
         Interface_V : Protocol.Chars_Ptr;
         Version     : Unsigned_32) with
        Convention => C,
        Global     => null;

      procedure Internal_Object_Added
        (Data        : Void_Ptr;
         Registry    : Thin.Registry_Ptr;
         Id          : Unsigned_32;
         Interface_V : Protocol.Chars_Ptr;
         Version     : Unsigned_32)
      is
         R : constant Protocol.Registry := (My_Registry => Registry);
      begin
         Global_Object_Added (Data_Ptr (Conversion.To_Pointer (Data)),
                              R,
                              Id,
                              Value (Interface_V),
                              Version);
      end Internal_Object_Added;

      procedure Internal_Object_Removed (Data     : Void_Ptr;
                                         Registry : Thin.Registry_Ptr;
                                         Id       : Unsigned_32) with
        Convention => C;

      procedure Internal_Object_Removed (Data     : Void_Ptr;
                                         Registry : Thin.Registry_Ptr;
                                         Id       : Unsigned_32)
      is
         R : constant Protocol.Registry := (My_Registry => Registry);
      begin
         Global_Object_Removed (Data_Ptr (Conversion.To_Pointer (Data)), R, Id);
      end Internal_Object_Removed;

      Listener : aliased Protocol.Registry_Listener_T :=
        (
         Global        => Internal_Object_Added'Unrestricted_Access,
         Global_Remove => Internal_Object_Removed'Unrestricted_Access
        );
      -- Note: It should be safe to use Unrestricted_Access here since
      -- this generic can only be instantiated at library level.

      function Subscribe
        (Registry : in out Protocol.Registry;
         Data     : not null Data_Ptr) return Call_Result_Code is
         I : int;
      begin
         I := Thin.Registry_Add_Listener (Registry.My_Registry,
                                             Listener'Access,
                                             Data.all'Address);

         if I = 0 then
            return Success;
         else
            return Error;
         end if;
      end Subscribe;

   end Registry_Events;

   package body Callback_Events is

      package Conversion is new
        System.Address_To_Access_Conversions (Data_Type);

      procedure Internal_Done (Data          : Void_Ptr;
                               Callback      : Thin.Callback_Ptr;
                               Callback_Data : Unsigned_32) with
        Convention => C;

      procedure Internal_Done (Data          : Void_Ptr;
                               Callback      : Thin.Callback_Ptr;
                               Callback_Data : Unsigned_32)
      is
         C : constant Protocol.Callback := (My_Callback => Callback);
      begin
         Done (Data_Ptr (Conversion.To_Pointer (Data)), C, Callback_Data);
      end Internal_Done;

      Listener : aliased Thin.Callback_Listener_T
        := (Done => Internal_Done'Unrestricted_Access);

      function Subscribe
        (Callback : in out Protocol.Callback;
         Data     : not null Data_Ptr) return Call_Result_Code
      is
         I : int;
      begin
         I := Thin.Callback_Add_Listener (Callback.My_Callback,
                                             Listener'Access,
                                             Data.all'Address);
         if I = 0 then
            return Success;
         else
            return Error;
         end if;
      end Subscribe;

   end Callback_Events;

   package body Shm_Events is

      package Conversion is new
        System.Address_To_Access_Conversions (Data_Type);

      procedure Internal_Format (Data   : Void_Ptr;
                                 Shm    : Thin.Shm_Ptr;
                                 Format : Shm_Format) with
        Convention => C;

      procedure Internal_Format (Data   : Void_Ptr;
                                 Shm    : Thin.Shm_Ptr;
                                 Format : Shm_Format)
      is
         S : constant Protocol.Shm := (My_Shm => Shm);
      begin
         Shm_Events.Format
           (Data_Ptr (Conversion.To_Pointer (Data)), S, Format);
      end Internal_Format;

      Listener : aliased Thin.Shm_Listener_T
        := (Format => Internal_Format'Unrestricted_Access);

      function Subscribe
        (Shm  : in out Protocol.Shm;
         Data : not null Data_Ptr) return Call_Result_Code
      is
         I : int;
      begin
         I := Thin.Shm_Add_Listener (Shm.My_Shm,
                                        Listener'Access,
                                        Data.all'Address);
         if I = 0 then
            return Success;
         else
            return Error;
         end if;
      end Subscribe;

   end Shm_Events;

   package body Buffer_Events is

      package Conversion is new
        System.Address_To_Access_Conversions (Data_Type);

      procedure Internal_Release (Data   : Void_Ptr;
                                  Buffer : Thin.Buffer_Ptr) with
        Convention => C;

      procedure Internal_Release (Data   : Void_Ptr;
                                  Buffer : Thin.Buffer_Ptr)
      is
         B : constant Protocol.Buffer := (My_Buffer => Buffer);
      begin
         Release (Data_Ptr (Conversion.To_Pointer (Data)), B);
      end Internal_Release;

      Listener : aliased Thin.Buffer_Listener_T
        := (Release => Internal_Release'Unrestricted_Access);

      function Subscribe
        (Buffer : in out Protocol.Buffer;
         Data   : not null Data_Ptr) return Call_Result_Code
      is
         I : int;
      begin
         I := Thin.Buffer_Add_Listener (Buffer.My_Buffer,
                                           Listener'Access,
                                           Data.all'Address);
         if I = 0 then
            return Success;
         else
            return Error;
         end if;
      end Subscribe;

   end Buffer_Events;

   package body Data_Offer_Events is

      package Conversion is new
        System.Address_To_Access_Conversions (Data_Type);

      procedure Internal_Offer (Data       : Void_Ptr;
                                Data_Offer : Thin.Data_Offer_Ptr;
                                Mime_Type  : chars_ptr) with
        Convention => C;

      procedure Internal_Source_Actions
        (Data           : Void_Ptr;
         Data_Offer     : Thin.Data_Offer_Ptr;
         Source_Actions : Unsigned_32) with
        Convention => C;

      procedure Internal_Action (Data       : Void_Ptr;
                                 Data_Offer : Thin.Data_Offer_Ptr;
                                 Dnd_Action : Unsigned_32) with
        Convention => C;

      procedure Internal_Offer (Data       : Void_Ptr;
                                Data_Offer : Thin.Data_Offer_Ptr;
                                Mime_Type  : chars_ptr)
      is
         D : constant Protocol.Data_Offer
           := (My_Data_Offer => Data_Offer);

         M : constant String := Interfaces.C.Strings.Value (Mime_Type);
      begin
         Offer (Data_Ptr (Conversion.To_Pointer (Data)), D, M);
      end Internal_Offer;

      procedure Internal_Source_Actions
        (Data           : Void_Ptr;
         Data_Offer     : Thin.Data_Offer_Ptr;
         Source_Actions : Unsigned_32)
      is
         D : constant Protocol.Data_Offer
           := (My_Data_Offer => Data_Offer);
      begin
         Data_Offer_Events.Source_Actions
           (Data_Ptr (Conversion.To_Pointer (Data)),
            D,
            Source_Actions);
      end Internal_Source_Actions;

      procedure Internal_Action (Data       : Void_Ptr;
                                 Data_Offer : Thin.Data_Offer_Ptr;
                                 Dnd_Action : Unsigned_32)
      is
         D : constant Protocol.Data_Offer
           := (My_Data_Offer => Data_Offer);
      begin
         Action (Data_Ptr (Conversion.To_Pointer (Data)),
                 D,
                 Dnd_Action);
      end Internal_Action;

      Listener : aliased Thin.Data_Offer_Listener_T
        := (Offer          => Internal_Offer'Unrestricted_Access,
            Source_Actions => Internal_Source_Actions'Unrestricted_Access,
            Action         => Internal_Action'Unrestricted_Access);

      function Subscribe
        (Data_Offer : in out Protocol.Data_Offer;
         Data       : not null Data_Ptr) return Call_Result_Code
      is
         I : int;
      begin
         I := Thin.Data_Offer_Add_Listener (Data_Offer.My_Data_Offer,
                                               Listener'Access,
                                               Data.all'Address);
         if I = 0 then
            return Success;
         else
            return Error;
         end if;
      end Subscribe;

   end Data_Offer_Events;

   package body Data_Source_Events is

      package Conversion is new
        System.Address_To_Access_Conversions (Data_Type);

      procedure Internal_Target (Data        : Void_Ptr;
                                 Data_Source : Thin.Data_Source_Ptr;
                                 Mime_Type   : chars_ptr) with
        Convention => C;

      procedure Internal_Send (Data        : Void_Ptr;
                               Data_Source : Thin.Data_Source_Ptr;
                               Mime_Type   : chars_ptr;
                               Fd          : Integer) with
        Convention => C;

      procedure Internal_Cancelled (Data        : Void_Ptr;
                                    Data_Source : Thin.Data_Source_Ptr) with
        Convention => C;

      procedure Internal_Dnd_Drop_Performed
        (Data        : Void_Ptr;
         Data_Source : Thin.Data_Source_Ptr) with
        Convention => C;

      procedure Internal_Dnd_Finished
        (Data        : Void_Ptr;
         Data_Source : Thin.Data_Source_Ptr) with
        Convention => C;

      procedure Internal_Action (Data        : Void_Ptr;
                                 Data_Source : Thin.Data_Source_Ptr;
                                 Dnd_Action  : Unsigned_32) with
        Convention => C;

      procedure Internal_Target (Data        : Void_Ptr;
                                 Data_Source : Thin.Data_Source_Ptr;
                                 Mime_Type   : chars_ptr)
      is
         D : constant Protocol.Data_Source
           := (My_Data_Source => Data_Source);

         M : constant String := Interfaces.C.Strings.Value (Mime_Type);
      begin
         Target (Data_Ptr (Conversion.To_Pointer (Data)), D, M);
      end Internal_Target;

      procedure Internal_Send (Data        : Void_Ptr;
                               Data_Source : Thin.Data_Source_Ptr;
                               Mime_Type   : chars_ptr;
                               Fd          : Integer)
      is
         D : constant Protocol.Data_Source
           := (My_Data_Source => Data_Source);

         M : constant String := Interfaces.C.Strings.Value (Mime_Type);
      begin
         Send (Data_Ptr (Conversion.To_Pointer (Data)), D, M, Fd);
      end Internal_Send;

      procedure Internal_Cancelled (Data        : Void_Ptr;
                                    Data_Source : Thin.Data_Source_Ptr)
      is
         D : constant Protocol.Data_Source
           := (My_Data_Source => Data_Source);
      begin
         Cancelled (Data_Ptr (Conversion.To_Pointer (Data)), D);
      end Internal_Cancelled;

      procedure Internal_Dnd_Drop_Performed
        (Data        : Void_Ptr;
         Data_Source : Thin.Data_Source_Ptr)
      is
         D : constant Protocol.Data_Source
           := (My_Data_Source => Data_Source);
      begin
         Dnd_Drop_Performed (Data_Ptr (Conversion.To_Pointer (Data)), D);
      end Internal_Dnd_Drop_Performed;

      procedure Internal_Dnd_Finished (Data        : Void_Ptr;
                                       Data_Source : Thin.Data_Source_Ptr)
      is
         D : constant Protocol.Data_Source
           := (My_Data_Source => Data_Source);
      begin
         Dnd_Drop_Performed (Data_Ptr (Conversion.To_Pointer (Data)), D);
      end Internal_Dnd_Finished;

      procedure Internal_Action (Data        : Void_Ptr;
                                 Data_Source : Thin.Data_Source_Ptr;
                                 Dnd_Action  : Unsigned_32)
      is
         D : constant Protocol.Data_Source
           := (My_Data_Source => Data_Source);
      begin
         Action (Data_Ptr (Conversion.To_Pointer (Data)), D, Dnd_Action);
      end Internal_Action;

      Listener : aliased Thin.Data_Source_Listener_T
        := (Target             => Internal_Target'Unrestricted_Access,
            Send               => Internal_Send'Unrestricted_Access,
            Cancelled          => Internal_Cancelled'Unrestricted_Access,
            Dnd_Drop_Performed =>
              Internal_Dnd_Drop_Performed'Unrestricted_Access,
            Dnd_Finished       => Internal_Dnd_Finished'Unrestricted_Access,
            Action             => Internal_Action'Unrestricted_Access);

      function Subscribe
        (Data_Source : in out Protocol.Data_Source;
         Data       : not null Data_Ptr) return Call_Result_Code
      is
         I : int;
      begin
         I := Thin.Data_Source_Add_Listener (Data_Source.My_Data_Source,
                                                Listener'Access,
                                                Data.all'Address);
         if I = 0 then
            return Success;
         else
            return Error;
         end if;
      end Subscribe;

   end Data_Source_Events;

   package body Data_Device_Events is

      package Conversion is new
        System.Address_To_Access_Conversions (Data_Type);

      procedure Internal_Data_Offer (Data        : Void_Ptr;
                                     Data_Device : Thin.Data_Device_Ptr;
                                     Id          : Unsigned_32) with
        Convention => C;

      procedure Internal_Enter (Data        : Void_Ptr;
                                Data_Device : Thin.Data_Device_Ptr;
                                Serial      : Unsigned_32;
                                Surface     : Thin.Surface_Ptr;
                                X           : Fixed;
                                Y           : Fixed;
                                Id          : Thin.Data_Offer_Ptr) with
        Convention => C;

      procedure Internal_Leave (Data        : Void_Ptr;
                                Data_Device : Thin.Data_Device_Ptr) with
        Convention => C;

      procedure Internal_Motion (Data        : Void_Ptr;
                                 Data_Device : Thin.Data_Device_Ptr;
                                 Time        : Unsigned_32;
                                 X           : Fixed;
                                 Y           : Fixed) with
        Convention => C;

      procedure Internal_Drop (Data        : Void_Ptr;
                               Data_Device : Thin.Data_Device_Ptr) with
        Convention => C;

      procedure Internal_Selection (Data        : Void_Ptr;
                                    Data_Device : Thin.Data_Device_Ptr;
                                    Id          : Thin.Data_Offer_Ptr) with
        Convention => C;

      procedure Internal_Data_Offer (Data        : Void_Ptr;
                                     Data_Device : Thin.Data_Device_Ptr;
                                     Id          : Unsigned_32)
      is
         D : constant Protocol.Data_Device
           := (My_Data_Device => Data_Device);
      begin
         Data_Offer (Data_Ptr (Conversion.To_Pointer (Data)), D, Id);
      end Internal_Data_Offer;

      procedure Internal_Enter (Data        : Void_Ptr;
                                Data_Device : Thin.Data_Device_Ptr;
                                Serial      : Unsigned_32;
                                Surface     : Thin.Surface_Ptr;
                                X           : Fixed;
                                Y           : Fixed;
                                Id          : Thin.Data_Offer_Ptr)
      is
         D : constant Protocol.Data_Device
           := (My_Data_Device => Data_Device);
         S : constant Protocol.Surface
           := (My_Surface => Surface);
         Offer : constant Protocol.Data_Offer
           := (My_Data_Offer => Id);
      begin
         Enter (Data_Ptr (Conversion.To_Pointer (Data)),
                D,
                Serial,
                S,
                X,
                Y,
                Offer);
      end Internal_Enter;

      procedure Internal_Leave (Data        : Void_Ptr;
                                Data_Device : Thin.Data_Device_Ptr)
      is
         D : constant Protocol.Data_Device
           := (My_Data_Device => Data_Device);
      begin
         Leave (Data_Ptr (Conversion.To_Pointer (Data)), D);
      end Internal_Leave;

      procedure Internal_Motion (Data        : Void_Ptr;
                                 Data_Device : Thin.Data_Device_Ptr;
                                 Time        : Unsigned_32;
                                 X           : Fixed;
                                 Y           : Fixed)
      is
         D : constant Protocol.Data_Device
           := (My_Data_Device => Data_Device);
      begin
         Motion (Data_Ptr (Conversion.To_Pointer (Data)), D, Time, X, Y);
      end Internal_Motion;

      procedure Internal_Drop (Data        : Void_Ptr;
                               Data_Device : Thin.Data_Device_Ptr)
      is
         D : constant Protocol.Data_Device
           := (My_Data_Device => Data_Device);
      begin
         Drop (Data_Ptr (Conversion.To_Pointer (Data)), D);
      end Internal_Drop;

      procedure Internal_Selection (Data        : Void_Ptr;
                                    Data_Device : Thin.Data_Device_Ptr;
                                    Id          : Thin.Data_Offer_Ptr)
      is
         D : constant Protocol.Data_Device
           := (My_Data_Device => Data_Device);
         Offer : constant Protocol.Data_Offer
           := (My_Data_Offer => Id);
      begin
         Selection (Data_Ptr (Conversion.To_Pointer (Data)), D, Offer);
      end Internal_Selection;

      Listener : aliased Thin.Data_Device_Listener_T
        := (Data_Offer => Internal_Data_Offer'Unrestricted_Access,
            Enter      => Internal_Enter'Unrestricted_Access,
            Leave      => Internal_Leave'Unrestricted_Access,
            Motion     => Internal_Motion'Unrestricted_Access,
            Drop       => Internal_Drop'Unrestricted_Access,
            Selection  => Internal_Selection'Unrestricted_Access);

      function Subscribe
        (Data_Device : in out Protocol.Data_Device;
         Data        : not null Data_Ptr) return Call_Result_Code
      is
         I : int;
      begin
         I := Thin.Data_Device_Add_Listener (Data_Device.My_Data_Device,
                                                Listener'Access,
                                                Data.all'Address);
         if I = 0 then
            return Success;
         else
            return Error;
         end if;
      end Subscribe;

   end Data_Device_Events;

   package body Surface_Events is

      package Conversion is new
        System.Address_To_Access_Conversions (Data_Type);

      procedure Internal_Enter (Data    : Void_Ptr;
                                Surface : Thin.Surface_Ptr;
                                Output  : Thin.Output_Ptr) with
        Convention => C;

      procedure Internal_Leave (Data    : Void_Ptr;
                                Surface : Thin.Surface_Ptr;
                                Output  : Thin.Output_Ptr) with
        Convention => C;

      procedure Internal_Enter (Data    : Void_Ptr;
                                Surface : Thin.Surface_Ptr;
                                Output  : Thin.Output_Ptr)
      is
         S : constant Protocol.Surface := (My_Surface => Surface);
         O : constant Protocol.Output := (My_Output => Output);
      begin
         Enter (Data_Ptr (Conversion.To_Pointer (Data)), S, O);
      end Internal_Enter;

      procedure Internal_Leave (Data    : Void_Ptr;
                                Surface : Thin.Surface_Ptr;
                                Output  : Thin.Output_Ptr)
      is
         S : constant Protocol.Surface := (My_Surface => Surface);
         O : constant Protocol.Output := (My_Output => Output);
      begin
         Leave (Data_Ptr (Conversion.To_Pointer (Data)), S, O);
      end Internal_Leave;

      Listener : aliased Thin.Surface_Listener_T
        := (Enter => Internal_Enter'Unrestricted_Access,
            Leave => Internal_Leave'Unrestricted_Access);

      function Subscribe
        (Surface : in out Protocol.Surface;
         Data    : not null Data_Ptr) return Call_Result_Code
      is
         I : int;
      begin
         I := Thin.Surface_Add_Listener (Surface.My_Surface,
                                            Listener'Access,
                                            Data.all'Address);

         if I = 0 then
            return Success;
         else
            return Error;
         end if;
      end Subscribe;

   end Surface_Events;

   package body Seat_Events is

      package Conversion is new
        System.Address_To_Access_Conversions (Data_Type);

      procedure Internal_Seat_Capabilities (Data         : Void_Ptr;
                                            Seat         : Thin.Seat_Ptr;
                                            Capabilities : Seat_Capability) with
        Convention => C;

      procedure Internal_Seat_Capabilities (Data         : Void_Ptr;
                                            Seat         : Thin.Seat_Ptr;
                                            Capabilities : Seat_Capability)
      is
         S : constant Protocol.Seat := (My_Seat => Seat);
      begin
         Seat_Capabilities (Data_Ptr (Conversion.To_Pointer (Data)),
                            S,
                            Capabilities);
      end Internal_Seat_Capabilities;

      procedure Internal_Seat_Name
        (Data : Void_Ptr;
         Seat : Thin.Seat_Ptr;
         Name : Interfaces.C.Strings.Chars_Ptr) with
        Convention => C;

      procedure Internal_Seat_Name
        (Data : Void_Ptr;
         Seat : Thin.Seat_Ptr;
         Name : Interfaces.C.Strings.Chars_Ptr)
      is
         N : constant String := Interfaces.C.Strings.Value (Name);

         S : constant Protocol.Seat := (My_Seat => Seat);
      begin
         Seat_Name (Data_Ptr (Conversion.To_Pointer (Data)), S, N);
      end Internal_Seat_Name;

      Seat_Listener : aliased Thin.Seat_Listener_T :=
        (
         Capabilities => Internal_Seat_Capabilities'Unrestricted_Access,
         Name         => Internal_Seat_Name'Unrestricted_Access
        );

      function Subscribe
        (Seat : in out Protocol.Seat;
         Data : not null Data_Ptr) return Call_Result_Code
      is
         I : int;
      begin
         I := Thin.Seat_Add_Listener
           (Seat     => Seat.My_Seat,
            Listener => Seat_Listener'Access,
            Data     => Data.all'Address);
         if I = 0 then
            return Success;
         else
            return Error;
         end if;
      end Subscribe;

   end Seat_Events;

   package body Pointer_Events is

      package Conversion is new
        System.Address_To_Access_Conversions (Data_Type);

      procedure Internal_Pointer_Enter
        (Data      : Void_Ptr;
         Pointer   : Thin.Pointer_Ptr;
         Serial    : Unsigned_32;
         Surface   : Thin.Surface_Ptr;
         Surface_X : Fixed;
         Surface_Y : Fixed) with
        Convention => C;

      procedure Internal_Pointer_Leave
        (Data    : Void_Ptr;
         Pointer : Thin.Pointer_Ptr;
         Serial  : Unsigned_32;
         Surface : Thin.Surface_Ptr) with
        Convention => C;

      procedure Internal_Pointer_Motion
        (Data      : Void_Ptr;
         Pointer   : Thin.Pointer_Ptr;
         Time      : Unsigned_32;
         Surface_X : Fixed;
         Surface_Y : Fixed) with
        Convention => C;

      procedure Internal_Pointer_Button
        (Data    : Void_Ptr;
         Pointer : Thin.Pointer_Ptr;
         Serial  : Unsigned_32;
         Time    : Unsigned_32;
         Button  : Unsigned_32;
         State   : Pointer_Button_State) with
        Convention => C;

      procedure Internal_Pointer_Axis
        (Data    : Void_Ptr;
         Pointer : Thin.Pointer_Ptr;
         Time    : Unsigned_32;
         Axis    : Pointer_Axis;
         Value   : Fixed) with
        Convention => C;

      procedure Internal_Pointer_Frame (Data    : Void_Ptr;
                                        Pointer : Thin.Pointer_Ptr) with
        Convention => C;

      procedure Internal_Pointer_Axis_Source
        (Data        : Void_Ptr;
         Pointer     : Thin.Pointer_Ptr;
         Axis_Source : Pointer_Axis_Source) with
        Convention => C;

      procedure Internal_Pointer_Axis_Stop
        (Data    : Void_Ptr;
         Pointer : Thin.Pointer_Ptr;
         Time    : Unsigned_32;
         Axis    : Pointer_Axis) with
        Convention => C;

      procedure Internal_Pointer_Axis_Discrete
        (Data     : Void_Ptr;
         Pointer  : Thin.Pointer_Ptr;
         Axis     : Pointer_Axis;
         Discrete : Integer) with
        Convention => C;

      procedure Internal_Pointer_Enter
        (Data      : Void_Ptr;
         Pointer   : Thin.Pointer_Ptr;
         Serial    : Unsigned_32;
         Surface   : Thin.Surface_Ptr;
         Surface_X : Fixed;
         Surface_Y : Fixed)
      is
         P : constant Protocol.Pointer := (My_Pointer => Pointer);
         S : constant Protocol.Surface := (My_Surface => Surface);
      begin
         Pointer_Enter (Data_Ptr (Conversion.To_Pointer (Data)),
                        P,
                        Serial,
                        S,
                        Surface_X,
                        Surface_Y);
      end Internal_Pointer_Enter;

      procedure Internal_Pointer_Leave
        (Data    : Void_Ptr;
         Pointer : Thin.Pointer_Ptr;
         Serial  : Unsigned_32;
         Surface : Thin.Surface_Ptr)
      is
         P : constant Protocol.Pointer := (My_Pointer => Pointer);
         S : constant Protocol.Surface := (My_Surface => Surface);
      begin
         Pointer_Leave (Data_Ptr (Conversion.To_Pointer (Data)), P, Serial, S);
      end Internal_Pointer_Leave;

      procedure Internal_Pointer_Motion
        (Data      : Void_Ptr;
         Pointer   : Thin.Pointer_Ptr;
         Time      : Unsigned_32;
         Surface_X : Fixed;
         Surface_Y : Fixed)
      is
         P : constant Protocol.Pointer := (My_Pointer => Pointer);
      begin
         Pointer_Motion (Data_Ptr (Conversion.To_Pointer (Data)),
                         P,
                         Time,
                         Surface_X,
                         Surface_Y);
      end Internal_Pointer_Motion;

      procedure Internal_Pointer_Button
        (Data    : Void_Ptr;
         Pointer : Thin.Pointer_Ptr;
         Serial  : Unsigned_32;
         Time    : Unsigned_32;
         Button  : Unsigned_32;
         State   : Pointer_Button_State)
      is
         P : constant Protocol.Pointer := (My_Pointer => Pointer);
      begin
         Pointer_Button (Data_Ptr (Conversion.To_Pointer (Data)),
                         P,
                         Serial,
                         Time,
                         Button,
                         State);
      end Internal_Pointer_Button;

      procedure Internal_Pointer_Axis
        (Data    : Void_Ptr;
         Pointer : Thin.Pointer_Ptr;
         Time    : Unsigned_32;
         Axis    : Pointer_Axis;
         Value   : Fixed)
      is
         P : constant Protocol.Pointer := (My_Pointer => Pointer);
      begin
         Pointer_Scroll (Data_Ptr (Conversion.To_Pointer (Data)),
                       P,
                       Time,
                       Axis,
                       Value);
      end Internal_Pointer_Axis;

      procedure Internal_Pointer_Frame (Data    : Void_Ptr;
                                        Pointer : Thin.Pointer_Ptr)
      is
         P : constant Protocol.Pointer := (My_Pointer => Pointer);
      begin
         Pointer_Frame (Data_Ptr (Conversion.To_Pointer (Data)), P);
      end Internal_Pointer_Frame;

      procedure Internal_Pointer_Axis_Source
        (Data        : Void_Ptr;
         Pointer     : Thin.Pointer_Ptr;
         Axis_Source : Pointer_Axis_Source)
      is
         P : constant Protocol.Pointer := (My_Pointer => Pointer);
      begin
         Pointer_Scroll_Source (Data_Ptr (Conversion.To_Pointer (Data)),
                              P,
                              Axis_Source);
      end Internal_Pointer_Axis_Source;

      procedure Internal_Pointer_Axis_Stop
        (Data    : Void_Ptr;
         Pointer : Thin.Pointer_Ptr;
         Time    : Unsigned_32;
         Axis    : Pointer_Axis)
      is
         P : constant Protocol.Pointer := (My_Pointer => Pointer);
      begin
         Pointer_Scroll_Stop (Data_Ptr (Conversion.To_Pointer (Data)),
                            P,
                            Time,
                            Axis);
      end Internal_Pointer_Axis_Stop;

      procedure Internal_Pointer_Axis_Discrete
        (Data     : Void_Ptr;
         Pointer  : Thin.Pointer_Ptr;
         Axis     : Pointer_Axis;
         Discrete : Integer)
      is
         P : constant Protocol.Pointer := (My_Pointer => Pointer);
      begin
         Pointer_Scroll_Discrete (Data_Ptr (Conversion.To_Pointer (Data)),
                                P,
                                Axis,
                                Discrete);
      end Internal_Pointer_Axis_Discrete;

      Pointer_Listener : aliased Thin.Pointer_Listener_T :=
        (
         Enter         => Internal_Pointer_Enter'Unrestricted_Access,
         Leave         => Internal_Pointer_Leave'Unrestricted_Access,
         Motion        => Internal_Pointer_Motion'Unrestricted_Access,
         Button        => Internal_Pointer_Button'Unrestricted_Access,
         Axis          => Internal_Pointer_Axis'Unrestricted_Access,
         Frame         => Internal_Pointer_Frame'Unrestricted_Access,
         Axis_Source   => Internal_Pointer_Axis_Source'Unrestricted_Access,
         Axis_Stop     => Internal_Pointer_Axis_Stop'Unrestricted_Access,
         Axis_Discrete => Internal_Pointer_Axis_Discrete'Unrestricted_Access
        );

      function Subscribe
        (Pointer : in out Protocol.Pointer;
         Data    : not null Data_Ptr) return Call_Result_Code
      is
         I : int;
      begin
         I := Thin.Pointer_Add_Listener
           (Pointer  => Pointer.My_Pointer,
            Listener => Pointer_Listener'Access,
            Data     => Data.all'Address);
         if I = 0 then
            return Success;
         else
            return Error;
         end if;
      end Subscribe;

   end Pointer_Events;

   package body Keyboard_Events is

      package Conversion is new
        System.Address_To_Access_Conversions (Data_Type);

      procedure Internal_Keymap (Data     : Void_Ptr;
                                 Keyboard : Thin.Keyboard_Ptr;
                                 Format   : Keyboard_Keymap_Format;
                                 Fd       : Integer;
                                 Size     : Unsigned_32) with
        Convention => C;

      procedure Internal_Enter (Data     : Void_Ptr;
                                Keyboard : Thin.Keyboard_Ptr;
                                Serial   : Unsigned_32;
                                Surface  : Thin.Surface_Ptr;
                                Keys     : Wayland_Array_T) with
        Convention => C;

      procedure Internal_Leave (Data     : Void_Ptr;
                                Keyboard : Thin.Keyboard_Ptr;
                                Serial   : Unsigned_32;
                                Surface  : Thin.Surface_Ptr) with
        Convention => C;

      procedure Internal_Key (Data     : Void_Ptr;
                              Keyboard : Thin.Keyboard_Ptr;
                              Serial   : Unsigned_32;
                              Time     : Unsigned_32;
                              Key      : Unsigned_32;
                              State    : Keyboard_Key_State) with
        Convention => C;

      procedure Internal_Modifiers (Data           : Void_Ptr;
                                    Keyboard       : Thin.Keyboard_Ptr;
                                    Serial         : Unsigned_32;
                                    Mods_Depressed : Unsigned_32;
                                    Mods_Latched   : Unsigned_32;
                                    Mods_Locked    : Unsigned_32;
                                    Group          : Unsigned_32) with
        Convention => C;

      procedure Internal_Repeat_Info (Data     : Void_Ptr;
                                      Keyboard : Thin.Keyboard_Ptr;
                                      Rate     : Integer;
                                      Delay_V  : Integer) with
        Convention => C;

      procedure Internal_Keymap (Data     : Void_Ptr;
                                 Keyboard : Thin.Keyboard_Ptr;
                                 Format   : Keyboard_Keymap_Format;
                                 Fd       : Integer;
                                 Size     : Unsigned_32)
      is
         K : constant Protocol.Keyboard := (My_Keyboard => Keyboard);
      begin
         Keymap (Data_Ptr (Conversion.To_Pointer (Data)),
                 K,
                 Format,
                 Fd,
                 Size);
      end Internal_Keymap;

      procedure Internal_Enter (Data     : Void_Ptr;
                                Keyboard : Thin.Keyboard_Ptr;
                                Serial   : Unsigned_32;
                                Surface  : Thin.Surface_Ptr;
                                Keys     : Wayland_Array_T)
      is
         K : constant Protocol.Keyboard := (My_Keyboard => Keyboard);
         S : constant Protocol.Surface  := (My_Surface => Surface);
      begin
         Enter (Data_Ptr (Conversion.To_Pointer (Data)),
                K,
                Serial,
                S,
                Keys);
      end Internal_Enter;

      procedure Internal_Leave (Data     : Void_Ptr;
                                Keyboard : Thin.Keyboard_Ptr;
                                Serial   : Unsigned_32;
                                Surface  : Thin.Surface_Ptr)
      is
         K : constant Protocol.Keyboard := (My_Keyboard => Keyboard);
         S : constant Protocol.Surface  := (My_Surface => Surface);
      begin
         Leave (Data_Ptr (Conversion.To_Pointer (Data)),
                K,
                Serial,
                S);
      end Internal_Leave;

      procedure Internal_Key (Data     : Void_Ptr;
                              Keyboard : Thin.Keyboard_Ptr;
                              Serial   : Unsigned_32;
                              Time     : Unsigned_32;
                              Key      : Unsigned_32;
                              State    : Keyboard_Key_State)
      is
         K : constant Protocol.Keyboard := (My_Keyboard => Keyboard);
      begin
         Keyboard_Events.Key (Data_Ptr (Conversion.To_Pointer (Data)),
                              K,
                              Serial,
                              Time,
                              Key,
                              State);
      end Internal_Key;

      procedure Internal_Modifiers (Data           : Void_Ptr;
                                    Keyboard       : Thin.Keyboard_Ptr;
                                    Serial         : Unsigned_32;
                                    Mods_Depressed : Unsigned_32;
                                    Mods_Latched   : Unsigned_32;
                                    Mods_Locked    : Unsigned_32;
                                    Group          : Unsigned_32)
      is
         K : constant Protocol.Keyboard := (My_Keyboard => Keyboard);
      begin
         Modifiers (Data_Ptr (Conversion.To_Pointer (Data)),
                    K,
                    Serial,
                    Mods_Depressed,
                    Mods_Latched,
                    Mods_Locked,
                    Group);
      end Internal_Modifiers;

      procedure Internal_Repeat_Info (Data     : Void_Ptr;
                                      Keyboard : Thin.Keyboard_Ptr;
                                      Rate     : Integer;
                                      Delay_V  : Integer)
      is
         K : constant Protocol.Keyboard := (My_Keyboard => Keyboard);
      begin
         Repeat_Info (Data_Ptr (Conversion.To_Pointer (Data)),
                      K,
                      Rate,
                      Delay_V);
      end Internal_Repeat_Info;

      Listener : aliased Thin.Keyboard_Listener_T
        := (Keymap      => Internal_Keymap'Unrestricted_Access,
            Enter       => Internal_Enter'Unrestricted_Access,
            Leave       => Internal_Leave'Unrestricted_Access,
            Key         => Internal_Key'Unrestricted_Access,
            Modifiers   => Internal_Modifiers'Unrestricted_Access,
            Repeat_Info => Internal_Repeat_Info'Unrestricted_Access);

      function Subscribe
        (Keyboard : in out Protocol.Keyboard;
         Data     : not null Data_Ptr) return Call_Result_Code
      is
         I : int;
      begin
         I := Thin.Keyboard_Add_Listener
           (Keyboard => Keyboard.My_Keyboard,
            Listener => Listener'Access,
            Data     => Data.all'Address);
         if I = 0 then
            return Success;
         else
            return Error;
         end if;
      end Subscribe;

   end Keyboard_Events;

   package body Touch_Events is

      package Conversion is new
        System.Address_To_Access_Conversions (Data_Type);

      procedure Internal_Down (Data    : Void_Ptr;
                               Touch   : Thin.Touch_Ptr;
                               Serial  : Unsigned_32;
                               Time    : Unsigned_32;
                               Surface : Thin.Surface_Ptr;
                               Id      : Integer;
                               X       : Fixed;
                               Y       : Fixed) with
        Convention => C;

      procedure Internal_Up (Data   : Void_Ptr;
                             Touch  : Thin.Touch_Ptr;
                             Serial : Unsigned_32;
                             Time   : Unsigned_32;
                             Id     : Integer) with
        Convention => C;

      procedure Internal_Motion (Data  : Void_Ptr;
                                 Touch : Thin.Touch_Ptr;
                                 Time  : Unsigned_32;
                                 Id    : Integer;
                                 X     : Fixed;
                                 Y     : Fixed) with
        Convention => C;

      procedure Internal_Frame (Data  : Void_Ptr;
                                Touch : Thin.Touch_Ptr) with
        Convention => C;

      procedure Internal_Cancel (Data  : Void_Ptr;
                                 Touch : Thin.Touch_Ptr) with
        Convention => C;

      procedure Internal_Shape (Data  : Void_Ptr;
                                Touch : Thin.Touch_Ptr;
                                Id    : Integer;
                                Major : Fixed;
                                Minor : Fixed) with
        Convention => C;

      procedure Internal_Orientation (Data        : Void_Ptr;
                                      Touch       : Thin.Touch_Ptr;
                                      Id          : Integer;
                                      Orientation : Fixed) with
        Convention => C;

      procedure Internal_Down (Data    : Void_Ptr;
                               Touch   : Thin.Touch_Ptr;
                               Serial  : Unsigned_32;
                               Time    : Unsigned_32;
                               Surface : Thin.Surface_Ptr;
                               Id      : Integer;
                               X       : Fixed;
                               Y       : Fixed)
      is
         T : constant Protocol.Touch := (My_Touch => Touch);
         S : constant Protocol.Surface := (My_Surface => Surface);
      begin
         Down (Data_Ptr (Conversion.To_Pointer (Data)),
               T,
               Serial,
               Time,
               S,
               Id,
               X,
               Y);
      end Internal_Down;

      procedure Internal_Up (Data   : Void_Ptr;
                             Touch  : Thin.Touch_Ptr;
                             Serial : Unsigned_32;
                             Time   : Unsigned_32;
                             Id     : Integer)
      is
         T : constant Protocol.Touch := (My_Touch => Touch);
      begin
         Up (Data_Ptr (Conversion.To_Pointer (Data)),
             T,
             Serial,
             Time,
             Id);
      end Internal_Up;

      procedure Internal_Motion (Data  : Void_Ptr;
                                 Touch : Thin.Touch_Ptr;
                                 Time  : Unsigned_32;
                                 Id    : Integer;
                                 X     : Fixed;
                                 Y     : Fixed)
      is
         T : constant Protocol.Touch := (My_Touch => Touch);
      begin
         Motion (Data_Ptr (Conversion.To_Pointer (Data)),
                 T,
                 Time,
                 Id,
                 X,
                 Y);
      end Internal_Motion;

      procedure Internal_Frame (Data  : Void_Ptr;
                                Touch : Thin.Touch_Ptr)
      is
         T : constant Protocol.Touch := (My_Touch => Touch);
      begin
         Frame (Data_Ptr (Conversion.To_Pointer (Data)), T);
      end Internal_Frame;

      procedure Internal_Cancel (Data  : Void_Ptr;
                                 Touch : Thin.Touch_Ptr)
      is
         T : constant Protocol.Touch := (My_Touch => Touch);
      begin
         Cancel (Data_Ptr (Conversion.To_Pointer (Data)), T);
      end Internal_Cancel;

      procedure Internal_Shape (Data  : Void_Ptr;
                                Touch : Thin.Touch_Ptr;
                                Id    : Integer;
                                Major : Fixed;
                                Minor : Fixed)
      is
         T : constant Protocol.Touch := (My_Touch => Touch);
      begin
         Shape (Data_Ptr (Conversion.To_Pointer (Data)),
                T,
                Id,
                Major,
                Minor);
      end Internal_Shape;

      procedure Internal_Orientation (Data        : Void_Ptr;
                                      Touch       : Thin.Touch_Ptr;
                                      Id          : Integer;
                                      Orientation : Fixed)
      is
         T : constant Protocol.Touch := (My_Touch => Touch);
      begin
         Touch_Events.Orientation (Data_Ptr (Conversion.To_Pointer (Data)),
                                   T,
                                   Id,
                                   Orientation);
      end Internal_Orientation;

      Listener : aliased Thin.Touch_Listener_T
        := (Down        => Internal_Down'Unrestricted_Access,
            Up          => Internal_Up'Unrestricted_Access,
            Motion      => Internal_Motion'Unrestricted_Access,
            Frame       => Internal_Frame'Unrestricted_Access,
            Cancel      => Internal_Cancel'Unrestricted_Access,
            Shape       => Internal_Shape'Unrestricted_Access,
            Orientation => Internal_Orientation'Unrestricted_Access);

      function Subscribe
        (Touch : in out Protocol.Touch;
         Data  : not null Data_Ptr) return Call_Result_Code
      is
         I : int;
      begin
         I := Thin.Touch_Add_Listener (Touch    => Touch.My_Touch,
                                          Listener => Listener'Access,
                                          Data     => Data.all'Address);
         if I = 0 then
            return Success;
         else
            return Error;
         end if;
      end Subscribe;

   end Touch_Events;

   package body Output_Events is

      package Conversion is new
        System.Address_To_Access_Conversions (Data_Type);

      procedure Internal_Geometry (Data            : Void_Ptr;
                                   Output          : Thin.Output_Ptr;
                                   X               : Integer;
                                   Y               : Integer;
                                   Physical_Width  : Integer;
                                   Physical_Height : Integer;
                                   Subpixel        : Output_Subpixel;
                                   Make            : chars_ptr;
                                   Model           : chars_ptr;
                                   Transform       : Output_Transform) with
        Convention => C;

      procedure Internal_Mode (Data    : Void_Ptr;
                               Output  : Thin.Output_Ptr;
                               Flags   : Output_Mode;
                               Width   : Integer;
                               Height  : Integer;
                               Refresh : Integer) with
        Convention => C;

      procedure Internal_Done (Data   : Void_Ptr;
                               Output : Thin.Output_Ptr) with
        Convention => C;

      procedure Internal_Scale (Data   : Void_Ptr;
                                Output : Thin.Output_Ptr;
                                Factor : Integer) with
        Convention => C;

      procedure Internal_Geometry (Data            : Void_Ptr;
                                   Output          : Thin.Output_Ptr;
                                   X               : Integer;
                                   Y               : Integer;
                                   Physical_Width  : Integer;
                                   Physical_Height : Integer;
                                   Subpixel        : Output_Subpixel;
                                   Make            : chars_ptr;
                                   Model           : chars_ptr;
                                   Transform       : Output_Transform)
      is
         O : constant Protocol.Output := (My_Output => Output);
         Ma : constant String := Interfaces.C.Strings.Value (Make);
         Mo : constant String := Interfaces.C.Strings.Value (Model);
      begin
         Geometry (Data_Ptr (Conversion.To_Pointer (Data)),
                   O,
                   X,
                   Y,
                   Physical_Width,
                   Physical_Height,
                   Subpixel,
                   Ma,
                   Mo,
                   Transform);
      end Internal_Geometry;

      procedure Internal_Mode (Data    : Void_Ptr;
                               Output  : Thin.Output_Ptr;
                               Flags   : Output_Mode;
                               Width   : Integer;
                               Height  : Integer;
                               Refresh : Integer)
      is
         O : constant Protocol.Output := (My_Output => Output);
      begin
         Mode (Data_Ptr (Conversion.To_Pointer (Data)),
               O,
               Flags,
               Width,
               Height,
               Refresh);
      end Internal_Mode;

      procedure Internal_Done (Data   : Void_Ptr;
                               Output : Thin.Output_Ptr)
      is
         O : constant Protocol.Output := (My_Output => Output);
      begin
         Done (Data_Ptr (Conversion.To_Pointer (Data)), O);
      end Internal_Done;

      procedure Internal_Scale (Data   : Void_Ptr;
                                Output : Thin.Output_Ptr;
                                Factor : Integer)
      is
         O : constant Protocol.Output := (My_Output => Output);
      begin
         Scale (Data_Ptr (Conversion.To_Pointer (Data)), O, Factor);
      end Internal_Scale;

      Listener : aliased Thin.Output_Listener_T
        := (Geometry => Internal_Geometry'Unrestricted_Access,
            Mode     => Internal_Mode'Unrestricted_Access,
            Done     => Internal_Done'Unrestricted_Access,
            Scale    => Internal_Scale'Unrestricted_Access);

      function Subscribe
        (Output : in out Protocol.Output;
         Data   : not null Data_Ptr) return Call_Result_Code
      is
         I : int;
      begin
         I := Thin.Output_Add_Listener (Output   => Output.My_Output,
                                           Listener => Listener'Access,
                                           Data     => Data.all'Address);
         if I = 0 then
            return Success;
         else
            return Error;
         end if;
      end Subscribe;

   end Output_Events;

   procedure Connect (Display : in out Protocol.Display;
                      Name    : String := Default_Display_Name) is
   begin
      Display.My_Display := Thin.Display_Connect (+Name);
      if Display.My_Display /= null then
         Display.My_Fd :=
           Wayland.API.Display_Get_File_Descriptor (Display.My_Display);
         if Display.My_Fd = -1 then
            Display.Disconnect;
            Display.My_Display := null;
         end if;
      end if;
   end Connect;

   procedure Disconnect (Display : in out Protocol.Display) is
   begin
      if Display.My_Display /= null then
         Thin.Display_Disconnect (Display.My_Display);
      end if;
   end Disconnect;

   function Check_For_Events
     (Display : Protocol.Display;
      Timeout : Integer) return Check_For_Events_Status
   is
      I : constant Integer := C_Binding.Linux.Poll_File_Descriptor_Until_Timeout (Display.My_Fd, Timeout);
   begin
      case I is
         when 1..Integer'Last   => return Events_Need_Processing;
         when 0                 => return No_Events;
         when Integer'First..-1 => return Error;
      end case;
   end Check_For_Events;

   function Get_Version
     (Display : Protocol.Display) return Unsigned_32 is
   begin
      return Thin.Display_Get_Version (Display.My_Display);
   end Get_Version;

   procedure Get_Registry (Display  : Protocol.Display;
                           Registry : in out Protocol.Registry) is
   begin
      Registry.My_Registry
        := Thin.Display_Get_Registry (Display.My_Display);
   end Get_Registry;

   procedure Destroy (Registry : in out Protocol.Registry) is
   begin
      if Registry.My_Registry /= null then
         Thin.Registry_Destroy (Registry.My_Registry);
         Registry.My_Registry := null;
      end if;
   end Destroy;

   function Dispatch (Display : Protocol.Display) return Integer is
   begin
      return Integer (Wayland.API.Display_Dispatch (Display.My_Display));
   end Dispatch;

   function Dispatch_Pending
     (Display : Protocol.Display) return Integer is
   begin
      return Wayland.API.Display_Dispatch_Pending (Display.My_Display);
   end Dispatch_Pending;

   function Prepare_Read
     (Display : Protocol.Display) return Integer is
   begin
      return Wayland.API.Display_Prepare_Read (Display.My_Display);
   end Prepare_Read;

   function Read_Events
     (Display : Protocol.Display) return Call_Result_Code
   is
      I : constant Integer
        := Wayland.API.Display_Read_Events (Display.My_Display);
   begin
      if I = 0 then
         return Success;
      else
         return Error;
      end if;
   end Read_Events;

   procedure Cancel_Read (Display : Protocol.Display) is
   begin
      Wayland.API.Display_Cancel_Read (Display.My_Display);
   end Cancel_Read;

   procedure Dispatch (Display : Protocol.Display) is
      I : Integer;
      pragma Unreferenced (I);
   begin
      I := Display.Dispatch;
   end Dispatch;

   function Roundtrip (Display : Protocol.Display) return Integer is
   begin
      return Integer (Wayland.API.Display_Roundtrip (Display.My_Display));
   end Roundtrip;

   procedure Roundtrip (Display : Protocol.Display) is
      I : Integer;
      pragma Unreferenced (I);
   begin
      I := Display.Roundtrip;
   end Roundtrip;

   procedure Get_Proxy (Compositor  : in out Protocol.Compositor;
                        Registry    : Protocol.Registry;
                        Id          : Unsigned_32;
                        Version     : Unsigned_32)
   is
      P : constant Thin.Proxy_Ptr :=
            Thin.Registry_Bind
              (Registry    => Registry.My_Registry,
               Name        => Id,
               Interface_V => Thin.Compositor_Interface'Access,
               New_Id      => Version);

   begin
      if P /= null then
         Compositor.My_Compositor := P.all'Access;
      end if;
   end Get_Proxy;

   procedure Get_Surface_Proxy (Compositor : Protocol.Compositor;
                             Surface    : in out Protocol.Surface) is
   begin
      Surface.My_Surface :=
        Thin.Compositor_Create_Surface (Compositor.My_Compositor);
   end Get_Surface_Proxy;

   procedure Get_Region_Proxy (Compositor : Protocol.Compositor;
                               Region     : in out Protocol.Region) is
   begin
      Region.My_Region :=
        Thin.Compositor_Create_Region (Compositor.My_Compositor);
   end Get_Region_Proxy;

   procedure Destroy (Compositor : in out Protocol.Compositor) is
   begin
      if Compositor.My_Compositor /= null then
         Thin.Compositor_Destroy (Compositor.My_Compositor);
         Compositor.My_Compositor := null;
      end if;
   end Destroy;

   function Get_Version (Seat : Protocol.Seat) return Unsigned_32 is
   begin
      return Thin.Seat_Get_Version (Seat.My_Seat);
   end Get_Version;

   procedure Get_Proxy (Seat     : in out Protocol.Seat;
                        Registry : Protocol.Registry;
                        Id       : Unsigned_32;
                        Version  : Unsigned_32)
   is
      P : constant Thin.Proxy_Ptr :=
        Thin.Registry_Bind (Registry    => Registry.My_Registry,
                               Name        => Id,
                               Interface_V => Thin.Seat_Interface'Access,
                               New_Id      => Version);

   begin
      if P /= null then
         Seat.My_Seat := P.all'Access;
      end if;
   end Get_Proxy;

   procedure Get_Pointer (Seat    : Protocol.Seat;
                          Pointer : in out Protocol.Pointer) is
   begin
      Pointer.My_Pointer := Thin.Seat_Get_Pointer (Seat.My_Seat);
   end Get_Pointer;

   procedure Get_Keyboard (Seat     : Protocol.Seat;
                           Keyboard : in out Protocol.Keyboard) is
   begin
      Keyboard.My_Keyboard := Thin.Seat_Get_Keyboard (Seat.My_Seat);
   end Get_Keyboard;

   procedure Get_Touch (Seat  : Protocol.Seat;
                        Touch : in out Protocol.Touch) is
   begin
      Touch.My_Touch := Thin.Seat_Get_Touch (Seat.My_Seat);
   end Get_Touch;

   procedure Release (Seat : in out Protocol.Seat) is
   begin
      Thin.Seat_Release (Seat.My_Seat);
      Seat.My_Seat := null;
   end Release;

   procedure Get_Proxy (Shm      : in out Protocol.Shm;
                        Registry : Protocol.Registry;
                        Id       : Unsigned_32;
                        Version  : Unsigned_32)
   is
      P : constant Thin.Proxy_Ptr :=
        Thin.Registry_Bind (Registry    => Registry.My_Registry,
                               Name        => Id,
                               Interface_V => Thin.Shm_Interface'Access,
                               New_Id      => Version);

   begin
      if P /= null then
         Shm.My_Shm := P.all'Access;
      end if;
   end Get_Proxy;

   procedure Create_Pool
     (Shm             : Protocol.Shm;
      File_Descriptor : C_Binding.Linux.Files.File;
      Size            : Integer;
      Pool            : in out Protocol.Shm_Pool) is
   begin
      Pool.My_Shm_Pool := Thin.Shm_Create_Pool
        (Shm.My_Shm,
         C_Binding.Linux.Files.File_Descriptor (File_Descriptor),
         Size);
   end Create_Pool;

   function Get_Version (Shm : Protocol.Shm) return Unsigned_32 is
   begin
      return Thin.Shm_Get_Version (Shm.My_Shm);
   end Get_Version;

   procedure Destroy (Shm : in out Protocol.Shm) is
   begin
      if Shm.My_Shm /= null then
         Thin.Shm_Destroy (Shm.My_Shm);
         Shm.My_Shm := null;
      end if;
   end Destroy;

   procedure Create_Buffer (Pool   : Protocol.Shm_Pool;
                            Offset   : Integer;
                            Width    : Integer;
                            Height   : Integer;
                            Stride   : Integer;
                            Format   : Shm_Format;
                            Buffer : in out Protocol.Buffer) is
   begin
      Buffer.My_Buffer := Thin.Shm_Pool_Create_Buffer (Pool.My_Shm_Pool,
                                                          Offset,
                                                          Width,
                                                          Height,
                                                          Stride,
                                                          Format);
   end Create_Buffer;

   procedure Resize (Pool : Protocol.Shm_Pool;
                     Size : Integer) is
   begin
      Thin.Shm_Pool_Resize (Pool.My_Shm_Pool, Size);
   end Resize;

   function Get_Version
     (Pool : Protocol.Shm_Pool) return Unsigned_32 is
   begin
      return Thin.Shm_Pool_Get_Version (Pool.My_Shm_Pool);
   end Get_Version;

   procedure Destroy (Pool : in out Protocol.Shm_Pool) is
   begin
      if Pool.My_Shm_Pool /= null then
         Thin.Shm_Pool_Destroy (Pool.My_Shm_Pool);
         Pool.My_Shm_Pool := null;
      end if;
   end Destroy;

   function Get_Version
     (Buffer : Protocol.Buffer) return Unsigned_32 is
   begin
      return Thin.Buffer_Get_Version (Buffer.My_Buffer);
   end Get_Version;

   procedure Destroy (Buffer : in out Protocol.Buffer) is
   begin
      if Buffer.My_Buffer /= null then
         Thin.Buffer_Destroy (Buffer.My_Buffer);
         Buffer.My_Buffer := null;
      end if;
   end Destroy;

   function Has_Proxy (Offer : Protocol.Data_Offer) return Boolean is
      (Offer.My_Data_Offer /= null);

   function Get_Version
     (Offer : Protocol.Data_Offer) return Unsigned_32 is
   begin
      return Thin.Data_Offer_Get_Version (Offer.My_Data_Offer);
   end Get_Version;

   procedure Destroy (Offer : in out Protocol.Data_Offer) is
   begin
      if Offer.My_Data_Offer /= null then
         Thin.Data_Offer_Destroy (Offer.My_Data_Offer);
         Offer.My_Data_Offer := null;
      end if;
   end Destroy;

   procedure Do_Accept (Offer     : Data_Offer;
                        Serial    : Unsigned_32;
                        Mime_Type : String) is
   begin
      Wayland.API.Proxy_Marshal
        (Wayland.API.Proxy (Offer.My_Data_Offer.all),
         Constants.Data_Offer_Accept,
         Serial,
         +Mime_Type);
   end Do_Accept;

   procedure Do_Not_Accept (Offer  : Data_Offer;
                            Serial : Unsigned_32) is
   begin
      Thin.Data_Offer_Accept (Offer.My_Data_Offer,
                                 Serial,
                                 Interfaces.C.Strings.Null_Ptr);
   end Do_Not_Accept;

   procedure Receive (Offer           : Data_Offer;
                      Mime_Type       : String;
                      File_Descriptor : Integer) is
   begin
      Thin.Data_Offer_Receive
        (Offer.My_Data_Offer, +Mime_Type, File_Descriptor);
   end Receive;

   procedure Finish (Offer : Data_Offer) is
   begin
      Thin.Data_Offer_Finish (Offer.My_Data_Offer);
   end Finish;

   procedure Set_Actions (Offer            : Data_Offer;
                          Dnd_Actions      : Unsigned_32;
                          Preferred_Action : Unsigned_32) is
   begin
      Thin.Data_Offer_Set_Actions (Offer.My_Data_Offer,
                                      Dnd_Actions,
                                      Preferred_Action);
   end Set_Actions;

   procedure Attach (Surface : Protocol.Surface;
                     Buffer  : Protocol.Buffer;
                     X       : Integer;
                     Y       : Integer) is
   begin
      Thin.Surface_Attach (Surface.My_Surface, Buffer.My_Buffer, X, Y);
   end Attach;

   procedure Damage (Surface : Protocol.Surface;
                     X       : Integer;
                     Y       : Integer;
                     Width   : Integer;
                     Height  : Integer) is
   begin
      Thin.Surface_Damage (Surface.My_Surface, X, Y, Width, Height);
   end Damage;

   function Frame (Surface : Protocol.Surface) return Callback is
   begin
      return C : Callback do
         C.My_Callback := Thin.Surface_Frame (Surface.My_Surface);
      end return;
   end Frame;

   procedure Set_Opaque_Region (Surface : Protocol.Surface;
                                Region  : Protocol.Region) is
   begin
      Thin.Surface_Set_Opaque_Region (Surface.My_Surface,
                                         Region.My_Region);
   end Set_Opaque_Region;

   procedure Set_Input_Region (Surface : Protocol.Surface;
                               Region  : Protocol.Region) is
   begin
      Thin.Surface_Set_Input_Region (Surface.My_Surface,
                                        Region.My_Region);

   end Set_Input_Region;

   procedure Commit (Surface : Protocol.Surface) is
   begin
      Thin.Surface_Commit (Surface.My_Surface);
   end Commit;

   procedure Set_Buffer_Transform (Surface   : Protocol.Surface;
                                   Transform : Output_Transform) is
   begin
      Thin.Surface_Set_Buffer_Transform (Surface.My_Surface, Transform);
   end Set_Buffer_Transform;

   procedure Set_Buffer_Scale (Surface : Protocol.Surface;
                               Scale   : Integer) is
   begin
      Thin.Surface_Set_Buffer_Scale (Surface.My_Surface,
                                        Scale);
   end Set_Buffer_Scale;

   procedure Damage_Buffer (Surface : Protocol.Surface;
                            X       : Integer;
                            Y       : Integer;
                            Width   : Integer;
                            Height  : Integer) is
   begin
      Thin.Surface_Damage_Buffer
        (Surface.My_Surface, X, Y, Width, Height);
   end Damage_Buffer;

   procedure Destroy (Surface : in out Protocol.Surface) is
   begin
      if Surface.My_Surface /= null then
         Thin.Surface_Destroy (Surface.My_Surface);
         Surface.My_Surface := null;
      end if;
   end Destroy;

   function Sync (Display : Protocol.Display) return Callback is
   begin
      return Callback : Protocol.Callback do
         Callback.My_Callback := Thin.Display_Sync (Display.My_Display);
      end return;
   end Sync;

   function Get_Version
     (Registry : Protocol.Registry) return Unsigned_32 is
   begin
      return Thin.Registry_Get_Version (Registry.My_Registry);
   end Get_Version;

   function Has_Proxy (Callback : Protocol.Callback) return Boolean is
      (Callback.My_Callback /= null);

   procedure Destroy (Callback : in out Protocol.Callback) is
   begin
      if Callback.My_Callback /= null then
         Thin.Callback_Destroy (Callback.My_Callback);
         Callback.My_Callback := null;
      end if;
   end Destroy;

   function Get_Version
     (Callback : Protocol.Callback) return Unsigned_32 is
   begin
      return Thin.Callback_Get_Version (Callback.My_Callback);
   end Get_Version;

   function Get_Version
     (Pointer : Protocol.Pointer) return Unsigned_32 is
   begin
      return Thin.Pointer_Get_Version (Pointer.My_Pointer);
   end Get_Version;

   procedure Destroy (Pointer : in out Protocol.Pointer) is
   begin
      if Pointer.My_Pointer /= null then
         Thin.Pointer_Destroy (Pointer.My_Pointer);
         Pointer.My_Pointer := null;
      end if;
   end Destroy;

   procedure Set_Cursor (Pointer   : Protocol.Pointer;
                         Serial    : Unsigned_32;
                         Surface   : Protocol.Surface;
                         Hotspot_X : Integer;
                         Hotspot_Y : Integer) is
   begin
      Thin.Pointer_Set_Cursor (Pointer.My_Pointer,
                                  Serial,
                                  Surface.My_Surface,
                                  Hotspot_X,
                                  Hotspot_Y);
   end Set_Cursor;

   procedure Release (Pointer : in out Protocol.Pointer) is
   begin
      if Pointer.My_Pointer /= null then
         Thin.Pointer_Release (Pointer.My_Pointer);
         Pointer.My_Pointer := null;
      end if;
   end Release;

   function Get_Version
     (Keyboard : Protocol.Keyboard) return Unsigned_32 is
   begin
      return Thin.Keyboard_Get_Version (Keyboard.My_Keyboard);
   end Get_Version;

   procedure Destroy (Keyboard : in out Protocol.Keyboard) is
   begin
      if Keyboard.My_Keyboard /= null then
         Thin.Keyboard_Destroy (Keyboard.My_Keyboard);
         Keyboard.My_Keyboard := null;
      end if;
   end Destroy;

   procedure Release (Keyboard : in out Protocol.Keyboard) is
   begin
      if Keyboard.My_Keyboard /= null then
         Thin.Keyboard_Release (Keyboard.My_Keyboard);
         Keyboard.My_Keyboard := null;
      end if;
   end Release;

   function Get_Version (Touch : Protocol.Touch) return Unsigned_32 is
   begin
      return Thin.Touch_Get_Version (Touch.My_Touch);
   end Get_Version;

   procedure Destroy (Touch : in out Protocol.Touch) is
   begin
      if Touch.My_Touch /= null then
         Thin.Touch_Destroy (Touch.My_Touch);
         Touch.My_Touch := null;
      end if;
   end Destroy;

   procedure Release (Touch : in out Protocol.Touch) is
   begin
      if Touch.My_Touch /= null then
         Thin.Touch_Release (Touch.My_Touch);
         Touch.My_Touch := null;
      end if;
   end Release;

   function Get_Version
     (Output : Protocol.Output) return Unsigned_32 is
   begin
      return Thin.Output_Get_Version (Output.My_Output);
   end Get_Version;

   procedure Destroy (Output : in out Protocol.Output) is
   begin
      if Output.My_Output /= null then
         Thin.Output_Destroy (Output.My_Output);
         Output.My_Output := null;
      end if;
   end Destroy;

   procedure Release (Output : in out Protocol.Output) is
   begin
      if Output.My_Output /= null then
         Thin.Output_Release (Output.My_Output);
         Output.My_Output := null;
      end if;
   end Release;

   function Get_Version
     (Region : Protocol.Region) return Unsigned_32 is
   begin
      return Thin.Region_Get_Version (Region.My_Region);
   end Get_Version;

   procedure Destroy (Region : in out Protocol.Region) is
   begin
      if Region.My_Region /= null then
         Thin.Region_Destroy (Region.My_Region);
         Region.My_Region := null;
      end if;
   end Destroy;

   procedure Add (Region : Protocol.Region;
                  X      : Integer;
                  Y      : Integer;
                  Width  : Integer;
                  Height : Integer) is
   begin
      Thin.Region_Add (Region.My_Region, X, Y, Width, Height);
   end Add;

   procedure Subtract (Region : Protocol.Region;
                       X      : Integer;
                       Y      : Integer;
                       Width  : Integer;
                       Height : Integer) is
   begin
      Thin.Region_Subtract (Region.My_Region, X, Y, Width, Height);
   end Subtract;

   function Get_Version
     (S : Protocol.Subcompositor) return Unsigned_32 is
   begin
      return Thin.Subcompositor_Get_Version (S.My_Subcompositor);
   end Get_Version;

   procedure Destroy (S : in out Protocol.Subcompositor) is
   begin
      if S.My_Subcompositor /= null then
         Thin.Subcompositor_Destroy (S.My_Subcompositor);
         S.My_Subcompositor := null;
      end if;
   end Destroy;

   procedure Get_Subsurface
     (Subcompositor : Protocol.Subcompositor;
      Surface       : Protocol.Surface;
      Parent        : Protocol.Surface;
      Subsurface    : in out Protocol.Subsurface) is
   begin
      Subsurface.My_Subsurface :=
        Thin.Subcompositor_Get_Subsurface
          (Subcompositor.My_Subcompositor,
           Surface.My_Surface,
           Parent.My_Surface);
   end Get_Subsurface;

   function Get_Version
     (Subsurface : Protocol.Subsurface) return Unsigned_32 is
   begin
      return Thin.Subsurface_Get_Version (Subsurface.My_Subsurface);
   end Get_Version;

   procedure Destroy (Subsurface : in out Protocol.Subsurface) is
   begin
      if Subsurface.My_Subsurface /= null then
         Thin.Subsurface_Destroy (Subsurface.My_Subsurface);
         Subsurface.My_Subsurface := null;
      end if;
   end Destroy;

   procedure Set_Position (Subsurface : Protocol.Subsurface;
                           X          : Integer;
                           Y          : Integer) is
   begin
      Thin.Subsurface_Set_Position (Subsurface.My_Subsurface,
                                       X,
                                       Y);
   end Set_Position;

   procedure Place_Above (Subsurface : Protocol.Subsurface;
                          Sibling    : Protocol.Surface) is
   begin
      Thin.Subsurface_Place_Above (Subsurface.My_Subsurface,
                                      Sibling.My_Surface);
   end Place_Above;

   procedure Place_Below (Subsurface : Protocol.Subsurface;
                          Sibling    : Protocol.Surface) is
   begin
      Thin.Subsurface_Place_Below (Subsurface.My_Subsurface,
                                      Sibling.My_Surface);
   end Place_Below;

   procedure Set_Sync (Subsurface : Protocol.Subsurface) is
   begin
      Thin.Subsurface_Set_Sync (Subsurface.My_Subsurface);
   end Set_Sync;

   procedure Set_Desync (Subsurface : Protocol.Subsurface) is
   begin
      Thin.Subsurface_Set_Desync (Subsurface.My_Subsurface);
   end Set_Desync;

   function Get_Version (Source : Data_Source) return Unsigned_32 is
   begin
      return Thin.Data_Source_Get_Version (Source.My_Data_Source);
   end Get_Version;

   procedure Destroy (Source : in out Data_Source) is
   begin
      if Source.My_Data_Source /= null then
         Thin.Data_Source_Destroy (Source.My_Data_Source);
         Source.My_Data_Source := null;
      end if;
   end Destroy;

   procedure Offer (Source    : Data_Source;
                    Mime_Type : String) is
   begin
      Wayland.API.Proxy_Marshal
        (Wayland.API.Proxy (Source.My_Data_Source.all),
         Constants.Data_Source_Offer,
         +Mime_Type);
   end Offer;

   procedure Set_Actions (Source      : Data_Source;
                          Dnd_Actions : Unsigned_32) is
   begin
      Thin.Data_Source_Set_Actions (Source.My_Data_Source,
                                       Dnd_Actions);
   end Set_Actions;

   function Get_Version (Device : Data_Device) return Unsigned_32 is
   begin
      return Thin.Data_Device_Get_Version (Device.My_Data_Device);
   end Get_Version;

   procedure Destroy (Device : in out Data_Device) is
   begin
      if Device.My_Data_Device /= null then
         Thin.Data_Device_Destroy (Device.My_Data_Device);
         Device.My_Data_Device := null;
      end if;
   end Destroy;

   procedure Start_Drag (Device : Data_Device;
                         Source : Data_Source;
                         Origin : Surface;
                         Icon   : Surface;
                         Serial : Unsigned_32) is
   begin
      Thin.Data_Device_Start_Drag (Device.My_Data_Device,
                                      Source.My_Data_Source,
                                      Origin.My_Surface,
                                      Icon.My_Surface,
                                      Serial);
   end Start_Drag;

   procedure Set_Selection (Device : Data_Device;
                            Source : Data_Source;
                            Serial : Unsigned_32) is
   begin
      Thin.Data_Device_Set_Selection (Device.My_Data_Device,
                                         Source.My_Data_Source,
                                         Serial);
   end Set_Selection;

   procedure Release (Device : in out Data_Device) is
   begin
      if Device.My_Data_Device /= null then
         Thin.Data_Device_Release (Device.My_Data_Device);
         Device.My_Data_Device := null;
      end if;
   end Release;

   function Get_Version (Manager : Data_Device_Manager) return Unsigned_32 is
   begin
      return Thin.Data_Device_Manager_Get_Version
        (Manager.My_Data_Device_Manager);
   end Get_Version;

   procedure Destroy (Manager : in out Data_Device_Manager) is
   begin
      if Manager.My_Data_Device_Manager /= null then
         Thin.Data_Device_Manager_Destroy (Manager.My_Data_Device_Manager);
         Manager.My_Data_Device_Manager := null;
      end if;
   end Destroy;

   procedure Create_Data_Source (Manager : Data_Device_Manager;
                                 Source  : in out Data_Source) is
   begin
      Source.My_Data_Source := Thin.Data_Device_Manager_Create_Data_Source
        (Manager.My_Data_Device_Manager);
   end Create_Data_Source;

   procedure Get_Data_Device (Manager : Data_Device_Manager;
                              Seat    : Protocol.Seat;
                              Device  : in out Data_Device) is
   begin
      Device.My_Data_Device := Thin.Data_Device_Manager_Get_Data_Device
        (Manager.My_Data_Device_Manager, Seat.My_Seat);
   end Get_Data_Device;

end Wayland.Client.Protocol;
