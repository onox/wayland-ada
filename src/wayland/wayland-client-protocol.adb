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
               Proxy                  => Display,
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
               Proxy                  => Display,
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
         I := Thin.Display_Add_Listener (Display.Proxy,
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
         R : constant Protocol.Registry := (Proxy => Registry);
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
         R : constant Protocol.Registry := (Proxy => Registry);
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
         I := Thin.Registry_Add_Listener (Registry.Proxy,
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
         C : constant Protocol.Callback := (Proxy => Callback);
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
         I := Thin.Callback_Add_Listener (Callback.Proxy,
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
         S : constant Protocol.Shm := (Proxy => Shm);
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
         I := Thin.Shm_Add_Listener (Shm.Proxy,
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
         B : constant Protocol.Buffer := (Proxy => Buffer);
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
         I := Thin.Buffer_Add_Listener (Buffer.Proxy,
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
           := (Proxy => Data_Offer);

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
           := (Proxy => Data_Offer);
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
           := (Proxy => Data_Offer);
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
         I := Thin.Data_Offer_Add_Listener (Data_Offer.Proxy,
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
         D : constant Protocol.Data_Source := (Proxy => Data_Source);

         M : constant String := Interfaces.C.Strings.Value (Mime_Type);
      begin
         Target (Data_Ptr (Conversion.To_Pointer (Data)), D, M);
      end Internal_Target;

      procedure Internal_Send (Data        : Void_Ptr;
                               Data_Source : Thin.Data_Source_Ptr;
                               Mime_Type   : chars_ptr;
                               Fd          : Integer)
      is
         D : constant Protocol.Data_Source := (Proxy => Data_Source);

         M : constant String := Interfaces.C.Strings.Value (Mime_Type);
      begin
         Send (Data_Ptr (Conversion.To_Pointer (Data)), D, M, Fd);
      end Internal_Send;

      procedure Internal_Cancelled (Data        : Void_Ptr;
                                    Data_Source : Thin.Data_Source_Ptr)
      is
         D : constant Protocol.Data_Source := (Proxy => Data_Source);
      begin
         Cancelled (Data_Ptr (Conversion.To_Pointer (Data)), D);
      end Internal_Cancelled;

      procedure Internal_Dnd_Drop_Performed
        (Data        : Void_Ptr;
         Data_Source : Thin.Data_Source_Ptr)
      is
         D : constant Protocol.Data_Source := (Proxy => Data_Source);
      begin
         Dnd_Drop_Performed (Data_Ptr (Conversion.To_Pointer (Data)), D);
      end Internal_Dnd_Drop_Performed;

      procedure Internal_Dnd_Finished (Data        : Void_Ptr;
                                       Data_Source : Thin.Data_Source_Ptr)
      is
         D : constant Protocol.Data_Source := (Proxy => Data_Source);
      begin
         Dnd_Drop_Performed (Data_Ptr (Conversion.To_Pointer (Data)), D);
      end Internal_Dnd_Finished;

      procedure Internal_Action (Data        : Void_Ptr;
                                 Data_Source : Thin.Data_Source_Ptr;
                                 Dnd_Action  : Unsigned_32)
      is
         D : constant Protocol.Data_Source := (Proxy => Data_Source);
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
         I := Thin.Data_Source_Add_Listener (Data_Source.Proxy,
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
         D : constant Protocol.Data_Device := (Proxy => Data_Device);
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
         D : constant Protocol.Data_Device := (Proxy => Data_Device);
         S : constant Protocol.Surface     := (Proxy => Surface);
         Offer : constant Protocol.Data_Offer := (Proxy => Id);
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
         D : constant Protocol.Data_Device := (Proxy => Data_Device);
      begin
         Leave (Data_Ptr (Conversion.To_Pointer (Data)), D);
      end Internal_Leave;

      procedure Internal_Motion (Data        : Void_Ptr;
                                 Data_Device : Thin.Data_Device_Ptr;
                                 Time        : Unsigned_32;
                                 X           : Fixed;
                                 Y           : Fixed)
      is
         D : constant Protocol.Data_Device := (Proxy => Data_Device);
      begin
         Motion (Data_Ptr (Conversion.To_Pointer (Data)), D, Time, X, Y);
      end Internal_Motion;

      procedure Internal_Drop (Data        : Void_Ptr;
                               Data_Device : Thin.Data_Device_Ptr)
      is
         D : constant Protocol.Data_Device := (Proxy => Data_Device);
      begin
         Drop (Data_Ptr (Conversion.To_Pointer (Data)), D);
      end Internal_Drop;

      procedure Internal_Selection (Data        : Void_Ptr;
                                    Data_Device : Thin.Data_Device_Ptr;
                                    Id          : Thin.Data_Offer_Ptr)
      is
         D : constant Protocol.Data_Device := (Proxy => Data_Device);
         Offer : constant Protocol.Data_Offer := (Proxy => Id);
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
         I := Thin.Data_Device_Add_Listener (Data_Device.Proxy,
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
         S : constant Protocol.Surface := (Proxy => Surface);
         O : constant Protocol.Output := (Proxy => Output);
      begin
         Enter (Data_Ptr (Conversion.To_Pointer (Data)), S, O);
      end Internal_Enter;

      procedure Internal_Leave (Data    : Void_Ptr;
                                Surface : Thin.Surface_Ptr;
                                Output  : Thin.Output_Ptr)
      is
         S : constant Protocol.Surface := (Proxy => Surface);
         O : constant Protocol.Output := (Proxy => Output);
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
         I := Thin.Surface_Add_Listener (Surface.Proxy,
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
         S : constant Protocol.Seat := (Proxy => Seat);
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

         S : constant Protocol.Seat := (Proxy => Seat);
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
           (Seat     => Seat.Proxy,
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
         P : constant Protocol.Pointer := (Proxy => Pointer);
         S : constant Protocol.Surface := (Proxy => Surface);
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
         P : constant Protocol.Pointer := (Proxy => Pointer);
         S : constant Protocol.Surface := (Proxy => Surface);
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
         P : constant Protocol.Pointer := (Proxy => Pointer);
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
         P : constant Protocol.Pointer := (Proxy => Pointer);
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
         P : constant Protocol.Pointer := (Proxy => Pointer);
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
         P : constant Protocol.Pointer := (Proxy => Pointer);
      begin
         Pointer_Frame (Data_Ptr (Conversion.To_Pointer (Data)), P);
      end Internal_Pointer_Frame;

      procedure Internal_Pointer_Axis_Source
        (Data        : Void_Ptr;
         Pointer     : Thin.Pointer_Ptr;
         Axis_Source : Pointer_Axis_Source)
      is
         P : constant Protocol.Pointer := (Proxy => Pointer);
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
         P : constant Protocol.Pointer := (Proxy => Pointer);
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
         P : constant Protocol.Pointer := (Proxy => Pointer);
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
           (Pointer  => Pointer.Proxy,
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
         K : constant Protocol.Keyboard := (Proxy => Keyboard);
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
         K : constant Protocol.Keyboard := (Proxy => Keyboard);
         S : constant Protocol.Surface  := (Proxy => Surface);
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
         K : constant Protocol.Keyboard := (Proxy => Keyboard);
         S : constant Protocol.Surface  := (Proxy => Surface);
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
         K : constant Protocol.Keyboard := (Proxy => Keyboard);
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
         K : constant Protocol.Keyboard := (Proxy => Keyboard);
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
         K : constant Protocol.Keyboard := (Proxy => Keyboard);
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
           (Keyboard => Keyboard.Proxy,
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
         T : constant Protocol.Touch := (Proxy => Touch);
         S : constant Protocol.Surface := (Proxy => Surface);
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
         T : constant Protocol.Touch := (Proxy => Touch);
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
         T : constant Protocol.Touch := (Proxy => Touch);
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
         T : constant Protocol.Touch := (Proxy => Touch);
      begin
         Frame (Data_Ptr (Conversion.To_Pointer (Data)), T);
      end Internal_Frame;

      procedure Internal_Cancel (Data  : Void_Ptr;
                                 Touch : Thin.Touch_Ptr)
      is
         T : constant Protocol.Touch := (Proxy => Touch);
      begin
         Cancel (Data_Ptr (Conversion.To_Pointer (Data)), T);
      end Internal_Cancel;

      procedure Internal_Shape (Data  : Void_Ptr;
                                Touch : Thin.Touch_Ptr;
                                Id    : Integer;
                                Major : Fixed;
                                Minor : Fixed)
      is
         T : constant Protocol.Touch := (Proxy => Touch);
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
         T : constant Protocol.Touch := (Proxy => Touch);
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
         I := Thin.Touch_Add_Listener (Touch    => Touch.Proxy,
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
         O : constant Protocol.Output := (Proxy => Output);
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
         O : constant Protocol.Output := (Proxy => Output);
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
         O : constant Protocol.Output := (Proxy => Output);
      begin
         Done (Data_Ptr (Conversion.To_Pointer (Data)), O);
      end Internal_Done;

      procedure Internal_Scale (Data   : Void_Ptr;
                                Output : Thin.Output_Ptr;
                                Factor : Integer)
      is
         O : constant Protocol.Output := (Proxy => Output);
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
         I := Thin.Output_Add_Listener (Output   => Output.Proxy,
                                           Listener => Listener'Access,
                                           Data     => Data.all'Address);
         if I = 0 then
            return Success;
         else
            return Error;
         end if;
      end Subscribe;

   end Output_Events;

   procedure Connect (Object : in out Display) is
   begin
      Object.Proxy := Thin.Display_Connect;
      if Object.Proxy /= null then
         Object.My_Fd := Wayland.API.Display_Get_File_Descriptor (Object.Proxy);
         if Object.My_Fd = -1 then
            Object.Disconnect;
            Object.Proxy := null;
         end if;
      end if;
   end Connect;

   procedure Disconnect (Object : in out Display) is
   begin
      if Object.Proxy /= null then
         Thin.Display_Disconnect (Object.Proxy);
      end if;
   end Disconnect;

   function Check_For_Events
     (Object  : Display;
      Timeout : Integer) return Check_For_Events_Status
   is
      I : constant Integer :=
        C_Binding.Linux.Poll_File_Descriptor_Until_Timeout (Object.My_Fd, Timeout);
   begin
      case I is
         when 1..Integer'Last   => return Events_Need_Processing;
         when 0                 => return No_Events;
         when Integer'First..-1 => return Error;
      end case;
   end Check_For_Events;

   procedure Get_Registry (Object   : Display;
                           Registry : in out Protocol.Registry'Class) is
   begin
      Registry.Proxy := Thin.Display_Get_Registry (Object.Proxy);
   end Get_Registry;

   procedure Destroy (Object : in out Registry) is
   begin
      if Object.Proxy /= null then
         Thin.Registry_Destroy (Object.Proxy);
         Object.Proxy := null;
      end if;
   end Destroy;

   function Get_Version (Object : Registry) return Unsigned_32 is
     (Thin.Registry_Get_Version (Object.Proxy));

   function Get_Version (Object : Display) return Unsigned_32 is
     (Thin.Display_Get_Version (Object.Proxy));

   function Dispatch (Object : Display) return Integer is
   begin
      return Integer (Wayland.API.Display_Dispatch (Object.Proxy));
   end Dispatch;

   function Dispatch_Pending (Object : Display) return Integer is
   begin
      return Wayland.API.Display_Dispatch_Pending (Object.Proxy);
   end Dispatch_Pending;

   function Prepare_Read (Object : Display) return Integer is
   begin
      return Wayland.API.Display_Prepare_Read (Object.Proxy);
   end Prepare_Read;

   function Read_Events (Object : Display) return Call_Result_Code is
      I : constant Integer
        := Wayland.API.Display_Read_Events (Object.Proxy);
   begin
      return (if I = 0 then Success else Error);
   end Read_Events;

   procedure Cancel_Read (Object : Display) is
   begin
      Wayland.API.Display_Cancel_Read (Object.Proxy);
   end Cancel_Read;

   procedure Dispatch (Object : Display) is
      I : Integer;
      pragma Unreferenced (I);
   begin
      I := Object.Dispatch;
   end Dispatch;

   function Roundtrip (Object : Display) return Integer is
   begin
      return Integer (Wayland.API.Display_Roundtrip (Object.Proxy));
   end Roundtrip;

   procedure Roundtrip (Object : Display) is
      I : Integer;
      pragma Unreferenced (I);
   begin
      I := Object.Roundtrip;
   end Roundtrip;

   procedure Bind (Object   : in out Compositor;
                   Registry : Protocol.Registry'Class;
                   Id       : Unsigned_32;
                   Version  : Unsigned_32)
   is
      Proxy : constant Thin.Proxy_Ptr :=
            Thin.Registry_Bind
              (Registry    => Registry.Proxy,
               Name        => Id,
               Interface_V => Thin.Compositor_Interface'Access,
               New_Id      => Version);

   begin
      if Proxy /= null then
         Object.Proxy := Proxy.all'Access;
      end if;
   end Bind;

   procedure Create_Surface (Object  : Compositor;
                             Surface : in out Protocol.Surface'Class) is
   begin
      Surface.Proxy := Thin.Compositor_Create_Surface (Object.Proxy);
   end Create_Surface;

   procedure Create_Region (Object : Compositor;
                            Region : in out Protocol.Region'Class) is
   begin
      Region.Proxy := Thin.Compositor_Create_Region (Object.Proxy);
   end Create_Region;

   procedure Destroy (Object : in out Compositor) is
   begin
      if Object.Proxy /= null then
         Thin.Compositor_Destroy (Object.Proxy);
         Object.Proxy := null;
      end if;
   end Destroy;

   function Get_Version (Object : Compositor) return Unsigned_32 is
     (Thin.Compositor_Get_Version (Object.Proxy));

   procedure Destroy (Object : in out Seat) is
   begin
      if Object.Proxy /= null then
         Thin.Seat_Destroy (Object.Proxy);
         Object.Proxy := null;
      end if;
   end Destroy;

   function Get_Version (Object : Seat) return Unsigned_32 is
     (Thin.Seat_Get_Version (Object.Proxy));

   procedure Bind (Object   : in out Seat;
                   Registry : Protocol.Registry'Class;
                   Id       : Unsigned_32;
                   Version  : Unsigned_32)
   is
      Proxy : constant Thin.Proxy_Ptr :=
        Thin.Registry_Bind (Registry    => Registry.Proxy,
                               Name        => Id,
                               Interface_V => Thin.Seat_Interface'Access,
                               New_Id      => Version);

   begin
      if Proxy /= null then
         Object.Proxy := Proxy.all'Access;
      end if;
   end Bind;

   procedure Get_Pointer (Object  : Seat;
                          Pointer : in out Protocol.Pointer'Class) is
   begin
      Pointer.Proxy := Thin.Seat_Get_Pointer (Object.Proxy);
   end Get_Pointer;

   procedure Get_Keyboard (Object   : Seat;
                           Keyboard : in out Protocol.Keyboard'Class) is
   begin
      Keyboard.Proxy := Thin.Seat_Get_Keyboard (Object.Proxy);
   end Get_Keyboard;

   procedure Get_Touch (Object : Seat;
                        Touch  : in out Protocol.Touch'Class) is
   begin
      Touch.Proxy := Thin.Seat_Get_Touch (Object.Proxy);
   end Get_Touch;

   procedure Release (Object : in out Seat) is
   begin
      if Object.Proxy /= null then
         Thin.Seat_Release (Object.Proxy);
         Object.Proxy := null;
      end if;
   end Release;

   procedure Bind (Object   : in out Shm;
                   Registry : Protocol.Registry'Class;
                   Id       : Unsigned_32;
                   Version  : Unsigned_32)
   is
      Proxy : constant Thin.Proxy_Ptr :=
        Thin.Registry_Bind (Registry    => Registry.Proxy,
                               Name        => Id,
                               Interface_V => Thin.Shm_Interface'Access,
                               New_Id      => Version);

   begin
      if Proxy /= null then
         Object.Proxy := Proxy.all'Access;
      end if;
   end Bind;

   procedure Create_Pool
     (Object          : Shm;
      File_Descriptor : C_Binding.Linux.Files.File;
      Size            : Positive;
      Pool            : in out Protocol.Shm_Pool'Class) is
   begin
      Pool.Proxy := Thin.Shm_Create_Pool
        (Object.Proxy,
         C_Binding.Linux.Files.File_Descriptor (File_Descriptor),
         Size);
   end Create_Pool;

   procedure Destroy (Object : in out Shm) is
   begin
      if Object.Proxy /= null then
         Thin.Shm_Destroy (Object.Proxy);
         Object.Proxy := null;
      end if;
   end Destroy;

   function Get_Version (Object : Shm) return Unsigned_32 is
     (Thin.Shm_Get_Version (Object.Proxy));

   procedure Create_Buffer (Object   : Shm_Pool;
                            Offset   : Natural;
                            Width    : Natural;
                            Height   : Natural;
                            Stride   : Natural;
                            Format   : Shm_Format;
                            Buffer : in out Protocol.Buffer'Class) is
   begin
      Buffer.Proxy := Thin.Shm_Pool_Create_Buffer
        (Object.Proxy, Offset, Width, Height, Stride, Format);
   end Create_Buffer;

   procedure Resize (Object : Shm_Pool;
                     Size   : Positive) is
   begin
      Thin.Shm_Pool_Resize (Object.Proxy, Size);
   end Resize;

   procedure Destroy (Object : in out Shm_Pool) is
   begin
      if Object.Proxy /= null then
         Thin.Shm_Pool_Destroy (Object.Proxy);
         Object.Proxy := null;
      end if;
   end Destroy;

   function Get_Version (Object : Shm_Pool) return Unsigned_32 is
     (Thin.Shm_Pool_Get_Version (Object.Proxy));

   procedure Destroy (Object : in out Buffer) is
   begin
      if Object.Proxy /= null then
         Thin.Buffer_Destroy (Object.Proxy);
         Object.Proxy := null;
      end if;
   end Destroy;

   function Get_Version (Object : Buffer) return Unsigned_32 is
     (Thin.Buffer_Get_Version (Object.Proxy));

   procedure Destroy (Object : in out Data_Offer) is
   begin
      if Object.Proxy /= null then
         Thin.Data_Offer_Destroy (Object.Proxy);
         Object.Proxy := null;
      end if;
   end Destroy;

   function Get_Version (Object : Data_Offer) return Unsigned_32 is
     (Thin.Data_Offer_Get_Version (Object.Proxy));

   procedure Do_Accept (Object    : Data_Offer;
                        Serial    : Unsigned_32;
                        Mime_Type : String) is
   begin
      Wayland.API.Proxy_Marshal
        (Wayland.API.Proxy (Object.Proxy.all),
         Constants.Data_Offer_Accept,
         Serial,
         +Mime_Type);
   end Do_Accept;

   procedure Do_Not_Accept (Object : Data_Offer;
                            Serial : Unsigned_32) is
   begin
      Thin.Data_Offer_Accept (Object.Proxy, Serial, Interfaces.C.Strings.Null_Ptr);
   end Do_Not_Accept;

   procedure Receive (Object          : Data_Offer;
                      Mime_Type       : String;
                      File_Descriptor : Integer) is
   begin
      Thin.Data_Offer_Receive
        (Object.Proxy, +Mime_Type, File_Descriptor);
   end Receive;

   procedure Finish (Object : Data_Offer) is
   begin
      Thin.Data_Offer_Finish (Object.Proxy);
   end Finish;

   procedure Set_Actions (Object           : Data_Offer;
                          Dnd_Actions      : Unsigned_32;
                          Preferred_Action : Unsigned_32) is
   begin
      Thin.Data_Offer_Set_Actions
        (Object.Proxy, Dnd_Actions, Preferred_Action);
   end Set_Actions;

   procedure Attach (Object : Protocol.Surface;
                     Buffer : Protocol.Buffer'Class;
                     X, Y   : Integer) is
   begin
      Thin.Surface_Attach (Object.Proxy, Buffer.Proxy, X, Y);
   end Attach;

   procedure Damage (Object : Surface;
                     X, Y   : Integer;
                     Width  : Natural;
                     Height : Natural) is
   begin
      Thin.Surface_Damage (Object.Proxy, X, Y, Width, Height);
   end Damage;

   function Frame (Object : Surface) return Callback'Class is
   begin
      return Result : Callback do
         Result.Proxy := Thin.Surface_Frame (Object.Proxy);
      end return;
   end Frame;

   procedure Set_Opaque_Region (Object : Surface;
                                Region : Protocol.Region'Class) is
   begin
      Thin.Surface_Set_Opaque_Region (Object.Proxy, Region.Proxy);
   end Set_Opaque_Region;

   procedure Set_Input_Region (Object : Surface;
                               Region : Protocol.Region'Class) is
   begin
      Thin.Surface_Set_Input_Region (Object.Proxy, Region.Proxy);

   end Set_Input_Region;

   procedure Commit (Object : Surface) is
   begin
      Thin.Surface_Commit (Object.Proxy);
   end Commit;

   procedure Set_Buffer_Transform (Object    : Surface;
                                   Transform : Output_Transform) is
   begin
      Thin.Surface_Set_Buffer_Transform (Object.Proxy, Transform);
   end Set_Buffer_Transform;

   procedure Set_Buffer_Scale (Object : Surface;
                               Scale  : Positive) is
   begin
      Thin.Surface_Set_Buffer_Scale (Object.Proxy, Scale);
   end Set_Buffer_Scale;

   procedure Damage_Buffer (Object : Surface;
                            X, Y   : Integer;
                            Width  : Natural;
                            Height : Natural) is
   begin
      Thin.Surface_Damage_Buffer
        (Object.Proxy, X, Y, Width, Height);
   end Damage_Buffer;

   procedure Destroy (Object : in out Surface) is
   begin
      if Object.Proxy /= null then
         Thin.Surface_Destroy (Object.Proxy);
         Object.Proxy := null;
      end if;
   end Destroy;

   function Get_Version (Object : Surface) return Unsigned_32 is
     (Thin.Surface_Get_Version (Object.Proxy));

   function Sync (Object : Display) return Callback'Class is
   begin
      return Callback : Protocol.Callback do
         Callback.Proxy := Thin.Display_Sync (Object.Proxy);
      end return;
   end Sync;

   procedure Destroy (Object : in out Callback) is
   begin
      if Object.Proxy /= null then
         Thin.Callback_Destroy (Object.Proxy);
         Object.Proxy := null;
      end if;
   end Destroy;

   function Get_Version (Object : Callback) return Unsigned_32 is
     (Thin.Callback_Get_Version (Object.Proxy));

   procedure Destroy (Object : in out Pointer) is
   begin
      if Object.Proxy /= null then
         Thin.Pointer_Destroy (Object.Proxy);
         Object.Proxy := null;
      end if;
   end Destroy;

   function Get_Version (Object : Pointer) return Unsigned_32 is
     (Thin.Pointer_Get_Version (Object.Proxy));

   procedure Set_Cursor (Object    : Pointer;
                         Serial    : Unsigned_32;
                         Surface   : Protocol.Surface'Class;
                         Hotspot_X : Integer;
                         Hotspot_Y : Integer) is
   begin
      Thin.Pointer_Set_Cursor (Object.Proxy,
                                  Serial,
                                  Surface.Proxy,
                                  Hotspot_X,
                                  Hotspot_Y);
   end Set_Cursor;

   procedure Release (Object : in out Pointer) is
   begin
      if Object.Proxy /= null then
         Thin.Pointer_Release (Object.Proxy);
         Object.Proxy := null;
      end if;
   end Release;

   procedure Destroy (Object : in out Keyboard) is
   begin
      if Object.Proxy /= null then
         Thin.Keyboard_Destroy (Object.Proxy);
         Object.Proxy := null;
      end if;
   end Destroy;

   function Get_Version (Object : Keyboard) return Unsigned_32 is
     (Thin.Keyboard_Get_Version (Object.Proxy));

   procedure Release (Object : in out Keyboard) is
   begin
      if Object.Proxy /= null then
         Thin.Keyboard_Release (Object.Proxy);
         Object.Proxy := null;
      end if;
   end Release;

   procedure Destroy (Object : in out Touch) is
   begin
      if Object.Proxy /= null then
         Thin.Touch_Destroy (Object.Proxy);
         Object.Proxy := null;
      end if;
   end Destroy;

   function Get_Version (Object : Touch) return Unsigned_32 is
     (Thin.Touch_Get_Version (Object.Proxy));

   procedure Release (Object : in out Touch) is
   begin
      if Object.Proxy /= null then
         Thin.Touch_Release (Object.Proxy);
         Object.Proxy := null;
      end if;
   end Release;

   procedure Destroy (Object : in out Output) is
   begin
      if Object.Proxy /= null then
         Thin.Output_Destroy (Object.Proxy);
         Object.Proxy := null;
      end if;
   end Destroy;

   function Get_Version (Object : Output) return Unsigned_32 is
     (Thin.Output_Get_Version (Object.Proxy));

   procedure Release (Object : in out Output) is
   begin
      if Object.Proxy /= null then
         Thin.Output_Release (Object.Proxy);
         Object.Proxy := null;
      end if;
   end Release;

   procedure Destroy (Object : in out Region) is
   begin
      if Object.Proxy /= null then
         Thin.Region_Destroy (Object.Proxy);
         Object.Proxy := null;
      end if;
   end Destroy;

   function Get_Version (Object : Region) return Unsigned_32 is
     (Thin.Region_Get_Version (Object.Proxy));

   procedure Add (Object : Region;
                  X, Y   : Integer;
                  Width  : Natural;
                  Height : Natural) is
   begin
      Thin.Region_Add (Object.Proxy, X, Y, Width, Height);
   end Add;

   procedure Subtract (Object : Region;
                       X, Y   : Integer;
                       Width  : Natural;
                       Height : Natural) is
   begin
      Thin.Region_Subtract (Object.Proxy, X, Y, Width, Height);
   end Subtract;

   procedure Destroy (Object : in out Subcompositor) is
   begin
      if Object.Proxy /= null then
         Thin.Subcompositor_Destroy (Object.Proxy);
         Object.Proxy := null;
      end if;
   end Destroy;

   function Get_Version (Object : Subcompositor) return Unsigned_32 is
     (Thin.Subcompositor_Get_Version (Object.Proxy));

   procedure Get_Subsurface
     (Object     : Protocol.Subcompositor;
      Surface    : Protocol.Surface'Class;
      Parent     : Protocol.Surface'Class;
      Subsurface : in out Protocol.Subsurface'Class) is
   begin
      Subsurface.Proxy :=
        Thin.Subcompositor_Get_Subsurface
          (Object.Proxy,
           Surface.Proxy,
           Parent.Proxy);
   end Get_Subsurface;

   procedure Destroy (Object : in out Subsurface) is
   begin
      if Object.Proxy /= null then
         Thin.Subsurface_Destroy (Object.Proxy);
         Object.Proxy := null;
      end if;
   end Destroy;

   function Get_Version (Object : Subsurface) return Unsigned_32 is
     (Thin.Subsurface_Get_Version (Object.Proxy));

   procedure Set_Position (Object : Protocol.Subsurface;
                           X, Y   : Integer) is
   begin
      Thin.Subsurface_Set_Position (Object.Proxy, X, Y);
   end Set_Position;

   procedure Place_Above (Object  : Subsurface;
                          Sibling : Surface'Class) is
   begin
      Thin.Subsurface_Place_Above (Object.Proxy, Sibling.Proxy);
   end Place_Above;

   procedure Place_Below (Object  : Subsurface;
                          Sibling : Surface'Class) is
   begin
      Thin.Subsurface_Place_Below (Object.Proxy, Sibling.Proxy);
   end Place_Below;

   procedure Set_Sync (Object : Subsurface) is
   begin
      Thin.Subsurface_Set_Sync (Object.Proxy);
   end Set_Sync;

   procedure Set_Desync (Object : Subsurface) is
   begin
      Thin.Subsurface_Set_Desync (Object.Proxy);
   end Set_Desync;

   procedure Destroy (Object : in out Data_Source) is
   begin
      if Object.Proxy /= null then
         Thin.Data_Source_Destroy (Object.Proxy);
         Object.Proxy := null;
      end if;
   end Destroy;

   function Get_Version (Object : Data_Source) return Unsigned_32 is
     (Thin.Data_Source_Get_Version (Object.Proxy));

   procedure Offer (Object    : Data_Source;
                    Mime_Type : String) is
   begin
      Wayland.API.Proxy_Marshal
        (Wayland.API.Proxy (Object.Proxy.all),
         Constants.Data_Source_Offer,
         +Mime_Type);
   end Offer;

   procedure Set_Actions (Object      : Data_Source;
                          Dnd_Actions : Unsigned_32) is
   begin
      Thin.Data_Source_Set_Actions (Object.Proxy, Dnd_Actions);
   end Set_Actions;

   procedure Destroy (Object : in out Data_Device) is
   begin
      if Object.Proxy /= null then
         Thin.Data_Device_Destroy (Object.Proxy);
         Object.Proxy := null;
      end if;
   end Destroy;

   function Get_Version (Object : Data_Device) return Unsigned_32 is
     (Thin.Data_Device_Get_Version (Object.Proxy));

   procedure Start_Drag (Object : Data_Device;
                         Source : Data_Source'class;
                         Origin : Surface'class;
                         Icon   : Surface'class;
                         Serial : Unsigned_32) is
   begin
      Thin.Data_Device_Start_Drag (Object.Proxy,
                                      Source.Proxy,
                                      Origin.Proxy,
                                      Icon.Proxy,
                                      Serial);
   end Start_Drag;

   procedure Set_Selection (Object : Data_Device;
                            Source : Data_Source'Class;
                            Serial : Unsigned_32) is
   begin
      Thin.Data_Device_Set_Selection (Object.Proxy, Source.Proxy, Serial);
   end Set_Selection;

   procedure Release (Object : in out Data_Device) is
   begin
      if Object.Proxy /= null then
         Thin.Data_Device_Release (Object.Proxy);
         Object.Proxy := null;
      end if;
   end Release;

   procedure Destroy (Object : in out Data_Device_Manager) is
   begin
      if Object.Proxy /= null then
         Thin.Data_Device_Manager_Destroy (Object.Proxy);
         Object.Proxy := null;
      end if;
   end Destroy;

   function Get_Version (Object : Data_Device_Manager) return Unsigned_32 is
     (Thin.Data_Device_Manager_Get_Version (Object.Proxy));

   procedure Create_Data_Source (Object : Data_Device_Manager;
                                 Source : in out Data_Source'Class) is
   begin
      Source.Proxy := Thin.Data_Device_Manager_Create_Data_Source
        (Object.Proxy);
   end Create_Data_Source;

   procedure Get_Data_Device (Object : Data_Device_Manager;
                              Seat   : Protocol.Seat'Class;
                              Device : in out Data_Device'Class) is
   begin
      Device.Proxy := Thin.Data_Device_Manager_Get_Data_Device
        (Object.Proxy, Seat.Proxy);
   end Get_Data_Device;

end Wayland.Client.Protocol;
