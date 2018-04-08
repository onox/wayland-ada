with Px;
with Wl;
with Ada.Text_IO;
-- sudo apt install libwayland-dev

-- This is a wayland hello world application. It uses the wayland
-- client library and the wayland protocol to display a window.
--
-- Original code that was translated from C to Ada:
-- https://github.com/hdante/hello_wayland
-- https://hdante.wordpress.com/2014/07/08/the-hello-wayland-tutorial/
--
-- When compiled go to the .../bin directory
-- and execute the executable from there.
procedure Example_Hdante_Hello_World is

   use type Px.S_FLag_T;

   use type Wl.Seat_Capability_T;

   Compositor : aliased Wl.Compositor_T;
   Pointer    : aliased Wl.Pointer_T;
   Seat       : aliased Wl.Seat_T;
   Shell      : aliased Wl.Shell_T;
   Shm        : aliased Wl.Shm_T;

   Done : Boolean := false;

   type Data_T is limited record
      Compositor : not null access Wl.Compositor_T;
      Pointer    : not null access Wl.Pointer_T;
      Seat       : not null access Wl.Seat_T;
      Shell      : not null access Wl.Shell_T;
      Shm        : not null access Wl.Shm_T;
   end record;

   type Data_Ptr is access all Data_T;

   Data : aliased Data_T :=
     (
      Compositor => Compositor'Unchecked_Access,
      Pointer    => Pointer'Unchecked_Access,
      Seat       => Seat'Unchecked_Access,
      Shell      => Shell'Unchecked_Access,
      Shm        => Shm'Unchecked_Access
     );

   function Min (L, R : Wl.Unsigned_32) return Wl.Unsigned_32 renames
     Wl.Unsigned_32'Min;

   Exists_Mouse    : Boolean := False;
   Exists_Keyboard : Boolean := False;

   procedure Seat_Capabilities
     (Data         : not null Data_Ptr;
      Seat         : Wl.Seat_T;
      Capabilities : Wl.Seat_Capability_T) is
   begin
      if (Capabilities and Wl.Seat_Capability_Pointer) > 0 then
         Px.Put_Line ("Display has a pointer");
         Exists_Mouse := True;
      end if;

      if (Capabilities and Wl.Seat_Capability_Keyboard) > 0 then
         Px.Put_Line ("Display has a keyboard");
         Exists_Keyboard := True;
      end if;

      if (Capabilities and Wl.Seat_Capability_Touch) > 0 then
         Px.Put_Line ("Display has a touch screen");
      end if;
   end Seat_Capabilities;

   procedure Seat_Name
     (Data : not null Data_Ptr;
      Seat : Wl.Seat_T;
      Name : String) is
   begin
      null;
   end Seat_Name;

   package Seat_Subscriber is new Wl.Seat_Capability_Subscriber (Data_Type         => Data_Ptr,
                                                                 Data              => Data'Unchecked_Access,
                                                                 Seat_Capabilities => Seat_Capabilities,
                                                                 Seat_Name         => Seat_Name);

   procedure Global_Registry_Handler (Data     : not null Data_Ptr;
                                      Registry : Wl.Registry_T;
                                      Id       : Wl.Unsigned_32;
                                      Name     : String;
                                      Version  : Wl.Unsigned_32) is
   begin
      if Name = Wl.Compositor_Interface.Name then
         Compositor.Bind (Registry,
                          Id,
                          Min (Version, 4));
         Ada.Text_IO.Put_Line ("Bind compositor");
      elsif Name = Wl.Shm_Interface.Name then
         Shm.Bind (Registry,
                   Id,
                   Min (Version, 1));
         Ada.Text_IO.Put_Line ("Bind shm");
      elsif Name = Wl.Shell_Interface.Name then
         Shell.Bind (Registry,
                     Id,
                     Min (Version, 1));
         Ada.Text_IO.Put_Line ("Bind shell");
      elsif Name = Wl.Seat_Interface.Name then
         Px.Put_Line ("Pointer listener is setup " & Wl.Seat_Interface.Name);
         Seat.Bind (Registry,
                    Id,
                    Min (Version, 2));
         Seat_Subscriber.Start_Subscription (Seat);
         --         Result := Wl_Thin.Pointer_Add_Listener (Pointer, Pointer_Listener'Access, Px.Nil);
      end if;
   end Global_Registry_Handler;

   procedure Global_Registry_Remover (Data     : not null Data_Ptr;
                                      Registry : Wl.Registry_T;
                                      Id       : Wl.Unsigned_32) is
   begin
      Px.Put_Line ("Got a registry losing event for" & Id'Image);
   end Global_Registry_Remover;

   package Subscriber is new Wl.Registry_Objects_Subscriber
     (Data_T                => Data_Ptr,
      Data                  => Data'Unchecked_Access,
      Global_Object_Added   => Global_Registry_Handler,
      Global_Object_Removed => Global_Registry_Remover);

   procedure Shell_Surface_Ping
     (Data    : not null Data_Ptr;
      Surface : Wl.Shell_Surface_T;
      Serial  : Wl.Unsigned_32) is
   begin
      Surface.Pong (Serial);
   end Shell_Surface_Ping;

   procedure Shell_Surface_Configure
     (Data    : not null Data_Ptr;
      Surface : Wl.Shell_Surface_T;
      Edges   : Wl.Unsigned_32;
      Width   : Integer;
      Height  : Integer) is
   begin
      null;
   end Shell_Surface_Configure;

   procedure Shell_Surface_Popup_Done
     (Data    : not null Data_Ptr;
      Surface : Wl.Shell_Surface_T) is
   begin
      null;
   end Shell_Surface_Popup_Done;

   package Shell_Surface_Subscriber is new Wl.Shell_Surface_Subscriber
     (Data_Type                => Data_Ptr,
      Data                     => Data'Unchecked_Access,
      Shell_Surface_Ping       => Shell_Surface_Ping,
      Shell_Surface_Configure  => Shell_Surface_Configure,
      Shell_Surface_Popup_Done => Shell_Surface_Popup_Done);

   procedure Mouse_Enter
     (Data      : not null Data_Ptr;
      Pointer   : Wl.Pointer_T;
      Serial    : Wl.Unsigned_32;
      Surface   : Wl.Surface_T;
      Surface_X : Wl.Fixed_T;
      Surface_Y : Wl.Fixed_T) is
   begin
      Ada.Text_IO.Put_Line ("Pointer enter");
   end Mouse_Enter;

   procedure Pointer_Leave
     (Data    : not null Data_Ptr;
      Pointer : Wl.Pointer_T;
      Serial  : Wl.Unsigned_32;
      Surface : Wl.Surface_T) is
   begin
      Ada.Text_IO.Put_Line ("Pointer leave");
   end Pointer_Leave;

   procedure Pointer_Motion
     (Data      : not null Data_Ptr;
      Pointer   : Wl.Pointer_T;
      Time      : Wl.Unsigned_32;
      Surface_X : Wl.Fixed_T;
      Surface_Y : Wl.Fixed_T) is
   begin
      Ada.Text_IO.Put_Line ("Pointer motion");
   end Pointer_Motion;

   procedure Pointer_Button
     (Data    : not null Data_Ptr;
      Pointer : Wl.Pointer_T;
      Serial  : Wl.Unsigned_32;
      Time    : Wl.Unsigned_32;
      Button  : Wl.Unsigned_32;
      State   : Wl.Unsigned_32) is
   begin
      Ada.Text_IO.Put_Line ("Pointer button");
      Done := True;
   end Pointer_Button;

   procedure Pointer_Axis
     (Data    : not null Data_Ptr;
      Pointer : Wl.Pointer_T;
      Time    : Wl.Unsigned_32;
      Axis    : Wl.Unsigned_32;
      Value   : Wl.Fixed_T) is
   begin
      Ada.Text_IO.Put_Line ("Pointer axis");
   end Pointer_Axis;

   procedure Pointer_Frame (Data    : not null Data_Ptr;
                            Pointer : Wl.Pointer_T) is
   begin
      Ada.Text_IO.Put_Line ("Pointer frame");
   end Pointer_Frame;

   procedure Pointer_Axis_Source
     (Data        : not null Data_Ptr;
      Pointer     : Wl.Pointer_T;
      Axis_Source : Wl.Unsigned_32) is
   begin
      Ada.Text_IO.Put_Line ("Pointer axis source");
   end Pointer_Axis_Source;

   procedure Pointer_Axis_Stop
     (Data    : not null Data_Ptr;
      Pointer : Wl.Pointer_T;
      Time    : Wl.Unsigned_32;
      Axis    : Wl.Unsigned_32) is
   begin
      Ada.Text_IO.Put_Line ("Pointer axis stop");
   end Pointer_Axis_Stop;

   procedure Pointer_Axis_Discrete
     (Data     : not null Data_Ptr;
      Pointer  : Wl.Pointer_T;
      Axis     : Wl.Unsigned_32;
      Discrete : Integer) is
   begin
      Ada.Text_IO.Put_Line ("Pointer axis discrete");
   end Pointer_Axis_Discrete;

   package Mouse_Subscriber is new Wl.Pointer_Subscriber
     (Data_Type             => Data_Ptr,
      Data                  => Data'Unchecked_Access,
      Pointer_Enter         => Mouse_Enter,
      Pointer_Leave         => Pointer_Leave,
      Pointer_Motion        => Pointer_Motion,
      Pointer_Button        => Pointer_Button,
      Pointer_Axis          => Pointer_Axis,
      Pointer_Frame         => Pointer_Frame,
      Pointer_Axis_Source   => Pointer_Axis_Source,
      Pointer_Axis_Stop     => Pointer_Axis_Stop,
      Pointer_Axis_Discrete => Pointer_Axis_Discrete);

   Display    : Wl.Display_T;
   Registry   : Wl.Registry_T;

   WIDTH : constant := 320;
   HEIGHT : constant := 200;
   --     CURSOR_WIDTH : constant := 100;
   --     CURSOR_HEIGHT : constant := 59;
   --     CURSOR_HOT_SPOT_X : constant := 10;
   --     CURSOR_HOT_SPOT_Y : constant := 35;
   --
   --
   --     --  void on_button(uint32_t button)
   --     --  {
   --     --      done = true;
   --     --  }
   --
   Buffer        : Wl.Buffer_T;
   Pool          : Wl.Shm_Pool_T;
   Surface       : Wl.Surface_T;
   Shell_Surface : Wl.Shell_Surface_T;
   Image         : Px.File_T;
   Stat          : Px.Status_T;

   use type Px.int;
   use type Px.C_String;

   File_Name : Px.C_String := "hello_world_image.bin" & Px.Nul;

   Memory_Map : Px.Memory_Map_T;

begin
   Display.Connect (Wl.Default_Display_Name);
   if not Display.Is_Connected then
      Px.Put_Line ("Can't connect to display");
      return;
   end if;
   Px.Put_Line ("Connected to display");

   Registry.Get (Display);
   if not Registry.Has_Registry_Object then
      Px.Put_Line ("Can't get global registry object");
      return;
   end if;

   Subscriber.Start_Subscription (Registry);
   Display.Dispatch;
   Display.Roundtrip;
   Registry.Destroy;

   if Exists_Mouse then
      Ada.Text_IO.Put_Line ("Start mosue subscription");
      Seat.Get_Pointer (Pointer);
      Mouse_Subscriber.Start_Subscription (Pointer);
   end if;

   Image.Open (File_Name,
               Px.O_RDWR,
               Px.S_IRUSR or Px.S_IRGRP or Px.S_IROTH);

   if Image.Is_Closed then
      Px.Put_Line ("Error opening surface image");
      return;
   end if;

   Image.Get_File_Status (Stat);

   if not Stat.Is_Valid then
      Px.Put_Line ("File does not exist?");
      return;
   end if;

   Image.Memory_Map (Px.Nil,
                     Px.unsigned_long (Stat.Size),
                     Px.PROT_READ, Px.MAP_SHARED, 0, Memory_Map);

   if Memory_Map.Has_Mapping then
      Shm.Create_Pool (Integer (Image.File_Descriptor), Integer (Stat.Size), Pool);
   else
      Px.Put_Line ("Failed to map file");
      return;
   end if;

   if not Pool.Exists then
      Px.Put_Line ("Failed to create pool");
      return;
   end if;


   Compositor.Create_Surface (Surface);

   if not Surface.Exists then
      Px.Put_Line ("Failed to create surface");
      return;
   end if;

   Shell.Get_Shell_Surface (Surface, Shell_Surface);

   if not Shell_Surface.Exists then
      Surface.Destroy;
      Px.Put_Line ("Failed to create shell surface");
      return;
   end if;

   Shell_Surface_Subscriber.Start_Subscription (Shell_Surface);

   Shell_Surface.Set_Toplevel;

   Pool.Create_Buffer (0,
                       Integer (Width),
                       Integer (Height),
                       Integer (Width)*4,
                       Wl.Unsigned_32 (Wl.Shm_Format_Argb_8888),
                       Buffer);

   if not Buffer.Exists then
      Px.Put_Line ("Failed to create buffer");
      return;
   end if;

   Surface.Attach (Buffer, 0, 0);

   Surface.Commit;

   while not Done loop
      if Display.Dispatch < 0 then
         Px.Put_Line ("Main loop error");
         Done := true;
      end if;
   end loop;
end Example_Hdante_Hello_World;
