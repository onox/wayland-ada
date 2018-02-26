with Px;
with Wl;

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

   Compositor : aliased Wl.Compositor_T;
   Pointer    : aliased Wl.Pointer_T;
   Seat       : aliased Wl.Seat_T;
   Shell      : aliased Wl.Shell_T;
   Shm        : aliased Wl.Shm_T;

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
      elsif Name = Wl.Shm_Interface.Name then
         Shm.Bind (Registry,
                   Id,
                   Min (Version, 1));
      elsif Name = Wl.Shell_Interface.Name then
         Shell.Bind (Registry,
                     Id,
                     Min (Version, 1));
      elsif Name = Wl.Seat_Interface.Name then
         Px.Put_Line ("Pointer listener is setup " & Wl.Seat_Interface.Name);
         Seat.Bind (Registry,
                    Id,
                    Min (Version, 2));
         Seat.Get_Pointer (Pointer);
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

   Display    : Wl.Display_T;
   Registry   : Wl.Registry_T;

   WIDTH : constant := 320;
   HEIGHT : constant := 200;
--     CURSOR_WIDTH : constant := 100;
--     CURSOR_HEIGHT : constant := 59;
--     CURSOR_HOT_SPOT_X : constant := 10;
--     CURSOR_HOT_SPOT_Y : constant := 35;
--
     Done : Boolean := false;
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

--     Data := new Pool_Data_T;
--
--     Data.Capacity := Interfaces.C.unsigned_long (Stat.Size);
--     Data.Size := 0;
--     Data.Fd := File.File_Descriptor;

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
--      Surface.Destroy;
      Px.Put_Line ("Failed to create shell surface");
      return;
   end if;

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
