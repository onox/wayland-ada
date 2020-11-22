private package Wayland.Constants is
   pragma Pure;

   Display_Sync : constant := 0;

   Display_Get_Registry : constant := 1;

   Display_Error_Since_Version : constant := 1;

   Display_Delete_Id_Since_Version : constant := 1;

   Display_Sync_Since_Version : constant := 1;

   Display_Get_Registry_Since_Version : constant := 1;

   Registry_Bind : constant := 0;

   Registry_Global_Since_Version : constant := 1;

   Registry_Global_Remove_Since_Version : constant := 1;

   Registry_Bind_Since_Version : constant := 1;

   Callback_Done_Since_Version : constant := 1;

   Compositor_Create_Surface : constant := 0;

   Compositor_Create_Region : constant := 1;

   Compositor_Create_Surface_Since_Version : constant := 1;

   Compositor_Create_Region_Since_Version : constant := 1;

   Shm_Pool_Create_Buffer : constant := 0;

   Shm_Pool_Destroy : constant := 1;

   Shm_Pool_Resize : constant := 2;

   Shm_Pool_Create_Buffer_Since_Version : constant := 1;

   Shm_Pool_Destroy_Since_Version : constant := 1;

   Shm_Pool_Resize_Since_Version : constant := 1;

   Shm_Create_Pool : constant := 0;

   Shm_Format_Since_Version : constant := 1;

   Shm_Create_Pool_Since_Version : constant := 1;

   Buffer_Destroy : constant := 0;

   Buffer_Release_Since_Version : constant := 1;

   Buffer_Destroy_Since_Version : constant := 1;

   Data_Offer_Accept : constant := 0;

   Data_Offer_Receive : constant := 1;

   Data_Offer_Destroy : constant := 2;

   Data_Offer_Finish : constant := 3;

   Data_Offer_Set_Actions : constant := 4;

   Data_Offer_Offer_Since_Version : constant := 1;

   Data_Offer_Source_Actions_Since_Version : constant := 3;

   Data_Offer_Action_Since_Version : constant := 3;

   Data_Offer_Accept_Since_Version : constant := 1;

   Data_Offer_Receive_Since_Version : constant := 1;

   Data_Offer_Destroy_Since_Version : constant := 1;

   Data_Offer_Finish_Since_Version : constant := 3;

   Data_Offer_Set_Actions_Since_Version : constant := 3;

   Data_Source_Offer : constant := 0;

   Data_Source_Destroy : constant := 1;

   Data_Source_Set_Actions : constant := 2;

   Data_Source_Target_Since_Version : constant := 1;

   Data_Source_Send_Since_Version : constant := 1;

   Data_Source_Cancelled_Since_Version : constant := 1;

   Data_Source_Dnd_Drop_Performed_Since_Version : constant := 3;

   Data_Source_Dnd_Finished_Since_Version : constant := 3;

   Data_Source_Action_Since_Version : constant := 3;

   Data_Source_Offer_Since_Version : constant := 1;

   Data_Source_Destroy_Since_Version : constant := 1;

   Data_Source_Set_Actions_Since_Version : constant := 3;

   Data_Device_Start_Drag : constant := 0;

   Data_Device_Set_Selection : constant := 1;

   Data_Device_Release : constant := 2;

   Data_Device_Data_Offer_Since_Version : constant := 1;

   Data_Device_Enter_Since_Version : constant := 1;

   Data_Device_Leave_Since_Version : constant := 1;

   Data_Device_Motion_Since_Version : constant := 1;

   Data_Device_Drop_Since_Version : constant := 1;

   Data_Device_Selection_Since_Version : constant := 1;

   Data_Device_Start_Drag_Since_Version : constant := 1;

   Data_Device_Set_Selection_Since_Version : constant := 1;

   Data_Device_Release_Since_Version : constant := 2;

   Data_Device_Manager_Create_Data_Source : constant := 0;

   Data_Device_Manager_Get_Data_Device : constant := 1;

   Data_Device_Manager_Create_Data_Source_Since_Version : constant := 1;

   Data_Device_Manager_Get_Data_Device_Since_Version : constant := 1;

   Shell_Get_Shell_Surface : constant := 0;

   Shell_Get_Shell_Surface_Since_Version : constant := 1;

   Shell_Surface_Pong : constant := 0;

   Shell_Surface_Move : constant := 1;

   Shell_Surface_Resize : constant := 2;

   Shell_Surface_Set_Toplevel : constant := 3;

   Shell_Surface_Set_Transient : constant := 4;

   Shell_Surface_Set_Fullscreen : constant := 5;

   Shell_Surface_Set_Popup : constant := 6;

   Shell_Surface_Set_Maximized : constant := 7;

   Shell_Surface_Set_Title : constant := 8;

   Shell_Surface_Set_Class : constant := 9;

   Shell_Surface_Ping_Since_Version : constant := 1;

   Shell_Surface_Configure_Since_Version : constant := 1;

   Shell_Surface_Popup_Done_Since_Version : constant := 1;

   Shell_Surface_Pong_Since_Version : constant := 1;

   Shell_Surface_Move_Since_Version : constant := 1;

   Shell_Surface_Resize_Since_Version : constant := 1;

   Shell_Surface_Set_Toplevel_Since_Version : constant := 1;

   Shell_Surface_Set_Transient_Since_Version : constant := 1;

   Shell_Surface_Set_Fullscreen_Since_Version : constant := 1;

   Shell_Surface_Set_Popup_Since_Version : constant := 1;

   Shell_Surface_Set_Maximized_Since_Version : constant := 1;

   Shell_Surface_Set_Title_Since_Version : constant := 1;

   Shell_Surface_Set_Class_Since_Version : constant := 1;

   Surface_Destroy : constant := 0;

   Surface_Attach : constant := 1;

   Surface_Damage : constant := 2;

   Surface_Frame : constant := 3;

   Surface_Set_Opaque_Region : constant := 4;

   Surface_Set_Input_Region : constant := 5;

   Surface_Commit : constant := 6;

   Surface_Set_Buffer_Transform : constant := 7;

   Surface_Set_Buffer_Scale : constant := 8;

   Surface_Damage_Buffer : constant := 9;

   Surface_Enter_Since_Version : constant := 1;

   Surface_Leave_Since_Version : constant := 1;

   Surface_Destroy_Since_Version : constant := 1;

   Surface_Attach_Since_Version : constant := 1;

   Surface_Damage_Since_Version : constant := 1;

   Surface_Frame_Since_Version : constant := 1;

   Surface_Set_Opaque_Region_Since_Version : constant := 1;

   Surface_Set_Input_Region_Since_Version : constant := 1;

   Surface_Commit_Since_Version : constant := 1;

   Surface_Set_Buffer_Transform_Since_Version : constant := 2;

   Surface_Set_Buffer_Scale_Since_Version : constant := 3;

   Surface_Damage_Buffer_Since_Version : constant := 4;

   Seat_Get_Pointer : constant := 0;

   Seat_Get_Keyboard : constant := 1;

   Seat_Get_Touch : constant := 2;

   Seat_Release : constant := 3;

   Seat_Capabilities_Since_Version : constant := 1;

   Seat_Name_Since_Version : constant := 2;

   Seat_Get_Pointer_Since_Version : constant := 1;

   Seat_Get_Keyboard_Since_Version : constant := 1;

   Seat_Get_Touch_Since_Version : constant := 1;

   Seat_Release_Since_Version : constant := 5;

   Pointer_Set_Cursor : constant := 0;

   Pointer_Release : constant := 1;

   Pointer_Enter_Since_Version : constant := 1;

   Pointer_Leave_Since_Version : constant := 1;

   Pointer_Motion_Since_Version : constant := 1;

   Pointer_Button_Since_Version : constant := 1;

   Pointer_Axis_Since_Version : constant := 1;

   Pointer_Frame_Since_Version : constant := 5;

   Pointer_Axis_Source_Since_Version : constant := 5;

   Pointer_Axis_Stop_Since_Version : constant := 5;

   Pointer_Axis_Discrete_Since_Version : constant := 5;

   Pointer_Set_Cursor_Since_Version : constant := 1;

   Pointer_Release_Since_Version : constant := 3;

   Keyboard_Release : constant := 0;

   Keyboard_Keymap_Since_Version : constant := 1;

   Keyboard_Enter_Since_Version : constant := 1;

   Keyboard_Leave_Since_Version : constant := 1;

   Keyboard_Key_Since_Version : constant := 1;

   Keyboard_Modifiers_Since_Version : constant := 1;

   Keyboard_Repeat_Info_Since_Version : constant := 4;

   Keyboard_Release_Since_Version : constant := 3;

   Touch_Release : constant := 0;

   Touch_Down_Since_Version : constant := 1;

   Touch_Up_Since_Version : constant := 1;

   Touch_Motion_Since_Version : constant := 1;

   Touch_Frame_Since_Version : constant := 1;

   Touch_Cancel_Since_Version : constant := 1;

   Touch_Shape_Since_Version : constant := 6;

   Touch_Orientation_Since_Version : constant := 6;

   Touch_Release_Since_Version : constant := 3;

   Output_Release : constant := 0;

   Output_Geometry_Since_Version : constant := 1;

   Output_Mode_Since_Version : constant := 1;

   Output_Done_Since_Version : constant := 2;

   Output_Scale_Since_Version : constant := 2;

   Output_Release_Since_Version : constant := 3;

   Region_Destroy : constant := 0;

   Region_Add : constant := 1;

   Region_Subtract : constant := 2;

   Region_Destroy_Since_Version : constant := 1;

   Region_Add_Since_Version : constant := 1;

   Region_Subtract_Since_Version : constant := 1;

   Subcompositor_Destroy : constant := 0;

   Subcompositor_Get_Subsurface : constant := 1;

   Subcompositor_Destroy_Since_Version : constant := 1;

   Subcompositor_Get_Subsurface_Since_Version : constant := 1;

   Subsurface_Destroy : constant := 0;

   Subsurface_Set_Position : constant := 1;

   Subsurface_Place_Above : constant := 2;

   Subsurface_Place_Below : constant := 3;

   Subsurface_Set_Sync : constant := 4;

   Subsurface_Set_Desync : constant := 5;

   Subsurface_Destroy_Since_Version : constant := 1;

   Subsurface_Set_Position_Since_Version : constant := 1;

   Subsurface_Place_Above_Since_Version : constant := 1;

   Subsurface_Place_Below_Since_Version : constant := 1;

   Subsurface_Set_Sync_Since_Version : constant := 1;

   Subsurface_Set_Desync_Since_Version : constant := 1;

end Wayland.Constants;
