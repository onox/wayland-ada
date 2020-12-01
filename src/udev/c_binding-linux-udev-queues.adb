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

package body C_Binding.Linux.Udev.Queues is

   use type int;

   function Udev_Queue_Ref (Queue : Udev_Queue_Ptr) return Udev_Queue_Ptr;
   pragma Import (C, Udev_Queue_Ref, "udev_queue_ref");

   function Udev_Queue_Unref (Queue : Udev_Queue_Ptr) return Udev_Queue_Ptr;
   pragma Import (C, Udev_Queue_Unref, "udev_queue_unref");

   function Udev_Queue_Get_Udev
     (Queue : Udev_Queue_Ptr) return Udev_Ptr;
   pragma Import (C, Udev_Queue_Get_Udev, "udev_queue_get_udev");

   function Udev_Queue_Get_Kernel_Seqnum
     (Queue : Udev_Queue_Ptr) return Interfaces.Integer_64;
   pragma Import
     (C, Udev_Queue_Get_Kernel_Seqnum, "udev_queue_get_kernel_seqnum");

   function Udev_Queue_Get_Udev_Seqnum
     (Queue : Udev_Queue_Ptr) return Interfaces.Integer_64;
   pragma Import
     (C, Udev_Queue_Get_Udev_Seqnum, "udev_queue_get_udev_seqnum");

   function Udev_Queue_Get_Udev_Is_Active
     (Queue : Udev_Queue_Ptr) return Int;
   pragma Import
     (C, Udev_Queue_Get_Udev_Is_Active, "udev_queue_get_udev_is_active");

   function Udev_Queue_Get_Queue_Is_Empty
     (Queue : Udev_Queue_Ptr) return Int;
   pragma Import
     (C, Udev_Queue_Get_Queue_Is_Empty, "udev_queue_get_queue_is_empty");

   function Udev_Queue_Get_Seqnum_Is_Finished
     (Queue  : Udev_Queue_Ptr;
      Seqnum : Interfaces.Integer_64) return Int;
   pragma Import
     (C,
      Udev_Queue_Get_Seqnum_Is_Finished,
      "udev_queue_get_seqnum_is_finished");

   function Udev_Queue_Get_Seqnum_Sequence_Is_Finished
     (Queue        : Udev_Queue_Ptr;
      Start_Number : Interfaces.Integer_64;
      End_Number   : Interfaces.Integer_64) return Int;
   pragma Import
     (C,
      Udev_Queue_Get_Seqnum_Sequence_Is_Finished,
      "udev_queue_get_seqnum_sequence_is_finished");

   function Udev_Queue_Get_Fd (Queue : Udev_Queue_Ptr) return Int;
   pragma Import (C, Udev_Queue_Get_Fd, "udev_queue_get_fd");

   function Udev_Queue_Flush (Queue : Udev_Queue_Ptr) return Int;
   pragma Import (C, Udev_Queue_Flush, "udev_queue_flush");

   function Udev_Queue_Get_Queued_List_Entry
     (Queue : Udev_Queue_Ptr) return Udev_List_Entry_Ptr;
   pragma Import
     (C,
      Udev_Queue_Get_Queued_List_Entry,
      "udev_queue_get_queued_list_entry");

   procedure Acquire
     (Original  : Queue;
      Reference : out Queue) is
   begin
      Reference.My_Ptr := Udev_Queue_Ref (Original.My_Ptr);
   end Acquire;

   function Exists (Queue : Queues.Queue) return Boolean is
     (Queue.My_Ptr /= null);

   procedure Delete (Queue : in out Queues.Queue) is
   begin
      Queue.My_Ptr := Udev_Queue_Unref (Queue.My_Ptr);

      Queue.My_Ptr := null;
      --  Is unnecessary, but static code analyzers cannot know
      --  Udev_Queue_Unref (..) always returns null.
   end Delete;

   procedure Context
     (Queue   : Queues.Queue;
      Context : out Contexts.Context) is
   begin
      Context_Base (Context).My_Ptr := Udev_Queue_Get_Udev (Queue.My_Ptr);
   end Context;

   function Kernel_Sequence_Number (Queue : Queues.Queue) return Long_Integer is
     (Long_Integer (Udev_Queue_Get_Kernel_Seqnum (Queue.My_Ptr)));

   function Sequence_Number (Queue : Queues.Queue) return Long_Integer is
     (Long_Integer (Udev_Queue_Get_Udev_Seqnum (Queue.My_Ptr)));

   function Is_Active (Queue : Queues.Queue) return Boolean is
     (if (Udev_Queue_Get_Udev_Is_Active (Queue.My_Ptr) /= 0) then
        (True)
      else
        (False));

   function Is_Empty (Queue : Queues.Queue) return Boolean is
     (if (Udev_Queue_Get_Queue_Is_Empty (Queue.My_Ptr) /= 0) then
        (True)
      else
        (False));

   function Is_Sequence_Number_Finished
     (Queue           : Queues.Queue;
      Sequence_Number : Long_Integer) return Boolean is
     (if
        (Udev_Queue_Get_Seqnum_Is_Finished
             (Queue.My_Ptr, Interfaces.Integer_64 (Sequence_Number)) /= 0)
      then
        (True)
      else
        (False));

   function Are_Sequence_Numbers_Finished
     (Queue                 : Queues.Queue;
      First_Sequence_Number : Long_Integer;
      Last_Sequence_Number  : Long_Integer) return Boolean is
     (if
        (Udev_Queue_Get_Seqnum_Sequence_Is_Finished
             (Queue.My_Ptr,
              Interfaces.Integer_64 (First_Sequence_Number),
              Interfaces.Integer_64 (First_Sequence_Number)) /= 0)
      then
         True
      else
         False);

   function File_Descriptor (Queue : Queues.Queue) return Integer is
     (Integer (Udev_Queue_Get_Fd (Queue.My_Ptr)));

   function Flush (Queue : Queues.Queue) return Success_Flag is
     (if (Udev_Queue_Flush (Queue.My_Ptr) = 0) then
         Success
      else
         Failure);

   procedure Queued_List_Entry
     (Queue      : Queues.Queue;
      List_Entry : out List_Entries.List_Entry) is
   begin
      List_Entry_Base (List_Entry).My_Ptr
        := Udev_Queue_Get_Queued_List_Entry (Queue.My_Ptr);
   end Queued_List_Entry;

   procedure Finalize (Queue : in out Queues.Queue) is
   begin
      if Queue.Exists then
         Queue.Delete;
      end if;
   end Finalize;

end C_Binding.Linux.Udev.Queues;
