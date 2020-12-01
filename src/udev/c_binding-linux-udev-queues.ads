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

with C_Binding.Linux.Udev.Contexts;
with C_Binding.Linux.Udev.List_Entries;

package C_Binding.Linux.Udev.Queues is
   pragma Obsolescent;

   type Queue;

   procedure Acquire
     (Original  : Queue;
      Reference : out Queue) with
     Pre => Queues.Exists (Original);

   type Queue is new Queue_Base with private;

   function Exists (Queue : Queues.Queue) return Boolean;

   procedure Delete (Queue : in out Queues.Queue) with
     Pre  => Queue.Exists,
     Post => not Queue.Exists;

   procedure Context
     (Queue   : Queues.Queue;
      Context : out Contexts.Context);

   procedure Queued_List_Entry
     (Queue      : Queues.Queue;
      List_Entry : out List_Entries.List_Entry);

   function Kernel_Sequence_Number (Queue : Queues.Queue) return Long_Integer;

   function Sequence_Number (Queue : Queues.Queue) return Long_Integer;

   function Is_Active (Queue : Queues.Queue) return Boolean;

   function Is_Empty (Queue : Queues.Queue) return Boolean;

   function Is_Sequence_Number_Finished
     (Queue           : Queues.Queue;
      Sequence_Number : Long_Integer) return Boolean;

   function Are_Sequence_Numbers_Finished
     (Queue                 : Queues.Queue;
      First_Sequence_Number : Long_Integer;
      Last_Sequence_Number  : Long_Integer) return Boolean;

   function File_Descriptor (Queue : Queues.Queue) return Integer;

   function Flush (Queue : Queues.Queue) return Success_Flag;

private

   type Queue is new Queue_Base with null record;

   overriding
   procedure Finalize (Queue : in out Queues.Queue);

end C_Binding.Linux.Udev.Queues;
