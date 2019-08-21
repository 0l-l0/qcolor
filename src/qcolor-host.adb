-------------------------------------------------------------------------
-- Copyright (C) 2019  0l-l0
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.
-------------------------------------------------------------------------

----------------------------------------------------------
-- Host program for Parallel Quasi-Coloring
----------------------------------------------------------
-- This file implements those parts of the parallel
-- program that will be running certainly on CPU.
--
-- The CPU's master process calls several kernel
-- processes that will be running on the accelerator
-- device (GPU).
----------------------------------------------------------

with Qcolor;		use Qcolor;
with Qcolor.Tools;	use Qcolor.Tools;

with System;

procedure Qcolor.Host(Source_File : in out S_IO.File_Type;
                      Clr_Num : in Unit;
                      CL_Device_Kind : in CL.Platforms.Device_Kind)
is
--     Log_File,
   Kernel_File	: T_IO.File_Type;

   -- Instance of a generic function that allows to allocate memory on the device
   function Create_Unit_Buf is
     new CL.Memory.Buffers.Constructors.Create_From_Source
       (Element => Unit, Element_List => Bit_Map);

   -- Instance of a generic package that allows the device memory handling
   package Unit_Objects is
     new CL.Queueing.Memory_Objects
       (Element => Unit, Element_List => Bit_Map);

   -- The fundamental objects to use an OpenCL environment
   Platform	: CL.Platforms.Platform;
   Device	: CL.Platforms.Device;
   Device_List	: CL.Platforms.Device_List (1 .. 1);
   Context	: CL.Contexts.Context;
---   Queue	: CL.Command_Queues.Command_Queue;
   Queue	: CL.Command_Queues.Queue;
   Event	: CL.Events.Event;
   Program	: CL.Programs.Program;

   -- The buffer objects that exist in the device memory
   Adjacency_Buffer,
   WT_Buffer,
   MA_Buffer,
   Nodes_Buffer,
   Color_Buffer,
   Range_Buffer,
   Slice_Buffer,
   Pivot_Buffer	: CL.Memory.Buffers.Buffer;

   -- Kernel objects that wrap the OpenCL subprograms implemented in *.cl file
   Kernel_1,
   Kernel_2,
   Kernel_3,
   Kernel_4	: CL.Kernels.Kernel;

   -- Helper objects to calculate some parameters
   M_Work_Size	: CL.Size;
   Local_Work_Size : aliased CL.Size_List := (1 => 1);
   Global_Work_Size : Size_List_Access;
   Params	: Bit_Map := Read_Graph(Source_File, Parameters);
   Nodes	: aliased Bit_Map := (1 => Params(1));
   Slices	: aliased Bit_Map := (1 => Params(2));
   Range_Size	: aliased Bit_Map := (1 => 0);
   Color_Number	: aliased Bit_Map := (1 => Clr_Num);
   Pivot	: aliased Bit_Map := (1..3 => 0);
begin

--     -- Creating the log file
--     T_IO.Create(Log_File, T_IO.Out_File, "../run_info.log");

   -- Initializing the OpenCL environment
   S_IO.Reset(Source_File);
   Platform	:= CL.Platforms.List(1);
   Device	:= Platform.Devices (CL_Device_Kind)(1);
   Device_List	:= (1 => Device);
   M_Work_Size	:= 80;--CL.Platforms.Max_Work_Group_Size(Device);
   Global_Work_Size := new CL.Size_List'(1 => M_Work_Size);
   Context	:= CL.Contexts.Constructors.Create_For_Devices (Platform, Device_List);

   -- Here we divide the Work-Table to equal-size ranges
   Range_Size(1) := Nat_To_Unit(Natural(Float'Ceiling(Float(Unit_To_Nat(Nodes(1))) / Float(M_Work_Size))));

   -- Declaring and initializing the object that contains the graph and the other ones that store the partial values
   declare
      --Work_Table : aliased Bit_Map := (1..Unit_To_Nat(Nodes(1)) * Unit_To_Nat(Color_Number(1)) => 0);
      Adj_Matrix : aliased Bit_Map := Read_Graph(Source_File, Data);
      Pivot_Map : aliased Bit_Map := (1..Unit_To_Nat(Slices(1)) => 0);
      Maximum_Array : aliased Bit_Map := (1..Size_To_Nat(M_Work_Size) * 4 => 0);
   begin

--        -- Testing the 'Adj_Matrix'
--        Display(Log_File, Adj_Matrix, Matrix, Unit_To_Nat(Slices(1)));

      -- Allocating the device memory for our work objects
---      Adjacency_Buffer	:= Create_Unit_Buf(Context, CL.Memory.Read_Only, Adj_Matrix'Access, True, True);
      Adjacency_Buffer	:= Create_Unit_Buf(Context, CL.Memory.Read_Only, Adj_Matrix, True, True);
      WT_Buffer		:= CL.Memory.Buffers.Constructors.Create
        (Context,
         CL.Memory.Read_Write,
         Unit'Size / System.Storage_Unit * Nat_To_Size(Unit_To_Nat(Nodes(1)) * Unit_To_Nat(Color_Number(1)))
        );
      MA_Buffer		:= CL.Memory.Buffers.Constructors.Create
        (Context,
         CL.Memory.Write_Only,
         Unit'Size / System.Storage_Unit * Maximum_Array'Length
        );
      Pivot_Buffer	:= CL.Memory.Buffers.Constructors.Create
        (Context,
         CL.Memory.Read_Only,
         Unit'Size / System.Storage_Unit * Nat_To_Size((Unit_To_Nat(Slices(1)) + 3))
        );
---      Nodes_Buffer	:= Create_Unit_Buf(Context, CL.Memory.Read_Only, Nodes'Access);
---      Color_Buffer	:= Create_Unit_Buf(Context, CL.Memory.Read_Only, Color_Number'Access);
---      Range_Buffer	:= Create_Unit_Buf(Context, CL.Memory.Read_Only, Range_Size'Access);
---      Slice_Buffer	:= Create_Unit_Buf(Context, CL.Memory.Read_Only, Slices'Access);
      Nodes_Buffer	:= Create_Unit_Buf(Context, CL.Memory.Read_Only, Nodes);
      Color_Buffer	:= Create_Unit_Buf(Context, CL.Memory.Read_Only, Color_Number);
      Range_Buffer	:= Create_Unit_Buf(Context, CL.Memory.Read_Only, Range_Size);
      Slice_Buffer	:= Create_Unit_Buf(Context, CL.Memory.Read_Only, Slices);
      Queue		:= CL.Command_Queues.Constructors.Create(Context, Device, CL.Platforms.CQ_Property_Vector'(others => False));

      -- Reading the kernel-file
      T_IO.Open(Kernel_File, T_IO.In_File, "../src/table_kernels.cl");
      declare
         Kernel_Source : aliased String := Read_Kernel(Kernel_File);
      begin
         T_IO.Close(Kernel_File);
         Program := CL.Programs.Constructors.Create_From_Source
---           (Context, (1 => Kernel_Source'Unchecked_Access));
           (Context, Kernel_Source);
      end;

      -- Building the OpenCL program from the kernel code
      Program.Build(Device_List, "", null);

      -- Setting up the kernels that we need
      Kernel_1 := CL.Kernels.Constructors.Create(Program, "fill_table");
      Kernel_1.Set_Kernel_Argument_Object(0, Adjacency_Buffer);
      Kernel_1.Set_Kernel_Argument_Object(1, WT_Buffer);
      Kernel_1.Set_Kernel_Argument_Object(2, Nodes_Buffer);
      Kernel_1.Set_Kernel_Argument_Object(3, Color_Buffer);
      Kernel_1.Set_Kernel_Argument_Object(4, Range_Buffer);
      Kernel_1.Set_Kernel_Argument_Object(5, Slice_Buffer);

      Kernel_2 := CL.Kernels.Constructors.Create(Program, "max_search");
      Kernel_2.Set_Kernel_Argument_Object(0, WT_Buffer);
      Kernel_2.Set_Kernel_Argument_Object(1, MA_Buffer);
      Kernel_2.Set_Kernel_Argument_Object(2, Nodes_Buffer);
      Kernel_2.Set_Kernel_Argument_Object(3, Color_Buffer);
      Kernel_2.Set_Kernel_Argument_Object(4, Range_Buffer);

      Kernel_3 := CL.Kernels.Constructors.Create(Program, "alter");
      Kernel_3.Set_Kernel_Argument_Object(0, WT_Buffer);
      Kernel_3.Set_Kernel_Argument_Object(1, Pivot_Buffer);
      Kernel_3.Set_Kernel_Argument_Object(2, Nodes_Buffer);
      Kernel_3.Set_Kernel_Argument_Object(3, Color_Buffer);
      Kernel_3.Set_Kernel_Argument_Object(4, Range_Buffer);

      Kernel_4 := CL.Kernels.Constructors.Create(Program, "check");
      Kernel_4.Set_Kernel_Argument_Object(0, WT_Buffer);
      Kernel_4.Set_Kernel_Argument_Object(1, MA_Buffer);
      Kernel_4.Set_Kernel_Argument_Object(2, Nodes_Buffer);
      Kernel_4.Set_Kernel_Argument_Object(3, Color_Buffer);
      Kernel_4.Set_Kernel_Argument_Object(4, Range_Buffer);

      -- Creating the content of Work-Table with 'fill_table' kernel
      Event := CL.Queueing.Execute_Kernel(Queue, Kernel_1, 1, Global_Work_Size, Local_Work_Size'Access, null);
      Event.Wait_For;

      -- Main loop that in every step checks and alters the Work-Table
      loop
         Event := CL.Queueing.Execute_Kernel
           (Queue, Kernel_2, 1, Global_Work_Size, Local_Work_Size'Access, null);
         Event.Wait_For;
---         Event := Unit_Objects.Read_Buffer(Queue, MA_Buffer, True, 0, Maximum_Array'Access, null);
---         Event := Unit_Objects.Read_Buffer(Queue, MA_Buffer, True, 0, Maximum_Array, null);
         Unit_Objects.Read_Buffer(Queue, MA_Buffer, True, 0, Maximum_Array, Event);
         Event.Wait_For;

--           -- Testing 'Work_Table'
--           Event := Unit_Objects.Read_Buffer(Queue, WT_Buffer, True, 0, Work_Table'Access, null);
--           Event.Wait_For;
--           Display(Log_File, Work_Table, Table, Unit_To_Nat(Color_Number(1)));
--
--           -- Testing 'Maximum_Array'
--           Display(Log_File, Maximum_Array, Vector);

         -- Terminal condition
         Global_Maximum(Maximum_Array, Pivot);
         exit when Pivot(1) = Unit'Last;

---         Event := Unit_Objects.Write_Buffer(Queue, Pivot_Buffer, True, 0, Pivot'Access, null);
---         Event := Unit_Objects.Write_Buffer(Queue, Pivot_Buffer, True, 0, Pivot, null);
         Unit_Objects.Write_Buffer(Queue, Pivot_Buffer, True, 0, Pivot, Event);
         Event.Wait_For;
         Pivot_Map := Adj_Matrix(Unit_To_Nat(Pivot(1))*Unit_To_Nat(Slices(1))+1..Unit_To_Nat(Pivot(1)+1)*Unit_To_Nat(Slices(1)));
---         Event := Unit_Objects.Write_Buffer
------           (Queue, Pivot_Buffer, True, 3*Unit'Size/8, Pivot_Map'Access, null);
---           (Queue, Pivot_Buffer, True, 3*Unit'Size/8, Pivot_Map, null);
         Unit_Objects.Write_Buffer(Queue, Pivot_Buffer, True, 3*Unit'Size/8, Pivot_Map, Event);
         Event.Wait_For;
         Event := CL.Queueing.Execute_Kernel
           (Queue, Kernel_3, 1, Global_Work_Size, Local_Work_Size'Access, null);
         Event.Wait_For;
      end loop;

      -- Checking the partitions in the Work-Table if they are empty
      Event := CL.Queueing.Execute_Kernel(Queue, Kernel_4, 1, Global_Work_Size, Local_Work_Size'Access, null);
      Event.Wait_For;
---      Event := Unit_Objects.Read_Buffer(Queue, MA_Buffer, True, 0, Maximum_Array'Access, null);
---      Event := Unit_Objects.Read_Buffer(Queue, MA_Buffer, True, 0, Maximum_Array, null);
      Unit_Objects.Read_Buffer(Queue, MA_Buffer, True, 0, Maximum_Array, Event);
      Event.Wait_For;

--        -- Testing 'Maximum_Array'
--        Display(Log_File, Maximum_Array, Vector);

      -- After global checking we can decide the colorability
      if Global_Check(Maximum_Array) = True then
         T_IO.Put_Line("YES - The graph is colorable with" & Natural'Image(Unit_To_Nat(Color_Number(1))) & " colors.");
      else
         T_IO.Put_Line("NO - The graph is not colorable with" & Natural'Image(Unit_To_Nat(Color_Number(1))) & " colors.");
      end if;

   end;
end Qcolor.Host;
