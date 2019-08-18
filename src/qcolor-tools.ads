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
-- CPU subprograms for Parallel Quasi-Coloring
-- (specification)
----------------------------------------------------------
-- This file contains the file handler and the CPU's
-- global search algorithms that summarize the partial
-- results of the several accelerator (GPU) threads.
----------------------------------------------------------

package Qcolor.Tools is

   -- Reading the binary graph from an external file
   function Read_Graph(File : S_IO.File_Type; Mode : Reading_Mode) return Bit_Map;

   -- Writing the binray graph to a file
   procedure Write_Graph(File : in S_IO.File_Type; Graph : in Dynamic_Matrix.Vector);--Graph_Type);

   -- Reading the kernel source code from the given *.cl file
   function Read_Kernel(File : T_IO.File_Type) return String;

   -- Reading the graph from an external file in ASCII format and converting it to binary format
   function Read_ASCII(File : T_IO.File_Type) return Dynamic_Matrix.Vector;

   -- Choosing the node that have the maximal difference between its partitions
   procedure Global_Maximum(Vector : in Bit_Map; Pivot : out Bit_Map);

   -- Checking if all of the partitions are empty
   function Global_Check(Vector : Bit_Map) return Boolean;

--     -- To test the partial results
--     procedure Display(Log_File : in T_IO.File_Type; Data : in Bit_Map; Mode : in Display_Mode; Info : in Natural := 0);

end Qcolor.Tools;
