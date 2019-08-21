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
-- The main specification file for Parallel Quasi-
-- Coloring
----------------------------------------------------------
-- In this file we declare the packages, types, conversion
-- functions, and exceptions that all parts of the program
-- can use.
----------------------------------------------------------

with Ada.Text_IO;
with Ada.Sequential_IO;
with Ada.Containers.Vectors;
with Ada.Unchecked_Conversion;
with Ada.Strings.Unbounded;

with Interfaces;

with CL.Platforms;
with CL.Contexts;
with CL.Memory.Buffers;
with CL.Command_Queues;
with CL.Programs;
with CL.Kernels;
with CL.Queueing;
with CL.Queueing.Memory_Objects;
with CL.Events;

package Qcolor is

   -- Main type
   subtype Unit is CL.Uint;

   -- Type declarations
---   type	Bit_Map is array(Positive range <>) of aliased Unit;
   type	Bit_Map is array(Integer range <>) of aliased Unit;
   type Help_Mode is (Short, Full);
   type Reading_Mode is (Parameters, Data);
   type Display_Mode is (Matrix, Vector, Table);
   type Size_List_Access is access CL.Size_List;

   -- Usings
   use type CL.Size;
   use type CL.Uint;
   use type Interfaces.Unsigned_32;

   -- Package declarations
   package T_IO renames Ada.Text_IO;
   package S_IO is new Ada.Sequential_IO(Unit);
   package Dynamic_Matrix is new Ada.Containers.Vectors(Positive, Unit);

   -- Unchecked conversions
   function Nat_To_Unit is new Ada.Unchecked_Conversion(Natural, Unit);
   function Unit_To_Nat is new Ada.Unchecked_Conversion(Unit, Natural);
   function Size_To_Nat is new Ada.Unchecked_Conversion(CL.Size, Natural);
   function Nat_To_Size is new Ada.Unchecked_Conversion(Natural, CL.Size);
   function Count_To_Nat is new Ada.Unchecked_Conversion(Ada.Containers.Count_Type, Natural);
   function Count_To_Unit is new Ada.Unchecked_Conversion(Ada.Containers.Count_Type, Unit);

   -- Exceptions
   Imperfect_File : exception;
   Illegal_Character : exception;
   No_Arguments : exception;
   Invalid_Device_Kind : exception;

end Qcolor;
