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
-- Main program for Parallel Quasi-Coloring
----------------------------------------------------------
-- In this file we enable two options:
--  -> generating the binary format of the input
--     graph from ASCII
--  -> computing the result of the coloring
----------------------------------------------------------

with Qcolor;		use Qcolor;
with Qcolor.Tools;	use Qcolor.Tools;
with Qcolor.Host;
with Qcolor.Help;

with Ada.Command_Line;	use Ada.Command_Line;
with Ada.Directories;	use Ada.Directories;

procedure Main is
   ASCII_File : T_IO.File_Type;
   Graph_File : S_IO.File_Type;
begin
   if Argument_Count = 0 then
      raise No_Arguments;
   end if;

   if Command_Name = "" then
      T_IO.New_Line;
      T_IO.Put_Line("The executable cannot be located. Please be sure that " &
                    "you changed the working directory to the one containing" &
                    "this executable before you run the program, otherwise " &
                    "errors can be occurred.");
   else
      Set_Directory(Containing_Directory(Command_Name));
   end if;

   -- ASCII to binary conversion with '-g' command-line switch
   if Argument(1) = "-g" and Argument_Count = 2 then
      T_IO.Open(ASCII_File, T_IO.In_File, "../graph/"&Argument(2)&".ascii");
      S_IO.Create(Graph_File, S_IO.Out_File, "../graph/"&Argument(2)&".bit");

      Write_Graph(Graph_File, Read_ASCII(ASCII_File));

      T_IO.Close(ASCII_File);
      S_IO.Close(Graph_File);

   -- Coloring with '-c' command-line switch
   elsif Argument(1) = "-c" and Argument_Count = 3 then
      S_IO.Open(Graph_File, S_IO.In_File, "../graph/"&Argument(2)&".bit");

      Host(Source_File => Graph_File,
           Clr_Num => Nat_To_Unit(Natural'Value(Argument(3))));

      S_IO.Close(Graph_File);

   -- In other cases we display the Help-Screen
   else
      if Argument_Count = 1 and Argument(1)="-h" then
         Help(Full);
      else
         Help(Short);
      end if;
   end if;
exception
   when Illegal_Character =>
      T_IO.New_Line;
      T_IO.Put_Line("The ASCII-file contains illegal character!");
   when Imperfect_File =>
      T_IO.New_Line;
      T_IO.Put_Line("The graph-file contains lesser nodes than it is defined!");
   when No_Arguments =>
      Help(Short);
end Main;
