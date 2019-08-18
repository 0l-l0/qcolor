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
-- Help-Screen file for Parallel Quasi-Coloring
----------------------------------------------------------
-- This file contains the short and full helps that will
-- be displayed either when we use '-h' command-line
-- switch in Main program or we call the program not in
-- in the right form.
----------------------------------------------------------

procedure Qcolor.Help(Mode : in Help_Mode) is
begin
   T_IO.New_Line;
   case Mode is

      -- Wrong use help-line
      when Short =>
         T_IO.Put_Line("Use '-h' to read the help!");

      -- Full Help-Screen
      when Full =>
         T_IO.Put_Line("===========================================");
         T_IO.Put_Line("==  Parallel Quasi-Coloring with OpenCL  ==");
         T_IO.Put_Line("===========================================");
         T_IO.New_Line;
         T_IO.Put_Line("Usage: qcolor <option> <arguments>");
         T_IO.New_Line;
         T_IO.Put_Line(ASCII.HT & "-g" & ASCII.HT & "Generate the graph-file from ASCII format.");
         T_IO.Put_Line(ASCII.HT & ASCII.HT & "Put the ASCII-file into the graph directory");
         T_IO.Put_Line(ASCII.HT & ASCII.HT & "and use the '.ascii' extension for it!");
         T_IO.Put_Line(ASCII.HT & ASCII.HT & "In <source> argument use the filename without");
         T_IO.Put_Line(ASCII.HT & ASCII.HT & "extension!");
         T_IO.New_Line;
         T_IO.Put_Line(ASCII.HT & ASCII.HT & "<arguments> : <source>");
         T_IO.New_Line;
         T_IO.Put_Line(ASCII.HT & "-c" & ASCII.HT & "Compute the result.");
         T_IO.Put_Line(ASCII.HT & ASCII.HT & "In <graph-file> argument use the filename");
         T_IO.Put_Line(ASCII.HT & ASCII.HT & "without extension!");
         T_IO.New_Line;
         T_IO.Put_Line(ASCII.HT & ASCII.HT & "<arguments> : <graph-file> <number-of-colors>");
         T_IO.New_Line;
         T_IO.Put_Line(ASCII.HT & "-h" & ASCII.HT & "Show this help.");
         T_IO.New_Line;
   end case;
end Qcolor.Help;
