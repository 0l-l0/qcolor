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

with CL.Platforms;

with Ada.Command_Line;	use Ada.Command_Line;
with Ada.Directories;	use Ada.Directories;
with Ada.Environment_Variables;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Equal_Case_Insensitive;

procedure Main is
   package Env_Var renames Ada.Environment_Variables;
   package Str_U renames Ada.Strings.Unbounded;

   -- String manipulation functions
   function Str2Unb(Orig : String) return Str_U.Unbounded_String
      renames Str_U.To_Unbounded_String;

   function Unb2Str(Orig : Str_U.Unbounded_String) return String
      renames Str_U.To_String;

   function Str_Eq_I(Left, Right : Str_U.Unbounded_String) return Boolean
      renames Str_U.Equal_Case_Insensitive;

   ASCII_File : T_IO.File_Type;
   Graph_File : S_IO.File_Type;

   -- Variables for device selection
   Dev_Kind_Name : Str_U.Unbounded_String := Str2Unb("GPU");
   Dev_Kind : CL.Platforms.Device_Kind;
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

      -- Selecting default OpenCL device
      if Env_Var.Exists("CL_DEFAULT_DEVICE")
      then
         Dev_Kind_Name := Str2Unb(Env_Var.Value("CL_DEFAULT_DEVICE"));
      end if;

      if Str_Eq_I(Dev_Kind_Name, Str2Unb("GPU")) then
         Dev_Kind := CL.Platforms.Device_Kind'(GPU => True, others => False);
      elsif Str_Eq_I(Dev_Kind_Name, Str2Unb("CPU")) then
         Dev_Kind := CL.Platforms.Device_Kind'(CPU => True, others => False);
      elsif Str_Eq_I(Dev_Kind_Name, Str2Unb("ACCELERATOR")) then
         Dev_Kind :=
            CL.Platforms.Device_Kind'(Accelerator => True, others => False);
      else
         raise Invalid_Device_Kind;
      end if;

      Host(Source_File => Graph_File,
           Clr_Num => Nat_To_Unit(Natural'Value(Argument(3))),
           CL_Device_Kind => Dev_Kind);

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
   when Invalid_Device_Kind =>
      T_IO.New_Line;
      T_IO.Put_Line("The value '" & Unb2Str(Dev_Kind_Name) & "' of the " &
         "environment variable CL_DEFAULT_DEVICE is not a valid device kind name.");
      T_IO.Put_Line(" Please select one of GPU, CPU and ACCELERATOR.");
   when No_Arguments =>
      Help(Short);
end Main;
