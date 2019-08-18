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
-- (body)
----------------------------------------------------------
-- This file contains the file handler and the CPU's
-- global search algorithms that summarize the partial
-- results of the several accelerator (GPU) threads.
----------------------------------------------------------

with Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded;

package body Qcolor.Tools is

   function Read_Graph(File : S_IO.File_Type; Mode : Reading_Mode) return Bit_Map is
      Nodes, Slices : Unit;
   begin
      S_IO.Read(File, Nodes);
      S_IO.Read(File, Slices);

      case Mode is
         when Parameters =>
            declare
               Params : Bit_Map := (1 => Nodes, 2 => Slices);
            begin
               return Params;
            end;
         when Data =>
            declare
               Vector : Bit_Map(1..Unit_To_Nat(Nodes)*Unit_To_Nat(Slices));
            begin
               for I in Vector'Range loop
                  if S_IO.End_Of_File(File) then
                     raise Imperfect_File;
                  end if;
                  S_IO.Read(File, Vector(I));
               end loop;
               return Vector;
            end;
      end case;
   end Read_Graph;

   procedure Write_Graph(File : in S_IO.File_Type; Graph : in Dynamic_Matrix.Vector) is
      procedure Write_To_File(C : Dynamic_Matrix.Cursor) is
      begin
         S_IO.Write(File, Dynamic_Matrix.Element(C));
      end Write_To_File;
   begin
      Dynamic_Matrix.Iterate(Graph, Write_To_File'Access);
   end Write_Graph;

   function Read_Kernel(File : Ada.Text_IO.File_Type) return String
   is
      use Ada.Strings.Unbounded;
      Text : Unbounded_String := Null_Unbounded_String;
   begin
      while not T_IO.End_Of_File(File) loop
         Append(Text, T_IO.Get_Line(File));
         Append(Text, ASCII.CR);
         Append(Text, ASCII.LF);
      end loop;
      return To_String(Text);
   end Read_Kernel;

   function Read_ASCII(File : T_IO.File_Type) return Dynamic_Matrix.Vector
   is
      package UIO renames Ada.Text_IO.Unbounded_IO;
      use Ada.Strings.Unbounded;

      Line : Ada.Strings.Unbounded.Unbounded_String;
      Size : Integer := Unit'Size - 1;
      Data :  Unit := 0;
      Line_Counter, Graph_Size : Natural := 0;
      Matrix_Temp : Dynamic_Matrix.Vector;
      Slices : Unit;
   begin
      while not T_IO.End_Of_File(File) loop
         begin
            Line := UIO.Get_Line(File);
            if Line = "" then
               Graph_Size := Line_Counter;
               Line_Counter := 0;
               Matrix_Temp.Append(Data);
               Size := Unit'Size - 1;
               Data := 0;
            else
               if Size = -1 then
                  Matrix_Temp.Append(Data);
                  Size := Unit'Size - 1;
                  Data := 0;
               end if;
               if Line="1" then
                  Data := Data or 2**Size;
               elsif Line/="0" then
                  raise Illegal_Character;
               end if;
               Size := Size - 1;
               Line_Counter := Line_Counter + 1;
            end if;
         end;
      end loop;
      Matrix_Temp.Append(Data);
      Slices := Nat_To_Unit(Natural(Float'Ceiling(Float(Graph_Size) / Float(Unit'Size))));
      Matrix_Temp.Insert(Matrix_Temp.First, Slices);
      Matrix_Temp.Insert(Matrix_Temp.First, Nat_To_Unit(Graph_Size));

      return Matrix_Temp;
   end Read_ASCII;

   procedure Global_Maximum(Vector : in Bit_Map; Pivot : out Bit_Map)
   is
      Max : Unit := Vector(2);
      Index : Unit := Vector(1);
   begin
      Pivot(1..3) := (Unit'Last, Vector(3), Vector(4));
      for I in 1..Vector'Length/4-1 loop
         if Unit_To_Nat(Vector(4*I+2)) > Unit_To_Nat(Max) then
            Max := Vector(4*I+2);
            Index := Vector(4*I+1);
            Pivot(2..3) := (Vector(4*I+3), Vector(4*I+4));
         end if;
      end loop;
      if Max /= 0 then
         Pivot(1) := Index;
      end if;
   end Global_Maximum;

   function Global_Check(Vector : Bit_Map) return Boolean
   is
   begin
      for I in 1..Vector'Length/4 loop
         if Unit_To_Nat(Vector(I)) /= 0 then
            return False;
         end if;
      end loop;
      return True;
   end Global_Check;

--     procedure Display(Log_File : in T_IO.File_Type; Data : in Bit_Map; Mode : in Display_Mode; Info : in Natural := 0)
--     is
--        Mask : Unit := 2**31;
--     begin
--        T_IO.New_Line(Log_File);
--        T_IO.Put_Line(Log_File, "=== New Section ===");
--        T_IO.New_Line(Log_File);
--        case Mode is
--           when Matrix =>
--              for I in Data'Range loop
--                 for N in reverse 0..31 loop
--                    if (Data(I) and 2**N) /= 0 then
--                       T_IO.Put(Log_File, "1");
--                    else
--                       T_IO.Put(Log_File, "0");
--                    end if;
--                 end loop;
--                 if I mod Info = 0 then
--                    T_IO.New_Line(Log_File);
--                 else
--                    T_IO.Put(Log_File, " ");
--                 end if;
--              end loop;
--           when Vector =>
--              for J in 0..Data'Length/4-1 loop
--                 T_IO.Put_Line(Log_File, Data(4*J+1)'Img & ASCII.HT & Data(4*J+2)'Img & ASCII.HT & Data(4*J+3)'Img & ASCII.HT & Data(4*J+4)'Img);
--              end loop;
--           when Table =>
--              for K in Data'Range loop
--                 if (Data(K) and Mask) = Mask then
--                    T_IO.Put(Log_File, "[");
--                    T_IO.Put(Log_File, Unit'Image(Data(K) xor Mask) & " ");
--                    T_IO.Put(Log_File, "]");
--                 else
--                    T_IO.Put(Log_File, Data(K)'Img);
--                 end if;
--                 if K mod Info = 0 then
--                    T_IO.New_Line(Log_File);
--                 end if;
--              end loop;
--        end case;
--     end Display;

end Qcolor.Tools;
