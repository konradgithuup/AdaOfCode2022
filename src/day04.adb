with GNAT.Spitbol.Patterns; use GNAT.Spitbol.Patterns;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories; use Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

procedure day04 is

   type Assignment is record
      LowerBound : Integer;
      UpperBound : Integer;
   end record;
   
   type ElfPair is record
      Elf1 : Assignment;
      Elf2 : Assignment;
   end record;

   package Data is new
     Ada.Containers.Vectors (Index_Type => Natural, Element_Type => ElfPair);
   
   -- task 1
   -- heh, butt joke :^)
   function FullyContains (Pair : ElfPair) return Boolean is
      Ass1 : Assignment := Pair.Elf1;
      Ass2 : Assignment := Pair.Elf2;
   begin
      if (Ass1.LowerBound <= Ass2.LowerBound and Ass1.UpperBound >= Ass2.UpperBound) then
         return True;
      elsif (Ass1.LowerBound >= Ass2.LowerBound and Ass1.UpperBound <= Ass2.UpperBound) then
         return True;
      end if;
      return False;
   end FullyContains;
   
   -- task 2
   function AssignmentOverlap (Pair : ElfPair) return Boolean is
      Ass1 : Assignment := Pair.Elf1;
      Ass2 : Assignment := Pair.Elf2;
   begin
      if (Ass1.LowerBound > Ass2.UpperBound or Ass1.UpperBound < Ass2.LowerBound) then
         return False;
      end if;
      return True;
   end AssignmentOverlap;
   
   -- read Input
   function GetAssignment (Lower : Unbounded_String; Upper : Unbounded_String) return Assignment is
      Result : Assignment;
   begin
      Result.LowerBound := Integer'Value (To_String (Lower));
      Result.UpperBound := Integer'Value (To_String (Upper));
      
      return Result;
   end GetAssignment;
   
   function ReadData (RelativePath : String) return Data.Vector is
      File : File_Type;
      Path : Unbounded_String;
      Line : Unbounded_String;
      Num1, Num2, Num3, Num4 : Unbounded_String;
      Delims : Pattern := Span(",-");
      Decimals : Pattern := Span("0123456789");
      Pat : Pattern := Decimals * Num1 & Delims & Decimals * Num2 & Delims & Decimals * Num3 & Delims & Decimals * Num4;
      Content : Data.Vector;
      CurrentPair : ElfPair;
   begin
      Path := (Current_Directory & "/" & To_Unbounded_String (RelativePath));
      Open (File, In_File, To_String (Path));

      while not End_Of_File (File) loop
         Line := To_Unbounded_String (Get_Line (File));
         Match (Line, Pat);
         
         CurrentPair.Elf1 := GetAssignment (Num1, Num2);
         CurrentPair.Elf2 := GetAssignment (Num3, Num4);         
         
         Content.Append (CurrentPair);
      end loop;
      return Content;
   end ReadData;
   Content : Data.Vector;
   Count, Count2 : Integer := 0;
begin
   Content := ReadData ("resources/04.txt");
   
   for Pair of Content loop
      if FullyContains (Pair) then
         Count := Count + 1;
      end if;
      if AssignmentOverlap (Pair) then
         Count2 := Count2 + 1;
      end if;
   end loop;
   
   Put_Line ("Task 1: " & Integer'Image (Count));
   Put_Line ("Task 2: " & Integer'Image (Count2));
end day04;
