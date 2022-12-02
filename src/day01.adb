with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories; use Ada.Directories;
with Ada.Containers.Vectors;
with Ada.Containers.Unbounded_Priority_Queues;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;


procedure Day01 is

   package Snacks is new
     Ada.Containers.Vectors(Index_Type => Natural, Element_Type => Integer);
   use Snacks;
   package Inventory is new
     Ada.Containers.Vectors(Index_Type => Natural, Element_Type => Snacks.Vector);
   use Inventory;

   type FetteBeute is array (0 .. 2) of Integer;

   function ReadInventory(Path : String) return Inventory.Vector is
      File : File_Type;
      Inv : Inventory.Vector;
      Sn : Snacks.Vector;
      CurrentLine : Unbounded_String := To_Unbounded_String ("");
   begin
      Open (File, In_File, Path);

      while not End_Of_File (File) loop
         CurrentLine := To_Unbounded_String (Get_Line (File));
         if (CurrentLine = To_Unbounded_String ("")) then
            Inv.Append(Sn);
            Sn.Clear;
         else
            Sn.Append(Integer'Value(To_String(CurrentLine)));
         end if;
      end loop;
      Close (File);

      return Inv;
   end ReadInventory;

   function FindBestThree(Inv : Inventory.Vector) return FetteBeute is
      Temp : Integer := 0;
      MinMax : Integer := 10000000;
      Result : FetteBeute := (0, 0, 0);
   begin
      for R of Inv loop
         for I of R loop
            Temp := Temp + I;
         end loop;

         -- determine smallest entry :^)
         for Index in Integer range 0 .. 2 loop
            if Result (Index) < MinMax then
               MinMax := Result (Index);
            end if;
         end loop;

         for Index in Integer range 0 .. 2 loop
            if Result (Index) < Temp and Result (Index) = MinMax then
               Result (Index) := Temp;
            end if;
            exit when Temp = Result (Index);
         end loop;

         MinMax := 10000000;
         Temp := 0;
      end loop;
      return Result;
   end FindBestThree;

   Result : Inventory.Vector;
   TopThree : FetteBeute;
   Path : constant String := Current_Directory & "\resources\01.txt";
begin
   Result := ReadInventory(Path);
   TopThree := FindBestThree (Result);
   for I in Integer range 0..2 loop
      Put_Line (Integer'Image (TopThree (I)));
   end loop;
   Put_Line ("Sum: " & Integer'Image(TopThree (0) + TopThree (1) + TopThree (2)));
end Day01;
