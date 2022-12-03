with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories; use Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;


procedure Day03 is
   
   package Supplies is new
     Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Integer);
   use Supplies;
   
   type Rucksack is record
      FirstHalf, SecondHalf : Supplies.Vector;
   end record;
     
   function GetScore (This : Rucksack) return Integer is
      Score : Integer := 0;
      Found : Boolean := False;
   begin
      for Item of This.FirstHalf loop
         if not Found and Contains (This.SecondHalf, Item) then
            Score := Score + Item;
            Found := True;
         end if;
      end loop;
      
      return Score;
   end GetScore;
   
   function FindCommon (Member1 : Rucksack; Member2 : Rucksack; Member3 : Rucksack) return Integer is
      Items1 : Supplies.Vector := Member1.FirstHalf;
      Items2 : Supplies.Vector := Member2.FirstHalf;
      Items3 : Supplies.Vector := Member3.FirstHalf;
   begin
      Items1.Append_Vector (Member1.SecondHalf);
      Items2.Append_Vector (Member2.SecondHalf);
      Items3.Append_Vector (Member3.SecondHalf);
      
      for Item1 of Items1 loop
         for Item2 of Items2 loop
            for Item3 of Items3 loop
               if Item1 = Item2 and Item1 = Item3 then
                  return Item1;
               end if;
            end loop;
         end loop;
      end loop;
      return 0;
   end FindCommon;
   
   function MapCharacter (Char : Character) return Integer is
      LowerCaseDelta : constant Integer := 96;
      UpperCaseDelta : constant Integer := 38;
   begin
      if Character'Pos (Char) >= 97 then
         return Character'Pos (Char) - LowerCaseDelta;
      else
         return Character'Pos (Char) - UpperCaseDelta;
      end if;
   end MapCharacter;
   
   package RucksackContainer is new
     Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Rucksack);
   use RucksackContainer;
   
   function ReadData (RelativePath : String) return RucksackContainer.Vector is
      File : File_Type;
      Path : Unbounded_String;
      Line : Unbounded_String;
      CurrentComp : Supplies.Vector;
      CurrentRuck : Rucksack;
      Content : RucksackContainer.Vector;
   begin
      Path := (Current_Directory & "/" & To_Unbounded_String (RelativePath));
      Open (File, In_File, To_String (Path));

      while not End_Of_File (File) loop
         Line := To_Unbounded_String (Get_Line (File));
         
         for I in 1 .. (Length (Line)/2) loop
            CurrentComp.Append (MapCharacter (Element (Line, I)));
         end loop;
         CurrentRuck.FirstHalf := CurrentComp;
         Clear (CurrentComp);
         
         for I in (Length (Line)/2) + 1 .. Length (Line) loop
            CurrentComp.Append (MapCharacter (Element (Line, I)));
         end loop;
         CurrentRuck.SecondHalf := CurrentComp;
         Content.Append (CurrentRuck);
         Clear (CurrentComp);
      end loop;
      return Content;
   end ReadData;
   
   Content : RucksackContainer.Vector;
   Sum : Integer := 0;
   Index : Natural := 0;
begin
   Content := ReadData ("resources/03.txt");
   
   for Ruck of Content loop
      Sum := Sum + GetScore (Ruck);
   end loop;
   
   Put_Line ("Part 1: " & Integer'Image (Sum));
   
   Sum := 0;
   
   while Index + 2 <= Natural (Content.Length) loop
      Sum := Sum + FindCommon (Content.Element (Index), Content.Element (Index + 1), Content.Element (Index + 2));
      Index := Index + 3;
   end loop;
   
   Put_Line ("Part 2: " & Integer'Image (Sum));
end Day03;
