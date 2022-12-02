with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Directories; use Ada.Directories;
with Ada.Containers.Vectors;


procedure Day02 is

   type Pick is
     (ROCK,
      PAPER,
      SCISSORS);
   
   for Pick use
     (ROCK => 1,
      PAPER => 2,
      SCISSORS => 3);
   
   type PickPair is record
      MyPick, OpponentPick : Pick;
   end record;

   package StrategyGuide is new
     Ada.Containers.Vectors(Index_Type => Natural, Element_Type => PickPair);
   use StrategyGuide;

   function Score (OpponentPick : Pick; MyPick : Pick) return Integer;

   function ReadData (RelativePath : String) return StrategyGuide.Vector;

   function MapInput (Char : Character) return Pick;
   
   function MatchOpponent (SupposedOutcome : Character; OpponentPick : Pick) return Pick;
   
   function ReadData (RelativePath : String) return StrategyGuide.Vector
   is
      File : File_Type;
      Line : Unbounded_String;
      Path : Unbounded_String;
      Pair : PickPair;
      Guide : StrategyGuide.Vector;
   begin
      Path := (Current_Directory & "/" & To_Unbounded_String (RelativePath));
      Open (File, In_File, To_String (Path));

      while not End_Of_File (File) loop
         Line := To_Unbounded_String (Get_Line (File));
         Pair.OpponentPick := MapInput (Element (Line, 1));
         Pair.MyPick := MatchOpponent (Element (Line, 3), Pair.OpponentPick);

         Guide.Append (Pair);

      end loop;
      return Guide;
   end ReadData;

   function MapInput (Char : Character) return Pick
   is
   begin
      if Char = 'A' then
         return ROCK;
      elsif Char = 'B' then
         return PAPER;
      else return SCISSORS;
      end if;
   end MapInput;
   
   function MatchOpponent (SupposedOutcome : Character; OpponentPick : Pick) return Pick
   is
      Draw : constant Character := 'Y';
      Win : constant Character := 'Z';
   begin
      if SupposedOutcome = Win then
         return Pick'Enum_Val (((Pick'Enum_Rep (OpponentPick)) mod 3) + 1);
      elsif SupposedOutcome = Draw then
         return OpponentPick;
      else
         return Pick'Enum_Val ((((Pick'Enum_Rep (OpponentPick)) - 2) mod 3) + 1);
      end if;
   end MatchOpponent;

   function Score (OpponentPick : Pick; MyPick : Pick) return Integer
   is
      Points : Integer := 0;
   begin
      
      Points := Pick'Enum_Rep (MyPick);
      
      if MatchOpponent ('Z', OpponentPick) = MyPick then
         Points := Points + 6;
      elsif OpponentPick = MyPick then
         Points := Points + 3;
      end if;

      return Points;
   end Score;

   Guide : StrategyGuide.Vector;
   Count : Integer := 0;
begin
   Guide := ReadData ("resources/02.txt");

   for Pair of Guide loop
      Count := Count + Score (Pair.OpponentPick, Pair.MyPick);
   end loop;

   Put_Line (Integer'Image (Count));
end Day02;
