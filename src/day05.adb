with GNAT.Spitbol.Patterns; use GNAT.Spitbol.Patterns;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories; use Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;


procedure day05 is
   
   package Stack is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Character);
   use Stack;
   
   type CargoIndex is new Natural range 1 .. 10000;
   
   package Cargo is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Stack.Vector);
   
   type Instruction is record
      Amount : Integer;
      Origin : Integer;
      Target : Integer;
   end record;
   
   package InstructionSet is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Instruction);
   
   -- task1
   procedure MoveItems (Origin : in out Stack.Vector; Target : in out Stack.Vector; Amount : Integer) is
      Item : Character := Origin.Last_Element;
   begin
      
      for I in 1 .. Amount loop
         Item := Origin.Last_Element;
         Origin.Delete_Last;
         Target.Append (Item);
      end loop;
   end;
   
   -- task2
   procedure MoveItemsStable (Origin : in out Stack.Vector; Target : in out Stack.Vector; Amount : Integer) is
      HelpStack : Stack.Vector;
   begin
      MoveItems (Origin, HelpStack, Amount);
      MoveItems (HelpStack, Target, Amount);
   end;
   
   function ReadCargo (RelativePath : String) return Cargo.Vector is
      File : File_Type;
      Path : Unbounded_String;
      Line : Unbounded_String;
      Content : Cargo.Vector;
      CurrentStack : Stack.Vector;
   begin
      Path := (Current_Directory & "/" & To_Unbounded_String (RelativePath));
      Open (File, In_File, To_String (Path));
      
      while not End_Of_File (File) loop
         Line := To_Unbounded_String (Get_Line (File));
         if Length (Line) = 0 then
            Close (File);
            return Content;
         end if;
         
         for I in 1 .. Length (Line) loop
            CurrentStack.Append (Element (Line, I));
         end loop;
         Content.Append (CurrentStack);
         CurrentStack.Clear;
      end loop;
      
      Close (File);
      
      return Content;
   end ReadCargo;
   
   function ReadInstructions (RelativePath : String) return InstructionSet.Vector is
      File : File_Type;
      Path : Unbounded_String;
      Line : Unbounded_String;
      Content : InstructionSet.Vector;
      CurrentInstruction : Instruction;
      Origin, Target, Amount : Unbounded_String;
      Cringe : Pattern := Span (" fromvet");
      Decimals : Pattern := Span ("0123456789");
      Pat : Pattern := Cringe & Decimals * Amount & Cringe & Decimals * Origin & Cringe & Decimals * Target;
   begin
      Path := (Current_Directory & "/" & To_Unbounded_String (RelativePath));
      Open (File, In_File, To_String (Path));

      while not End_Of_File (File) loop
         Line := To_Unbounded_String (Get_Line (File));
         if (Match (Line, Pat)) then
            CurrentInstruction.Amount := Integer'Value (To_String(Amount));
            CurrentInstruction.Origin := Integer'Value (To_String(Origin)) - 1;
            CurrentInstruction.Target := Integer'Value (To_String(Target)) - 1;
                       
            Content.Append (CurrentInstruction);
         end if;
      end loop;
      
      Close (File);
      
      return Content;
   end ReadInstructions;
   
   Content : Cargo.Vector;
   Instructions : InstructionSet.Vector;
   TargetStack : Stack.Vector;
   OriginStack : Stack.Vector;
begin
   Content := ReadCargo ("resources/05.txt");
   Instructions := ReadInstructions ("resources/05.txt");
   
   for Inst of Instructions loop
      OriginStack := Content.Element (Inst.Origin);
      TargetStack := Content.Element (Inst.Target);
      MoveItemsStable (OriginStack, TargetStack, Inst.Amount);
      Content.Replace_Element (Inst.Origin, OriginStack);
      Content.Replace_Element (Inst.Target, TargetStack);
   end loop;
   
   for Crate of Content loop
      Put (Character'Image (Crate.Last_Element));
   end loop;
end day05;
