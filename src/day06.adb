with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories; use Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

procedure day06 is
   
   package Buffer is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Character);
   use Buffer;
   
   function ReadDataStream (RelativePath : String) return Unbounded_String is
      File : File_Type;
      Path : Unbounded_String;
   begin     
      Path := (Current_Directory & "/" & To_Unbounded_String (RelativePath));
      Open (File, In_File, To_String (Path));
      return To_Unbounded_String (Get_Line (File));
   end ReadDataStream;
   
   function FindFrameHead (DataStream : Unbounded_String; HeadLen : Integer) return Integer is
      Buf : Buffer.Vector;
      Char : Character;
      HEAD_LEN : constant Integer := HeadLen;
   begin
      for I in 0 .. Length (DataStream) loop
         for J in 1 .. HEAD_LEN loop
            Char := Element (DataStream, I+J);
            exit when (Buf.Find (Char) /= No_Element);
            Buf.Append (Char);
         end loop;
         if Integer (Length (Buf)) = HEAD_LEN then
            return I + HEAD_LEN;
         end if;
         Buf.Clear;
      end loop;
      return -1;
   end FindFrameHead;
   
   Data : Unbounded_String := ReadDataStream ("resources/06.txt");
begin
   Put_Line (Integer'Image (FindFrameHead (Data, 4)));
   Put_Line (Integer'Image (FindFrameHead (Data, 14)));
end day06;
