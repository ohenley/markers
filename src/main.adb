with Markers;         use Markers;
with Ada.Numerics.Discrete_Random;
with Ada.Text_IO;   use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Containers.Vectors;

with Ada.Numerics.Float_Random;

use Ada.Containers;

procedure Main is
   type Rand_T is range 1 .. 4_000;
   package Rand_Pos is new Ada.Numerics.Discrete_Random (Rand_T);
   use Rand_Pos;
   Gen     : Generator;
   Size    : Rand_T;
   Start   : Time;
   Elapsed : Time_Span;
   Cont   : Boolean := True;
   Top_Value : Integer := 0;

   package Allocations_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Markers.Allocation_T);

   Allocation : Markers.Allocation_T;

   Idx : Natural;

   function Get_Random_Natural (Upper_Bound : Natural) return Natural is
      G : Ada.Numerics.Float_Random.Generator;
   begin
      return
        Natural (Ada.Numerics.Float_Random.Random (G) * Float (Upper_Bound));
   end Get_Random_Natural;

   procedure Deallocate_In_Order (V : Allocations_Vectors.Vector) is
   begin
      for I of V loop
         Markers.Deallocate (I.Location, I.Size);
      end loop;
   end;

   procedure Deallocate_In_Random_Order (V : in out Allocations_Vectors.Vector) is
      D : Allocations_Vectors.Vector := V;
      C : Allocations_Vectors.Cursor;
   begin
      while D.Length > 0 loop
         Idx := Get_Random_Natural (Natural(D.Length - 1));
         Start := Clock;
         Markers.Deallocate (D (Idx).Location, D (Idx).Size);
         Elapsed := Clock - Start;
         C := V.Find (D (Idx));
         V (C).Dealloc_Time := Elapsed;
         D.Delete (Idx);
      end loop;
   end;

   function Run return Integer is
      A : Allocations_Vectors.Vector;
      Match : Markers.Match_T;
      use Markers;
   begin
      Reset (Gen);
      while True loop
         Allocation.Size       := Natural (Random (Gen));
         Start                 := Clock;
         Match                 := Markers.Allocate (Positive (Allocation.Size), Allocation.Location);
         Allocation.Alloc_Time := Clock - Start;
         if Match /= Markers.Impossible then
            A.Append (Allocation);
         else
            Put_Line("Failed to allocate " & Allocation.Size'Image & " in " & Allocation.Alloc_Time'Image);
            Deallocate_In_Order (A);
            -- Deallocate_In_Random_Order (A);
            return Markers.Get_Top_Marker (0);
         end if;
      end loop;
   end;

begin

   while Cont loop
      Put_Line ("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++");
      Markers.Reset_Markers;
      Top_Value := Run;
      if Top_Value /= Markers.Get_Covered_Memory_Size then
         Put_Line ("Top Value = " & Top_Value'Image);
         Markers.Print_Markers;
         Cont := False;
      end if;
   end loop;

end Main;
