with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Generic_Elementary_Functions;
with System;

package body Markers is
    -- pragma Optimize( Off );
   pragma Suppress( All_Checks );

   Packet_Size : Positive := 512;
   Nbr_Layers  : Positive := 5;

   type Array_Positive_T is array (Natural range <>) of Positive;
   type Array_Natural_T is array (Natural range <>) of Natural;
   type Markers_T is array (Natural range <>) of Marker_T;
   type Combined_Array_T is array (Natural range <>) of Integer;

   type Idxs_T is array (0 .. 1) of Natural;

   subtype Fork_Idx is Natural range 0 .. 1;
   type Choice_T is record
      Value : Natural := 0;
      Fork_Idx : Natural := 0;
      Local_Idx : Natural := 0;
      Match : Match_T := Impossible;
   end record;
   type Choices_Array_T is array (Natural range <>) of Choice_T;

   Max_Layer_Values    : Array_Positive_T (0 .. Nbr_Layers - 1);
   Nbr_Layer_Markers   : Array_Positive_T (0 .. Nbr_Layers - 1);
   Offset_Layer_Values : Array_Positive_T (0 .. Nbr_Layers - 1);

   function Get_Total_Nbr_Markers (Nbr_Layers : Positive) return Positive is
      Total       : Natural  := 0;
      Nbr_Packets : Positive := 2**(Nbr_Layers - 1);
   begin
      Total := Nbr_Packets;
      for I in 1 .. Nbr_Layers loop
         Total := Total + Nbr_Packets / (2**I);
      end loop;
      return Total + 1;
   end;

   Markers : Markers_T (0 .. Get_Total_Nbr_Markers (Nbr_Layers) - 1);

   function Get_Top_Marker return Marker_T is
      (Markers (Markers'Length - 2));

   function Get (Idx : Natural; Layer : Natural) return Marker_T is
   begin
      return Markers (Offset_Layer_Values (Layer) + Idx);
   end Get;

   procedure Set (Idx : Natural; Layer : Natural; M : Marker_T) is
   begin
      Markers (Offset_Layer_Values (Layer) + Idx) := M;
   end Set;

   procedure Set
     (Idx : Natural; Layer : Natural; Local_Idx : Natural; Value : Integer)
   is
   begin
      Markers (Offset_Layer_Values (Layer) + Idx) (Local_Idx) := Value;
   end Set;

   procedure Print_Marker (M : Marker_T) is
   begin
      Put ("[" & M (0)'Image & "," & M (1)'Image & "," & M (2)'Image & "]");
   end Print_Marker;

   procedure Print_Markers is
   begin
      for Layer in 0 .. Nbr_Layers - 1 loop
         for I in 0 .. Nbr_Layer_Markers (Layer) - 1 loop
            Print_Marker (Get (I, Layer));
         end loop;
         Put_Line ("");
      end loop;
      Put_Line ("");
   end Print_Markers;

   procedure Compute_Max_Layer_Values is
   begin
      for I in Max_Layer_Values'Range loop
         Max_Layer_Values (I) := (2**I) * Packet_Size;
      end loop;
   end Compute_Max_Layer_Values;

   procedure Compute_Nbr_Layer_Markers is
      Nbr_Packets : Positive := 2**(Nbr_Layer_Markers'Length - 1);
   begin
      for I in Nbr_Layer_Markers'Range loop
         Nbr_Layer_Markers (I) := Nbr_Packets / (2**I);
      end loop;
      Nbr_Layer_Markers (Nbr_Layer_Markers'Last) :=
        Nbr_Layer_Markers (Nbr_Layer_Markers'Last) + 1;
   end Compute_Nbr_Layer_Markers;

   procedure Compute_Offset_Layer_Values is
      Offset : Natural := 0;
   begin
      for Layer in 0 .. Nbr_Layers - 1 loop
         Offset_Layer_Values (Layer) := Offset;
         Offset                      := Offset + Nbr_Layer_Markers (Layer);
      end loop;
   end Compute_Offset_Layer_Values;

   procedure Build_Markers is
      Idx       : Natural := 0;
      Max_Value : Positive;
   begin
      for Layer in 0 .. Nbr_Layers - 1 loop
         for I in 0 .. Nbr_Layer_Markers (Layer) - 1 loop
            Idx           := Offset_Layer_Values (Layer) + I;
            Max_Value     := Max_Layer_Values (Layer);
            Markers (Idx) := (Natural (Max_Value), 0, Natural (Max_Value));
         end loop;
      end loop;
      Markers (Idx) := (0, 0, 0);
   end Build_Markers;

   procedure Reset_Markers is
   begin
      Compute_Max_Layer_Values;
      Compute_Nbr_Layer_Markers;
      Compute_Offset_Layer_Values;
      Build_Markers;
   end Reset_Markers;

   function Build_Marker
     (M0 : Marker_T; M1 : Marker_T; Layer : Natural) return Marker_T
   is
      function Combine (M0 : Marker_T; M1 : Marker_T) return Combined_Array_T
      is
      begin
         if (M0 (2) > 0 and M1 (0) > 0) or (M0 (2) < 0 and M1 (0) < 0) then
            return (M0 (0), M0 (1), M0 (2) + M1 (0), M1 (1), M1 (2));
         else
            return (M0 (0), M0 (1), M0 (2), M1 (0), M1 (1), M1 (2));
         end if;
      end Combine;

      function Get_Maximum (C : Combined_Array_T) return Natural is
         Max : Natural := 0;
      begin
         for I in C'Range loop
            if C (I) > Max then
               Max := C (I);
            end if;
         end loop;
         return Max;
      end Get_Maximum;

      Max_Value       : Positive         := Max_Layer_Values (Layer);
      Lower_Max_Value : Positive         := Max_Layer_Values (Layer - 1);
      M               : Combined_Array_T := Combine (M0, M1);
      Res             : Marker_T         := (0, 0, 0);
   begin
      if M'Length = 5 then
         if M (2) = Max_Value then
            Res := (Max_Value, 0, Max_Value);
         elsif M (1) = 0 and abs M (0) = Lower_Max_Value then
            Res := (M (2), M (3), M (4));
         elsif M (3) = 0 and abs M (4) = Lower_Max_Value then
            Res := (M (0), M (1), M (2));
         else
            Res := (M (0), Get_Maximum ((M (1), M (2), M (3))), M (4));
         end if;
      end if;

      if M'Length = 6 then
         if M (0) = M (2) and M (3) = M (5) then
            Res := (M (0), 0, M (5));
         elsif M (0) = M (2) and M (1) = 0 then
            Res := (M (0), Get_Maximum ((M (3), M (4))), M (5));
         elsif M (3) = M (5) and M (4) = 0 then
            Res := (M (0), Get_Maximum ((M (1), M (2))), M (5));
         else
            Res := (M (0), Get_Maximum ((M (1), M (2), M (3), M (4))), M (5));
         end if;
      end if;
      return Res;
   end Build_Marker;


   function Reset_Marker(M : in out Marker_T; Max_Value : Natural) return Marker_T is
   begin
      if (M (0) + M (2)) = Max_Value and M (1) = 0 then
         M := (Max_Value, 0, Max_Value);
      end if;
      return M;
   end;

   procedure Build_Marker_Down (True_M : Marker_T;
                                M0 : in out Marker_T;
                                M1 : in out Marker_T;
                                Layer : Natural) is
      Max_Value : Positive := Max_Layer_Values (Layer);
      Max_Value_Down : Positive := Max_Value / 2;
      New_M0    : Marker_T := M0;
      New_M1    : Marker_T := M1;
      F         : Integer := True_M (0);
      L         : Integer := True_M (2);
      Rest      : Integer := 0;
   begin

      if F < 0 then                                          -- [-F, ...]
         if F < -Max_Value_Down then                         -- max value down : 1024, True_M : [-2012, ...]
            New_M0 := (-Max_Value_Down, 0, -Max_Value_Down); -- New_M0 : [-1024, 0, -1024]
            Rest := F + Max_Value_Down;                      -- -2012 + 1024 = -988
            New_M1 (0) := Rest;                              -- New_M1 : [-988, ...]
         elsif F = -Max_Value_Down then
            New_M0 := (-Max_Value_Down, 0, -Max_Value_Down); -- New_M0 : [-1024, 0, -1024]
         else                                                -- max value down : 1024, True_M : [-36, ...]
            New_M0 (0) := F;                                 -- New_M0 : [-36, ..., ...]
            New_M0 (1) := M0 (1);                            -- New_M0 : [..., M0 (1), ...]
         end if;
      else                                                   -- [F, ...]
         if F > Max_Value_Down then                          -- max value down : 1024, True_M : [2012, ...]
            New_M0 := (Max_Value_Down, 0, Max_Value_Down);   -- New_M0 : [1024, 0, 1024]
            Rest := F - Max_Value_Down;                      -- 2012 - 1024 = 988
            New_M1 (0) := Rest;                              -- New_M1 : [988, ...]
         elsif F = Max_Value_Down then
            New_M0 := (Max_Value_Down, 0, Max_Value_Down);
         else
            New_M0 (0) := F;                                 -- New_M0 : [36, ..., ...]
            New_M0 (1) := M0 (1);                            -- New_M0 : [..., M0 (1), ...]
         end if;
      end if;

      if L < 0 then                                          -- [..., -L]
         if L < -Max_Value_Down then                         -- max value down : 1024, True_M : [..., -2012]
            New_M1 := (-Max_Value_Down, 0, -Max_Value_Down); -- New_M1 : [-1024, 0, -1024]
            Rest := L + Max_Value_Down;                      -- -2012 + 1024 = -988
            New_M0 (2) := Rest;                              -- New_M0 : [..., -988]
         elsif L = -Max_Value_Down then
            New_M1 := (-Max_Value_Down, 0, -Max_Value_Down);
         else                                                -- max value down : 1024, True_M : [..., -36]
            New_M1 (1) := M1( 1);                            -- New_M1 : [..., M1 (1), ...]
            New_M1 (2) := L;                                 -- New_M1 : [..., -36]
         end if;
      else                                                   -- [..., L]
         if L > Max_Value_Down then                          -- max value down : 1024, True_M : [..., 2012]
            New_M1 := (Max_Value_Down, 0, Max_Value_Down);   -- New_M1 : [1024, 0, 1024]
            Rest := L - Max_Value_Down;                      -- 2012 - 1024 = 988
            New_M0 (2) := Rest;                              -- New_M0 : [..., 988]
         elsif L = Max_Value_Down then
            New_M1 := (Max_Value_Down, 0, Max_Value_Down);
         else
            New_M1 (1) := M1( 1);                            -- New_M1 : [..., M1 (1), ...]
            New_M1 (2) := L;                                 -- New_M1 : [..., 36]                   
         end if;
      end if;

      if True_M (0) /= True_M (2) then
         if True_M (0) = -Max_Value_Down then
            New_M0 := (-Max_Value_Down, 0, 0);
         end if;
         if True_M (2) = -Max_Value_Down then
            New_M1 := (0, 0, -Max_Value_Down);
         end if;
      end if;

      M0 := Reset_Marker(New_M0, Max_Value_Down);
      M1 := Reset_Marker(New_M1, Max_Value_Down);
   end;

   procedure Sanitize_Marker_Down (Idx : Natural; Layer : Natural) is
      True_M    : Marker_T := Get (Idx, Layer);
      M0        : Marker_T := Get (Idx * 2, Layer-1);
      M1        : Marker_T := Get (Idx *2  + 1, Layer-1);
      M         : Marker_T := Build_Marker (M0, M1, Layer);
      Equal     : Boolean := True;
   begin
      for I in M'Range loop
         if M (I) /= True_M (I) then
            Equal := False;
            exit;
         end if;
      end loop;

      if not Equal then
         Build_Marker_Down (True_M, M0, M1, Layer);
         Set (Idx * 2, Layer-1, M0);
         Set (Idx * 2 + 1, Layer-1, M1);
      end if;  
   end Sanitize_Marker_Down;

   function Compute_Choices (M0 : Marker_T; M1 : Marker_T; Max_Value : Natural) return Choices_Array_T is
      Cs : Choices_Array_T (0 .. 5);
      C  : Choice_T;
      Counter : Natural := 0;
      Max_Value_Added : Boolean := False;
   
      function Combine return Combined_Array_T is
         ((M0 (0), M0 (1), M0 (2), M1 (0), M1 (1), M1 (2)));

      Vs : Combined_Array_T := Combine;

      function Make_Choices (Count : Natural) return Choices_Array_T is
         Chs : Choices_Array_T (0 .. Count - 1);
         Idx : Natural := 0;
      begin
         for I in Cs'Range loop
            if Cs (I).Match /= Impossible then
               Chs (Idx) := Cs (I);
               Idx := Idx + 1;
            end if;
         end loop;
         return Chs;
      end;
   begin
      for I in Vs'Range loop
         if Vs (I) = Max_Value and not Max_Value_Added then
            C := (Max_Value, I/3, I mod 3, Unsettled);
            Cs (I) := C;
            Max_Value_Added := True;
            Counter := Counter + 1;
         elsif Vs (I) > 0 and Vs (I) /= Max_Value then
            C := (Vs (I), I/3, I mod 3, Unsettled);
            Cs (I) := C;
            Counter := Counter + 1;
         end if;
      end loop;
      return Make_Choices (Counter);
   end;

   function Is_At_Deepest_Layer (Layer : Natural; Size : Natural; C : Choice_T) return Boolean is
   begin
      if (Layer = 0 or Max_Layer_Values (Layer - 1) - Size < 0) then
         return True;
      end if;
         return False;
   end Is_At_Deepest_Layer;


   function Find_Best_Choice (Choices : Choices_Array_T; Size : Natural) return Choice_T is
      C : Choice_T := (0,0,0,Impossible);
      Remain : Integer := 0;
      Closest : Natural := Natural'Last;
   begin
      for I in Choices'Range loop
            Remain := Choices (I).Value - Size;
            if Remain = 0 then
               C := Choices (I);
               C.Match := Exact;
               exit;
            end if;
            if Remain > 0 and Remain < Closest then
               Closest     := Remain;
               C := Choices (I);
               C.Match := Not_Exact;
            end if;
      end loop;
      return C;
   end;
      

   procedure Find_Marker
     (Size :     Positive; Match : in out Match_T; Layer : out Natural;
      Idx  : out Natural; Local_Idx : out Natural)
   is
      C : Choice_T;
   begin
      Idx := 0;
      for L in reverse 0 .. Nbr_Layers - 1 loop
         Layer := L;

         declare
            Choices : Choices_Array_T := Compute_Choices (Get (Idx, L), Get (Idx + 1, L), Max_Layer_Values (L));
         begin
            C := Find_Best_Choice (Choices, Size);
         end;

         if C.Local_Idx /= 1 and (C.Match /= Not_Exact or Is_At_Deepest_Layer (L, Size, C)) then
            exit;
         end if;

         if L > 0 then
            Sanitize_Marker_Down (Idx + C.Fork_Idx, L);
            Idx := (Idx + C.Fork_Idx) * 2;
         end if;

      end loop;
      Match := C.Match;
      Local_Idx := C.Local_Idx;
      Idx := Idx + C.Fork_Idx;
   end Find_Marker;


   function Is_Marker_Contiguous
     (Idx : Natural; Layer : Natural) return Boolean
   is
      M         : Marker_T := Get (Idx, Layer);
      Max_Value : Positive := Max_Layer_Values (Layer);
   begin
      return (M (0) = M (2)) and (M (1) = 0) and (abs M (0) = Max_Value);
   end Is_Marker_Contiguous;


   procedure Update_Marker
     (Size : Positive; Idx : Natural; Layer : Natural; Local_Idx : Natural)
   is
      M    : Marker_T := Get (Idx, Layer);
      Diff : Integer  := 0;
   begin
      if Is_Marker_Contiguous (Idx, Layer) then
         if Local_Idx = 0 then
            M := (-Size, 0, M (0) - Size);
         end if;
      else
         M (Local_Idx) := -M (Local_Idx);
      end if;
      Set (Idx, Layer, M);
   end Update_Marker;


   function Get_Idxs (Idx : Natural) return Idxs_T is
   begin
      if Idx mod 2 = 0 then
         return (Idx, Idx + 1);
      else
         return (Idx - 1, Idx);
      end if;
   end;


   procedure Backtrack_From_Markers (Idx : in out Natural; Layer : Natural) is
      Idxs : Idxs_T;
      M0 : Marker_T;
      M1 : Marker_T;
      M  : Marker_T;
      Up_Idx : Natural;
   begin
      for I in Layer .. Nbr_Layers - 2 loop
         Idxs := Get_Idxs (Idx);
         M0 := Get (Idxs (0), I);
         M1 := Get (Idxs (1), I);
         M  := Build_Marker (M0, M1, I + 1);
         Idx := Idxs (0) / 2;
         Set (Idx, I + 1, M);
      end loop;
   end Backtrack_From_Markers;


   function Allocate (Size      : Positive; 
                      Layer     : out Natural; 
                      Idx       : out Natural;
                      Local_Idx : out Natural) return Match_T
   is
      Match : Match_T := Impossible;
   begin
      Idx       := 0;
      Local_Idx := 0;
      Find_Marker (Size, Match, Layer, Idx, Local_Idx);

      if Match /= Impossible then
            Update_Marker (Size, Idx, Layer, Local_Idx mod 3);
         declare
            Idx_Copy : Natural := Idx;
         begin
            Backtrack_From_Markers (Idx_Copy, Layer);
         end;

         Put_Line ("Allocated " & Positive'Image (Size) & " at Idx " &
                   Natural'Image (Idx) & " Layer " & Natural'Image (Layer) & " Local_Idx " &
                   Natural'Image (Local_Idx));
      end if;
      return Match;
   end Allocate;


   function Allocate(Size: Positive; Location : out Natural) return Match_T is
      Layer     : Natural := 0; 
      Idx       : Natural := 0;
      Local_Idx : Natural := 0;
      Match : Match_T := Allocate (Size, Layer, Idx, Local_Idx);
   begin
      if Match /= Impossible then
         --Print_Markers;
         if Local_Idx = 0 then
            Location := Idx * Max_Layer_Values (Layer);
         elsif Local_Idx = 2 then
            declare
               stride : Natural := (Idx + 1) * Max_Layer_Values (Layer);
               offset : Natural := abs (Get (Idx, Layer) (2));
            begin
               Location := stride - offset;
            end;  
         end if;
      else
         Location := 0;
      end if;
      return Match;
   end;

   function Find_Aligned_Marker (Location : Natural; Layer : out Natural; Idx : out Natural; Local_Idx : out Natural) return Marker_T is
      B : Natural := 0;
      E : Natural := 0;
      M : Marker_T;
   begin
      for L in reverse Max_Layer_Values'Range loop
         Layer := L;
         Idx := Location / Max_Layer_Values (L);

         if L > 0 then
            Sanitize_Marker_Down (Idx, L);
         end if;
         M := Get (Idx, L);

         B := Idx * Max_Layer_Values (L);
         E := B + Max_Layer_Values (L) + M (2);

         if Location = B then
            Local_Idx := 0;
            exit;
         end if;

         if Location = E then
            Local_Idx := 2;
            exit;
         end if;

      end loop;
      return M;
   end;

   procedure Deallocate (Location : Natural; Size : Natural) is
      Idx : Natural := 0;
      Local_Idx : Natural := 0;
      M : Marker_T;
      Layer : Natural := 0;
   begin
      Put_Line ("Deallocating " & Size'Image & " at Location " & Location'Image);
      M := Find_Aligned_Marker (Location, Layer, Idx, Local_Idx);
      M (Local_Idx) := -M (Local_Idx);
      M := Reset_Marker (M, Max_Layer_Values (Layer));
      Set (Idx, Layer, M);
      Backtrack_From_Markers (Idx, Layer);
      --Print_Markers;
      Put_Line ("---------------------------------------------------------------------------------");
   end;

   function Log2 (Value : Long_Long_Integer) return Long_Long_Integer is
      package Long_Float_F is new Ada.Numerics.Generic_Elementary_Functions (Long_Float);
   begin
      return Long_Long_Integer (Long_Float_F.Log (Long_Float(Value), 2.0));
   end;

   function Compute_Nbr_Bits_For_Marker (Layer : Natural) return Positive is
      Nbr_bits : Positive := Positive (2 * Log2 ((2 * Long_Long_Integer (Max_Layer_Values (Layer))) - 1));
   begin
      if Layer /= 0 then
         Nbr_bits := Nbr_bits + Positive (Log2 (Long_Long_Integer (Max_Layer_Values (Layer - 1))));
      end if;
      return Nbr_bits;
   end;

   function Get_Covered_Memory_Size return Positive is
      (Max_Layer_Values (Nbr_Layers - 1));

   procedure Print_Memory_Statistics is
      Bits_For_Marker : Positive := Compute_Nbr_Bits_For_Marker (0);
      Bytes_For_Layer : Positive := (Bits_For_Marker * Nbr_Layer_Markers (0)) / 8;
      Total_Bytes : Positive := Bytes_For_Layer;
      Percentage : Float;
   begin
      Put_Line ("Memory space size = " & Max_Layer_Values (Nbr_Layers - 1)'Image & " bytes");
      Put_Line ("Marker, Layer 0   = " & Bits_For_Marker'Image & " bits");
      Put_Line ("Markers Layer 0   = " & Bytes_For_Layer'Image & " bytes");
      for I in 1 .. Nbr_Layers - 1 loop
         Bits_For_Marker := Compute_Nbr_Bits_For_Marker (I);
         Bytes_For_Layer := (Bits_For_Marker * Nbr_Layer_Markers (I)) / 8;
         Put_Line ("Marker, Layer" & I'Image & "  = " & Bits_For_Marker'Image & " bits");
         Put_Line ("Markers Layer" & I'Image & "  = " & Bytes_For_Layer'Image & " bytes");
         Total_Bytes := Total_Bytes + Bytes_For_Layer;
      end loop;
      Put_Line ("Total used =" & Total_Bytes'Image & " bytes");
      Percentage := Float(Total_Bytes) / Float(Max_Layer_Values (Nbr_Layers - 1)) * 100.0;
      Put_Line ("Percentage of memory space used =" & Percentage'Image);
   end;

begin
   Reset_Markers;
end Markers;