with System;
with Ada.Real_Time; use Ada.Real_Time;

package Markers is

   type Match_T is (Exact, Not_Exact, Impossible, Unsettled);
   type Marker_T is array (0 .. 2) of Integer;

   type Allocation_T is record
      Size : Natural;
      Location : Natural;
      Alloc_Time : Time_Span;
      Dealloc_Time : Time_Span;
   end record;

   function Allocate(Size: Positive; Location : out Natural) return Match_T;
   procedure Deallocate (Location : Natural; Size : Natural);

   function Get_Covered_Memory_Size return Positive;
   function Get_Top_Marker return Marker_T;
   procedure Print_Memory_Statistics;
   procedure Print_Markers;
   procedure Reset_Markers;

end Markers;
