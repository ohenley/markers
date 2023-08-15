with Markers; use Markers;

-- [691,746,2129,61]
-- [657, 537, 2466]
-- [2084,430,838,3317]
-- [3460, 2303, 88]
-- [1544, 3965, 117]
-- [1080, 323, 1509]
-- [1875, 1462, 209]
-- [170, 230, 2184]
-- [2036, 512, 392] 
-- [404, 41, 477, 743, 2258]
-- [657, 537, 2466]
-- [1864, 512]
-- [3072, 425]
-- [278,20,17,230,2305]
-- [3841, 3415, 2087, 255, 1272, 3644]
-- [2293, 1187, 342, 244, 175, 2148, 83, 1938, 627, 507, 363, 3188]

procedure Main2 is
   l : Natural := 12;
   Sizes : array (0 .. l-1) of Natural := [2293, 1187, 342, 244, 175, 2148, 83, 1938, 627, 507, 363, 3188];
   Locations : array (0 .. l-1) of Natural := [2293, 1187, 342, 244, 175, 2148, 83, 1938, 627, 507, 363, 3188];
   Location : Natural;
   Match : Markers.Match_T;
begin
   for I in 0 .. l-1 loop
      Match := Markers.Allocate (Sizes (I), Locations (I));
   end loop;

   for I in 0 .. l-1 loop
      Markers.Deallocate (Locations (I), Sizes (I));
   end loop;
   Markers.Print_Markers;
end Main2;
