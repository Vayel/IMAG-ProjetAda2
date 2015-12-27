with Math; use Math;

procedure Test_Math is
   use Liste_Points;

   procedure Test_Egal(A, B : Vecteur) is
   begin
      if not (A = B) then
         raise PROGRAM_ERROR;
      end if;
   end;

   A, B : Point2D;
   Points, Points2 : Liste;
begin
   -- Vecteurs
   A := (1.0, 2.5);
   B := (1.0, 2.5);
   Test_Egal(A, B);

   A := (1.0, 2.5);
   B := (1.0, 2.0);
   if A = B then
      raise PROGRAM_ERROR;
   end if;

   Test_Egal((2.0, 1.5) + (0.0, 3.0), (2.0, 4.5));
   Test_Egal(3.0 * (0.5, 2.5), (1.5, 7.5));

   -- Bezier
   -- TODO: verifier le resultat
   Bezier((1.0, 0.0), (0.0, 1.0), (2.0, 1.0), (3.0, 0.0), 10, Points);
   Bezier((0.0, 0.0), (2.0, 2.0), (4.0, 0.0), 10, Points2);
end;
