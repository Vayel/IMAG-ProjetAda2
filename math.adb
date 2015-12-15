package body Math is

   function "+" (A : Vecteur ; B : Vecteur) return Vecteur is
      R : Vecteur(A'Range);
   begin
      return R;
   end;

   function "*" (Facteur : Float ; V : Vecteur) return Vecteur is
      R : Vecteur(V'Range);
   begin
      return R;
   end;

   procedure Bezier(P1, C1, C2, P2 : Point2D ; Nb_Points : Positive ;
                    Points : out Liste) is
   begin
      null;
   end;

   procedure Bezier(P1, C, P2 : Point2D ; Nb_Points : Positive ;
                    Points : out Liste) is
   begin
      null;
   end;
end;
