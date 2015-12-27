package body Math is

   function "+" (A : Vecteur ; B : Vecteur) return Vecteur is
      R : Vecteur(A'Range);
   begin
      for i in A'Range loop
         R(i) := A(i) + B(i);
      end loop;

      return R;
   end;

   function "*" (Facteur : Float ; V : Vecteur) return Vecteur is
      R : Vecteur(V'Range);
   begin
      for i in V'Range loop
         R(i) := Facteur * V(i);
      end loop;

      return R;
   end;

   function "=" (A : Vecteur ; B : Vecteur) return Boolean is
   begin
      if A'Length /= B'Length then
         return false;
      end if;

      for i in 0..A'Length-1 loop
         if A(A'First + i) /= B(B'First + i) then
            return false;
         end if;
      end loop;

      return true;
   end;

   -- Cubique
   procedure Bezier(P1, C1, C2, P2 : Point2D ; Nb_Points : Positive ;
                    Points : out Liste) is
      T : Float := 0.0;
      Step : Float := 1.0/Float(Nb_Points);
   begin
      for i in 1..Nb_Points loop
         Insertion_Queue(Points, (1.0 - T)**3 * P1 + 3.0 * T * (1.0 - T)**2 * C1 +
                         3.0 * T**2 * (1.0 - T) * C2 + T**3 * P2);
         T := T + Step;
      end loop;
   end;

   -- Quadratique
   procedure Bezier(P1, C, P2 : Point2D ; Nb_Points : Positive ;
                    Points : out Liste) is
      T : Float := 0.0;
      Step : Float := 1.0/Float(Nb_Points);
   begin
      for i in 1..Nb_Points loop
         Insertion_Queue(Points, (1.0 - T)**2 * P1 + 2.0 * T * (1.0 - T) * C + T**2 * P2);
         T := T + Step;
      end loop;
   end;
end;
