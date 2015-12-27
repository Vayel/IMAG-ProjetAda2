with Ada.Unchecked_Deallocation;

package body Liste_Generique is

   procedure Liberer is new Ada.Unchecked_Deallocation(Cellule, Pointeur);

   -- Vide la liste L mais ne libere pas la memoire occupee par
   -- ses elements
   procedure Nettoyer(L : out Liste) is
   begin
      L := (null, null, 0);
   end;

   function Est_Vide(L : Liste) return Boolean is
   begin
      return L.Taille = 0;
   end;

   procedure Vider(L : in out Liste) is
      Cour, Suiv : Pointeur := L.Debut;
   begin
      while Cour /= null loop
         Suiv := Cour.Suivant;
         Liberer(Cour);
         Cour := Suiv;
      end loop;

      Nettoyer(L);
   end;

   procedure Insertion_Tete(L : in out Liste ; E : Element) is
   begin
      L.Debut := new Cellule'(E, L.Debut);

      if L.Fin = null then
          L.Fin := L.Debut;
      end if;
   end;

   procedure Insertion_Queue(L : in out Liste ; E : Element) is
   begin
      if Est_Vide(L) then
         Insertion_Tete(L, E);
      else
         L.Fin.Suivant := new Cellule'(E, null);
         L.Fin := L.Fin.Suivant;
      end if;
   end;

   procedure Parcourir (L : Liste) is
      Cour : Pointeur := L.Debut;
   begin
      while Cour /= null loop
         Traiter(Cour.Contenu);
         Cour := Cour.Suivant;
      end loop;
   end;

   procedure Parcourir_Par_Couples(L : Liste) is
      Cour, Suiv : Pointeur;
   begin
      Cour := L.Debut;
      if Cour /= null then
         Suiv := Cour.Suivant;
      else
         Suiv := null;
      end if;

      while Suiv /= null loop
         Traiter(Cour.Contenu, Suiv.Contenu);
         Cour := Suiv;
         Suiv := Suiv.Suivant;
      end loop;
   end;

   procedure Fusion(L1 : in out Liste ; L2 : in out Liste) is
   begin
      if Est_Vide(L1) then
         L1.Debut := L2.Debut;
      else
         L1.Fin.Suivant := L2.Debut;
      end if;

      L1.Fin := L2.Fin;
      L1.Taille := L1.Taille + L2.Taille;

      Nettoyer(L2);
   end;

   function Taille(L : Liste) return Natural is
   begin
      return L.Taille;
   end;

   function Tete(L : Liste) return Element is
   begin
      return L.Debut.Contenu;
   end;

   function Queue(L : Liste) return Element is
   begin
      return L.Fin.Contenu;
   end;

end;
