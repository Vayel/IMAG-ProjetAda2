with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with GNAT.String_Split; use GNAT.String_Split;
with Ada.Characters.Handling; use Ada.Characters.Handling;

package body Parser_Svg is
   BEZIER_NB_PTS : constant Positive := 50;

   function Recupere_Chemin(Nom_Fichier : String) return String is
      F : File_type;
      Line : Unbounded_String;
   begin
      Open(F, In_File, Nom_Fichier);
      Line := To_Unbounded_String(Trim(Get_Line(F), Both));

      while Length(Line) < 2 or else (To_String(Line)(1..2) /= "d=") loop
         Line := To_Unbounded_String(Trim(Get_Line(F), Both));
      end loop;
      -- Line = 'd="..."'

      Close(F);

      return To_String(Line)(4..Length(Line)-1);
   end;

   function Chaine_Vers_Point(P : String) return Point2D is
      -- P est de la forme "X,Y"

      Subs : Slice_Set;
      Pt : Point2D;
   begin
      Create(S => Subs,
             From => P,
             Separators => ",",
             Mode => Multiple);

      Pt(1) := Float'Value(Slice(Subs, 1));
      Pt(2) := Float'Value(Slice(Subs, 2));

      return Pt;
   end;

   -- Coordonnees relatives vers absolues
   function Rel_Vers_Abs(P : Point2D; L : Liste) return Point2D is
   begin
      if Taille(L) /= 0 then
         return P + Queue(L);
      else
         return P; -- Relatif par rapport a l'origine
      end if;
   end;

   -- Ajoute un point de la forme "x,y" ou "dx,dy" en queue de liste
   procedure Lit_Point(Ps : String; L : out Liste; Rel : Boolean := False) is
      P : Point2D := Chaine_Vers_Point(Ps);
   begin
      if Rel then
         P := Rel_Vers_Abs(P, L);
      end if;

      Insertion_Queue(L, P);
   end;

   -- Ajoute les points correspondant à "C c1x,c1y c2x,c2y x,y" en fin de liste
   procedure Lit_Bezier(C1s, C2s, P2s : String; L : out Liste; Rel : Boolean := False) is
      C1 : Point2D := Chaine_Vers_Point(C1s);
      C2 : Point2D := Chaine_Vers_Point(C2s);
      P2 : Point2D := Chaine_Vers_Point(P2s);
   begin
      if Rel then
         C1 := Rel_Vers_Abs(C1, L);
         C2 := Rel_Vers_Abs(C2, L);
         P2 := Rel_Vers_Abs(P2, L);
      end if;

      Bezier(Queue(L), C1, C2, P2, BEZIER_NB_PTS, L);
   end;

   -- Ajoute les points correspondant à "C cx,cy x,y" en fin de liste
   procedure Lit_Bezier(Cs, P2s : String; L : out Liste; Rel : Boolean := False) is
      C : Point2D := Chaine_Vers_Point(Cs);
      P2 : Point2D := Chaine_Vers_Point(P2s);
   begin
      if Rel then
         C := Rel_Vers_Abs(C, L);
         P2 := Rel_Vers_Abs(P2, L);
      end if;

      Bezier(Queue(L), C, P2, BEZIER_NB_PTS, L);
   end;

   procedure Chemin_Vers_Points(Chemin : String; L : out Liste) is
      I : Slice_Number := Slice_Number(1);
      Der_Lettre : Character := ' ';
      Subs : Slice_Set;

      procedure Cas_Lettres is
      begin
         -- I designe l'index du premier parametre, et non de la lettre
         -- Ca permet de gerer les cas du genre "M x1,y1 x2,y2 x3,y3"
         case Der_Lettre is
            when 'm'|'M'|'l'|'L' =>
               Lit_Point(Slice(Subs, I), L, Is_Lower(Der_Lettre));
               I := I + 1;
            when 'h'|'H' => -- Droite horizontale : H x
               Lit_Point(Slice(Subs, I) & "," & Float'Image(Queue(L)(2)), L, Is_Lower(Der_Lettre));
               I := I + 1;
            when 'v'|'V' => -- Droite verticale : V y
               Lit_Point(Float'Image(Queue(L)(1)) & "," & Slice(Subs, I), L, Is_Lower(Der_Lettre));
               I := I + 1;
            when 'c'|'C' => -- Bezier cubique : C c1x,c1y c2x,c2y endx,endy
               Lit_Bezier(Slice(Subs, I), Slice(Subs, I+1), Slice(Subs, I+2), L, Is_Lower(Der_Lettre));
               I := I + 3;
            when 'q'|'Q' => -- Bezier quadratique : Q cx,cy endx,endy
               Lit_Bezier(Slice(Subs, I), Slice(Subs, I+1), L, Is_Lower(Der_Lettre));
               I := I + 2;
            when others => raise DATA_ERROR with "Seules les lettres MLHVCQ sont supportees.";
         end case;
      end;
   begin
      Create(S => Subs,
             From => Chemin,
             Separators => " ",
             Mode => Multiple);

      while I <= Slice_Count(Subs) loop
         declare
            Sub : constant String := Slice(Subs, I);
         begin
            case Sub(Sub'First) is
               when 'm'|'M'|'l'|'L'|'h'|'H'|'v'|'V'|'c'|'C'|'q'|'Q' =>
                  Der_Lettre := Sub(Sub'First);
                  I := I + 1;
               when others => null; -- On conserve la lettre du tour precedent
            end case;

            Cas_Lettres;
         end;
      end loop;
   end;

   procedure Chargement_Bezier(Nom_Fichier : String; L : out Liste) is
   begin
      Chemin_Vers_Points(Recupere_Chemin(Nom_Fichier), L);
   end;

end;
