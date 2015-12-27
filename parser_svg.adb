with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with GNAT.String_Split; use GNAT.String_Split;

package body Parser_Svg is
   BEZIER_NB_PTS : constant Positive := 100;

   function Recupere_Chemin(Nom_Fichier : String) return String is
      F : File_type;
      Line : Unbounded_String;
   begin
      Open(F, In_File, Nom_Fichier);
      Line := To_Unbounded_String(Trim(Get_Line(F), Both));

      while Length(Line) < 2 or else not (To_String(Line)(1..2) = "d=") loop
         Line := To_Unbounded_String(Trim(Get_Line(F), Both));
      end loop;

      Close(F);

      return To_String(Line)(4..Length(Line)-1); -- Entre les guillemets
   end;

   function Convertit_Point(P : String) return Point2D is
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

   procedure Lit_Point(P : String; L : out Liste) is
   begin
      Insertion_Queue(L, Convertit_Point(P));
   end;

   procedure Lit_Bezier_C(C1, C2, P2 : String; L : out Liste) is
      Bezier_L : Liste;
   begin
      Bezier(Queue(L), Convertit_Point(C1), Convertit_Point(C2), Convertit_Point(P2),
             BEZIER_NB_PTS, Bezier_L);
      Fusion(L, Bezier_L);
   end;

   procedure Lit_Bezier_Q(C, P2 : String; L : out Liste) is
      Bezier_L : Liste;
   begin
      Bezier(Queue(L), Convertit_Point(C), Convertit_Point(P2), BEZIER_NB_PTS, Bezier_L);
      Fusion(L, Bezier_L);
   end;

   procedure Chemin_Vers_Points(Chemin : String; L : out Liste) is
      I : Slice_Number := Slice_Number(1);
      Der_Lettre : Character := ' ';
      Subs : Slice_Set;

      procedure Cas_Lettres is
      begin
         -- I designe l'index du premier parametre, et non de la lettre
         case Der_Lettre is
            when 'm'|'M' => -- Debut du chemin : M x,y
               Lit_Point(Slice(Subs, I), L);
               I := I + 1;
            when 'l'|'L' => -- Ligne droite : L x,y
               Lit_Point(Slice(Subs, I), L);
               I := I + 1;
            when 'h'|'H' => -- Droite horizontale : H y
               Lit_Point("0.0," & Slice(Subs, I), L);
               I := I + 1;
            when 'v'|'V' => -- Droite verticale : V x
               Lit_Point(Slice(Subs, I) & ",0.0", L);
               I := I + 1;
            when 'c'|'C' => -- Bezier cubique : C c1x,c1y c2x,c2y endx,endy
               Lit_Bezier_C(Slice(Subs, I), Slice(Subs, I+1), Slice(Subs, I+2), L);
               I := I + 3;
            when 'q'|'Q' => -- Bezier quadratique : Q cx,cy endx,endy
               Lit_Bezier_Q(Slice(Subs, I), Slice(Subs, I+1), L);
               I := I + 2;
            when others => raise DATA_ERROR with "Seules les lettres MLHVCQ sont supportees.";
         end case;
      end;
   begin
      Create(S => Subs,
             From => Chemin,
             Separators => " ",
             Mode => Multiple);

      while I < Slice_Count(Subs) loop
         declare
            Sub : constant String := Slice(Subs, I);
         begin
            case Sub(Sub'First) is
               when 'm'|'M'|'l'|'L'|'h'|'H'|'v'|'V'|'c'|'C'|'q'|'Q' => -- Nouvelle lettre
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
