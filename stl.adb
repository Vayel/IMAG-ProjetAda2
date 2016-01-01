with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Numerics;
use Ada.Numerics;
with Ada.Numerics.Elementary_Functions;
use Ada.Numerics.Elementary_Functions;

package body STL is

   -- On translate tous les points de (-xmin, -ymin)
   procedure Decaler(Segments : in out Liste_Points.Liste) is
      Xmin : Float := Liste_Points.Tete(Segments)(1);
      Ymin : Float := Liste_Points.Tete(Segments)(2);

      procedure Maj_Min(P : in out Point2D) is
      begin
         Xmin := Float'Min(P(1), Xmin);
         Ymin := Float'Min(P(2), Ymin);
      end;
      procedure Calc_Min is new Liste_Points.Parcourir(Maj_Min);

      procedure Translater_Pt(P : in out Point2D) is
      begin
         P(1) := P(1) - Xmin;
         P(2) := P(2) - Ymin;
      end;
      procedure Translater_Pts is new Liste_Points.Parcourir(Translater_Pt);
   begin
      Calc_Min(Segments);
      Translater_Pts(Segments);
   end;

   -- On joint, si besoin, les premier et dernier points a l'axe y=0
   procedure Joindre_Axe(Segments : in out Liste_Points.Liste) is
   begin
      if Liste_Points.Tete(Segments)(2) /= 0.0 then
         Liste_Points.Insertion_Tete(Segments, (Liste_Points.Tete(Segments)(1), 0.0));
      end if;
      if Liste_Points.Queue(Segments)(2) /= 0.0 then
         Liste_Points.Insertion_Queue(Segments, (Liste_Points.Queue(Segments)(1), 0.0));
      end if;
   end;

   function Pivote_Point(P : Point2D; Angle : Float) return Point3D is
      P2 : Point3D := (0.0, P(2), 0.0);
   begin
      P2(1) := P(1) * Cos(X => Angle, Cycle => 360.0);
      P2(3) := P(1) * Sin(X => Angle, Cycle => 360.0);

      return P2;
   end;

   procedure Creation(Segments : in out Liste_Points.Liste ;
                      Facettes :    out Liste_Facettes.Liste) is
      -- Prend deux points 2D, les fait tourner autour de l'axe y=0 et relie
      -- les cercles obtenus par des facettes
      procedure Cree_Facettes_Cote(P1, P2 : Point2D) is
        Facettes_Cote : Liste_Facettes.Liste;
        P1C1, P2C1, P1C2, P2C2 : Point3D;
      begin
         for i in 1..NB_ANGLES loop
            P1C1 := Pivote_Point(P1, Float(i) * PAS_ANGLE);
            P2C1 := Pivote_Point(P1, Float(i+1) * PAS_ANGLE);
            P1C2 := Pivote_Point(P2, Float(i) * PAS_ANGLE);
            P2C2 := Pivote_Point(P2, Float(i+1) * PAS_ANGLE);

            Liste_Facettes.Insertion_Queue(Facettes_Cote, (P1C1, P1C2, P2C1));
            Liste_Facettes.Insertion_Queue(Facettes_Cote, (P2C1, P1C2, P2C2));
         end loop;

        Liste_Facettes.Fusion(Facettes, Facettes_Cote);
      end;
      procedure Cree_Facettes_Cotes is new Liste_Points.Parcourir_Par_Couples(Cree_Facettes_Cote);
   begin
      Decaler(Segments);
      Joindre_Axe(Segments);
      Cree_Facettes_Cotes(Segments);
   end;

   function Header(Nom : String) return String is
   begin
      return "solid " & Nom;
   end;

   function Footer(Nom : String) return String is
   begin
      return "endsolid " & Nom;
   end;

   procedure Sauvegarder(Nom_Fichier : String ;
                         Facettes : Liste_Facettes.Liste) is
      F : File_type;

      procedure Dessiner_Facette(Fac : in out Facette) is
      begin
         Put_Line(F, "  facet");
         Put_Line(F, "    outer loop");

         Put_Line(F, "      vertex " & Float'Image(Fac.P1(1)) & " " &
                  Float'Image(Fac.P1(2)) & " " & Float'Image(Fac.P1(3)));

         Put_Line(F, "      vertex " & Float'Image(Fac.P2(1)) & " " &
                  Float'Image(Fac.P2(2)) & " " & Float'Image(Fac.P2(3)));

         Put_Line(F, "      vertex " & Float'Image(Fac.P3(1)) & " " &
                  Float'Image(Fac.P3(2)) & " " & Float'Image(Fac.P3(3)));

         Put_Line(F, "    endloop");
         Put_Line(F, "  endfacet");
      end;
      procedure Dessiner_Facettes is new Liste_Facettes.Parcourir(Dessiner_Facette);
   begin
      Create(F, Out_File, Nom_Fichier);
      Put(F, Header("test"));
      Dessiner_Facettes(Facettes);
      Put(F, Footer("test"));
      Close(F);
   end;

end;
