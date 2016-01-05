with Parser_Svg; use Parser_Svg;
with Math; use Math;

procedure Test_Parser_SVG is
   procedure Test_Recupere_Chemin(Nom_Fichier, Chemin : String) is
   begin
      if Recupere_Chemin(Nom_Fichier) /= Chemin then
         raise PROGRAM_ERROR;
      end if;
   end;
   
   procedure Test_Chaine_Vers_Point(P : String; Pt : Point2D) is
   begin
      if Chaine_Vers_Point(P) /= Pt then
         raise PROGRAM_ERROR;
      end if;
   end;
begin
   Test_Recupere_Chemin("bezier.svg", "m 30,190.93361 c 0,0 -45.714286,172.85714 42.857143,217.14286 C 161.42857,452.36218 197.14286,140.93361 255.71429,319.50504 314.28571,498.07647 47.142857,622.36218 320,652.36218 c 272.85714,30 248.57143,-402.85714 301.42857,-371.42857 52.85714,31.42857 92.85714,-74.28571 92.85714,-74.28571");

   Test_Chaine_Vers_Point("0.0,1.5", (0.0, 1.5));
end;
