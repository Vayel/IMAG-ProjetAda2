with Math; use Math;

package Parser_Svg is
   use Liste_Points;

   -- Pour les appeler dans le fichier de test
   function Recupere_Chemin(Nom_Fichier : String) return String;
   function Convertit_Point(P : String) return Point2D;

   --parse un fichier svg et retourne une liste de points (voir documentation)
   procedure Chargement_Bezier(Nom_Fichier : String; L : out Liste);
end;
