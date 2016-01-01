with Liste_Generique;
with Math; use Math;

package STL is
   NB_ANGLES : constant Positive := 72;
   PAS_ANGLE : constant Float := 360.0 / Float(NB_ANGLES);

   type Facette is record
      P1, P2, P3 : Point3D;
   end record;

   package Liste_Facettes is new Liste_Generique(Facette);

   --prend une liste de segments et cree l'objet 3d par rotations
   procedure Creation(Segments : in out Liste_Points.Liste ;
                      Facettes :    out Liste_Facettes.Liste);

   --sauvegarde le fichier stl
   procedure Sauvegarder(Nom_Fichier : String ;
                         Facettes : Liste_Facettes.Liste);
end;
