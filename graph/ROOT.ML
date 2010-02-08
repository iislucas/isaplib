PolyML.Project.make "pregraph.ML";
PolyML.Project.make "rgraph.ML";

(* testing *)
cd "test";
PolyML.Project.make "ROOT.ML";
cd "..";
