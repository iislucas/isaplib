structure Posix = struct end; 

(* using this file should be equivalent to "make run-isap" *)
PolyML.use "ML-Systems/polyml.ML";

cd "basic";
PolyML.use "ROOT.ML";
cd "..";

cd "concurrent";
PolyML.use "ROOT.ML";
cd "..";

cd "names";
PolyML.use "ROOT.ML";
cd "..";

cd "graph";
PolyML.use "ROOT.ML";
cd "..";

cd "parser";
PolyML.use "ROOT.ML";
cd "..";
