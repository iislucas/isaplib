(* using this file should be equivalent to "make run-isap" *)
use "ML-Systems/polyml.ML";

use "basics/log.ML";

cd "project";
use "ROOT.ML";
cd "..";

PolyML.Project.make "General/ROOT.ML";
(* PolyML.Project.make "Concurrent/ROOT.ML"; *)
PolyML.Project.make "names/ROOT.ML";
PolyML.Project.make "graph/ROOT.ML";
(* PolyML.Project.make  "parser/ROOT.ML"; *)
