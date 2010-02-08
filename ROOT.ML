structure Posix = struct end; (* Avoid posix stuff: runnable in windows *)

(* using this file should be equivalent to "make run-isap" *)
use "ML-Systems/polyml.ML";

cd "project";
use "ROOT.ML";
cd "..";

cd "basic";
PolyML.Project.make "ROOT.ML";
cd "..";

cd "concurrent";
PolyML.Project.make "ROOT.ML";
cd "..";

cd "names";
PolyML.Project.make "ROOT.ML";
cd "..";

cd "graph";
PolyML.Project.make "ROOT.ML";
cd "..";


(*
cd "parser";
use "ROOT.ML";
cd "..";
*)
