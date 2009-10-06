structure Posix = struct end; (* Avoid posix stuff: runnable in windows *)

(* using this file should be equivalent to "make run-isap" *)
use "ML-Systems/polyml.ML";

cd "basic";
use "ROOT.ML";
cd "..";

cd "concurrent";
use "ROOT.ML";
cd "..";

cd "names";
use "ROOT.ML";
cd "..";

cd "graph";
use "ROOT.ML";
cd "..";

(*
cd "parser";
use "ROOT.ML";
cd "..";
*)
