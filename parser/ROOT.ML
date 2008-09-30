(* From IsaPlanner 
System.use "tools/dbg.ML"; (* debugging tools *)
System.use "tools/fnamer.ML"; (* idea of creating fresh names *)
System.use "tools/namers.ML"; (* some things to choose names *)
System.use "tools/fnametab.ML"; (* name tables which provide fresh names *)
System.use "tools/collection.ML"; (* collections of things  *)
*)
(* basic tools for datatypes and parsing *)
use "parser_lib.ML";

(* basic datatypes *)
use "base_names.ML";
use "base_data.ML";
use "data_pos.ML"; (* possible positions in a data type *)
use "data.ML";
(* parsing *)
use "adj_nds.ML";
use "sym_state.ML";
use "parse_state.ML";
use "parser.ML";
(* tools *)
use "bnf_morpisms.ML"; (* such as elim epsilon from grammar *)
use "bnf_function.ML"; (* functions to help defining bnfs *)

(* testing/examples... *)
cd "examples";
use "ROOT.ML";
cd "..";
