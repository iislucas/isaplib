(* From IsaPlanner 
System.use "tools/dbg.ML"; (* debugging tools *)
System.use "tools/fnamer.ML"; (* idea of creating fresh names *)
System.use "tools/namers.ML"; (* some things to choose names *)
System.use "tools/fnametab.ML"; (* name tables which provide fresh names *)
System.use "tools/collection.ML"; (* collections of things  *)
cd "parser";
*)
use "parser_lib.ML";
use "base_data.ML";
use "under_data.ML";
use "data_pos.ML"; (* possible positions in a data type *)
(* use "dataloc.ML"; *) (* possible locations in a data context *)
(* use "dsubs.ML"; *) (* direct subtypes of data (by or) *)
(* use "moves.ML"; *) 
use "data.ML";
use "adj_nds.ML";
use "sym_state.ML";
use "parse_state.ML";
use "parser.ML";
(* 
use "ex_data2.ML";
use "ex_parser.ML"; 
*)
