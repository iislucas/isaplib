(* Generic parts of the Isabelle library *)
val version = "Isabelle/IsaPlanner Library";

print_depth 10;

(* basics from Isabelle/src/Pure *)
use "basics.ML";
use "library.ML";

(* from Isabelle/src/Generic *)
use "print_mode.ML";
use "alist.ML";
use "table.ML";
use "output.ML";
use "properties.ML";
use "markup.ML";
use "scan.ML";
use "source.ML";
use "symbol.ML";
use "seq.ML";
use "position.ML";
use "symbol_pos.ML";

(* from Isabelle/src/ML *)
use "ml_lex.ML";
use "ml_parse.ML";

(* from Isabelle/src/Generic *)
(* use "secure.ML"; -- isabelle specific *)
use "integer.ML";
use "stack.ML";
use "queue.ML";
use "heap.ML";
use "ord_list.ML";
use "balanced_tree.ML";
use "graph.ML";
use "binding.ML";
use "name_space.ML";
use "lazy.ML";
use "path.ML";
use "url.ML";
use "buffer.ML";
use "file.ML";
use "xml.ML";
use "yxml.ML";

(* from Isabelle/src/General, but loaded late in Isabelle, 
   I've cut out Isabelle term specifics. *)
use "pretty.ML";

(* From IsaPlanner *)
use "collection.ML";
