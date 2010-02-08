(* Generic parts of the Isabelle library *)
val version = "Isabelle/IsaPlanner Library";

print_depth 10;

(* basics from Isabelle/src/Pure *)
PolyML.Project.make "basics.ML";
PolyML.Project.make "library.ML";

(* from Isabelle/src/Generic *)
PolyML.Project.make "print_mode.ML";
PolyML.Project.make "alist.ML";
PolyML.Project.make "table.ML";
PolyML.Project.make "output.ML";
PolyML.Project.make "properties.ML";
PolyML.Project.make "markup.ML";
PolyML.Project.make "scan.ML";
PolyML.Project.make "source.ML";
PolyML.Project.make "symbol.ML";
PolyML.Project.make "seq.ML";
PolyML.Project.make "position.ML";
PolyML.Project.make "symbol_pos.ML";

(* from Isabelle/src/ML *)
PolyML.Project.make "ml_lex.ML";
PolyML.Project.make "ml_parse.ML";

(* from Isabelle/src/Generic *)
(* PolyML.Project.make "secure.ML"; -- isabelle specific *)
PolyML.Project.make "integer.ML";
PolyML.Project.make "stack.ML";
PolyML.Project.make "queue.ML";
PolyML.Project.make "heap.ML";
PolyML.Project.make "ord_list.ML";
PolyML.Project.make "balanced_tree.ML";
PolyML.Project.make "graph.ML";
PolyML.Project.make "binding.ML";
PolyML.Project.make "name_space.ML";
PolyML.Project.make "lazy.ML";
PolyML.Project.make "path.ML";
PolyML.Project.make "url.ML";
PolyML.Project.make "buffer.ML";
PolyML.Project.make "file.ML";
PolyML.Project.make "xml.ML";
PolyML.Project.make "yxml.ML";

(* from Isabelle/src/General, but loaded late in Isabelle, 
   I've cut out Isabelle term specifics. *)
PolyML.Project.make "pretty.ML";

(* From IsaPlanner *)
PolyML.Project.make "collection.ML";
