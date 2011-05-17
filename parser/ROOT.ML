(* basic tools for datatypes and parsing *)
PolyML.Project.make "parser_lib.ML";

(* basic datatypes *)
PolyML.Project.make "base_names.ML";
PolyML.Project.make "base_data.ML";
PolyML.Project.make "data_pos.ML"; (* possible positions in a data type *)
PolyML.Project.make "data.ML";
(* parsing *)
PolyML.Project.make "adj_nds.ML";
PolyML.Project.make "sym_state.ML";
PolyML.Project.make "parse_state.ML";
PolyML.Project.make "parser.ML";
PolyML.Project.make "lr_parser.ML";
(* tools *)
PolyML.Project.make "bnf_morpisms.ML"; (* such as elim epsilon from grammar *)
PolyML.Project.make "bnf_function.ML"; (* functions to help defining bnfs *)

(* testing/examples... *)
PolyML.Project.use_root "examples/ROOT.ML";
