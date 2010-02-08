(* test all examples... *)

PolyML.Project.make "prop_ill.ML";

(* 
(* direct BNFs; no morphisms *)
PolyML.Project.make "minimal_cat.ML";
PolyML.Project.make "animals.ML";
PolyML.Project.make "cat1list.ML";

(* BNFS with epsilon-elim morphisms *)
PolyML.Project.make "a_cat_option.ML";
PolyML.Project.make "catlist.ML";
PolyML.Project.make "spaced_cats.ML";


(* PolyML.Project.make "catlist.ML"; *)
PolyML.Project.make "arith_expr.ML";
PolyML.Project.make "arith_expr2.ML";
PolyML.Project.make "bnf.ML";
*)

writeln "All exmaples finished happily!";
