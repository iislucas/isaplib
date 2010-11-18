val print = PolyML.print;

(* a bit hacky here, maybe move to library.ML or basics.ML *)
fun maybef f NONE = NONE
  | maybef f (SOME x) = SOME (f x);

