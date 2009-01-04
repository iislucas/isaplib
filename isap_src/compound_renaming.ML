
signature COMPOUND_RENAMABLE =
sig
structure Rnm : RENAMING
type T
val get_nameset : T -> Rnm.Nm.NSet.T
val try_rename1 : Rnm.Nm.name -> Rnm.Nm.name -> T -> T option
end;

signature COMPOUND_RENAMING =
sig
structure Rnm : RENAMING
type T
(* assumes that existing renaming does not cause clashes,
   preserves non-clashing of renamings *)
val rename : Rnm.T -> T -> Rnm.T * T 
val mk_renaming : T -> Rnm.T -> Rnm.T
val do_renaming : Rnm.T -> T -> T
(* Note: 
fun rename rn x = let val rn2 = (mk_renaming x rn) 
                  in (rn2, do_renaming rn2 x) end;
  (but more efficient)
*)
end;


functor CompoundRenamingFun(CR : COMPOUND_RENAMABLE) =
struct
  structure Rnm = CR.Rnm;

  fun do_renaming rn x = 
      Rnm.Nm.NSet.fold 
        (fn n => fn x2 =>
            case Rnm.lookup rn n of 
              NONE => x2
            | SOME n2 => (case CR.try_rename1 n n2 x2 
                           of NONE => raise Rnm.invalid_renaming_exp "do_renaming"
                            | SOME x3 => x3 ))
        (CR.get_nameset x) x;

  fun mk_renaming x rn = 
      Rnm.Nm.NSet.fold (snd oo Rnm.rename1) (CR.get_nameset x) rn;

(* note: takes time of nset * ln(nset), 
   could be inverted to take time of avoids(rn) * ln(nset) *)

  fun rename rn x = 
      Rnm.Nm.NSet.fold 
        (fn n => fn (rn2,x2) =>
            case Rnm.rename1 n rn2 of 
              (NONE, rn3) => (rn3,x2)
            | (SOME n2, rn3) => 
              (case CR.try_rename1 n n2 x2 
                of NONE => raise Rnm.invalid_renaming_exp "rename"
                 | SOME x3 => (rn3, x3)))
        (CR.get_nameset x) (rn,x);
end;



