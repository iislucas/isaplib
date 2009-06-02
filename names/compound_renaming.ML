(* basic things needed for renaming names within another datatype;
   allows separation of name spaces *)
signature COMPOUND_RENAMABLE =
sig
structure Nm : BASIC_NAMES
type obj
val get_nameset : obj -> Nm.NSet.T (* get set of all names *)
val try_rename1 : Nm.name -> Nm.name -> obj -> obj option (* rename 1 name *)
end;


(* IMPROVE: ADD: can also make compount renaming from pairwise
compound renamings. *)


(* generic signature for renaming within a datatype *)
signature COMPOUND_RENAMING =
sig
type renaming
type obj
(* assumes that existing renaming does not cause clashes,
   preserves non-clashing of renamings *)
val empty_renaming : renaming
val rename : renaming -> obj -> renaming * obj
val mk_renaming : obj -> renaming -> renaming
val do_renaming : renaming -> obj -> obj
(* Note: 
rename rn x = ((mk_renaming x rn), do_renaming (mk_renaming x rn) x)
  (but is more efficient)
*)
end;


functor CompoundRenamingFun(CR : COMPOUND_RENAMABLE)
: COMPOUND_RENAMING
= struct
  structure Rnm = CR.Nm.Rnm;
  structure Nm = CR.Nm;

  type renaming = Rnm.T;
  type obj = CR.obj;

  val empty_renaming = Rnm.empty;
  
  fun do_renaming rn x = 
      Nm.NSet.fold 
        (fn n => fn x2 =>
            case Rnm.lookup rn n of 
              NONE => x2
            | SOME n2 => (case CR.try_rename1 n n2 x2 
                           of NONE => raise Rnm.invalid_renaming_exp "do_renaming"
                            | SOME x3 => x3 ))
        (CR.get_nameset x) x;

  fun mk_renaming x rn = 
      Nm.NSet.fold (snd oo Rnm.rename1) (CR.get_nameset x) rn;

(* note: takes time of nset * ln(nset), 
   could be inverted to take time of avoids(rn) * ln(nset) *)
  fun rename rn x = 
      Nm.NSet.fold 
        (fn n => fn (rn2,x2) =>
            case Rnm.rename1 n rn2 of 
              (NONE, rn3) => (rn3,x2)
            | (SOME n2, rn3) => 
              (case CR.try_rename1 n n2 x2 
                of NONE => raise Rnm.invalid_renaming_exp "compound rename"
                 | SOME x3 => (rn3, x3)))
        (CR.get_nameset x) (rn,x);
end;



