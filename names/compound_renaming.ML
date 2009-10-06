(* basic things needed for renaming names within another datatype;
   allows separation of name spaces *)
signature COMPOUND_RENAMABLE =
sig
structure Nm : BASIC_NAMES
type obj

(* get set of all names *)
val get_nameset : obj -> Nm.NSet.T 

(* rename 1 name *)
val try_rename1 : Nm.name -> Nm.name -> obj -> obj option 
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
  val print_renaming = Rnm.print;

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

(*
signature ALPHA_CONVABLE
= sig 
include COMPOUND_RENAMABLE;

( * fold over two objects as far as they have the same structure 
   assumes f is applied to each pair of names; returns NONE is structure doesn't 
   match.
* )
val fold_2obj_names : (Nm.name * Nm.name -> 'a -> 'a) 
                      -> obj * obj -> 'a -> 'a option
                      
end;


functor AlphaFun(S : ALPHA) 
= struct

exception stop_exp of unit;

fun alpha (objs as (obj1,obj2)) rn = 
    (S.fold_2obj_names 
       (fn (n1,n2) => fn rnm => 
          case S.try_rename1 n1 n2 rnm
            of NONE => raise stop_exp()
             | SOME rnm2 => rnm2) 
       objs
       rn) 
     handle stop_exp _ => NONE;

end;
*)

