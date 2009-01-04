(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  Title:      isaplib/isap_src/nametab.ML (sym link in src/generic/)
    Author:     Lucas Dixon, University of Edinburgh
                lucas.dixon@ed.ac.uk
                Initially written Oct 2005, Updated 27 Dec 2008        *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  DESCRIPTION:
    Table with names as keys. 
    for holding things associated with nice fresh namers. 

    Each entry has a name - but we allow some names not to have
    entries: you can use up names without providing entries.

    Note: Currently 
*)

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
signature NAME_TAB =
sig
  include BASIC_NAME_TAB

  structure Rnm : RENAMING
  sharing Rnm.Nm.NSet = NSet;
  sharing Rnm.Nm.Tab = Tab;

  sharing type Rnm.Nm.NTab.T = T;

  val rename : Rnm.T -> 'a T -> Rnm.T * 'a T 
  val mk_renaming : 'a T -> Rnm.T -> Rnm.T
  val do_renaming : Rnm.T -> 'a T -> 'a T

end;

(* Should be able to do below, but can not because of ML's broken functor mechanism *)
(*
functor NameTabFun(Rnm : RENAMING) =
struct
  open Rnm.Nm.NTab;

  structure CR = CompoundRenamingFun(
                 struct
                 structure Rnm = Rnm;
                 type 'a T = 'a Rnm.Nm.NTab.T
                 val get_nameset = Rnm.Nm.NTab.get_nameset
                 val try_rename1 = Rnm.Nm.NTab.try_rename1
                 end)

  open CR;
end;
*)

(* instead, we end up with a second copy of the same code as in 
   compound_renaming: very crappy *)
functor NameTabFun(Rnm : RENAMING) 
: NAME_TAB
= struct
  structure Rnm = Rnm;

  open Rnm.Nm.NTab;

  fun do_renaming rn x = 
      Rnm.Nm.NSet.fold 
        (fn n => fn x2 =>
            case Rnm.lookup rn n of 
              NONE => x2
            | SOME n2 => (case try_rename1 n n2 x2 
                           of NONE => raise Rnm.invalid_renaming_exp "do_renaming"
                            | SOME x3 => x3 ))
        (get_nameset x) x;

  fun mk_renaming x rn = 
      Rnm.Nm.NSet.fold (snd oo Rnm.rename1) (get_nameset x) rn;

(* note: takes time of nset * ln(nset), 
   could be inverted to take time of avoids(rn) * ln(nset) *)

  fun rename rn x = 
      Rnm.Nm.NSet.fold 
        (fn n => fn (rn2,x2) =>
            case Rnm.rename1 n rn2 of 
              (NONE, rn3) => (rn3,x2)
            | (SOME n2, rn3) => 
              (case try_rename1 n n2 x2 
                of NONE => raise Rnm.invalid_renaming_exp "rename"
                 | SOME x3 => (rn3, x3)))
        (get_nameset x) (rn,x);
end;
