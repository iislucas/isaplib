(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  Title:      nameset.ML
    Author:     Lucas Dixon
                lucas.dixon@ed.ac.uk
                Initially written Oct 2005, Last Updated 29 Jan 2008   *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  DESCRIPTION:

    Used to get new nice fresh names (quickly). With a slight
    adaptation, it can also be thought of as a memory management where
    names are memory spaces.

    A descrete interval table (for fresh names): a specialised, AVL
    tree inspired, representation for holding intervals and getting
    quick lookup/add/delete, worst case O(Log(n)^2); but when adjacent
    names are used it is much faster (often constant time).

*)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
signature NAME_SET =
sig
  include BASIC_NAME_SET

  structure Rnm : RENAMING
  sharing Rnm.Nm.NSet.N = N;
  sharing type Rnm.Nm.NSet.T = T;

  val rename : Rnm.T -> T -> Rnm.T * T 
  val mk_renaming : T -> Rnm.T -> Rnm.T
  val do_renaming : Rnm.T -> T -> T

end;


functor NameSetFun(Rnm : RENAMING) =
struct
  open Rnm.Nm.NSet;

  structure CR = CompoundRenamingFun(
                 struct
                 structure Rnm = Rnm;
                 type T = Rnm.Nm.NSet.T
                 val get_nameset = I 
                 val try_rename1 = Rnm.Nm.NSet.try_rename1
                 end)

  open CR;
end;


