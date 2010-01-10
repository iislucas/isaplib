(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  Title:      renaming.ML
    Author:     Lucas Dixon
                lucas.dixon@ed.ac.uk
                30 May 2008   *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  DESCRIPTION:

Note: This is essentially isomorphisms on a finite set of names.  We
do not store the inverse name function and thus this does not allow
efficient renaming. Efficient renaming can be done using the
change_names structure.

Generic renaming with a set of ignored names, a set of names to be
avoided, and a table of renamings. 

*)

(* Renamings which can themselves be renamed *)
signature RENAMING 
= sig
include BASIC_RENAMING;

structure Rnm : COMPOUND_RENAMING
sharing type Rnm.renaming = T
sharing type Rnm.obj = T

(* first renaming is added after second renaming; extra renamings are added *)
(* val compose_renaming : T -> T -> T *)
end;


(* *)
functor RenamingFun(Nm : BASIC_NAMES)
: RENAMING
= struct
  open Nm.Rnm;
  
  structure Rnm = CompoundRenamingFun(
    struct
      structure Nm = Nm
      type obj = T
      val get_nameset = get_avoids
      val try_rename1 = try_rename1_renaming 
    end)

end;
