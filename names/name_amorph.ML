(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  Title:      names_amoph.ML
    Author:     Lucas Dixon
                lucas.dixon@ed.ac.uk
                30 May 2008   *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  DESCRIPTION:

Note: Name Automorphisms
(isomorphism (1-1) function from one name kind to itself)
 name_amorph = CHANGE_NAMES where
 translate_name = id; Unlike renaming, we don't keep the newnames.

*)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
signature NAME_AMORPH = 
sig 
  include NAME_ISO
  sharing Dom = Cod
end;

(* Name endomorphisms *)
functor NameAmorphFun(Nm : NAMES) 
: NAME_AMORPH
= struct 
structure NameIso = NameIsoFun(
  structure Dom = Nm 
  structure Cod = Nm 
  val translate_name = I) : NAME_ISO;
open NameIso;
end;
