(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  Title:      names_iso.ML
    Author:     Lucas Dixon
                lucas.dixon@ed.ac.uk
                30 May 2008   *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  DESCRIPTION:

Note: isomorphisms within a name set = CHANGE_NAMES where
translate_name = id; Unlike renaming, we don't keep the newnames.

*)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
signature NAME_ISO = 
sig 
  include CHANGE_NAMES
  sharing Dom = Cod
end;

(* *)
functor NameIsoFun(Nm : BASIC_NAMES) 
: NAME_ISO
= struct 
structure ChangeNames = ChangeNamesFun(
  structure Dom = Nm 
  structure Cod = Nm 
  val translate_name = I) : CHANGE_NAMES;
open ChangeNames;
end;
