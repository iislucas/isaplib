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
  sharing Sharing.Dom = Sharing.Cod

  (* renamings are trivially name automorphisms *)
  val of_rnm : Dom.Rnm.T -> T
  val rnm_of : T -> Dom.Rnm.T
end;

(* Name endomorphisms *)
functor NameAmorphFun(Nm : NAMES) 
: NAME_AMORPH
= struct 
  structure Nm = Nm;
  structure NameIso = NameIsoFun(
    structure Dom = Nm 
    structure Cod = Nm 
    val translate_name = I) : NAME_ISO;
  open NameIso;

  fun of_rnm rn = 
      let val rep = (Dom.Rnm.destr rn)
      in
        constr {ignore = #ignore rep,
            avoids = #avoids rep,
            domf = #nmap rep,
            codf = 
              Dom.NTab.fold (fn (n1,n2) => Cod.NTab.doadd (n2,n1)) 
                (#nmap rep) Cod.NTab.empty} 
      end;

  fun rnm_of a = 
      let val rep = (destr a)
      in
        Dom.Rnm.constr {ignore = #ignore rep,
                    avoids = #avoids rep,
                    new = Dom.NSet.empty,
                    nmap = #domf rep}
      end;

end;
