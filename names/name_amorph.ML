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
(* sharing substuff issue: would like to make a subtype of ISO for automorphism that adds the Dom=Cod constraint. 
*)
signature NAME_AMORPH = 
sig 
  include NAME_ISO
  sharing Sharing.Dom = Sharing.Cod

  (* renamings are trivially name automorphisms *)
  val of_rnm : Dom.Rnm.T -> T
  val rnm_of : T -> Dom.Rnm.T
end;

(* Name endomorphisms *)
functor NameAmorphWithDataFun(structure Nm : NAMES
  type data
  val empty_data : data) 
: NAME_AMORPH
= struct 
  structure Nm = Nm;
  structure NameIso = NameIsoWithDataFun(
    structure Dom = Nm 
    structure Cod = Nm 
    val translate_name = I
    type data = data
    val empty_data = empty_data) : NAME_ISO;
  open NameIso;

  fun of_rnm rn = 
      let val rep = (Dom.Rnm.destr rn)
      in
        constr {ignore = #ignore rep,
            avoids = #avoids rep,
            domf = #nmap rep,
            codf = 
              Dom.NTab.fold (fn (n1,n2) => Cod.NTab.doadd (n2,n1)) 
                (#nmap rep) Cod.NTab.empty,
            data = empty_data} 
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

functor NameAmorphFun(Nm : NAMES)
: NAME_AMORPH
= NameAmorphWithDataFun(structure Nm = Nm
  type data = unit
  val empty_data = ());
  