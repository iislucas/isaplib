(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
signature BNF_Morphisms
= sig

end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
structure BNFMorphisms
= struct

exception bad_dcx_exp of string * Data.dname * Data.dcontext;

val unit_dname = UnderData.dname "unit";

(* remove every drop_dn from elem table *)
fun remove_all_etab_dnames drop_dn etab = 
    EnTab.fold 
      (fn (en,dn) => fn etab2 =>
          if DnTab.name_eq (dn, drop_dn) then 
            EnTab.delete en etab2
          else etab2)
      etab
      etab;

(* remove every drop_dn from constructor table *)
fun remove_all_ctab_dnames drop_dn ctab = 
    CnTab.fold 
      (fn (cn,dn) => 
          if DnTab.name_eq (dn, drop_dn) then 
            CnTab.delete cn
          else I)
      ctab
      ctab;

(* make optdn an optional name in etab; returns a list containing an
   etab for every subset of deletions of optdn *)
fun mk_optional_in_etab optdn etab = 
    let 
      val positions_of_dn = 
          EnTab.fold 
            (fn (en,dn) => if DnTab.name_eq (dn,optdn) then
                             EnTab.N.add en else I)
            etab
            EnTab.N.empty
      fun drop_poses etab drop_poses = 
          EnTab.N.fold EnTab.delete drop_poses etab;
    in
      map (drop_poses etab) (EnTab.N.powerset positions_of_dn)
    end;

(* make optdn optional in dn; if dn is and-type this explodes into all
   possibilities where optdn is dropped; is dn is or-type then we just
   give it back in more_opts as another thing that might be empty *)
fun mk_optional_in_dn optdn dn (more_opts, dcx : Data.dcontext) = 
    case Data.get_datatyp dcx dn of 
      Data.BType _ => raise bad_dcx_exp ("elim_1epsilon", dn, dcx)
    | Data.Ors _ => 
      (dn::more_opts, dcx)
    | Data.Ands etab => 
      let val etabl = mk_optional_in_etab optdn etab
          val (or_dns, dcx2) =
              fold 
                (fn etab => fn (or_dns,dcx2) => 
                    let val (newdn, dcx3) = 
                            Data.addfresh_datatyp (dn, Data.Ands etab) dcx2
                    in (DnTab.N.add newdn or_dns, dcx3) end)
                etabl
                (DnTab.N.empty, dcx)

          val ctab = 
              DnTab.N.fold
                (fn ctab => fn newdn => CnTab.doadd (UnderData.cname "1", 
                                                     newdn) ctab)
                or_dns
                CnTab.empty
      in 
        (more_opts, 
         dcx2 |> Data.del_data dn
              |> Data.add_datatyp (dn, Data.Ors ctab))
      end;

(* for the identity group of or-choices; make the optdn an optional
type everywhere it is used.

(an identiy or-choice group has the form
   {A ::= B | ... ; B ::= A | ... })
*)
fun mk_optional_everywhere optdn (done_dns,dcx) = 
    if DnTab.N.contains done_dns optdn 
    then dcx else 
    let 
      val parents = Data.parents_of_data (Data.get dcx optdn)
      val (more_otps, done_dns2, dcx2) = 
          DnTab.N.fold 
            mk_optional_in_dn
            parents
            ([], DnTab.N.add optdn done_dns, dcx)
    in
      fold mk_optional_everywhere more_otps (done_dns2, dcx2)
    end;

(* eliminate or-unit (empty string/epsilon) from n; *)
fun elim_1epsilon n dcx = 
    let val d = Data.get dcx n
        val dtyp = Data.datatyp_of_data d;
    in
      case dtyp of
        Data.BType _ => raise bad_dcx_exp ("elim_1epsilon", n, dcx)
      | Data.Ors ctab => 
        let val ctab2 = remove_all_ctab_dnames unit_dname ctab
        in dcx |> mk_optional_everywhere n
               |> Data.del_data n
               |> Data.add_datatyp (n,Data.Ors ctab2)
        end
      | Data.Ands etab => 
        let val etab2 = remove_all_etab_dnames unit_dname etab
        in dcx |> Data.del_data n
               |> Data.add_datatyp (n,Data.Ands etab2)
        end
    end;

(* eliminate/expand-out all unit/epsilon references *)
fun elim_epsilon dcx = 
    DnTab.N.fold 
      elim_1epsilon
      (Data.parents_of_data (Data.get dcx unit_dname))
      dcx;

end;
