 
(* DEBUG STUFF *)
val dcx = ExData_MinimalCat.dcx;
val str = "ccat";

val (p as Parser.PStep (r as (lset,rset,st))) = Parser.pstep_of_string dcx str;
Parser.print_pstep_active p;
ParseState.assert_agreeing_adjs st;

(* raises exception *)
val [] = Seq.list_of (Parser.parse_of_string dcx str);

val (p as DB_Parser.PStep (r as (lset,rset,st))) = !DB_Parser.dbg_st;
Parser.print_pstep_active p;


val SOME (is0 as (i,s)) = (ParseState.get_next_open_sym st);
val leftedge = An.NSet.contains lset i;
val rightedge = An.NSet.contains rset i;

val ragenda = SymState.get_rnds s;

val (addnds, addlset, addrset, newst) = 
    DB_Parser.check1_right lset rset is0 (An.mk 5)
                 (An.NSet.empty, An.NSet.empty, An.NSet.empty, 
                  ParseState.del1_from_open i st);

Parser.print_pstep_active (Parser.PStep (lset,rset,newst));


PStep (lset, rset, 
       ParseState.union_to_open addnds newst)


open DB_Parser;

val 



Parser.print_pstep_active p;
Parser.print_pstep_full p;

val (p as Parser.PStep (r as (lset,rset,st))) = Parser.do_pstep r;
Parser.print_pstep_active p;

Parser.print_pstep_active (Parser.do_pstep r);



val (addnds, addlset, addrset, newst) = 
                    (* check against everything to the right of is0 *)
                    (An.NSet.empty, An.NSet.empty, An.NSet.empty, 
                     ParseState.del1_from_open i st)
                      |> (An.NSet.fold (check1_right lset rset is0) 
                                       (AdjNds.get_awakened ragenda))
;


Parser.print_pstep_active (Parser.PStep (lset,rset,newst));




val SOME (n,md,dcx) = (System.use "exdata.ML"; NONE) handle Data.bug_exp x => SOME x;
open DB_Data;

    PolyML.exception_trace (fn () =>          case md of MaybeAnds etab => 
                     ((SubTySet.empty, dcx2), Ands etab)
                   | MaybeOrs ctab => 
                     (CnTab.fold 
                        (fn (c,n2) => fn (subs, dcx') => 
                         if (dname_eq n n2) then (subs, dcx')
                         else
                           (subs |> SubTySet.union1 (SubTyPos (n,[c]))
                                 |> SubTySet.union_set 
                                 (inc1_subpos_set c (get_subs dcx' n2)), 
                            dcx' 
                              |> add_parent_and_supers_in_dcx
                              (n, inc1_suppos_set c supers2) n2
                        ))
                        ctab (SubTySet.empty, dcx2),
                      Ors ctab));
