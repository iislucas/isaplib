(* Minimal Example: for morphism: a list of cats with space-list between *)

(* --- *)
structure ExData_SpacedCats
= struct 
local open ParserLib; open BaseData; open Data; in

val dcx = base_dcx;

(* cat *)
val cat_id = "cat";
val cat_md = 
    mk_ands_dtyp [("1c", id_of_str "c"), 
                  ("2a", id_of_str "a"), 
                  ("3t", id_of_str "t")];     
val dcx = dcx |> add_datatyp (dname cat_id, cat_md);

(* space *)
(* *)
val aspace_base_ids = (map id_of_str ["\\t", "\\n", " "]);
val aspace_id = "aspace";
val aspace_md = Ors (fold (fn x => CnTab.update (cname x, dname x))
                          aspace_base_ids CnTab.empty);
val dcx = dcx |> add_datatyp (dname aspace_id,aspace_md);

(* lists of spaces *)
val ((space_list_id, space_list_cons_id), dcx) = 
    dcx |> BNFFunction.mk_list aspace_id;

(* cat prefixed by space *)
val spaced_cat_id = "spaced_cat";
val spaced_cat_md = 
    mk_ands_dtyp [("1space", space_list_id), 
                  ("2cat", cat_id)];     
val dcx = dcx |> add_datatyp (dname spaced_cat_id, spaced_cat_md);

(* spaced cat list *)
val ((spaced_cat_list_id, spaced_cat_list_cons_id), dcx) = 
    dcx |> BNFFunction.mk_list spaced_cat_id;

val dcx = dcx |> BNFMorphisms.elim_epsilon;

val _ = print_dcx dcx;

end; end;



(* --- *)
local
val dcx = ExData_SpacedCats.dcx;
 
val str = "ccat";
val [] = Seq.list_of (Parser.parse_of_string dcx str);

val str = " cat";
val ((i,st)::more) = Seq.list_of (Parser.parse_of_string dcx str);
val _ = ParseState.print_sym_parse st i;

val str = "  cat";
val ((i,st)::more) = Seq.list_of (Parser.parse_of_string dcx str);
val _ = ParseState.print_sym_parse st i;


val str = "cat    cat cat   cat";
val ((i,st)::more) = Seq.list_of (Parser.parse_of_string dcx str);
val _ = ParseState.print_sym_parse st i;
 
val str = "cat  cat cat    cat cat cat   cat";
val ((i,st)::more) = Seq.list_of (Parser.parse_of_string dcx str);
val _ = ParseState.print_sym_parse st i;

val L = Dn.NTab.N.dest (Data.parents_of_data (Data.get dcx (Data.dname (ParserLib.id_of_str " "))));
val space_d = (Data.get dcx (Data.dname (ParserLib.id_of_str " ")));
val apsace_d = (Data.get dcx (Data.dname (ParserLib.id_of_str "aspace")));

in
val _ = Pretty.writeln (Pretty.str "TEST PASSED: a_cat_option.ML")
end;


(* 
val dcx = ExData_SpacedCats.dcx;
val str = "cat    cat cat   cat";
(* val ((i,st)::more) = Seq.list_of (Parser.parse_of_string dcx str); *)
val _ = ParseState.print_sym_parse st i;

val _ = PolyML.exception_trace (fn () =>  
                                   Seq.list_of (Parser.parse_of_string dcx str)
                               );

val (p as DB_Parser.PStep (r as (lset,rset,st))) = !DB_Parser.dbg_st;

ParseState.print st;

(* raises exception *)
val (p as Parser.PStep (r as (lset,rset,st))) = Parser.do_pstep r;

(* FROM: do_pstep TO: *)
val SOME (is0 as (i,s)) = (ParseState.get_next_open_sym st);

open DB_Parser;
val leftedge = An.NSet.contains lset i;
val rightedge = An.NSet.contains rset i;


val SOME ((_,newids), st2) = (ParseState.consider_parents_of_top is0 st);

An.NSet.print_full rset;

(* raises exception *)
An.NSet.union_merge newids rset;

(* in ParseState.check1_right *)
val (newids1, newlset1, newrset1, st1) = 
    (An.NSet.empty, An.NSet.empty, An.NSet.empty, 
     ParseState.del1_from_open i st);

val (is1 as (i,s)) = is0;
val ragenda_list = An.NSet.dest (AdjNds.get_awakened ragenda);

val (i2::ragenda_list) = ragenda_list;
val (newids1, newlset1, newrset1, st1) = 
    check1_right lset rset is1 i2 (newids1, newlset1, newrset1, st1);

(* in ParseState... *)
ParseState.consider is1 (i2,ParseState.get_sym st1 i2) st1

val (is2 as (i2, s2)) = (i2,ParseState.get_sym st1 i2);
val (is1 as (i1, s1)) = is1;

open DB_ParseState;

(* 
(* DEBUG STUFF *)



val (p as Parser.PStep (r as (lset,rset,st))) = Parser.pstep_of_string dcx str;
Parser.print_pstep_active p;

val (p as Parser.PStep (r as (lset,rset,st))) = Parser.pstep_of_string dcx str;
Parser.print_pstep_active p;

Parser.print_pstep_full p;

val (p as Parser.PStep (r as (lset,rset,st))) = Parser.do_pstep r;
Parser.print_pstep_active p;

val SOME (is0 as (i,s)) = (ParseState.get_next_open_sym st);


(* raises exception *)
Parser.print_pstep_active (Parser.do_pstep r);

val SOME (is0 as (i,s)) = (ParseState.get_next_open_sym st);

val leftedge = An.NSet.contains lset i;
val rightedge = An.NSet.contains rset i;

val ragenda = SymState.get_rnds s;

(* in check_right *)
val (newids1, newlset1, newrset1, st1) = 
    (ITab.N.empty, ITab.N.empty, ITab.N.empty, 
     ParseState.del1_from_open i st);

(* in ParseState.consider *)
val [i2] = ITab.N.dest (AdjNds.get_awakened ragenda);
val (is2 as (_, s2)) = (i2,ParseState.get_sym st1 i2);



val st1 = ParseState.del1_from_open i st;

val is1 = (i,s);
val (i1,s1) = is1;


*)

*)
