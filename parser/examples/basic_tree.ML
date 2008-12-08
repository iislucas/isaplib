open ParserLib; open ParserBaseData; open Data;

val dcx = Data.mk_base_dcx [(id_of_str " ", BName.mk " "), 
                            (id_of_str "+", BName.mk "+"), 
                            (id_of_str "0", BName.mk "0")];

val aspace_id = id_of_str " ";
val ((space_id, space_cons_id), dcx) = dcx |> BNFFunction.mk_list aspace_id;

val br_id = id_of_str "+";
(* val ((brs_id, brs_cons_id), dcx) = dcx |> BNFFunction.mk_list br_id; *)

val lf_id = id_of_str "0";
val ((lfs_id, lfs_cons_id), dcx) = dcx |> BNFFunction.mk_1list lf_id;

val tree_id = "tree"
val br_tree_id = "br_tree";

val br_tree_md = 
    mk_ands_dtyp
      [("0space", space_id), 
       ("1tree", tree_id),
       ("2space", space_id), 
       ("3br", br_id),
       ("5tree", tree_id)];
val dcx = dcx |> add_datatyp (dname br_tree_id, br_tree_md);

val tree_md = mk_ors_dtyp
                [("Lf", lf_id),
                 ("Br", br_tree_id)];

val dcx = dcx |> add_datatyp (dname tree_id, tree_md);

val pre_bnf_pretty = 
Pretty.chunks 
[
 Data.pretty_dname_in_dcx dcx (dname "tree"),
 Data.pretty_dname_in_dcx dcx (dname "br_tree"),
 Data.pretty_dname_in_dcx dcx (dname "\" \"_list"),
 Data.pretty_dname_in_dcx dcx (dname "\" \"_list.cons")
];


val dcx = dcx |> BNFMorphisms.elim_epsilon;

val _ = print_dcx dcx;

Data.print_dname_in_dcx dcx (dname tree_id);

val str = "0+0";
val [(i1,st1)] = Seq.list_of (Parser.parse_of_string dcx str);
val _ = ParseState.print_sym_parse st1 i1;


val str = "0++0";
val [] = Seq.list_of (Parser.parse_of_string dcx str);

(* 
val [(i1,st1)] = Seq.list_of (Parser.parse_of_string dcx str);
val _ = ParseState.print_sym_parse st1 i1;
val _ = ParseState.print_active st1;
val _ = ParseState.print_full st1;
val _ = ParseState.print st1;
val _ = ParseState.print_sym_parse st1 (An.mk 20);
val _ = ParseState.print_symst st1 (An.mk 20);

writeln "BEFORE: ";
Pretty.writeln pre_bnf_pretty;

writeln "AFTER: ";
(Data.print_dname_in_dcx dcx (dname "tree");
Data.print_dname_in_dcx dcx (dname "br_tree");
Data.print_dname_in_dcx dcx (dname "br_tree.a");
Data.print_dname_in_dcx dcx (dname "br_tree.b");
Data.print_dname_in_dcx dcx (dname "br_tree.c");
Data.print_dname_in_dcx dcx (dname "br_tree.d");
Data.print_dname_in_dcx dcx (dname "\" \"_list");
Data.print_dname_in_dcx dcx (dname "\" \"_list.cons"); 
Data.print_dname_in_dcx dcx (dname "\" \"_list.cons.a");
Data.print_dname_in_dcx dcx (dname "\" \"_list.cons.b");
());


val str = "0++0";
val (p as Parser.PStep (r as (lset,rset,st))) = Parser.pstep_of_string dcx str;
val _ = Parser.print_pstep_active p;

val (p as Parser.PStep (r as (lset,rset,st))) = Parser.do_pstep r;
val _ = Parser.print_pstep_active p;
*)

(* 
PolyML.exception_trace (fn () => 
  Seq.list_of (Parser.parse_of_string dcx str)
);

val SOME (i,st) = 
    (Seq.list_of (Parser.parse_of_string dcx str); NONE)
    handle ParseState.assert_well_formed_sym_exp x => SOME x;

val _ = ParseState.print_full st;

val (p0 as Parser.PStep (r0 as (lset0,rset0,st0))) = ! (Parser.dbg_st);
val _ = ParseState.print_full st0;

SymState.is_well_formed (ParseState.get_sym st i);
open DB_SymState;
val (s as Middle midstat) = s;

val s = ParseState.get_sym st0 (An.mk 1);
val andposes = DB_ParseState.get_andposes st0 (SymState.get_parent_dname s);
val _ = ParseState.print_full st0;

val andp = List.nth (Data.AndSet.dest andposes, 1);
val (newids,st2) = add_parent_sym andp (An.NSet.empty, st0) ;


val s = ParseState.get_sym st2 (An.mk 13);
SymState.is_well_formed s;


val (p as Parser.PStep (r as (lset,rset,st))) = Parser.pstep_of_string dcx str;
val _ = Parser.print_pstep_active p;

val (p as Parser.PStep (r as (lset,rset,st))) = Parser.do_pstep r;
val _ = ParseState.print_full st;


*)

(* 


PolyML.exception_trace (fn () =>   Seq.list_of (Parser.parse_of_string dcx str) );

val (p as Parser.PStep (r as (lset,rset,st))) = ! (Parser.dbg_st);


val str = "s s 0";
val [] = Seq.list_of (Parser.parse_of_string dcx str);

val str = "s 0 0";
val [] = Seq.list_of (Parser.parse_of_string dcx str);
val _ = ParseState.print_sym_parse st1 i1;

val str = "ss 0";
val [(i1,st1)] = Seq.list_of (Parser.parse_of_string dcx str);
val _ = ParseState.print_sym_parse st1 i1;



val (p as Parser.PStep (r as (lset,rset,st))) = Parser.pstep_of_string dcx str;
val _ = Parser.print_pstep_active p;

val (p as Parser.PStep (r as (lset,rset,st))) = Parser.do_pstep r;
val _ = Parser.print_pstep_active p;
 *)
