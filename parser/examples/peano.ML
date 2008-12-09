
open ParserLib; open ParserBaseData; open Data;

val dcx = Parser.base_dcx;

val aspace_id = id_of_str " ";
val ((space_id, space_cons_id), dcx) = dcx |> BNFFunction.mk_list aspace_id;

val digit_suc_id = id_of_str "s";
val ((sucs_id, sucs_cons_id), dcx) = dcx |> BNFFunction.mk_list digit_suc_id;

val digit_zero_id = id_of_str "0";
val ((zeros_id, zeros_cons_id), dcx) = dcx |> BNFFunction.mk_1list digit_zero_id;

val piano_expr_id = id_of_str "piano_expr"
val piano_expr_md = 
    mk_ands_dtyp
      [("1suc", sucs_id ),
       ("2space", space_id), 
       ("3zeros", zeros_id)
       ];
val dcx = dcx |> add_datatyp (dname piano_expr_id, piano_expr_md);
val dcx = dcx |> BNFMorphisms.elim_epsilon;

Data.print_dname_in_dcx dcx (dname "piano_expr");
Data.print_dname_in_dcx dcx (dname "piano_expr.option-a");
Data.print_dname_in_dcx dcx (dname "piano_expr.option-a.option-a");
Data.print_dname_in_dcx dcx (dname "piano_expr.option-a.option-b");
Data.print_dname_in_dcx dcx (dname "piano_expr.option-b");
Data.print_dname_in_dcx dcx (dname "piano_expr.option-b.option-a");
Data.print_dname_in_dcx dcx (dname "piano_expr.option-b.option-b");


(* 

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
