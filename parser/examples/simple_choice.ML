
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
      [("1sucC", sucs_id ),
       ("2spaceC", space_id), 
       ("3zerosC", zeros_id)
       ];
val dcx = dcx |> add_datatyp (dname piano_expr_id, piano_expr_md);


val str = "1**2";
val [(i1,st1)] = Seq.list_of (Parser.parse_of_string dcx str);
val _ = ParseState.print_sym_parse st1 i1;
