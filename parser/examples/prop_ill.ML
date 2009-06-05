(* Propositional ILL *)

(* --- *)
structure ExData_PropIll
= struct 
local open ParserLib; open ParserBaseData; open Data; in
val dcx = Parser.base_dcx;

(* a space (new line, tab, or space char): 
   aspace ::= "\n" | "\t" | " " 
*)
val aspace_base_ids = (map id_of_str ["\\t", "\\n", " "]);
val aspace_id = "aspace";
val aspace_md = Ors (fold (fn x => CnTab.update (cname x, dname x))
                               aspace_base_ids CnTab.empty);
val dcx = dcx |> add_datatyp (dname aspace_id,aspace_md);

(* make list of spaces
   space ::= list(aspace) 
*)
val ((space_id, space_cons_id), dcx) = dcx |> BNFFunction.mk_list aspace_id;


(* ident ::= a | ... | z *)
val idchar_base_ids = 
    map (id_of_str o Char.toString) 
        (chars_between #"a" #"z");
val idchar_id = "idchar"
val idchar_md = Ors (fold (fn x => CnTab.update (cname x, dname x))
                         idchar_base_ids CnTab.empty);
val dcx = dcx |> add_datatyp (dname idchar_id, idchar_md);

(* numbers are 1lists of digits 
        digit_list ::= digit | digit_list.cons  
   digit_list.cons ::= digit, digit_list
*)
val ((ident_id, ident_cons_id), dcx) = dcx |> BNFFunction.mk_1list idchar_id;

(* binary arithmetic operators: 
   lolli ::= -o / 
*)
val lolli_id = "lolli";
val lolli_md = 
    mk_ands_dtyp
      [("0-", id_of_str "-"), 
       ("1o", id_of_str "o")]; 

val dcx = dcx |> add_datatyp (dname lolli_id, lolli_md);


(* Expressions and operator expressions: 
   op_expr ::= expr -o expr 
   expr ::= ident | op_expr 
*)
val expr_id = "expr"
val op_expr_id = "op_expr";
val op_expr_md = 
    mk_ands_dtyp
      [("0space", space_id), 
       ("1expr", expr_id),
       ("2space", space_id), 
       ("3op", lolli_id),
       ("4space", space_id), 
       ("5expr", expr_id),
       ("6space", space_id)];
val dcx = dcx |> add_datatyp (dname op_expr_id, op_expr_md);

val expr_md = mk_ors_dtyp
                [("identC", ident_id),
                 ("opC", op_expr_id)];

val dcx = dcx |> add_datatyp (dname expr_id, expr_md);

val dcx = dcx |> BNFMorphisms.elim_epsilon;

val _ = print_dcx dcx;
end; end;


(* --- *)
local 
val dcx = ExData_PropIll.dcx;

val str = "a -o b";
val [(i,st)] = Seq.list_of (LR_Parser.parse_of_string dcx str);
val _ = ParseState.print_sym_parse st i;


val _ = map (ParseState.print_sym_parse st) 
            (Seq.list_of (Parser.parse_of_string dcx str));


in
  val _ = Pretty.writeln (Pretty.str "TEST PASSED: prop_ill.ML")
end;


(* 
val dcx = ExData_PropIll.dcx;
val str = "a -o b";
val [(i1,st1)] = Seq.list_of (Parser.parse_of_string dcx str);
val _ = ParseState.print_sym_parse st1 i1;


val (p as LR_Parser.PStep (r as (lset,rset,st))) = LR_Parser.pstep_of_string dcx str;
val _ = LR_Parser.print_pstep_active p;

val (SOME (x as (i,s))) = get_next_awake_Left r;
SymState.print s;

val (p as LR_Parser.PStep (r as (lset,rset,st))) = LR_Parser.do_pstep x r;
val _ = LR_Parser.print_pstep_active p;



val aposes = ParseState.get_andposes st (UnderData.dname "1");
val aposes_set = Data.AndSet.dest aposes;

*)
