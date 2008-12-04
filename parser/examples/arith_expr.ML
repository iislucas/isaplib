(* Arithmetic Expressions *)

(* --- *)
structure ExData_ArithExpr
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
(* val ((space_id, space_cons_id), dcx) = dcx |> mk_1list aspace_id; *)


(* digits ::= 0 | 1 | ... | 9 *)
val digit_base_ids = 
    map (id_of_str o Char.toString) 
        (chars_between #"0" #"9");
val digit_id = "digit"
val digit_md = Ors (fold (fn x => CnTab.update (cname x, dname x))
                         digit_base_ids CnTab.empty);
val dcx = dcx |> add_datatyp (dname digit_id, digit_md);

(* numbers are lists of digits 
        digit_list ::= digit | digit_list.cons  
   digit_list.cons ::= digit, digit_list
*)
(* val ((num_id, num_cons_id), dcx) = dcx |> mk_1list digit_id; *)

(* binary arithmetic operators: 
   arith_op ::= + | - | * | / 
*)
val op_base_ids = 
    map (id_of_str o Char.toString) 
        [#"+", #"-", #"*", #"/"];
val op_id = "arith_op"
val op_md = Ors (fold (fn x => CnTab.update (cname x, dname x))
                      op_base_ids CnTab.empty);
val dcx = dcx |> add_datatyp (dname op_id, op_md);

(* Expressions and operator expressions: 
   op_expr ::= expr, arith_op, expr 
   expr ::= number | op_expr 
*)
val expr_id = "expr"
val op_expr_id = "op_expr";

val op_expr_md = 
    mk_ands_dtyp
      [("1exprC", expr_id),
       ("3opC", op_id),
       ("5exprC", expr_id)];
(* val op_expr_md = 
    mk_ands_dtyp
      [("1exprC", expr_id),
       ("2spaceC", aspace_id), 
       ("3opC", op_id),
       ("4spaceC", aspace_id), 
       ("5exprC", expr_id)]; *)
val dcx = dcx |> add_datatyp (dname op_expr_id, op_expr_md);

val expr_md = mk_ors_dtyp
                [("numC", digit_id),
                 ("opC", op_expr_id)];

val dcx = dcx |> add_datatyp (dname expr_id, expr_md);


val _ = print_dcx dcx;
end; end;

(* --- *)
local 

val dcx = ExData_ArithExpr.dcx;

val str = "1*2+";
val [] = Seq.list_of (Parser.parse_of_string dcx str);

val str = "1**2";
val [] = Seq.list_of (Parser.parse_of_string dcx str);

val str = "1+2";
val [(i,st)] = Seq.list_of (Parser.parse_of_string dcx str); 
val _ = ParseState.print_sym_parse st i;

val str = "1*2+1";
val ([(i1,st1),(i2,st2)]) = Seq.list_of (Parser.parse_of_string dcx str); 
val _ = ParseState.print_sym_parse st1 i1;
val _ = ParseState.print_sym_parse st2 i2;

in
  val _ = Pretty.writeln (Pretty.str "TEST PASSED: arith_expr.ML")
end;
(* 
raise ERROR "stop";

val aposes = ParseState.get_andposes st (UnderData.dname "1");
val aposes_set = Data.AndSet.dest aposes;

val str = "1*2+1";
val ([(i1,st1),(i2,st2)]) = Seq.list_of (Parser.parse_of_string dcx str); 
ParseState.print_sym_parse st1 i1;
ParseState.print_sym_parse st2 i2;

(* -- useful tools... *)
if false then let 
val (p as Parser.PStep (r as (lset,rset,st))) = Parser.pstep_of_string dcx str;
val _ = Parser.print_pstep_active p;

val ((is1 as (i, s as DB_SymState.Top intype))) = 
    the (ParseState.get_next_open_sym st);

val (s1) = ParseState.get_sym st 10;
val (s2) = ParseState.get_sym st 7;


val andposes_list = Data.AndSet.dest andposes; val andp::_ =
andposes_list; val ps = SymState.mk_parent dcx (i,intype) andp;

open DB_SymState;
val (sid, InAndType rep) = (i, intype);

open DB_ParseState;

val (p as Parser.PStep (r as (lset,rset,st))) = Parser.do_pstep r;
val _ = Parser.print_pstep_active p;

val _ = Parser.print_pstep_active (Parser.do_pstep r);
in () end else ();
 *)
