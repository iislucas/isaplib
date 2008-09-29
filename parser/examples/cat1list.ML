(* Minimal Example: parse a 1list of the word "cat" *)

(* --- *)
structure ExData_Cat1List
= struct 
local open ParserLib; open BaseData; open Data; in
val dcx = base_dcx;
val cat_md = 
    mk_ands_dtyp [("1c", id_of_str "c"), 
                  ("2a", id_of_str "a"), 
                  ("3t", id_of_str "t")];     
val dcx = dcx |> add_datatyp (dname "cat", cat_md);

fun mk_1list s_id dcx = 
let 
val s_list_id = (s_id ^ "_list")
val s_list_cons_id = (s_list_id ^ ".cons")
val s_list_md = 
mk_ors_dtyp [("1elem", s_id), ("2cons", s_list_cons_id)];
val s_list_cons_md = 
mk_ands_dtyp [("1hd", s_id), ("2tl", s_list_id)];     
in 
((s_list_id, s_list_cons_id),
  dcx |> add_datatyp (dname s_list_id, s_list_md)
  |> add_datatyp (dname s_list_cons_id, s_list_cons_md))
end;

val ((catlist_id, catlist_cons_id), dcx) = dcx |> mk_1list "cat";

val _ = print_dcx dcx;

end; 
end;


(* --- TEST CASES --- *)
local 
val dcx = ExData_Cat1List.dcx;

val str = "ccat";
val [] = Seq.list_of (Parser.parse_of_string dcx str);

val str = "cc";
val [] = Seq.list_of (Parser.parse_of_string dcx str);

val str = "cat";
val ((i,st)::more) = Seq.list_of (Parser.parse_of_string dcx str);
val _ = ParseState.print_sym_parse st i;

val str = "catca";
val [] = Seq.list_of (Parser.parse_of_string dcx str);
val _ = ParseState.print_sym_parse st i;

val str = "catcat";
val ((i,st)::more) = Seq.list_of (Parser.parse_of_string dcx str);
val _ = ParseState.print_sym_parse st i;



val (p as Parser.PStep (r as (lset,rset,st))) = Parser.pstep_of_string dcx str;
val _ = Parser.print_pstep_active p;

val (p as Parser.PStep (r as (lset,rset,st))) = Parser.do_pstep r;
val _ = Parser.print_pstep_active p;

val _ = Parser.print_pstep_active (Parser.do_pstep r);


(* does some pretty serious stress testing of NameTables... *)
val str = "catcatca";
val [] = Seq.list_of (Parser.parse_of_string dcx str);

val str = "catcatcat";
val ((i,st)::more) = Seq.list_of (Parser.parse_of_string dcx str); 
val _ = ParseState.print_sym_parse st i;


val str = "catcatcatcatcat";
val ((i,st)::more) = Seq.list_of (Parser.parse_of_string dcx str); 
val _ = ParseState.print_sym_parse st i;

val str = "catcatcatcatcatcatcatcatcatcatcatc";
val [] = Seq.list_of (Parser.parse_of_string dcx str); 

val str = "catcatcatcatcatcatcatcatcatcatcatcat";
val ((i,st)::more) = Seq.list_of (Parser.parse_of_string dcx str); 
val _ = ParseState.print_sym_parse st i;

in
  val _ = Pretty.writeln (Pretty.str "TEST PASSED: cat1list.ML")
end;
