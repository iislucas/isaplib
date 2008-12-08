(* Minimal Example: parse the word "cat" *)

(* --- *)
structure ExData_MinimalCat
= struct 
local open ParserLib; open ParserBaseData; open Data; in
val dcx = Parser.base_dcx;
val cat_md = 
    mk_ands_dtyp [("1c", id_of_str "c"), 
                  ("2a", id_of_str "a"), 
                  ("3t", id_of_str "t")];     
val dcx = dcx |> add_datatyp (dname "cat", cat_md);
val _ = print_dcx dcx;


end; end;


(* --- *)
local 
val dcx = ExData_MinimalCat.dcx;

val str = "ccat";
val [] = Seq.list_of (Parser.parse_of_string dcx str);

val str = "cc";
val [] = Seq.list_of (Parser.parse_of_string dcx str);

val str = "cat";
val ((i,st)::more) = Seq.list_of (Parser.parse_of_string dcx str);
val _ = ParseState.print_sym_parse st i;

in
val _ = Pretty.writeln (Pretty.str "TEST PASED: minimal_cat.ML")
end;

(* 
(* debugging *)

val dcx = ExData_MinimalCat.dcx;


val str = "ccat";
(* val [] = Seq.list_of (Parser.parse_of_string dcx str); *)
val [] = Seq.list_of (Parser.parse_of_string dcx str);

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
val s = ParseState.get_sym st i;
open DB_SymState;
val (s as Middle midstat) = s;


val (p as Parser.PStep (r as (lset,rset,st))) = Parser.pstep_of_string dcx str;
val _ = Parser.print_pstep_active p;

val (p as Parser.PStep (r as (lset,rset,st))) = Parser.do_pstep r;
val _ = ParseState.print_full st;

*)
