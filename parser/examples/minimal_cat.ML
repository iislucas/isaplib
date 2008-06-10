(* Minimal Example: parse the word "cat" *)

(* --- *)
structure ExData
= struct 
local open ParserLib; open UnderData; open Data; in
val dcx = base_dcx;
val cat_md = 
    mk_ands_dtyp [("1c", id_of_str "c"), 
                  ("2a", id_of_str "a"), 
                  ("3t", id_of_str "t")];     
val dcx = dcx |> add_datatyp (dname "cat", cat_md);
val _ = print_dcx dcx;
end; end;

(* --- *)
val dcx = ExData.dcx;

val str = "ccat";
val [] = Seq.list_of (Parser.parse_of_string dcx str);

val str = "cc";
val [] = Seq.list_of (Parser.parse_of_string dcx str);

val str = "cat";
val ((i,st)::more) = Seq.list_of (Parser.parse_of_string dcx str);
ParseState.print_sym_parse st i;



(* 
(* DEBUG STUFF *)

val (p as Parser.PStep (r as (lset,rset,st))) = Parser.pstep_of_string dcx str;
Parser.print_pstep_active p;

Parser.print_pstep_full p;

val (p as Parser.PStep (r as (lset,rset,st))) = Parser.do_pstep r;
Parser.print_pstep_active p;

Parser.print_pstep_active (Parser.do_pstep r);



val SOME (is0 as (i,s)) = (ParseState.get_next_open_sym st);
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
