(* Minimal Example: parse a list of the word "cat" *)

(* --- *)
structure ExData_CatList
= struct 
local open ParserLib; open BaseData; open Data; in

val dcx = base_dcx;
val cat_md = 
    mk_ands_dtyp [("1c", id_of_str "c"), 
                  ("2a", id_of_str "a"), 
                  ("3t", id_of_str "t")];     
val dcx = dcx |> add_datatyp (dname "cat", cat_md);

val unit_strname = BNFMorphisms.unit_strname;
val unit_dname = BNFMorphisms.unit_dname;

(* make a list of s_id, possibly empty 
        X_list(A) ::= unit | X_list.cons(A)  
   X_list.cons(A) ::= A, X_listist(A) 
*)
fun mk_list s_id dcx = 
    let 
      val s_list_id = (s_id ^ "_list")
      val s_list_cons_id = (s_list_id ^ ".cons")
      val s_list_md = 
          Data.mk_ors_dtyp [("1empty", unit_strname), 
                            ("2cons", s_list_cons_id)];
      val s_list_cons_md = 
          Data.mk_ands_dtyp [("1hdC", s_id), ("2tlC", s_list_id)];     
    in 
      ((s_list_id, s_list_cons_id),
       dcx |> add_datatyp (dname s_list_id, s_list_md)
           |> add_datatyp (dname s_list_cons_id, s_list_cons_md))
    end;

(* lists of cats *)
val ((catlist_id, catlist_cons_id), dcx) = dcx |> mk_list "cat";

(* remove units to make standard bnf *)
val dcx = dcx |> BNFMorphisms.elim_epsilon;

val _ = print_dcx dcx;
end; end;


local 
(* --- *)
val dcx = ExData_CatList.dcx;

val str = "ccat";
val [] = Seq.list_of (Parser.parse_of_string dcx str);

val str = "cc";
val [] = Seq.list_of (Parser.parse_of_string dcx str);

val str = "cat";
val ((i,st)::more) = Seq.list_of (Parser.parse_of_string dcx str);
ParseState.print_sym_parse st i;

val str = "catca";
val [] = Seq.list_of (Parser.parse_of_string dcx str);
ParseState.print_sym_parse st i;

val str = "catcat";
val ((i,st)::more) = Seq.list_of (Parser.parse_of_string dcx str);
ParseState.print_sym_parse st i;

(* FIXME: should allow empty list *)
(* 
val str = "";
val ((i,st)::more) = Seq.list_of (Parser.parse_of_string dcx str);
ParseState.print_sym_parse st i;

val (p as Parser.PStep (r as (lset,rset,st))) = Parser.pstep_of_string dcx str;
Parser.print_pstep_active p;

val (p as Parser.PStep (r as (lset,rset,st))) = Parser.do_pstep r;
Parser.print_pstep_active p;

Parser.print_pstep_active (Parser.do_pstep r);
*)


(* does some pretty serious stress testing of NameTables... *)
val str = "catcatca";
val [] = Seq.list_of (Parser.parse_of_string dcx str);

val str = "catcatcat";
val ((i,st)::more) = Seq.list_of (Parser.parse_of_string dcx str); 
ParseState.print_sym_parse st i;


val str = "catcatcatcatcat";
val ((i,st)::more) = Seq.list_of (Parser.parse_of_string dcx str); 
ParseState.print_sym_parse st i;

val str = "catcatcatcatcatcatcatcatcatcatcatc";
val [] = Seq.list_of (Parser.parse_of_string dcx str); 

val str = "catcatcatcatcatcatcatcatcatcatcatcat";
val ((i,st)::more) = Seq.list_of (Parser.parse_of_string dcx str); 
ParseState.print_sym_parse st i;




(* dbg *)

val [cat_list_dname] = Dn.NTab.N.dest (Data.parents_of_data (Data.get dcx Data.unit_dname));


in
val _ = Pretty.writeln (Pretty.str "TEST PASSED: a_cat_option.ML")
end;
