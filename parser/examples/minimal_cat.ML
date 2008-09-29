(* Minimal Example: parse the word "cat" *)

(* --- *)
structure ExData_MinimalCat
= struct 
local open ParserLib; open BaseData; open Data; in
val dcx = base_dcx;
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
