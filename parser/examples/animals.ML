(* Small Example: a list of animal names "cat" "dog" "cow" "goat" *)

(* --- *)
structure ExData_Animals
= struct 
local open ParserLib; open BaseData; open Data; in
val dcx = base_dcx;
val cat_md = 
    mk_ands_dtyp [("1c", id_of_str "c"), 
                  ("2a", id_of_str "a"), 
                  ("3t", id_of_str "t")]; 
val dcx = dcx |> add_datatyp (dname "cat", cat_md);

val dog_md = 
    mk_ands_dtyp [("1d", id_of_str "d"), 
                  ("2o", id_of_str "o"), 
                  ("3g", id_of_str "g")]; 
val dcx = dcx |> add_datatyp (dname "dog", dog_md);

val cow_md = 
    mk_ands_dtyp [("1c", id_of_str "c"), 
                  ("2o", id_of_str "o"), 
                  ("3w", id_of_str "w")]; 
val dcx = dcx |> add_datatyp (dname "cow", cow_md);

val goat_md = 
    mk_ands_dtyp [("1g", id_of_str "g"), 
                  ("2o", id_of_str "o"), 
                  ("3a", id_of_str "a"), 
                  ("4t", id_of_str "t")]; 
val dcx = dcx |> add_datatyp (dname "goat", goat_md);

val animal_md = 
    mk_ors_dtyp [("cat", "cat"), ("dog", "dog"), 
                 ("cow", "cow"), ("goat", "goat")];
val dcx = dcx |> add_datatyp (dname "animal", animal_md);



val _ = print_dcx dcx;

end; end;

local 
val dcx = ExData_Animals.dcx;

(* --- *)
val str = "ccat";
val [] = Seq.list_of (Parser.parse_of_string dcx str);

val str = "cc";
val [] = Seq.list_of (Parser.parse_of_string dcx str);

val str = "cat";
val ((i,st)::more) = Seq.list_of (Parser.parse_of_string dcx str);
val _ = ParseState.print_sym_parse st i;

val str = "dog";
val ((i,st)::more) = Seq.list_of (Parser.parse_of_string dcx str);
val _ = ParseState.print_sym_parse st i;

val str = "cow";
val ((i,st)::more) = Seq.list_of (Parser.parse_of_string dcx str);
val _ = ParseState.print_sym_parse st i;

val str = "goat";
val ((i,st)::more) = Seq.list_of (Parser.parse_of_string dcx str);
val _ = ParseState.print_sym_parse st i;


in
val _ = Pretty.writeln (Pretty.str "TEST PASSED: animals.ML")
end;

