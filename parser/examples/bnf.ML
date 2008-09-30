(* Grammar for parsing grammars (BNFs) *)
structure ExData_BNF 
= struct 

local 
  open ParserLib;
  open BaseData;
  open Data;
in

(* context which we will build up *)
val dcx = base_dcx;

(* alpha numeric and "_" and "." *)
val alphnum_base_ids = 
  map (id_of_str o Char.toString) 
  ([#"_", #"."]
    @ (chars_between #"a" #"z")
    @ (chars_between #"0" #"9")
    @ (chars_between #"A" #"Z"));
val alphanum_id = "alphanum"
val alphanum_md = Ors (fold (fn x => CnTab.update (cname x, dname x))
  alphnum_base_ids CnTab.empty);
val dcx = dcx |> add_datatyp (dname alphanum_id, alphanum_md);

val ((alphanum_1list_id,alphanum_1list_cons_id),dcx) = 
    dcx |> BNFFunction.mk_1list alphanum_id;

(* *)
val aspace_base_ids = (map id_of_str ["\\t", "\\n", " "]);
val aspace_id = "aspace";
val aspace_md = Ors (fold (fn x => CnTab.update (cname x, dname x))
                               aspace_base_ids CnTab.empty);
val dcx = dcx |> add_datatyp (dname aspace_id,aspace_md);

val ((aspace_list_id, aspace_list_cons_id),dcx) = 
    dcx |> BNFFunction.mk_list aspace_id;

val ((aspace_1list_id, aspace_1list_cons_id),dcx) = 
    dcx |> BNFFunction.mk_1list aspace_id;

(* quoted strings within "'" using "\" as escape *)
val id_of_char = id_of_str o Char.toString;
val quote_id = id_of_char ParserLib.quote_char;
fun quoted_id_of c = 
    "quoted_char_" ^ (Char.toString c);
fun mkquoted_md c = 
    mk_ands_dtyp [("1quote", quote_id), ("2char", id_of_char c)];

val dcx = dcx |> fold add_datatyp 
              (map (fn c => (dname (quoted_id_of c),
                             mkquoted_md c)) 
                   ParserLib.quoted_chars);

val quoted_el_id = "quoted_el";
val quoted_el_md = 
    let val ss = (map quoted_id_of ParserLib.quoted_chars)
    in mk_ors_dtyp (ss ~~ ss) end;

val dcx = dcx |> add_datatyp (dname quoted_el_id, quoted_el_md);

val unquoted_el_id = "unquoted_el";
val unquoted_el_md = 
    let 
      val s = (map (id_of_str o Char.toString) ParserLib.unquoted_chars)
    in mk_ors_dtyp (s ~~ s) end;

val dcx = dcx |> add_datatyp (dname unquoted_el_id, unquoted_el_md);

val quoted_or_unquoted_el_id = "quoted_or_unquoted_el"
val quoted_or_unquoted_el_md = 
   mk_ors_dtyp
   [("unquoted", "unquoted_el"),
    ("quoted", "quoted_el")]

val dcx = dcx |> add_datatyp (dname quoted_or_unquoted_el_id, 
                                quoted_or_unquoted_el_md);

val ((quoted_or_unquoted_list_id, quoted_or_unquoted_list_cons_id), dcx) = 
    dcx |> BNFFunction.mk_list quoted_or_unquoted_el_id;

val quotes_and_quoted_string_id = "quotes_and_quoted_string";
val quotes_and_quoted_string_md = 
    mk_ands_dtyp
      [("1openquote", id_of_str "'"),
       ("2string", quoted_or_unquoted_list_id),
       ("3closequote", id_of_str "'")];

val dcx = dcx |> add_datatyp (dname quotes_and_quoted_string_id, 
                                quotes_and_quoted_string_md);

(* quoated strings or other rule names *)
val string_or_name_id = "string_or_name";
val string_or_name_md = 
    mk_ors_dtyp
      [("1Name",alphanum_1list_id),
       ("2String",quotes_and_quoted_string_id)];
val dcx = dcx |> add_datatyp (dname string_or_name_id, string_or_name_md);

val spaced_string_or_name_id = "spaced_string_or_name";
val spaced_string_or_name_md = 
    mk_ands_dtyp
      [("1space",aspace_1list_id),
       ("2string_or_name",string_or_name_id)];
val dcx = dcx |> add_datatyp (dname spaced_string_or_name_id, 
                              spaced_string_or_name_md);

val ((spaced_strname_list_id, spaced_strname_list_cons_id), dcx) = 
    dcx |> BNFFunction.mk_list string_or_name_id

val string_or_names_id = "string_or_names";
val string_or_names_md = 
    mk_ors_dtyp
      [("1FstStringOrName",string_or_name_id),
       ("2MoreStringOrNames",spaced_strname_list_id)];
val dcx = dcx |> add_datatyp (dname string_or_names_id, string_or_names_md);

(* a BNF rule *)
val rule_id = "rule";
val rule_md = 
    mk_ands_dtyp 
      [("1", alphanum_1list_id), (* list_cons = 1list *)
       ("2", aspace_list_id),
       ("3", id_of_str "="),
       ("4", aspace_list_id),
       ("5", string_or_names_id) 
      ];

val dcx = dcx |> add_datatyp (dname rule_id, rule_md);
 
val _ = print_dcx dcx;

end; end;

(* 
local 
val dcx = ExData_BNF.dcx;
val str = "bnf = cat";
(* val str = "some_alpha_string"; *)
(* val ((i,st)::more) = Seq.list_of (Parser.parse_of_string dcx str);
val _ = ParseState.print_sym_parse st i; *)

val str = "  dsds  sdds";
val (L as (i,st)::more) = Seq.list_of (Parser.parse_of_string dcx str);
val _ = Pretty.writeln 
          (Pretty.chunks 
             (map (fn (i,st) => 
                      Pretty.chunks
                        [Pretty.str "Parse {",
                         Pretty.indent 
                           2 (ParseState.pretty_sym_parse st i),
                     Pretty.str "}"])
                  L));
val _ = Pretty.writeln 
        (Pretty.block 
           [Pretty.str "Number of results: ",
            Pretty.str (Int.toString (length L))]);

in
val _ = Pretty.writeln (Pretty.str "TEST PASSED: bnf.ML")
end;
*)
