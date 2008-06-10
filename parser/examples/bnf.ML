structure ExData 
= struct 

local 
  open ParserLib;
  open UnderData;
  open Data;
in

(* context which we will build up *)
val dcx = base_dcx;

val unit_id = "unit";

(* boolean *)
val bool_md = mk_ors_dtyp [("False", unit_id), ("True", unit_id)];
val dcx = dcx |> add_datatyp (dname "bool", bool_md);

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

  
val alphanum_list_cons_id = "alphanum_list.cons";
val alphanum_list_id = "alphanum_list";

val alphanum_list_cons_md = 
    mk_ands_dtyp [("1hd", alphanum_id), ("2tl", alphanum_list_id)];
val dcx = dcx |> add_datatyp (dname alphanum_list_cons_id, 
                                alphanum_list_cons_md);

val alphanum_list_md = 
    mk_ors_dtyp [("nil", unit_id), ("cons", alphanum_list_cons_id)];
val dcx = dcx |> add_datatyp (dname alphanum_list_id, alphanum_list_md);

(* *)
val aspace_base_ids = (map id_of_str ["\\t", "\\n", " "]);
val aspace_id = "aspace";
val aspace_md = Ors (fold (fn x => CnTab.update (cname x, dname x))
                               aspace_base_ids CnTab.empty);
val dcx = dcx |> add_datatyp (dname aspace_id,aspace_md);


(* generic list construction *)
fun mk_list s_id dcx = 
let 
val s_list_id = (s_id ^ "_list")
val s_list_cons_id = (s_list_id ^ ".cons")
val s_list_md = 
mk_ors_dtyp [("nil", unit_id), ("cons", s_list_cons_id)];
val s_list_cons_md = 
mk_ands_dtyp [("1hd", s_id), ("2tl", s_list_id)];     
in 
((s_list_id, s_list_cons_id),
  dcx |> add_datatyp (dname s_list_id, s_list_md)
  |> add_datatyp (dname s_list_cons_id, s_list_cons_md))
end;

val ((aspace_list_id, aspace_list_cons_id),dcx) = 
    dcx |> mk_list aspace_id;

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
    dcx |> mk_list quoted_or_unquoted_el_id;

val quotes_and_quoted_string_id = "quotes_and_quoted_string";
val quotes_and_quoted_string_md = 
    mk_ands_dtyp
      [("1openquote", id_of_str "'"),
       ("2string", quoted_or_unquoted_list_id),
       ("3closequote", id_of_str "'")];

val dcx = dcx |> add_datatyp (dname quotes_and_quoted_string_id, 
                                quotes_and_quoted_string_md);

val string_or_name_id = "string_or_name";
val string_or_name_md = 
    mk_ors_dtyp
      [("name",alphanum_list_id),
       ("string",quotes_and_quoted_string_id)];

val dcx = dcx |> add_datatyp (dname string_or_name_id, string_or_name_md);

(* 1list with space separator of strings or names *)
val strname_1slist_many_id = "strname_1slist.many";
val strname_1slist_id = "strname_1slist";
val strname_1slist_many_md = 
    mk_ands_dtyp [("1hd", string_or_name_id), 
                   ("2space", aspace_list_id),
                   ("3tl", strname_1slist_id)];

val dcx = dcx |> add_datatyp (dname strname_1slist_many_id, 
                                strname_1slist_many_md);

val dc = dcx;
val (n,md) = (dname strname_1slist_many_id, strname_1slist_many_md)

val strname_1slist_md = 
    mk_ors_dtyp [("one", string_or_name_id), 
                  ("many", strname_1slist_many_id)];
val dcx = dcx |> add_datatyp (dname strname_1slist_id, strname_1slist_md);

val rule_id = "rule";
val rule_md = 
mk_ands_dtyp 
[("1", alphanum_list_cons_id), (* list_cons = 1list *)
 ("2", aspace_list_id),
 ("3", id_of_str "="),
 ("4", aspace_list_id),
 ("5", strname_1slist_id) 
];

val dcx = dcx |> add_datatyp (dname rule_id, rule_md);
 
val _ = print_dcx dcx;

end; end;
