(* Minimal XML-like/light Parser *)

signature XML_PARSER =
sig

val dcx : Data.dcontext;

end;

structure XMLParser = 
struct
local open ParserLib; open ParserBaseData; open Data; in


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

(* initial context *)
val dcx = base_dcx;

(* XML data (not: "<" and ">") *)
val xml_data_char_ids = 
    map (id_of_str o Char.toString)
        ([#"\t", #"\n"] @ 
         (chars_between (#" ") (#";")) @
         (* not: #"<" *)
         [#";"] @     
         (* not: #">"  -- could actually leave this in. *)
         (chars_between (#"?") (#"~")));
val xml_data_char_id = "xml_data_char"
val xml_data_char_md = Ors (fold (fn x => CnTab.update (cname x, dname x))
                                 xml_data_char_ids CnTab.empty);
val dcx = dcx |> add_datatyp (dname xml_data_char_id, xml_data_char_md);

(* xml-data 1list *)
val ((xml_data_1list_id,xml_data_1list_cons_id),dcx) = 
    dcx |> mk_1list xml_data_char_id;

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

(* alphanum 1list *)
val ((alphanum_list_id,alphanum_list_cons_id),dcx) = 
    dcx |> mk_1list alphanum_id;


(* alpha numeric and "_" and "." *)
val num_base_ids = 
  map (id_of_str o Char.toString) 
      (chars_between #"0" #"9")
val num_id = "num"
val num_md = Ors (fold (fn x => CnTab.update (cname x, dname x))
                       num_base_ids CnTab.empty);
val dcx = dcx |> add_datatyp (dname num_id, num_md);

(* number 1lists / digit strings *)
val ((num_list_id, num_list_cons_id),dcx) = 
    dcx |> mk_1list num_id;

(* xml-open-tag *)
val xml_open_id = "xml-open";
val xml_open_md = 
    mk_ands_dtyp [("1<", id_of_str "<"), 
                  ("2n", alphanum_list_id), 
                  ("3>", id_of_str ">")];     
val dcx = dcx |> add_datatyp (dname xml_open_id, xml_open_md);


(* xml-close-tag *)
val xml_close_id = "xml-close";
val xml_close_md = 
    mk_ands_dtyp [("1<", id_of_str "<"), 
                  ("2/", id_of_str "/"), 
                  ("3n", alphanum_list_id), 
                  ("4>", id_of_str ">")];     
val dcx = dcx |> add_datatyp (dname xml_close_id, xml_close_md);


(* xml open comment tag *)
val xml_open_comment_id = "xml-open-comment";
val xml_open_comment_md = 
    mk_ands_dtyp [("1<", id_of_str "<"), 
                  ("2!", id_of_str "!"), 
                  ("3-", id_of_str "-"), 
                  ("4-", id_of_str "-")];     
val dcx = dcx |> add_datatyp (dname xml_open_comment_id, xml_open_comment_md);

(* xml close comment tag *)
val xml_close_comment_id = "xml-close-comment";
val xml_close_comment_md = 
    mk_ands_dtyp [("1-", id_of_str "-"), 
                  ("2-", id_of_str "-"), 
                  ("4>", id_of_str ">")];     
val dcx = dcx |> add_datatyp (dname xml_close_comment_id, xml_close_comment_md);

(* xml comment tag *)
val xml_comment_id = "xml-comment";
val xml_comment_md = 
    mk_ands_dtyp [("1open", xml_open_comment_id), 
                  ("2data", xml_data_1list_id), 
                  ("3close", xml_close_comment_id)];     
val dcx = dcx |> add_datatyp (dname xml_comment_id, xml_comment_md);


(* xml leaf tag, not empty *)
val xml_leaf_id = "xml-leaf";
val xml_leaf_md = 
    mk_ands_dtyp [("1open", xml_open_id), 
                  ("2data", xml_data_1list_id), 
                  ("3close", xml_close_id)];     
val dcx = dcx |> add_datatyp (dname xml_leaf_id, xml_leaf_md);

(* a compound node with sub elements mutually recursively defined *)
val xml_compound_id = "xml-compound"

(* a node, either a comment, a compound, or a leaf *)
val xml_node_id = "xml-node";
val xml_node_md = 
    mk_ors_dtyp [("1compound", xml_compound_id), 
                 ("2comment", xml_comment_id),
                 ("3leaf", xml_leaf_id)];     
val dcx = dcx |> add_datatyp (dname xml_node_id, xml_node_md);

(* space and node *)
val xml_compound_md = 
    mk_ands_dtyp [("1open", xml_open_id), 
                  ("2data", xml_space_and_data_1list_id), 
                  ("3close", xml_space_and_close_id)];     
val dcx = dcx |> add_datatyp (dname xml_compound_id, xml_compound_md);


val xml_optspace_and_node_id = "xml-space-and-node";
val xml_optspace_and_node_md = 
    mk_ors_dtyp [("1space_and_node", xml_space_and_node_id), 
                 ("2node", xml_node_id),
                 ("3leaf", xml_leaf_id)];     


(* list of xml-nodes *)
val ((xml_node_list_id, xml_node_list_cons_id),dcx) = 
    dcx |> mk_1list xml_node_id;

val xml_compound_md = 
    mk_ands_dtyp [("1open", xml_open_id), 
                  ("2data", xml_space_and_data_1list_id), 
                  ("3close", xml_space_and_close_id)];     
val dcx = dcx |> add_datatyp (dname xml_compound_id, xml_compound_md);

end; (* local *)
end;



