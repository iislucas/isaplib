(* Testing Data *)

(* --- *)
structure ExData
= struct 
local open ParserLib; open BaseData; open Data; in
val dcx = Data.mk_base_dcx 
            (map (fn c => (id_of_str (Char.toString c), c))
                 [#"c", #"a", #"t"]);
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

val dcx = Data.del_data (Dn.mk (id_of_str "a")) dcx;

val _ = print_dcx dcx;

end; 
end;
