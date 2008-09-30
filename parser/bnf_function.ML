structure BNFFunction
= struct

val unit_id = BNFMorphisms.unit_id;
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
          Data.mk_ors_dtyp [("1Empty", unit_id), 
                            ("2Cons", s_list_cons_id)];
      val s_list_cons_md = 
          Data.mk_ands_dtyp [("1hd", s_id), ("2tl", s_list_id)];     
    in 
      ((s_list_id, s_list_cons_id),
       dcx |> Data.add_datatyp (Data.dname s_list_id, s_list_md)
           |> Data.add_datatyp (Data.dname s_list_cons_id, s_list_cons_md))
    end;

(* *)
fun mk_optional n_id dcx = 
    let 
      val n_opt_id = n_id ^ "_opt";
      val opt_md = 
          Data.mk_ors_dtyp [("1Some", n_id), ("2None", unit_id)];
    in (n_opt_id, dcx |> Data.add_datatyp (Data.dname n_opt_id, opt_md)) end;

(* helper function list consturction (substitue X): 
        X_1list(A) ::= A | X_1list.cons(A)  
   X_1list.cons(A) ::= A, X_1list(A) 
*)
fun mk_1list s_id dcx = 
    let 
      val s_list_id = (s_id ^ "_1list")
      val s_list_cons_id = (s_list_id ^ ".cons")
      val s_list_md = 
          Data.mk_ors_dtyp [("1Elem", s_id), ("2Cons", s_list_cons_id)];
      val s_list_cons_md = 
          Data.mk_ands_dtyp [("1hd", s_id), ("2tl", s_list_id)];     
    in 
      ((s_list_id, s_list_cons_id),
       dcx |> Data.add_datatyp (Data.dname s_list_id, s_list_md)
           |> Data.add_datatyp (Data.dname s_list_cons_id, s_list_cons_md))
    end;

end;
