(* a parser *)

signature PARSER
= sig

datatype pstep = PStep of ITab.N.T * ITab.N.T * ParseState.T
               | PFinal of ITab.name * (ITab.N.T * ITab.N.T * ParseState.T)
               | PFail of ITab.N.T * ITab.N.T * ParseState.T;

(* initial pstep preperation *)
val pstep_of_dlist : Data.dcontext -> DnTab.name list -> pstep
val pstep_of_string : Data.dcontext -> string -> pstep

(* parsing *)
val parse_of_string : Data.dcontext -> string -> (ITab.name * ParseState.T) Seq.seq
val parse_of_pstep : pstep -> (ITab.name * ParseState.T) Seq.seq

(* internal *)
val do_pstep : ITab.N.T * ITab.N.T * ParseState.T -> pstep
val finite_parse_aux : 
    pstep -> unit 
    -> ((ITab.name * ParseState.T) * (ITab.name * ParseState.T) Seq.seq) option

(* pretty printing *)
val pretty_pstep_full : pstep -> Pretty.T
val print_pstep_full : pstep -> unit

val pretty_pstep_active : pstep -> Pretty.T
val print_pstep_active : pstep -> unit

end;



(* a parser *)
structure DB_Parser
= struct

(* SMALL IMPROVE: slight duplication of work: two calls to containment
for left and right id, this is already done in do_pstep: could reuse
the result(s) *)


(* 
ParseState.NewTop (ParseState.Pruned st2) => (* i2 is top *)
         (newids1, newlset1, newrset1, st2)
       | ParseState.NewTop (ParseState.Expanded ((fromid,newids2), st2)) => 
         (ITab.N.union_merge newids2 newids1,
          if ITab.N.contains lset fromid then
            ITab.N.union_merge newids2 newlset1 else newlset1,
          if ITab.N.contains rset fromid then
            ITab.N.union_merge newids2 newrset1 else newrset1,
          ParseState.del1_from_open  st2)
       | 
*)


(* rset is set of nodes touching right end of parsed text, we consider
all adjacent awake nodes and update the state acordingly.  *)
fun check1_right lset rset (is1 as (i,s)) i2 
                 (newids1, newlset1, newrset1, st1) =
    (case ParseState.consider is1 (i2,ParseState.get_sym st1 i2) st1
      of ParseState.Merged (ParseState.Pruned st2) => 
         (newids1, newlset1, newrset1, st2)
       | ParseState.Merged (ParseState.Expanded (((id1,id2),id12), st2)) => 
           (ITab.N.add id12 newids1, 
            if ITab.N.contains lset id1 then ITab.N.add id12 newlset1 
            else newlset1,
            if ITab.N.contains rset id2 then ITab.N.add id12 newrset1 
            else newrset1,
            st2)
       | ParseState.Delayed st2 => (newids1, newlset1, newrset1, st2));

(* datatye for steps of parsing *)
datatype pstep = PStep of (ITab.N.T * ITab.N.T * ParseState.T)
               | PFinal of ITab.name * (ITab.N.T * ITab.N.T * ParseState.T)
               | PFail of ITab.N.T * ITab.N.T * ParseState.T;

(* lset and rset are set of nodes touching the left and right edges of
the text to be parsed. *)
fun do_pstep (lset, rset, st) = 
    (case (ParseState.get_next_open_sym st) 
     of NONE => PFail (lset, rset, st)
      | SOME (is0 as (i,s)) => 
        let val leftedge = ITab.N.contains lset i;
            val rightedge = ITab.N.contains rset i;
        in
          if leftedge andalso rightedge andalso SymState.is_top s
          then PFinal (i, (lset, rset, ParseState.del1_from_open i st))
          else 
            if ((SymState.needs_more_right s andalso 
                 SymState.empty_right_agenda s)
                orelse (SymState.needs_more_left s andalso 
                        SymState.empty_left_agenda s))
            then
               PStep (if leftedge then ITab.N.delete i lset else lset,
                      if rightedge then ITab.N.delete i rset else rset, 
                      ParseState.remove_sym is0 st)
            else if SymState.is_top s then 
              case (ParseState.consider_parents_of_top is0 st) of 
                NONE => PStep (if leftedge then ITab.N.delete i lset 
                               else lset,
                               if rightedge then ITab.N.delete i rset 
                               else rset,
                               ParseState.remove_sym is0 st)
              | SOME ((_,newids), st2) =>
                PStep (if leftedge then ITab.N.union_merge newids lset 
                       else lset,
                       if rightedge then ITab.N.union_merge newids rset 
                       else rset,
                       st2 |> ParseState.del1_from_open i
                           |> ParseState.union_to_open newids )
            else
              let 
                val ragenda = SymState.get_rnds s;
                val (addnds, addlset, addrset, newst) = 
                    (* check against everything to the right of is0 *)
                    (ITab.N.empty, ITab.N.empty, ITab.N.empty, 
                     ParseState.del1_from_open i st)
                      |> (ITab.N.fold (check1_right lset rset is0) 
                                      (AdjNds.get_awakened ragenda))
              in
                PStep (ITab.N.union_merge addlset lset, 
                       ITab.N.union_merge addrset rset, 
                       ParseState.union_to_open addnds newst)
              end
        end);

(* preperation functions *)
fun pstep_of_dlist dcx l = 
    let val ((i0,iK),st) = ParseState.mk_from_dname_list dcx l
        val lset = ITab.N.add i0 ITab.N.empty
        val rset = ITab.N.add iK ITab.N.empty
    in (PStep (lset, rset, st)) end;

fun pstep_of_string dcx s = 
    let val dname_of_char = (Data.dname o ParserLib.id_of_str o Char.toString);
        val dname_of_str = (Data.dname o ParserLib.id_of_str);
        val dlist = map dname_of_char (String.explode s);
    in pstep_of_dlist dcx dlist end;

(* sequence construction for iterating parse steps *)
fun finite_parse_aux (p as PStep r) () = finite_parse_aux (do_pstep r) ()
  | finite_parse_aux (p as PFinal (i,r as (rset,lset,st))) () = 
    SOME ((i,st), Seq.make (finite_parse_aux (do_pstep r)))
  | finite_parse_aux (PFail _) () = NONE;

(* parsing *)
fun parse_of_pstep pstep0 = Seq.make (finite_parse_aux pstep0);
fun parse_of_string dcx s = parse_of_pstep (pstep_of_string dcx s);


fun pretty_isopen_presym st i =
    (case ParseState.get_next_open_id st 
      of NONE => Pretty.str "?"
       | SOME openi => 
         (if ITab.name_eq (openi,i)
          then Pretty.str "*" else Pretty.str "-"));

fun pretty_pstate_active (l,r,st) = 
     Pretty.chunks 
       ([Pretty.block [Pretty.str "lset: ", ITab.N.pretty l],
         Pretty.block [Pretty.str "rset: ", ITab.N.pretty r],
         Pretty.block [Pretty.str "open sts: ", ITab.N.pretty (ParseState.get_opensts st)]
        ]
        @ (map (fn (i,s) => Pretty.block [pretty_isopen_presym st i,
                                          Pretty.str "(",
                                          ITab.pretty_name i,
                                          Pretty.str ")=",
                                          SymState.pretty s])
               (Library.sort 
                  (fn ((i1,s1),(i2,s2)) => SymState.lr_order (s1,s2))
                  (filter 
                     (fn (i,s) => SymState.is_alive s)
                     (ITab.dest (ParseState.get_sts st))))));

fun pretty_pstate_full (l,r,st) = 
     Pretty.chunks 
       ([Pretty.block [Pretty.str "lset: ", ITab.N.pretty l],
         Pretty.block [Pretty.str "rset: ", ITab.N.pretty r],
         Pretty.block [Pretty.str "open sts: ", ITab.N.pretty (ParseState.get_opensts st)]
        ]
        @ (map (fn (i,s) => Pretty.block [Pretty.str "Symid: ",
                                          ITab.pretty_name i,
                                          Pretty.str " : ",
                                          SymState.pretty s])
               (ITab.dest (ParseState.get_sts st))));

fun pretty_pstep_full (PStep r) = 
    Pretty.block [Pretty.str "PStep: ", pretty_pstate_full r]
  | pretty_pstep_full (PFinal (i,(l,r,st))) = 
    Pretty.block [Pretty.str "PFinal: ", ITab.pretty_name i,
                  Pretty.str " : ",
                  SymState.pretty (ParseState.get_sym st i)]
  | pretty_pstep_full (PFail r) = 
    Pretty.block [Pretty.str "PFail: ", pretty_pstate_full r];

val print_pstep_full = Pretty.writeln o pretty_pstep_full;


fun pretty_pstep_active (PStep r) = 
    Pretty.block [Pretty.str "PStep: ", pretty_pstate_active r]
  | pretty_pstep_active (PFinal (i,(l,r,st))) = 
    Pretty.block [Pretty.str "PFinal: ", ITab.pretty_name i,
                  Pretty.str " : ",
                  SymState.pretty (ParseState.get_sym st i)]
  | pretty_pstep_active (PFail r) = 
    Pretty.block [Pretty.str "PFail: ", pretty_pstate_active r];

val print_pstep_active = Pretty.writeln o pretty_pstep_active;


end;

structure Parser : PARSER = DB_Parser;
