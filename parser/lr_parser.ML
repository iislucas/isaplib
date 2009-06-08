(* a parser *)

signature LR_PARSER
= sig

type parser_state = (An.NSet.T (* set of nodes touching left edge *)
                     * An.NSet.T (* set of nodes touching right edge *)
                     * ParseState.T); (* state of parsing *)

(* datatye for steps of parsing *)
datatype pstep = PStep of parser_state
               | PFinal of AnTab.name * parser_state
               | PFail of parser_state;

               
val base_dcx : Data.dcontext;

val dbg_st : pstep ref;

(* initial pstep preperation *)
val pstep_of_dlist : Data.dcontext -> DnTab.name list -> pstep
val pstep_of_string : Data.dcontext -> string -> pstep

(* parsing *)
val parse_of_string : Data.dcontext -> string -> (AnTab.name * ParseState.T) Seq.seq
val parse_of_pstep : pstep -> (AnTab.name * ParseState.T) Seq.seq

(* internal *)
val do_pstep : (AnTab.name * SymState.T) -> parser_state -> pstep
val finite_parse_aux : 
    pstep -> unit 
    -> ((AnTab.name * ParseState.T) * (AnTab.name * ParseState.T) Seq.seq) option

(* pretty printing *)
val pretty_pstep_full : pstep -> Pretty.T
val print_pstep_full : pstep -> unit

val pretty_pstep_active : pstep -> Pretty.T
val print_pstep_active : pstep -> unit

end;



(* a parser *)
structure DB_LR_Parser
= struct

(* from string positions to open symbols *)
structure OpenLocMap = NameMapFun(structure Dom = An; structure Cod = An);

val base_dcx = Data.mk_base_dcx ParserBaseData.basetype_names;

(* SMALL IMPROVE: slight duplication of work: two calls to containment
for left and right id, this is already done in do_pstep: could reuse
the result(s) *)


(* 
ParseState.NewTop (ParseState.Pruned st2) => (* i2 is top *)
         (newids1, newlset1, newrset1, st2)
       | ParseState.NewTop (ParseState.Expanded ((fromid,newids2), st2)) => 
         (An.NSet.union_merge newids2 newids1,
          if An.NSet.contains lset fromid then
            An.NSet.union_merge newids2 newlset1 else newlset1,
          if An.NSet.contains rset fromid then
            An.NSet.union_merge newids2 newrset1 else newrset1,
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
           (An.NSet.add id12 newids1, 
            if An.NSet.contains lset id1 then An.NSet.add id12 newlset1 
            else newlset1,
            if An.NSet.contains rset id2 then An.NSet.add id12 newrset1 
            else newrset1,
            st2)
       | ParseState.Delayed st2 => (newids1, newlset1, newrset1, st2));

       
(* state of the parser *)
datatype parser_state = PsrState of 
{ lset : An.NSet.T, (* set of nodes touching left edge *)
  rset : An.NSet.T, (* set of nodes touching right edge *)
  st : ParseState.T, (* parse state *)
  openls : OpenLocMap.T (* root positions to higher level symbol states *)
}; (* state of parsing *)


val init_state = 
PsrState {
  lset = An.NSet.empty,
  rset = An.NSet.empty,
  st = ParseState.init_state base_dcx,
  openls = OpenLocMap.empty
}

fun get_lset (PsrState rep) = #lset rep;
fun get_rset (PsrState rep) = #rset rep;
fun get_st (PsrState rep) = #st rep;
fun get_openls (PsrState rep) = #openls rep;

fun update_lset f (PsrState rep) = 
PsrState { 
  lset = f (#lset rep), 
  rset = (#rset rep), 
  st = (#st rep),
  openls = (#openls rep)
};

fun update_rset f (PsrState rep) = 
PsrState { 
  lset = (#lset rep), 
  rset = f (#rset rep), 
  st = (#st rep),
  openls = (#openls rep)
};
      
fun update_st f (PsrState rep) = 
PsrState { 
  lset = (#lset rep), 
  rset = (#rset rep), 
  st = f (#st rep),
  openls = (#openls rep)
};
      
fun update_openls f (PsrState rep) = 
PsrState { 
  lset = (#lset rep), 
  rset = (#rset rep), 
  st = (#st rep),
  openls = f (#openls rep)
};


fun remove_sym (is0 as (i,s)) (r as PsrState rep) = 
    PsrState { 
      lset = An.NSet.delete i (#lset rep), 
      rset = An.NSet.delete i (#rset rep), 
      st = ParseState.remove_sym is0 (#st rep),
      openls = OpenLocMap.del_cod i (#openls rep)
    };

fun close_sym (is0 as (i,s)) (r as PsrState rep) = 
    PsrState { 
      lset = An.NSet.delete i (#lset rep), 
      rset = An.NSet.delete i (#rset rep), 
      st = ParseState.del1_from_open i (#st rep),
      openls = OpenLocMap.del_cod i (#openls rep)
    };

fun new_syms (i,(leftedge, rightedge)) newids st2 (r as PsrState rep) = 
    PsrState { 
      lset = if leftedge then An.NSet.union_merge newids (#lset rep) 
             else (#lset rep), 
      rset =if rightedge then An.NSet.union_merge newids (#rset rep) 
             else (#rset rep), 
      st = st2 |> ParseState.del1_from_open i
               |> ParseState.union_to_open newids,
      openls = (#openls rep) |> new_open_locs i newids
    };
    
    
(* IMPROVE: this kind of pruning should probably happen in parse_state *)
(* prune symbols that have impossible links *)
fun prune1 (is0 as (i,s)) r = 
    if not (has_correct_adj_links s)
    then remove_sym is0 r else r;

fun prune_set aset r = 
    (An.NSet.fold 
      (fn ln => prune1 (n, ParseState.get_sym (get_st r) n))
      aset r);

fun edge_status r i = 
    (An.NSet.contains (get_lset r) i, An.NSet.contains (get_rset r) i);


(* datatye for steps of parsing *)
datatype pstep = PStep of parser_state
               | PFinal of AnTab.name * parser_state
               | PFail of parser_state;

val dbg_st = ref (PFail init_state);

(* lset and rset are set of nodes touching the left and right edges of
the text to be parsed. *)
fun do_pstep (is0 as (i,s)) r = 
    let val (lr_status as (leftedge, rightedge)) = edge_status r i
    in
      if leftedge andalso rightedge andalso SymState.is_top s
      then PFinal (i, close_sym is0 r)
      else (* if we are impossibly stuck on left or right, remove this state *)
        if not (has_correct_adj_links s)
        then PStep (remove_sym r)
        else if SymState.is_top s then (* if you are top... *)
          case (ParseState.consider_parents_of_top is0 st) of 
            NONE => PStep (remove_sym is0 r)
          | SOME ((_,newids), st2) =>
            PStep (r |> new_syms (i,newids) st2
                     |> prune_set newids)
        else
          let 
            val ragenda = SymState.get_rnds s;
            val (addnds, addlset, addrset, newst) = 
              (* check against everything to the right of is0 *)
                (An.NSet.empty, An.NSet.empty, An.NSet.empty, 
                ParseState.del1_from_open i st)
                |> (An.NSet.fold (check1_right lset rset is0) 
                   (AdjNds.get_awakened ragenda))
          in
            PStep (prune_lset (An.NSet.union_merge addlset lset, 
                   An.NSet.union_merge addrset rset, 
                   ParseState.union_to_open addnds newst))
          end
    end;

(* preperation functions *)
fun pstep_of_dlist dcx l = 
    let val ((i0,iK),st) = ParseState.mk_from_dname_list dcx l
        val lset = An.NSet.add i0 An.NSet.empty
        val rset = An.NSet.add iK An.NSet.empty
    in (PStep (lset, rset, st)) end
    handle ParseState.empty_parse_state_exp _ => 
           PFail (An.NSet.empty,An.NSet.empty,ParseState.init_state dcx);

fun pstep_of_string dcx s = 
    let val dname_of_char = (Data.dname o ParserLib.id_of_str o Char.toString);
        val dname_of_str = (Data.dname o ParserLib.id_of_str);
        val dlist = map dname_of_char (String.explode s);
    in pstep_of_dlist dcx dlist end;

fun get_next_awake_Left (r as (lset,rset,ps)) = 
  (case An.NSet.get_exists (ParseState.is_open ps) lset 
   of NONE => NONE 
    | SOME n => SOME (n,ParseState.get_sym ps n));

(* sequence construction for iterating parse steps *)
fun finite_parse_aux (p as PStep (r as (lset,rset,ps))) () = 
    (case get_next_awake_Left r of NONE => NONE 
     | SOME i => 
       (ParseState.assert_well_formed (#3 r); (* for debugging *)
        dbg_st := p; 
        finite_parse_aux (do_pstep i r) ()))
  | finite_parse_aux (p as PFinal (i,r as (rset,lset,st))) () = 
    SOME ((i,st), Seq.make (finite_parse_aux (PStep r)))
  | finite_parse_aux (PFail _) () = NONE;

(* parsing *)
fun parse_of_pstep pstep0 = Seq.make (finite_parse_aux pstep0);
fun parse_of_string dcx s = parse_of_pstep (pstep_of_string dcx s);


fun pretty_pstate_active (l,r,st) = 
     Pretty.chunks 
       [Pretty.block [Pretty.str "lset: ", An.NSet.pretty l],
        Pretty.block [Pretty.str "rset: ", An.NSet.pretty r],
        ParseState.pretty_active st];

fun pretty_pstate_full (l,r,st) = 
     Pretty.chunks 
       [Pretty.block [Pretty.str "lset: ", An.NSet.pretty l],
        Pretty.block [Pretty.str "rset: ", An.NSet.pretty r],
        ParseState.pretty_full st];

fun pretty_pstep_full (PStep r) = 
    Pretty.block [Pretty.str "PStep: ", pretty_pstate_full r]
  | pretty_pstep_full (PFinal (i,(l,r,st))) = 
    Pretty.block [Pretty.str "PFinal: ", AnTab.pretty_name i,
                  Pretty.str " : ",
                  SymState.pretty (ParseState.get_sym st i)]
  | pretty_pstep_full (PFail r) = 
    Pretty.block [Pretty.str "PFail: ", pretty_pstate_full r];

val print_pstep_full = Pretty.writeln o pretty_pstep_full;


fun pretty_pstep_active (PStep r) = 
    Pretty.block [Pretty.str "PStep: ", pretty_pstate_active r]
  | pretty_pstep_active (PFinal (i,(l,r,st))) = 
    Pretty.block [Pretty.str "PFinal: ", AnTab.pretty_name i,
                  Pretty.str " : ",
                  SymState.pretty (ParseState.get_sym st i)]
  | pretty_pstep_active (PFail r) = 
    Pretty.block [Pretty.str "PFail: ", pretty_pstate_active r];

val print_pstep_active = Pretty.writeln o pretty_pstep_active;


end;

structure LR_Parser : LR_PARSER = DB_LR_Parser;
