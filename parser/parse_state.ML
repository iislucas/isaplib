(* The state of parsing *)

signature PARSE_STATE 
= sig

type cx

val mk_cx : Data.dcontext -> cx

type T

datatype 'a pruned_option =
         Expanded of 'a 
       | Pruned of T
datatype progress =
         Merged of (((AnTab.name * AnTab.name) * AnTab.name) * T) pruned_option
       | Delayed of T

(* things that can go wrong *)
exception bad_data_ctxt_exp of string;
exception empty_parse_state_exp of string;
exception symbol_not_in_any_rule_exp of string * Data.dname * T;

(* consturctor functions *)
val init_state : Data.dcontext -> T
val mk_from_dname_list 
    : Data.dcontext -> DnTab.name list 
      -> ((AnTab.name * AnTab.name) * T) (* end points + init parse state *)

val is_open : T -> AnTab.name -> bool
val get_next_open_id : T -> AnTab.name option
val get_next_open_sym : T -> (AnTab.name * SymState.T) option

val base_name : AnTab.name

(* agenda management *)
val del1_from_open : AnTab.name -> T -> T
val add1_to_open : AnTab.name -> T -> T
val union_to_open : An.NSet.T -> T -> T

(* considering states in the agenda; incremental agenda update *)
val consider : AnTab.name * SymState.T 
               -> AnTab.name * SymState.T 
               -> T -> progress

val consider_parents_of_top : 
    AnTab.name * SymState.T -> T -> ((AnTab.name * An.NSet.T) * T) option
val drop_link_and_merge : 
    AnTab.name * SymState.T -> AnTab.name * SymState.T -> T 
    -> (((AnTab.name * AnTab.name) * AnTab.name) * T) pruned_option
val ddelay : AnTab.name * 'a -> AnTab.name * 'b -> T -> T
val delay_until_left_tops : AnTab.name * 'a -> AnTab.name * 'b -> T -> T
val delay_until_right_tops : AnTab.name * 'a -> AnTab.name * 'b -> T -> T

(* remove a symbol and all links to it *)
val remove_sym : AnTab.name * SymState.T -> T -> T

(* managing links between symbol states *)
val drop_adj_link : AnTab.name * 'a -> AnTab.name * 'b -> T -> T
val link_merged_syms :
    AnTab.name * SymState.T -> AnTab.name * SymState.T ->
    AnTab.name * 'a -> T -> T

(* 
val awaken_delayed_lefts : AnTab.name * SymState.T -> T -> T
val awaken_delayed_rights : AnTab.name * SymState.T -> T -> T
val move_left_ddelay_to_delayed : AnTab.name * SymState.T -> T -> T
val move_right_ddelay_to_delayed : AnTab.name * SymState.T -> T -> T
*)

(* basic/internal construction *)
val add_firstleft : DnTab.name -> T -> AnTab.name * T
val add_new_bottom_sym : DnTab.name * AdjNds.T * AdjNds.T ->
                         T -> AnTab.name * T
val add_new_open_sym : SymState.T -> T -> AnTab.name * T
val add_nextright : DnTab.name -> AnTab.name * T -> AnTab.name * T


(* basic/internal gets, sets, and updates *)
val get_andposes : T -> DnTab.name -> Data.AndSet.T
val get_andptab : T -> Data.AndSet.T DnTab.T
val get_dcx : T -> Data.dcontext
val get_opensts : T -> An.NSet.T
val get_sts : T -> SymState.T AnTab.T
val get_sym : T -> AnTab.name -> SymState.T
val get_cx : T -> cx

val upd_ls_active_and_sleeping : AnTab.name * SymState.T -> An.NSet.T -> T -> T
val upd_rs_active_and_sleeping : AnTab.name * SymState.T -> An.NSet.T -> T -> T

val update_nextright_of : AnTab.name -> AnTab.name * T -> AnTab.name * T
val update_opensts : (An.NSet.T -> An.NSet.T) -> T -> T
val update_sts : (SymState.T AnTab.T -> SymState.T AnTab.T) -> T -> T
val update_sym : (SymState.T -> SymState.T) -> AnTab.name -> T -> T
val set_sts : SymState.T AnTab.T -> T -> T
val set_sym : SymState.T -> AnTab.name -> T -> T

val dcx_of_cx : cx -> Data.dcontext
val andptab_of_cx : cx -> Data.AndSet.T DnTab.T

(* invarient checks *)
exception assert_agreeing_adjs_exp of An.name * An.name * T;
val assert_agreeing_adjs : T -> T;

exception assert_well_formed_sym_exp of An.name * T;
val assert_well_formed_syms : T -> T;

val assert_well_formed : T -> T;

(* pretty printing *)
val pretty_symst : T -> AnTab.name -> Pretty.T
val print_symst : T -> AnTab.name -> unit

val pretty_active : T -> Pretty.T
val print_active : T -> unit
val pretty_full : T -> Pretty.T
val print_full : T -> unit

val pretty : T -> Pretty.T
val print : T -> unit

val pretty_sym_parse : T -> AnTab.name -> Pretty.T
val print_sym_parse : T -> AnTab.name -> unit

end;



(* state of a parser *)
structure DB_ParseState
= struct 

(* context and cached tabs *)
datatype cx = Cx of {dcx : Data.dcontext,
                     andptab : Data.AndSet.T DnTab.T
                    };

(* make cached tabs *)
fun mk_cx dcx = 
    Cx {dcx = dcx,
        andptab = Data.mk_andptab dcx
       };

fun dcx_of_cx (Cx rep) = #dcx rep;
fun andptab_of_cx (Cx rep) = #andptab rep;

(* state of a parser: context + symbols states + open symbols *)
datatype T = 
  State of {cx : cx,
    symsts : SymState.T AnTab.T,
    opensts : An.NSet.T
  };

(* base name for new symbols *)
val base_name = An.default_name;

(* basic constructor function *)
fun init_state dcx = 
    State {cx = mk_cx dcx, symsts = AnTab.empty, opensts = An.NSet.empty};

(* basic gets *)
fun get_opensts (State rep) = #opensts rep;
fun get_sts (State rep) = #symsts rep;
fun get_cx (State rep) = #cx rep;
(* basic updates *)
fun update_sts f (State rep) = 
    State {cx = #cx rep, symsts = f (#symsts rep), 
           opensts = #opensts rep};
fun update_opensts f (State rep) = 
    State {cx = #cx rep, symsts = #symsts rep, 
           opensts = f (#opensts rep)};
(* basic sets *)
val set_sts = update_sts o K;

(* other stuff to get from parser state *)
val get_dcx = dcx_of_cx o get_cx;

(* working with and-poses *)
val get_andptab = andptab_of_cx o get_cx;
fun get_andposes st dname = 
    case DnTab.lookup (get_andptab st) dname 
     of NONE => Data.AndSet.empty | SOME x => x;

(* working with particular symbol states *)
(* implicit: id *)
fun get_sym (State rep) = AnTab.get (#symsts rep);
(* implicit: f n st *)
val update_sym = update_sts oo AnTab.map_entry;
val set_sym = update_sym o K;

(* Working with opennes of symstates *) 
  (* implicit: st i *)
val is_open = An.NSet.contains o get_opensts;
  (* implicit: i st *)
val del1_from_open = update_opensts o An.NSet.delete;
val add1_to_open = update_opensts o An.NSet.add;
val union_to_open = update_opensts o An.NSet.union_merge;

(* maybe more efficient to get_bot or get_top - random middle values with
constant access time ? *)
fun get_next_open_id st = An.NSet.get_first (get_opensts st);
fun get_next_open_sym st = 
    case (get_next_open_id st) 
     of NONE => NONE | SOME i => SOME (i,get_sym st i);

fun add_new_open_sym s st =
    let val (i,sts2) = AnTab.add (base_name,s) (get_sts st)
    in (i, st |> set_sts sts2
              |> add1_to_open i)
    end;

exception symbol_not_in_any_rule_exp of string * Data.dname * T;

(* add a new bottom symbol to the parse state *)
fun add_new_bottom_sym (dname, left, right) (st as State rep) = 
    let 
      val andpset = 
          case DnTab.lookup (get_andptab st) dname of 
            NONE => raise symbol_not_in_any_rule_exp ("add_new_bottom_sym", dname, st)
          | SOME andpset => andpset
      val (nextid, symsts') = AnTab.new_name base_name (#symsts rep);
      val s = SymState.mk_leaf (dname, nextid, left, right);
      val symsts2 = AnTab.update (nextid,s) symsts';
      val opensts2 = An.NSet.add nextid (#opensts rep);
    in (nextid, State {cx = #cx rep, symsts = symsts2, opensts = opensts2}) end;

fun update_nextright_of lastid (nextid, st) = 
    (nextid, 
     update_sts (AnTab.map_entry 
                   (SymState.add1_awakened_right nextid)
                   lastid) st);

fun add_nextright dname (lastid,st) = 
  st
  |> add_new_bottom_sym (dname, 
                         AdjNds.mk_awakened (An.NSet.add lastid An.NSet.empty), 
                         AdjNds.empty)
  |> update_nextright_of lastid;

fun add_firstleft dname st = 
    add_new_bottom_sym (dname, AdjNds.empty, AdjNds.empty) st;

exception empty_parse_state_exp of string;

fun mk_from_dname_list dcx [] = 
    raise empty_parse_state_exp "mk_from_dname_list: list is empty"
  | mk_from_dname_list dcx (a::l) = 
    let val (i0,st0) = (init_state dcx) |> add_firstleft a
        val (iK,stK) = fold add_nextright l (i0,st0)
    in ((i0,iK),stK) end;

datatype 'a pruned_option =
         Expanded of 'a 
       | Pruned of T
datatype progress =
         Merged of (((AnTab.name * AnTab.name) * AnTab.name) * T) pruned_option
       | Delayed of T

(* remove a symstate and all left and right links. Does not remove
parents - or children. *)
 fun remove_sym (i,s) (st : T) = 
     st |> (AdjNds.foldall
              (update_sym (SymState.update_lnds (AdjNds.delete1 i)))
              (SymState.get_rnds s))
        |> (AdjNds.foldall
              (update_sym (SymState.update_rnds (AdjNds.delete1 i)))
              (SymState.get_lnds s))
        |> update_sts (AnTab.delete i)
        |> del1_from_open i

(* the symbol state (i,s) has become top and been replaced by
newids. Note the sleeping/delayed duality: for our sleeping links, we
are delayed, so update every sleeping links delayed set replacing old
top symbol with new generated symbols (newids) *)
fun upd_ls_active_and_sleeping (i,s) newids st = 
    st |> (AdjNds.fold_active_and_sleeping
             (update_sym (SymState.update_rnds 
                            (AdjNds.subst_in_active_and_delayed i newids)))
             (SymState.get_lnds s));
fun upd_rs_active_and_sleeping (i,s) newids st = 
    st |> (AdjNds.fold_active_and_sleeping
             (update_sym (SymState.update_lnds
                            (AdjNds.subst_in_active_and_delayed i newids)))
             (SymState.get_rnds s));

(* i and lid are incompatible, so drop the possible parse combination of
their adjacency. *)
fun drop_adj_link (lid,s) (rid,s2) st =
    st |> (update_sym (SymState.update_lnds (AdjNds.delete1 lid)) rid)
       |> (update_sym (SymState.update_rnds (AdjNds.delete1 rid)) lid);

(* delay i until i2 is top - when that happens i2 will activate i and
   make them both open states again with active links. *)
fun delay_until_right_tops (i,s) (i2,s2) st =
    st |> update_sym (SymState.move1_left_to_delayed i) i2
       |> update_sym (SymState.move1_right_to_sleeping i2) i;
fun delay_until_left_tops (i,s) (i2,s2) st =
    st |> update_sym (SymState.move1_right_to_delayed i2) i
       |> update_sym (SymState.move1_left_to_sleeping i) i2;

(* delay considering this adjacency until both are top. When one top
 this will make the other one delayed as normal. When it becomes top
 they will both be made into open states again. *)
fun ddelay (i,s) (i2,s2) st =
    st |> update_sym (SymState.move1_right_to_ddelayed i2) i
       |> update_sym (SymState.move1_left_to_ddelayed i) i2;

(* awaking of delayed states: i has become top and changes to newids, 
   tell all delayed states to wake up to make newids active lefts! *)
fun upd_ls_awaken_delayed (i, s) newids st = 
    An.NSet.fold 
      (fn adjid => 
          update_sym (SymState.subst_right_sleeping_to_awake i newids) adjid
          o add1_to_open adjid)
      (SymState.get_left_delayed s) st;
fun upd_rs_awaken_delayed (i, s) newids st = 
    An.NSet.fold 
      (fn adjid => 
          update_sym (SymState.subst_left_sleeping_to_awake i newids) adjid
          o add1_to_open adjid) 
      (SymState.get_right_delayed s) st;
(* i became top, so all adj ddelayed nodes become delayed *)
fun upd_ls_ddelay_to_delayed (i, s) newids st = 
    An.NSet.fold (update_sym (SymState.subst_right_ddelay_to_delayed i newids))
                (SymState.get_left_ddelayed s) st;
fun upd_rs_ddelay_to_delayed (i, s) newids st = 
    An.NSet.fold (update_sym (SymState.subst_left_ddelay_to_delayed i newids)) 
                (SymState.get_right_ddelayed s) st;

(* given symstate s with id i, in parsing state st, find where the
symbol s can occur in some parent state. Assumes: s is Top / fully
instantiated. *)
fun consider_parents_of_top (is1 as (i, s as SymState.Top intype)) st = 
    let 
      val andposes = get_andposes st (SymState.get_parent_dname s);
      val dcx = get_dcx st;
    in 
      if Data.AndSet.is_empty andposes then NONE
      else 
        let 
          fun add_parent_sym andp (newids, st2) = 
              let val ps = SymState.mk_parent dcx (i,intype) andp
                  val (pi, st2') = add_new_open_sym ps st2
              in (An.NSet.add pi newids, st2') end;

          val (newids, st2) = 
              Data.AndSet.fold add_parent_sym andposes (An.NSet.empty, st)
        in
          SOME 
            ((i,newids), st2 |> upd_ls_awaken_delayed is1 newids
                             |> upd_rs_awaken_delayed is1 newids
                             |> upd_ls_ddelay_to_delayed is1 newids
                             |> upd_rs_ddelay_to_delayed is1 newids
                             (* and now for adj links which just need updating *)
                             |> upd_ls_active_and_sleeping is1 newids 
                             |> upd_rs_active_and_sleeping is1 newids
                             (* links in old top symbol can now be dropped *)
                             |> update_sym SymState.drop_all_links i)
        end
    end;

(* everything left of is1 has it's right link that went to s1 copied
to s12, symmetrically, everything right of s2 has it's left link to s2
copied to s12. *)
fun link_merged_syms (i1,s1) (i2,s2) (i12,s12) st = 
    st |> (AdjNds.foldall
             (update_sym (SymState.copy_right_link i1 i12))
             (SymState.get_lnds s1))
       |> (AdjNds.foldall
             (update_sym (SymState.copy_left_link i2 i12))
             (SymState.get_rnds s2));

(* assumes: s1 and s2 have are adjacent and need to gobble each other's symbols *)
fun drop_link_and_merge (is1 as (i1, s1)) (is2 as (i2, s2)) st =
    if SymState.are_adjacent s1 s2 then 
      let val s12 = (SymState.mk_merged s1 s2) 
          val (i12,st2) = st |> add_new_open_sym s12
          val st3 = st2 |> link_merged_syms is1 is2 (i12,s12)
                        |> drop_adj_link is1 is2
      in Expanded (((i1,i2),i12),st3) end
    else Pruned (st |> drop_adj_link is1 is2)

(* 
NewTop (case consider_parents_of_top is1 st 
             of NONE => Pruned (remove_sym i1 st)
              | SOME newtops => Expanded newtops)

 NewTop (case consider_parents_of_top is2 st
             of NONE => Pruned (remove_sym i2 st)
              | SOME newtops => Expanded newtops)

*)

(* assume is1 is adj-left of is2, and both are not delayed, returns states what has been done *)
fun consider (is1 as (i1, SymState.Top _)) is2 st = 
    Delayed (delay_until_left_tops is1 is2 st)
  | consider is1 (is2 as (i2, SymState.Top _)) st = 
    Delayed (delay_until_right_tops is1 is2 st)
  | consider (is1 as (_, SymState.RightEdge _)) 
             (is2 as (_, SymState.LeftEdge _)) st = 
    Delayed (ddelay is1 is2 st)
  | consider (is1 as (_, SymState.RightEdge _)) 
             (is2 as (_, SymState.RightEdge _)) st = 
    Delayed (delay_until_left_tops is1 is2 st)
  | consider (is1 as (_, SymState.RightEdge _)) 
             (is2 as (_, SymState.Middle _)) st =
    Delayed (delay_until_left_tops is1 is2 st)
  | consider (is1 as (_, SymState.LeftEdge _)) 
             (is2 as (_, SymState.LeftEdge _)) st = 
    Delayed (delay_until_right_tops is1 is2 st)
  | consider (is1 as (_, SymState.LeftEdge _)) 
             (is2 as (_, SymState.RightEdge _)) st = 
    Merged (drop_link_and_merge is1 is2 st)
  | consider (is1 as (_, SymState.LeftEdge _)) 
             (is2 as (_, SymState.Middle _)) st = 
    Merged (drop_link_and_merge is1 is2 st)
  | consider (is1 as (_, SymState.Middle _)) 
             (is2 as (_, SymState.LeftEdge _)) st = 
    Delayed (delay_until_right_tops is1 is2 st)
  | consider (is1 as (_, SymState.Middle _)) 
             (is2 as (_, SymState.RightEdge _)) st =
    Merged (drop_link_and_merge is1 is2 st)
  | consider (is1 as (_, SymState.Middle _)) 
             (is2 as (_, SymState.Middle _)) st = 
    Merged (drop_link_and_merge is1 is2 st);


exception assert_agreeing_adjs_exp of An.name * An.name * T;
fun assert_agreeing_adjs st = 
    (An.NTab.forall
         (fn (i1,s1) => 
             let val lnds1 = (SymState.get_lnds s1)
                 val rnds1 = (SymState.get_rnds s1)
                 val assert_left_ok = 
                     AdjNds.foldall
                       (fn i2 => 
                           if AdjNds.agreed_adj 
                                (i1,lnds1) (i2,SymState.get_rnds 
                                                 (get_sym st i2))
                           then I else raise assert_agreeing_adjs_exp (i1,i2,st)
                       ) lnds1 ()
                 val assert_right_ok = 
                     AdjNds.foldall
                       (fn i2 => 
                           if AdjNds.agreed_adj 
                                (i1,rnds1) (i2,SymState.get_lnds 
                                                 (get_sym st i2))
                           then I else raise assert_agreeing_adjs_exp (i1,i2,st)
                       ) rnds1 ()
             in true end)
         (get_sts st)
   ; st);

exception assert_well_formed_sym_exp of An.name * T;
fun assert_well_formed_syms st = 
    (An.NTab.forall
       (fn (i1,s1) => 
           if SymState.is_well_formed s1 then true
           else raise assert_well_formed_sym_exp (i1,st))
       (get_sts st); 
     st);

fun assert_well_formed st = 
   (assert_well_formed_syms st; assert_agreeing_adjs st; st)



exception bad_data_ctxt_exp of string;

fun pretty_symst st i = 
    SymState.pretty (get_sym st i);

val print_symst = Pretty.writeln oo pretty_symst;

fun pretty_next_open_sym st = 
    (case get_next_open_sym st of
       SOME (i,s) => 
       Pretty.block 
         [AnTab.pretty_name i, 
          Pretty.str ": ",
         SymState.pretty s]
     | NONE => Pretty.str "no more open states.");

fun pretty_next_open_sym_brief st = 
    (case get_next_open_sym st of
       SOME (i,s) => 
       Pretty.block 
         [Pretty.str "SOME (",
          AnTab.pretty_name i, 
          Pretty.str ": ",
          DnTab.pretty_name (SymState.get_parent_dname s),
          Pretty.str ")"
         ]
     | NONE => Pretty.str "NONE");


fun pretty_sym_parse st i = 
    let val s = (get_sym st i)
        val pn = SymState.get_parent_dname s;
        val elems = SymState.get_elems s;
        val undernds = SymState.get_undernds s;
    in
      Pretty.block 
      ([Dn.pretty_name pn] @  
       (if EnTab.N.is_empty elems then 
          let val l = SymState.get_maxlnd s
              val r = SymState.get_maxrnd s
          in
            [Pretty.str " @ ("] 
            @ (if AnTab.name_eq (l,r) then [AnTab.pretty_name l]
               else [AnTab.pretty_name l, Pretty.str "-", AnTab.pretty_name r])
            @ [Pretty.str ")"]
          end
        else
          [Pretty.str " = ",
           Pretty.chunks 
             (map 
                (fn e => case EnTab.lookup undernds e of 
                           NONE => Pretty.str "?"
                         | SOME i2 => pretty_sym_parse st i2)
                (EnTab.N.dest elems))])
      )
    end;

val print_sym_parse = Pretty.writeln oo pretty_sym_parse;


fun pretty_isopen_presym st i =
    (case get_next_open_id st 
      of NONE => Pretty.str "?"
       | SOME openi => 
         (if AnTab.name_eq (openi,i)
          then Pretty.str "*" else Pretty.str "-"));

fun pretty_active st = 
    Pretty.chunks
    ([Pretty.block [Pretty.str "open sts: ", An.NSet.pretty (get_opensts st)]]
     @ (map (fn (i,s) => Pretty.block [pretty_isopen_presym st i,
                                       Pretty.str "(",
                                       AnTab.pretty_name i,
                                       Pretty.str ")=",
                                       SymState.pretty s])
            (Library.sort 
               (fn ((i1,s1),(i2,s2)) => SymState.lr_order (s1,s2))
               (filter 
                  (fn (i,s) => SymState.is_alive s)
                  (AnTab.dest (get_sts st))))));
val print_active = Pretty.writeln o pretty_active;

fun pretty_full st = 
     Pretty.chunks 
     ([Pretty.block [Pretty.str "sts: ", An.NSet.pretty (get_opensts st)]]
      @ (map (fn (i,s) => Pretty.block [pretty_isopen_presym st i,
                                        Pretty.str "(",
                                        AnTab.pretty_name i,
                                        Pretty.str ")=",
                                        SymState.pretty s])
             (AnTab.dest (get_sts st))));
val print_full = Pretty.writeln o pretty_full;


fun pretty st = 
    Pretty.block 
    [ Pretty.str "ParseState{",
      Pretty.block 
        [Pretty.block [Pretty.str "Open: ", 
                       An.NSet.pretty (get_opensts st)],
         Pretty.block [Pretty.str "; Next: ",
                       pretty_next_open_sym_brief st] ],
      Pretty.str "}"] ;

val print = Pretty.writeln o pretty;


end;


structure ParseState : PARSE_STATE = DB_ParseState;

install_pp (make_pp ["ParseState", "T"] 
                    (Pretty.pprint o ParseState.pretty));
