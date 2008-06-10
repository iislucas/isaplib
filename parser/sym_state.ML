(* the status of a symbol (including derived symbols) during parsing;
holds the agenda of adjacent nodes needing to be checked as well as
nodes which form this symbol. *)
signature SYM_STATE
= sig

  type intype
  type endstatus
  type midstatus

  datatype T =
           LeftEdge of endstatus (* no more stuff on left to be parsed *) 
         | Middle of midstatus (* more stuff to be parsed on left and right *)
         | RightEdge of endstatus  (* no more stuff on right to be parsed *) 
         | Top of intype (* finished parsing this symbol successfully *)

  exception bad_data_ctxt_exp of string
  exception bad_symstate of string * T
  exception nothing_under_intype_exp of string
  exception not_mergeable_exp of T * T;

  val mk_leaf : DnTab.name * ITab.name * AdjNds.T * AdjNds.T -> T
  val mk_parent : Data.dcontext -> ITab.name * intype -> Data.andpos -> T
  val mk_merged : T -> T -> T

  val is_top : T -> bool;
  val is_alive : T -> bool; 

  val are_adjacent : T -> T -> bool
  val have_same_parent : T -> T -> bool
  val needs_more_left : T -> bool
  val needs_more_right : T -> bool

  (* nothing ever mergeable *)
  val empty_left_agenda : T -> bool
  val empty_right_agenda : T -> bool;

  (* for consturcting the bottom nds, simply adds node to right agenda *)
  val add1_awakened_right : ITab.name -> T -> T

  (* agenda management *)
  val subst_left_sleeping_to_awake : ITab.name -> ITab.N.T -> T -> T
  val subst_right_sleeping_to_awake : ITab.name -> ITab.N.T -> T -> T

  val subst_left_ddelay_to_delayed : ITab.name -> ITab.N.T -> T -> T
  val subst_right_ddelay_to_delayed : ITab.name -> ITab.N.T -> T -> T

  (* delays *)
  val move1_left_to_delayed : ITab.name -> T -> T
  val move1_right_to_delayed : ITab.name -> T -> T
  val move1_left_to_ddelayed : ITab.name -> T -> T
  val move1_right_to_ddelayed : ITab.name -> T -> T


(*
  val move1_left_ddelay_to_sleeping : ITab.name -> T -> T
  val move1_right_ddelay_to_sleeping : ITab.name -> T -> T

  val move1_left_to_sleeping : ITab.name -> T -> T
  val move1_right_to_sleeping : ITab.name -> T -> T

*)
  val copy_right_link : ITab.name -> ITab.name -> T -> T
  val copy_left_link : ITab.name -> ITab.name -> T -> T

  val drop_all_links : T -> T

  val lr_order : T * T -> General.order

  (* gets *)
  val get_left_awakened : T -> ITab.N.T
  val get_right_awakened : T -> ITab.N.T
  val get_left_delayed : T -> ITab.N.T
  val get_right_delayed : T -> ITab.N.T
  val get_left_sleeping : T -> ITab.N.T
  val get_right_sleeping : T -> ITab.N.T
  val get_left_ddelayed : T -> ITab.N.T
  val get_right_ddelayed : T -> ITab.N.T

  val get_maxlnd : T -> ITab.name
  val get_maxrnd : T -> ITab.name

  val get_max_left_elem : T -> EnTab.name
  val get_max_right_elem : T -> EnTab.name
  val get_next_lelem : T -> EnTab.name * DnTab.name
  val get_next_relem : T -> EnTab.name * DnTab.name

  (* internal manipulation stuff *)
  val get_elems : T -> EnTab.N.T
  val get_parent_dname : T -> DnTab.name
  val get_lnds : T -> AdjNds.T
  val get_rnds : T -> AdjNds.T
  val get_undernds : T -> ITab.name UnderData.EnTab.T

  val update_elems : (EnTab.N.T -> EnTab.N.T) -> T -> T
  val update_parent_dname : (DnTab.name -> DnTab.name) -> T -> T
  val update_lnds : (AdjNds.T -> AdjNds.T) -> T -> T
  val update_rnds : (AdjNds.T -> AdjNds.T) -> T -> T
  val update_undernds :  (ITab.name EnTab.T -> ITab.name EnTab.T) -> T -> T

  val set_elems : EnTab.N.T -> T -> T
  val set_parent_dname : DnTab.name -> T -> T
  val set_lnds : AdjNds.T -> T -> T
  val set_rnds : AdjNds.T -> T -> T
  val set_undernds : ITab.name EnTab.T -> T -> T

  (* pretty printing *)
  val pretty : T -> Pretty.T
  val print : T -> unit
end;
  
(* state of a collection of adjacent symbols in the parser. We store
adjacent adjavent node info using AdjNodes for nodes to the left and
right of us. *)
structure DB_SymState
= struct

structure SnTab :> FNAME_TAB where type name = string = StrNameTab;


(* intype hold infor for being inside a type - consturcting it from
   bottom up, uding parsing. *)
datatype intype = InAndType of 
         { parent : UnderData.dname, (* parent an and-type being made *)
           elems: UnderData.EnTab.N.T, (* all and-elems *)
           undernds : ITab.name UnderData.EnTab.T, (* link to known and-elems *)
           lnds : AdjNds.T, (* or-choices of left node ids *)
           rnds : AdjNds.T, (* or-choics of right node ids *)
           maxlnd : ITab.name, (* most-left id on bottom level *)
           maxrnd : ITab.name (* most-right id on bottom level *)
         };

(* status at left or right end of a dname, more stuff needed only on 1 side *)
datatype endstatus = EndStatus of
         { intype : intype,
           nextelem : (UnderData.ename * UnderData.dname)
         };
         
(* status when we have more elems needed for the dname to the right and left *) 
datatype midstatus = MidStatus of
         { intype : intype,
           nextlelem : (UnderData.ename * UnderData.dname),
           nextrelem : (UnderData.ename  * UnderData.dname)
         };

(* state of a collection of adjacent symbols *)
datatype T = 
  LeftEdge of endstatus (* at left of a dname *)
| RightEdge of endstatus (* on right of a dname *)
| Middle of midstatus
| Top of intype;

fun is_top (Top _) = true
  | is_top _ = false;

(* make a node which has no children *)
fun mk_leaf (dname, thisnd, left, right) = 
    Top (InAndType {parent = dname, 
                    elems = UnderData.EnTab.N.empty,
                    undernds = UnderData.EnTab.empty,
                    lnds = left, 
                    rnds = right,
                    maxlnd = thisnd,
                    maxrnd = thisnd
                    });

fun intype_of_endstatus (EndStatus rep) = #intype rep;
fun nextelem_of_endstatus (EndStatus rep) = #nextelem rep;
fun upd_intype_of_endstatus f (EndStatus rep) = 
    EndStatus{ intype = f (#intype rep), nextelem = (#nextelem rep)};
fun upd_nextelem_of_endstatus f (EndStatus rep) = 
    EndStatus{ intype = (#intype rep), nextelem = f (#nextelem rep)};

fun intype_of_midstatus (MidStatus rep) = #intype rep;
fun nextlelem_of_midstatus (MidStatus rep) = #nextlelem rep;
fun nextrelem_of_midstatus (MidStatus rep) = #nextrelem rep;
fun upd_intype_of_midstatus f (MidStatus rep) = 
    MidStatus {intype = f (#intype rep), 
               nextlelem = (#nextlelem rep), 
               nextrelem = (#nextrelem rep)};
fun upd_nextlelem_of_midstatus f (MidStatus rep) = 
    MidStatus {intype = (#intype rep), 
               nextlelem = f (#nextlelem rep), 
               nextrelem = (#nextrelem rep)};
fun upd_nextrelem_of_midstatus f (MidStatus rep) = 
    MidStatus {intype = (#intype rep), 
               nextlelem = (#nextlelem rep), 
               nextrelem = f (#nextrelem rep) };

fun lnds_of_intype (InAndType rep) = #lnds rep;
fun rnds_of_intype (InAndType rep) = #rnds rep;
fun parent_of_intype (InAndType rep) = #parent rep;
fun elems_of_intype (InAndType rep) = #elems rep;
fun undernds_of_intype (InAndType rep) = #undernds rep;
fun maxlnd_of_intype (InAndType rep) = #maxlnd rep;
fun maxrnd_of_intype (InAndType rep) = #maxrnd rep;

fun upd_lnds_of_intype f (InAndType rep) = 
    InAndType{ lnds = f (#lnds rep), rnds = #rnds rep, parent = #parent rep,
               elems = #elems rep, 
               undernds = #undernds rep,
               maxlnd = #maxlnd rep, maxrnd = #maxrnd rep
             };
fun upd_rnds_of_intype f (InAndType rep) = 
    InAndType{ lnds = #lnds rep, rnds = f (#rnds rep), parent = #parent rep, 
               elems = #elems rep, 
               undernds = #undernds rep,
               maxlnd = #maxlnd rep, maxrnd = #maxrnd rep
             };
fun upd_parent_of_intype f (InAndType rep) = 
    InAndType{ lnds = #lnds rep, rnds = #rnds rep, parent = #parent rep,
               elems = #elems rep, 
               undernds = #undernds rep,
               maxlnd = #maxlnd rep, maxrnd = #maxrnd rep
             };
fun upd_elems_of_intype f (InAndType rep) = 
    InAndType{ lnds = #lnds rep, rnds = #rnds rep, parent = #parent rep,
               elems = f (#elems rep), 
               undernds = #undernds rep,
               maxlnd = #maxlnd rep, maxrnd = #maxrnd rep
             };

fun upd_undernds_of_intype f (InAndType rep) = 
    InAndType{ lnds = #lnds rep, rnds = #rnds rep, parent = #parent rep,
               elems = #elems rep, 
               undernds = f (#undernds rep),
               maxlnd = #maxlnd rep, maxrnd = #maxrnd rep
             };

fun get_intype (LeftEdge endstatus) = intype_of_endstatus endstatus
  | get_intype (RightEdge endstatus) = intype_of_endstatus endstatus
  | get_intype (Middle midstatus) = intype_of_midstatus midstatus
  | get_intype (Top intype) = intype;

fun update_intype f (LeftEdge endstatus) = 
    LeftEdge (upd_intype_of_endstatus f endstatus)
  | update_intype f (RightEdge endstatus) = 
    RightEdge (upd_intype_of_endstatus f endstatus)
  | update_intype f (Middle midstatus) = 
    Middle (upd_intype_of_midstatus f midstatus)
  | update_intype f (Top intype) = 
    Top (f intype);

fun needs_more_left (RightEdge _) = true
  | needs_more_left (Middle _) = true
  | needs_more_left _ = false;

fun needs_more_right (LeftEdge _) = true
  | needs_more_right (Middle _) = true
  | needs_more_right _ = false;

(* implicit s *)
val get_lnds = lnds_of_intype o get_intype;
val get_rnds = rnds_of_intype o get_intype;
val get_elems = elems_of_intype o get_intype;
val get_undernds = undernds_of_intype o get_intype;
val get_parent_dname = parent_of_intype o get_intype;
val get_maxlnd = maxlnd_of_intype o get_intype;
val get_maxrnd = maxrnd_of_intype o get_intype;

(* left-to-right order: left-most first, then biggest *)
fun lr_order (s1,s2) = 
    case ITab.ord (get_maxlnd s1, get_maxlnd s2)
     of LESS => LESS
      | MORE => MORE
      | EQUAL => ITab.ord (get_maxrnd s2, get_maxrnd s1);

(* implicit f s *)
val update_lnds = update_intype o upd_lnds_of_intype;
val update_rnds = update_intype o upd_rnds_of_intype;
val update_elems = update_intype o upd_elems_of_intype;
val update_undernds = update_intype o upd_undernds_of_intype;
val update_parent_dname : (DnTab.name -> DnTab.name) -> T -> T = 
    update_intype o upd_parent_of_intype;

(* implicit v s *)
val set_lnds = update_lnds o K;
val set_rnds = update_rnds o K;
val set_elems = update_elems o K;
val set_undernds = update_undernds o K;
val set_parent_dname = update_parent_dname o K;

(* check if the left/right agenda is empty (thing you can merge with in the future) *)
val empty_left_agenda = AdjNds.has_mergeable o get_lnds;
val empty_right_agenda = AdjNds.has_mergeable o get_rnds;

(* fun is_alive s = not (empty_left_agenda s andalso empty_right_agenda s); *)
fun is_alive s = not (AdjNds.is_empty (get_lnds s) andalso AdjNds.is_empty (get_rnds s));


(* implicit intype *)
exception nothing_under_intype_exp of string;

val get_max_right_elem_of_intype = 
    (fn NONE => raise nothing_under_intype_exp "get_max_right_elem_of_intype" 
      | SOME x => x) 
    o UnderData.EnTab.N.get_last o UnderData.EnTab.get_names 
    o undernds_of_intype;
val get_max_left_elem_of_intype = 
    (fn NONE => raise nothing_under_intype_exp "get_max_left_elem_of_intype" 
      | SOME x => x) 
    o UnderData.EnTab.N.get_first o UnderData.EnTab.get_names 
    o undernds_of_intype;

(* implicit s *)
val get_max_right_elem = get_max_right_elem_of_intype o get_intype;
val get_max_left_elem = get_max_left_elem_of_intype o get_intype;


(* get nextr and nextl elemetns, raising exceptions for symstates *)
exception bad_symstate of string * T;
fun get_next_lelem (s as Top _) = raise bad_symstate ("next_lelem", s)
  | get_next_lelem (s as Middle midstatus) = nextlelem_of_midstatus midstatus
  | get_next_lelem (s as LeftEdge _) = raise bad_symstate ("next_lelem", s)
  | get_next_lelem (s as RightEdge endstatus) = nextelem_of_endstatus endstatus;

fun get_next_relem (s as Top _) = raise bad_symstate ("next_relem", s)
  | get_next_relem (s as Middle midstatus) = nextrelem_of_midstatus midstatus
  | get_next_relem (s as LeftEdge endstatus) = nextelem_of_endstatus endstatus
  | get_next_relem (s as RightEdge _) = raise bad_symstate ("next_relem", s);


(* Are two symstates left-to-right adjacent *)
fun have_same_parent s1 s2 = 
    UnderData.dname_eq (get_parent_dname s1) (get_parent_dname s2);

fun are_adjacent (Top _) s2 = false
  | are_adjacent _ (Top _) = false
  | are_adjacent (RightEdge _) _ = false
  | are_adjacent _ (LeftEdge _) = false
  | are_adjacent s1 s2 = 
    (have_same_parent s1 s2)
    andalso 
    (UnderData.ename_eq (fst (get_next_relem s1)) 
                        (get_max_left_elem s2));

(* for making the bottom nodes *)
val add1_awakened_right = 
    update_intype o upd_rnds_of_intype o AdjNds.add1_awake;

(* to be done when an adj node becomes top *)
(* implicit i ns s *)
val subst_left_sleeping_to_awake = 
    update_intype o upd_lnds_of_intype oo AdjNds.subst_sleeping_to_awake;
val subst_right_sleeping_to_awake = 
    update_intype o upd_rnds_of_intype oo AdjNds.subst_sleeping_to_awake;

val subst_left_ddelay_to_delayed = 
    update_intype o upd_lnds_of_intype oo AdjNds.subst_ddelayed_to_delayed;
val subst_right_ddelay_to_delayed = 
    update_intype o upd_rnds_of_intype oo AdjNds.subst_ddelayed_to_delayed;

(* delays *)
val move1_left_to_delayed = 
    update_intype o upd_lnds_of_intype o AdjNds.move1_to_delayed;
val move1_right_to_delayed = 
    update_intype o upd_rnds_of_intype o AdjNds.move1_to_delayed;
val move1_left_to_ddelayed = 
    update_intype o upd_lnds_of_intype o AdjNds.move1_to_ddelayed;
val move1_right_to_ddelayed = 
    update_intype o upd_rnds_of_intype o AdjNds.move1_to_ddelayed;



(* not sure when these happen ... maybe can be pruned? *)
(* 
val move1_left_ddelay_to_sleeping = 
    update_intype o upd_lnds_of_intype o AdjNds.move1_ddelay_to_sleeping;
val move1_right_ddelay_to_sleeping = 
    update_intype o upd_rnds_of_intype o AdjNds.move1_ddelay_to_sleeping;

val move1_left_to_sleeping = 
    update_intype o upd_lnds_of_intype o AdjNds.move1_to_sleeping;
val move1_right_to_sleeping = 
    update_intype o upd_rnds_of_intype o AdjNds.move1_to_sleeping;
*)


(* gets *)
val get_left_awakened = 
    AdjNds.get_awakened o get_lnds;
val get_right_awakened = 
    AdjNds.get_awakened o get_rnds;

val get_left_sleeping = 
    AdjNds.get_sleeping o get_lnds;
val get_right_sleeping = 
    AdjNds.get_sleeping o get_rnds;

val get_left_delayed = 
    AdjNds.get_delayed o get_lnds;
val get_right_delayed = 
    AdjNds.get_delayed o get_rnds;

val get_left_ddelayed = 
    AdjNds.get_ddelayed o get_lnds;
val get_right_ddelayed = 
    AdjNds.get_ddelayed o get_rnds;


val copy_right_link = update_rnds oo AdjNds.copy;
val copy_left_link = update_lnds oo AdjNds.copy;

val drop_all_links = set_rnds AdjNds.empty o set_lnds AdjNds.empty;

exception bad_data_ctxt_exp of string;

(* make a new symstate node from a position in an and-type and an 
   old InAndType data *)
fun mk_parent dcx (sid, InAndType rep) andp = 
    let
      val new_parent_dname = Data.parentn_of_andpos andp
      val sym_ename = Data.field_of_andpos andp
      val parent_dtyp = Data.get_datatyp dcx new_parent_dname 
    in
      case parent_dtyp of 
        Data.Ands etab => 
        let 
          val parent_inandtype = 
              InAndType {parent=new_parent_dname,
                         elems = UnderData.EnTab.get_names etab,
                         undernds = UnderData.EnTab.update 
                                      (sym_ename,sid)
                                      UnderData.EnTab.empty,
                         lnds = AdjNds.activate (#lnds rep),
                         rnds = AdjNds.activate (#rnds rep),
                         maxlnd = #maxlnd rep,
                         maxrnd = #maxrnd rep
                        };
        in
          (case (UnderData.EnTab.next_smaller etab sym_ename, 
								 UnderData.EnTab.next_bigger etab sym_ename) 
            of (SOME (l as (le,ldn)), SOME (r as (re,rdn))) => 
               Middle (MidStatus 
												 { intype = parent_inandtype,
													 nextlelem = l,
													 nextrelem = r })
             | (NONE, SOME (next as (le,ldn))) => 
               LeftEdge (EndStatus 
														{ intype = parent_inandtype,
															nextelem = next })
             | (SOME (next as (le,ldn)), NONE) => 
               RightEdge (EndStatus 
														{ intype = parent_inandtype,
															nextelem = next })
             (* a type definition: maybe useful for distinguishing
                multiple occurances of similar types *)
             | (NONE, NONE) => 
               Top parent_inandtype)
				end
      | _ => raise bad_data_ctxt_exp "parent of andpos should be an and-type."
    end;

(* created a merge intype, assumes s1 and s2 are adjacent (thus disjoint w.r.t. undernds). *)
fun mk_merged_intype s1 s2 =
    InAndType {parent = get_parent_dname s1,
               elems = get_elems s1,
               undernds = EnTab.merge_disjoint 
                            (get_undernds s1) (get_undernds s2),
               lnds = get_lnds s1,
               rnds = get_rnds s2,
               maxlnd = get_maxlnd s1,
               maxrnd = get_maxrnd s2
              };

exception not_mergeable_exp of T * T;

(* assumes s1 is adj-left of s2 are adjacent, returns merged symstate *)
fun mk_merged (s1 as Top _) s2 = raise not_mergeable_exp (s1, s2)
  | mk_merged s2 (s1 as Top _) = raise not_mergeable_exp (s1, s2)
  | mk_merged (s1 as RightEdge _) s2 = raise not_mergeable_exp (s1, s2)
  | mk_merged s1 (s2 as LeftEdge _) = raise not_mergeable_exp (s1, s2)
  | mk_merged (s1 as Middle midst) (s2 as RightEdge _) = 
    RightEdge (EndStatus { intype = mk_merged_intype s1 s2,
                           nextelem = nextlelem_of_midstatus midst })
  | mk_merged (s1 as Middle midst1) (s2 as Middle midst2) = 
    Middle (MidStatus
              { intype = mk_merged_intype s1 s2,
                nextlelem = nextlelem_of_midstatus midst1,
                nextrelem = nextrelem_of_midstatus midst2 })
  | mk_merged (s1 as LeftEdge _) (s2 as Middle midst) = 
     LeftEdge (EndStatus
                 { intype = mk_merged_intype s1 s2,
                   nextelem = nextrelem_of_midstatus midst })
  | mk_merged (s1 as LeftEdge _) (s2 as RightEdge _) = 
    Top (mk_merged_intype s1 s2);


(* pretty printing *)
fun pretty_intype (InAndType rep) = 
    Pretty.chunks
      [Pretty.block 
         [Pretty.block 
            ([Pretty.str "@("]
             @ (if ITab.name_eq (#maxlnd rep, #maxrnd rep) then [ITab.pretty_name (#maxlnd rep)]
                else [ITab.pretty_name (#maxlnd rep), Pretty.str "-", 
                      ITab.pretty_name (#maxrnd rep)])
             @ [Pretty.str ")"]),
          Pretty.str " : ", UnderData.pretty_dname (#parent rep), Pretty.str " = ",
          Pretty.list "[" "]" 
            (map 
               (fn e => 
                    Pretty.block 
                      [EnTab.pretty_name e, Pretty.str "=",
                       case EnTab.lookup (#undernds rep) e of 
                         NONE => Pretty.str "?"
                       | SOME i => ITab.pretty_name i])
               (EnTab.N.dest (#elems rep)))],
       Pretty.block [Pretty.str "ls: ", AdjNds.pretty (#lnds rep)],
       Pretty.block [Pretty.str "rs: ", AdjNds.pretty (#rnds rep)]
      ];

(* pretty printing *)
fun pretty_intype_full (InAndType rep) = 
    Pretty.chunks
      [Pretty.block [Pretty.str "Left nds: ", AdjNds.pretty (#lnds rep)],
       Pretty.block [Pretty.str "Right nds: ", AdjNds.pretty (#rnds rep)],
       Pretty.block [Pretty.str "MostLeft base nd: ",  
                     ITab.pretty_name (#maxlnd rep)],
       Pretty.block [Pretty.str "MostRight base nd: ",  
                     ITab.pretty_name (#maxrnd rep)],
       Pretty.block [Pretty.str "Parent dname: ",  
                     UnderData.pretty_dname (#parent rep)],
       Pretty.block [Pretty.str "All children elems: ", 
                     Pretty.list "[" "]" 
                     (map UnderData.pretty_ename (UnderData.EnTab.N.dest 
                                                    (#elems rep)))],
       Pretty.block [Pretty.str "Known children nds: ", 
                     Pretty.list "[" "]" 
                                 (map (fn (e,i) => 
                                          Pretty.block 
                                            [(UnderData.pretty_ename e),
                                             Pretty.str ": ",
                                             (ITab.pretty_name i)])
                                      (UnderData.EnTab.dest (#undernds rep)))]
      ];



fun pretty_endstatus (EndStatus rep) = 
    Pretty.chunks [
      Pretty.block [pretty_intype (#intype rep)],
      Pretty.block [Pretty.str "Next: ", 
                    UnderData.pretty_ename (fst (#nextelem rep)),
										Pretty.str " -> ",
										UnderData.pretty_dname (snd (#nextelem rep))
									 ]
    ];

fun pretty_midstatus (MidStatus rep) = 
    Pretty.chunks [
    Pretty.block [pretty_intype (#intype rep)],
    Pretty.block [Pretty.str "Next Left: ", 
                  UnderData.pretty_ename (fst (#nextlelem rep)),
									Pretty.str " -> ",
									UnderData.pretty_dname (snd (#nextlelem rep)),
								  Pretty.str ";   Next Right: ", 
                  UnderData.pretty_ename (fst (#nextrelem rep)),
									Pretty.str " -> ",
									UnderData.pretty_dname (snd (#nextrelem rep))
								 ]
    ];

fun pretty (LeftEdge endstatus) = 
    Pretty.block [Pretty.str "L: ", (pretty_endstatus endstatus)]
  | pretty (RightEdge endstatus) = 
    Pretty.block [Pretty.str "R: ", (pretty_endstatus endstatus)]
  | pretty (Middle midstatus) = 
    Pretty.block [Pretty.str "M: ", (pretty_midstatus midstatus)]
  | pretty (Top intype) = 
    Pretty.block [Pretty.str "T: ", (pretty_intype intype)];

val print = Pretty.writeln o pretty;

end;

structure SymState : SYM_STATE = DB_SymState;
