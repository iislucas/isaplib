(* structure to manage adjacent node names: adjacent to this going to
some other node. These can be: 

nds - adj nodes needing to be checked (we are open and need to check
link to them), 

sleeping - we are a member of these modes delayed set (when they
become top, they will tell us)

delayed - delayed nodes are those that need to become open again (and
linked to us) when we reach top.

double delayed (sleeping and delayed) - they become delayed (waiting to tell us) when they
reach top. *)
signature ADJ_NDS
= sig
  type T

  exception id_missing_exp of string * AnTab.name * AnTab.name * T

  val empty : T
  val is_empty : T -> bool
  val mk_awakened : An.NSet.T -> T

  (* has some awakened or sleeping - some possibility to merge *)
  val has_mergeable : T -> bool
  
  (* have mergable links and no delayed ones *)
  val alive_and_no_stuck_links : T -> bool

  val get_awakened : T -> An.NSet.T
  val get_sleeping : T -> An.NSet.T
  val get_delayed : T -> An.NSet.T
  val get_ddelayed : T -> An.NSet.T

  val add1_awake : AnTab.name -> T -> T (* simply adds 1 awake, for making bottom nodes *)

  (* these when another node named i becomes ns *)
  val subst_sleeping_to_awake : AnTab.name -> An.NSet.T -> T -> T  (* fst became top snds *)
  val subst_ddelayed_to_delayed : AnTab.name -> An.NSet.T -> T -> T  (* fst became top snds *)

  (* when this node becomes top do this *)
  val activate : T -> T (* delayed become active, ddelayed become sleeping *)

  (* kinds of delay/sleep *)
  val move1_to_sleeping : AnTab.name -> T -> T
  val move1_to_delayed : AnTab.name -> T -> T
  val move1_to_ddelayed : AnTab.name -> T -> T

(* 
  val move1_ddelay_to_delayed : AnTab.name -> T -> T (* they became top *)
  val move1_ddelay_to_sleeping : AnTab.name -> T -> T (* we became top *)
  val move1_to_sleeping : AnTab.name -> T -> T
*)


  (* Utility *)
  (* copies first name to second *)
  val copy : AnTab.name -> AnTab.name -> T -> T (* raises: id_missing_exp *)
  val delete1 : AnTab.name -> T -> T
  val subst1_for_many : AnTab.name -> An.NSet.T -> T -> T

  val subst_in_active_and_delayed : AnTab.name -> An.NSet.T -> T -> T

  val fold_active_and_sleeping : (AnTab.name -> 'a -> 'a) -> T -> 'a -> 'a
  val update_active_and_delayed : (An.NSet.T -> An.NSet.T) -> T -> T

  val foldall : (AnTab.name -> 'a -> 'a) -> T -> 'a -> 'a
  val update_all : (An.NSet.T -> An.NSet.T) -> T -> T

  (* relative correctness *)
  val agreed_adj : (An.name * T) -> (An.name * T) -> bool

  (* pretty printing *)
  val pretty : T -> Pretty.T
  val print : T -> unit
end;

structure AdjNds
: ADJ_NDS
= struct

(* datatype to hold adjacent node links: these can be needing to be
checked, waiting to be checked after we move up, and waiting to be
checked until both us and the adjavent nodes moves up. In this sense
AdjNds represent the agenda of parsing still needing to be
considered. When we consider a pair of adjacent nodes we remove the
adj links. 

invarient: nds, dnds, ddnds are disjoint. 
*)
datatype T = Adj of {
             (* nodes which should be checked now: current active agenda *)
             nds : An.NSet.T, 
             (* they will announce when they become top; and then link to us *)
             snds : An.NSet.T, 
             (* we link to these when we becomes toped. 
                they are delayed until we are activated, we will add a link 
                from us to them and us to them for each parent of our top. *)
             dnds : An.NSet.T, 
             (* double-delayed: waiting for us and them; when we reach 
                top we will change every one of them to be delayed for us. *)
             ddnds : An.NSet.T 
             };

(* constructors *)
val empty = Adj { nds = An.NSet.empty,
                  snds = An.NSet.empty,
                  dnds = An.NSet.empty, 
                  ddnds = An.NSet.empty };


fun is_empty (Adj rep) = 
    An.NSet.is_empty (#nds rep) andalso An.NSet.is_empty (#snds rep)
    andalso An.NSet.is_empty (#dnds rep) andalso An.NSet.is_empty (#ddnds rep);


fun mk_awakened nds = Adj { nds = nds,
                            snds = An.NSet.empty,
                            dnds = An.NSet.empty, 
                            ddnds = An.NSet.empty };

(* add 1 awake node - for making bottom nodes *)
fun add1_awake i (Adj rep) = 
      Adj {nds = An.NSet.add i (#nds rep), 
           snds = (#snds rep),
           dnds = (#dnds rep),
           ddnds = (#ddnds rep)        
          };

(* get functions *)
fun get_awakened (Adj rep) = #nds rep;
fun get_sleeping (Adj rep) = #snds rep;
fun get_delayed (Adj rep) = #dnds rep;
fun get_ddelayed (Adj rep) = #ddnds rep;


(* if there are some awakened or sleeping, then we have some
possibility to merge (when they become top) or when our adjacency
is considered. Delayed and ddelayed are not considered, because those
happend after we get top, and we only get top if we merge enough
first. *)
fun has_mergeable s = An.NSet.is_empty (get_awakened s) 
                      andalso An.NSet.is_empty (get_sleeping s);

(* has progress links and no delayed links *)
fun alive_and_no_stuck_links nds = 
    has_mergeable nds 
    andalso An.NSet.is_empty (get_delayed nds)
    andalso An.NSet.is_empty (get_ddelayed nds);
    
    
(* utility functions *)
fun update_all updf (Adj rep) = 
      Adj {nds = updf (#nds rep), 
           snds = updf (#snds rep),
           dnds = updf (#dnds rep),
           ddnds = updf (#ddnds rep)        
          };

fun update_active_and_delayed updf (Adj rep) = 
      Adj {nds = updf (#nds rep), 
           snds = #snds rep,
           dnds = updf (#dnds rep),
           ddnds = #ddnds rep      
          };

fun foldall f (Adj rep) a = 
    a |> An.NSet.fold f (#nds rep)
      |> An.NSet.fold f (#snds rep)
      |> An.NSet.fold f (#dnds rep)
      |> An.NSet.fold f (#ddnds rep);

fun fold_active_and_sleeping f (Adj rep) a = 
    a |> An.NSet.fold f (#nds rep)
      |> An.NSet.fold f (#snds rep);


(* implicit T *)
fun subst1_for_many oldid newids = 
    update_all (fn tb => if An.NSet.contains tb oldid then 
                           tb |> (An.NSet.delete oldid) 
                              |> (An.NSet.union_merge newids)
                         else tb);

fun subst_in_active_and_delayed oldid newids = 
    update_active_and_delayed 
      (fn tb => if An.NSet.contains tb oldid then 
                  tb |> (An.NSet.delete oldid) 
                     |> (An.NSet.union_merge newids)
                else tb);

fun delete1 id = 
    update_all (An.NSet.delete id)

exception id_missing_exp of string * AnTab.name * AnTab.name * T

(* copy i to i2; assumes i is somewhere in the adj table, else exception. 
   Adds i2 with the same adjacency relationship as i. *)
fun copy i i2 (Adj rep) = 
    if An.NSet.contains (#nds rep) i  then 
      Adj {nds = An.NSet.add i2 (#nds rep),
           snds = #snds rep,
           dnds = #dnds rep,
           ddnds = #ddnds rep
          }
    else if An.NSet.contains (#snds rep) i then 
      Adj {nds = #nds rep,
           snds = An.NSet.add i2 (#snds rep),
           dnds = #dnds rep,
           ddnds = #ddnds rep
          }
    else if An.NSet.contains (#dnds rep) i then 
      Adj {nds = #nds rep,
           snds = #snds rep,
           dnds = An.NSet.add i2 (#dnds rep),
           ddnds = #ddnds rep
          }
    else if An.NSet.contains (#ddnds rep) i then 
      Adj {nds = #nds rep,
           snds = #snds rep,
           dnds = #dnds rep,
           ddnds = An.NSet.add i2 (#ddnds rep)
          }
    else raise id_missing_exp ("copy",i,i2,(Adj rep))



fun get_awakened (Adj rep) = #nds rep;
fun get_sleeping (Adj rep) = #snds rep;
fun get_delayed (Adj rep) = #dnds rep;
fun get_ddelayed (Adj rep) = #ddnds rep;

(* i and i2 agree they are adjacent in the same way.
   Given the adjacency information. *)
fun agreed_adj (i1,a1) (i2,a2) = 
    (An.NSet.contains (get_awakened a1) i2 andalso  
     An.NSet.contains (get_awakened a2) i1) orelse
    (An.NSet.contains (get_sleeping a1) i2 andalso  
     An.NSet.contains (get_delayed a2) i1) orelse
    (An.NSet.contains (get_delayed a1) i2 andalso  
     An.NSet.contains (get_sleeping a2) i1) orelse
    (An.NSet.contains (get_ddelayed a1) i2 andalso  
     An.NSet.contains (get_ddelayed a2) i1);


(* when we become top, every ddelay adjacent node becomes delayed, our
delayed set becomes active, and our ddelay set becomes sleeping so
that when they become top they will tell us they are awakened, and
we'll both become open nodes *)
fun activate (Adj rep) = 
    Adj {nds = An.NSet.union_merge (#dnds rep) (#nds rep),
         snds = An.NSet.union_merge (#ddnds rep) (#snds rep),
         dnds = An.NSet.empty, 
         ddnds = An.NSet.empty
        };

(* i become top, so add i to the active agenda *)
fun subst_sleeping_to_awake i ns (Adj rep) = 
    Adj {nds = An.NSet.union_merge ns (#nds rep),
         snds = An.NSet.delete i (#snds rep),
         dnds = #dnds rep,
         ddnds = #ddnds rep
        };

(* i become top, so add i to the active agenda *)
fun subst_ddelayed_to_delayed i ns (Adj rep) = 
    Adj {nds = #nds rep,
         snds = #snds rep,
         dnds = An.NSet.union_merge ns (#dnds rep),
         ddnds = An.NSet.delete i (#ddnds rep)
        };


(* when we become top in the future, tell i to become open again and
consider their adjacency to us. *)
fun move1_to_delayed i (Adj rep) = 
    Adj {nds = An.NSet.delete i (#nds rep),
         snds = #snds rep,
         dnds = An.NSet.add i (#dnds rep),
         ddnds = #ddnds rep
        };
(* when they become top in the future, they will tell us to become active again. *)
fun move1_to_sleeping i (Adj rep) = 
    Adj {nds = An.NSet.delete i (#nds rep),
         snds = An.NSet.add i (#snds rep),
         dnds = #dnds rep,
         ddnds = #ddnds rep
        };
(* can't make progress with i until both them and us are top/different *)
fun move1_to_ddelayed i (Adj rep) = 
    Adj {nds = An.NSet.delete i (#nds rep),
         snds = #snds rep,
         dnds = #dnds rep,
         ddnds = An.NSet.add i (#ddnds rep)
        };



fun pretty (Adj rep) = 
    Pretty.block 
      [Pretty.str "(",
       Pretty.str "a: ", An.NSet.pretty (#nds rep),
       Pretty.str "; s: ", An.NSet.pretty (#snds rep),
       Pretty.str "; d: ", An.NSet.pretty (#dnds rep),
       Pretty.str "; dd: ", An.NSet.pretty (#ddnds rep),
       Pretty.str ")"
      ];

val print = Pretty.writeln o pretty;

end;


(* install_pp (make_pp ["AdjNds", "T"] 
                    (Pretty.pprint o AdjNds.pretty));
*)



(* 
(* not sure I need this. *)
fun move1_ddelay_to_delayed i (Adj rep) = 
    Adj {nds = #nds rep,
         snds = #snds rep,
         dnds = An.NSet.add i (#dnds rep),
         ddnds = An.NSet.delete i (#ddnds rep)
        };

(* not sure I need this. *)
fun move1_ddelay_to_sleeping i (Adj rep) = 
    Adj {nds = #nds rep,
         snds = An.NSet.add i (#snds rep),
         dnds = #dnds rep,
         ddnds = An.NSet.delete i (#ddnds rep)
        };

*)
