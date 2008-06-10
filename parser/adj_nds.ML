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

  exception id_missing_exp of string * ITab.name * T

  val empty : T
  val is_empty : T -> bool
  val mk_awakened : ITab.N.T -> T

  (* has some awakened or sleeping - some possibility to merge *)
  val has_mergeable : T -> bool

  val get_awakened : T -> ITab.N.T
  val get_sleeping : T -> ITab.N.T
  val get_delayed : T -> ITab.N.T
  val get_ddelayed : T -> ITab.N.T

  val add1_awake : ITab.name -> T -> T (* simply adds 1 awake, for making bottom nodes *)

  (* these when another node named i becomes ns *)
  val subst_sleeping_to_awake : ITab.name -> ITab.N.T -> T -> T  (* fst became top snds *)
  val subst_ddelayed_to_delayed : ITab.name -> ITab.N.T -> T -> T  (* fst became top snds *)

  (* when this node becomes top do this *)
  val activate : T -> T (* delayed become active, ddelayed become sleeping *)

  (* kinds of delay/sleep *)
  val move1_to_delayed : ITab.name -> T -> T
  val move1_to_ddelayed : ITab.name -> T -> T

(* 
  val move1_ddelay_to_delayed : ITab.name -> T -> T (* they became top *)
  val move1_ddelay_to_sleeping : ITab.name -> T -> T (* we became top *)
  val move1_to_sleeping : ITab.name -> T -> T
*)


  (* Utility *)
  (* copies first name to second *)
  val copy : ITab.name -> ITab.name -> T -> T (* raises: id_missing_exp *)
  val delete1 : ITab.name -> T -> T
  val subst1_for_many : ITab.name -> ITab.N.T -> T -> T

  val subst_in_active_and_delayed : ITab.name -> ITab.N.T -> T -> T

  val fold_active_and_sleeping : (ITab.name -> 'a -> 'a) -> T -> 'a -> 'a
  val update_active_and_delayed : (ITab.N.T -> ITab.N.T) -> T -> T

  val foldall : (ITab.name -> 'a -> 'a) -> T -> 'a -> 'a
  val update_all : (ITab.N.T -> ITab.N.T) -> T -> T

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
             nds : ITab.N.T, 
             (* they will announce when they become top and tell us *)
             snds : ITab.N.T, 
             (* we announce when we becomes toped. 
                delayed until we are activated, we will add a link 
                from us to them and us to them for each parent of our top. *)
             dnds : ITab.N.T, 
             (* double-delayed: waiting for us and them; when we reach 
                top we will they every one of them to delayed for us. *)
             ddnds : ITab.N.T 
             };

(* constructors *)
val empty = Adj { nds = ITab.N.empty,
                  snds = ITab.N.empty,
                  dnds = ITab.N.empty, 
                  ddnds = ITab.N.empty };


fun is_empty (Adj rep) = 
    ITab.N.is_empty (#nds rep) andalso ITab.N.is_empty (#snds rep)
    andalso ITab.N.is_empty (#dnds rep) andalso ITab.N.is_empty (#ddnds rep);


fun mk_awakened nds = Adj { nds = nds,
                            snds = ITab.N.empty,
                            dnds = ITab.N.empty, 
                            ddnds = ITab.N.empty };

(* add 1 awake node - for making bottom nodes *)
fun add1_awake i (Adj rep) = 
      Adj {nds = ITab.N.add i (#nds rep), 
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
possibility to merge (when they become top) or when we our adjacency
is considered. Delayed and ddelayed are not consiered, because those
happend after we get top, and we only get top if we merge enough
first. *)
fun has_mergeable s = ITab.N.is_empty (get_awakened s) 
                       andalso ITab.N.is_empty (get_sleeping s);

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
    a |> ITab.N.fold f (#nds rep)
      |> ITab.N.fold f (#snds rep)
      |> ITab.N.fold f (#dnds rep)
      |> ITab.N.fold f (#ddnds rep);

fun fold_active_and_sleeping f (Adj rep) a = 
    a |> ITab.N.fold f (#nds rep)
      |> ITab.N.fold f (#snds rep);


(* implicit T *)
fun subst1_for_many oldid newids = 
    update_all (fn tb => if ITab.N.contains tb oldid then 
                           tb |> (ITab.N.delete oldid) 
                              |> (ITab.N.union_merge newids)
                         else tb);

fun subst_in_active_and_delayed oldid newids = 
    update_active_and_delayed 
      (fn tb => if ITab.N.contains tb oldid then 
                  tb |> (ITab.N.delete oldid) 
                     |> (ITab.N.union_merge newids)
                else tb);

fun delete1 id = 
    update_all (ITab.N.delete id)

exception id_missing_exp of string * ITab.name * T
(* copy i to i2 *)

fun copy i i2 (Adj rep) = 
    if ITab.N.contains (#nds rep) i  then 
      Adj {nds = ITab.N.add i2 (#nds rep),
           snds = #snds rep,
           dnds = #dnds rep,
           ddnds = #ddnds rep
          }
    else if ITab.N.contains (#snds rep) i then 
      Adj {nds = #nds rep,
           snds = ITab.N.add i2 (#snds rep),
           dnds = #dnds rep,
           ddnds = #ddnds rep
          }
    else if ITab.N.contains (#dnds rep) i then 
      Adj {nds = #nds rep,
           snds = #snds rep,
           dnds = ITab.N.add i2 (#dnds rep),
           ddnds = #ddnds rep
          }
    else if ITab.N.contains (#ddnds rep) i then 
      Adj {nds = #nds rep,
           snds = #snds rep,
           dnds = #dnds rep,
           ddnds = ITab.N.add i2 (#ddnds rep)
          }
    else raise id_missing_exp ("copy",i,(Adj rep))


(* when we become top, every ddelay adjacent node becomes delayed, our
delayed set becomes active, and our ddelay set becomes sleeping so
that when they become top they will tell us they are awakened, and
we'll both become open nodes *)
fun activate (Adj rep) = 
    Adj {nds = ITab.N.union_merge (#dnds rep) (#nds rep),
         snds = ITab.N.union_merge (#ddnds rep) (#snds rep),
         dnds = ITab.N.empty, 
         ddnds = ITab.N.empty
        };

(* i become top, so add i to the active agenda *)
fun subst_sleeping_to_awake i ns (Adj rep) = 
    Adj {nds = ITab.N.union_merge ns (#nds rep),
         snds = ITab.N.delete i (#snds rep),
         dnds = #dnds rep,
         ddnds = #ddnds rep
        };

(* i become top, so add i to the active agenda *)
fun subst_ddelayed_to_delayed i ns (Adj rep) = 
    Adj {nds = #nds rep,
         snds = #snds rep,
         dnds = ITab.N.union_merge ns (#dnds rep),
         ddnds = ITab.N.delete i (#ddnds rep)
        };


(* when we become top in the future, tell i to become open again and
consider their adjacency to us. *)
fun move1_to_delayed i (Adj rep) = 
    Adj {nds = ITab.N.delete i (#nds rep),
         snds = #snds rep,
         dnds = ITab.N.add i (#dnds rep),
         ddnds = #ddnds rep
        };
(* can't make progress with i until both them and us are top/different *)
fun move1_to_ddelayed i (Adj rep) = 
    Adj {nds = ITab.N.delete i (#nds rep),
         snds = #snds rep,
         dnds = #dnds rep,
         ddnds = ITab.N.add i (#ddnds rep)
        };



fun pretty (Adj rep) = 
    Pretty.block 
      [Pretty.str "(",
       Pretty.str "a: ", ITab.N.pretty (#nds rep),
       Pretty.str "; s: ", ITab.N.pretty (#snds rep),
       Pretty.str "; d: ", ITab.N.pretty (#dnds rep),
       Pretty.str "; dd: ", ITab.N.pretty (#ddnds rep),
       Pretty.str ")"
      ];

val print = Pretty.writeln o pretty;

end;


install_pp (make_pp ["AdjNds", "T"] 
                    (Pretty.pprint o AdjNds.pretty));




(* 


fun move1_to_sleeping i (Adj rep) = 
    Adj {nds = ITab.N.delete i (#nds rep),
         snds = ITab.N.add i (#snds rep),
         dnds = #dnds rep,
         ddnds = #ddnds rep
        };

(* not sure I need this. *)
fun move1_ddelay_to_delayed i (Adj rep) = 
    Adj {nds = #nds rep,
         snds = #snds rep,
         dnds = ITab.N.add i (#dnds rep),
         ddnds = ITab.N.delete i (#ddnds rep)
        };

(* not sure I need this. *)
fun move1_ddelay_to_sleeping i (Adj rep) = 
    Adj {nds = #nds rep,
         snds = ITab.N.add i (#snds rep),
         dnds = #dnds rep,
         ddnds = ITab.N.delete i (#ddnds rep)
        };

*)
