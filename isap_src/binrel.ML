(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  Title:      binrel.ML
    Author:     Lucas Dixon, University of Edinburgh
                lucas.dixon@ed.ac.uk
    Updated:    10 Dec 2008
    Date:       10 Dec 2008
*)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  
    Binary Relations over finite name sets 
    (objects with order and fresh construction operations)
    (e.g. two way linked dependencies on names)

    POSSIBLE IMPROVEMENT: lazy updates? 
*)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
signature BINREL 
= sig

structure Dom : NAMES (* domain elements *)
structure Cod : NAMES (* codomain elements *)
type T

val empty : T

(* add?: union, intersection, ... *)

(* get/lookup *)
val contains : T -> (Dom.name * Cod.name) -> bool
val lookup : T -> (Dom.name * Cod.name) -> bool option

(* getting related sets of names (domain and codomain functions) *)
val domf : T -> Dom.name -> Cod.NSet.T (* is empty is none *)
val codf : T -> Cod.name -> Dom.NSet.T (* is empty is none *)

(* internal; partial function, NONE when there is no such set *)
val lookup_domf : T -> Dom.name -> Cod.NSet.T option
val lookup_codf : T -> Cod.name -> Dom.NSet.T option

(* add and delete many binary relations *)
val add_many : Dom.NSet.T -> Cod.NSet.T -> T -> T
val del_many : Dom.NSet.T -> Cod.NSet.T -> T -> T

(* 1 to many relations *)
val add_to_dom : Dom.name -> Cod.NSet.T -> T -> T
val add_to_cod : Cod.name -> Dom.NSet.T -> T -> T

val del_from_dom : Dom.name -> Cod.NSet.T -> T -> T
val del_from_cod : Cod.name -> Dom.NSet.T -> T -> T

val del_dom : Dom.name -> T -> T
val del_cod : Cod.name -> T -> T

(* 1-1 relations *)
val add1 : Dom.name -> Cod.name -> T -> T
val del1 : Dom.name -> Cod.name -> T -> T

(* rename *)
val rename_dom : Dom.name -> Dom.name -> T -> T
val rename_cod : Cod.name -> Cod.name -> T -> T

(* converting to/from lists *)
val of_rawlist : (Dom.name * Cod.name) list -> T
val rawlist_of : T -> (Dom.name * Cod.name) list

val of_list : (Dom.name * Cod.NSet.T) list -> T
val list_of : T -> (Dom.name * Cod.NSet.T) list

(* get internal tables *)
val get_domset : T -> Dom.NSet.T
val get_codset : T -> Cod.NSet.T
val get_domtab : T -> Cod.NSet.T Dom.NTab.T
val get_codtab : T -> Dom.NSet.T Cod.NTab.T


(* pretty printing *)
val pretty : T -> Pretty.T
val print : T -> unit

end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
functor BinRelFun(structure Dom : NAMES and Cod : NAMES) 
: BINREL
= struct

structure Dom = Dom; (* argument 1 name type *)
structure Cod = Cod; (* argument 2 name type *)

(* datatype for meta variable environments *)
(* invarient: *)
datatype T = BinRel of {
         domf : Cod.NSet.T Dom.NTab.T,
         codf : Dom.NSet.T Cod.NTab.T
};

val empty = BinRel {domf = Dom.NTab.empty,
                    codf = Cod.NTab.empty};

fun get_domtab (BinRel rep) = #domf rep;
fun get_codtab (BinRel rep) = #codf rep;

val get_domset = Dom.NTab.get_nameset o get_domtab;
val get_codset = Cod.NTab.get_nameset o get_codtab;

fun update_domf f (BinRel rep) = 
    BinRel {domf = f (#domf rep), codf = (#codf rep)}

fun update_codf f (BinRel rep) = 
    BinRel {domf = (#domf rep), codf = f (#codf rep)}

(* implicit: rel n *)
val lookup_domf = Dom.NTab.lookup o get_domtab;
val lookup_codf = Cod.NTab.lookup o get_codtab;

fun domf rel n = 
    case lookup_domf rel n of NONE => Cod.NSet.empty | SOME x => x;

fun codf rel n = 
    case lookup_codf rel n of NONE => Dom.NSet.empty | SOME x => x;

(* given two elements check if bin-rel holds for them *)
fun contains rel (a,b) = Cod.NSet.contains (domf rel a) b;
fun lookup rel (a,b) = 
    (case lookup_domf rel a 
      of NONE => NONE
       | SOME bset => SOME (Cod.NSet.contains bset b));

(* add and remove things from the binary relation. *)
fun add_many aset bset rel = 
    rel |> update_domf 
        (fn abtab => 
            Dom.NSet.fold
              (fn a => fn abtab2 => 
                  case Dom.NTab.lookup abtab2 a of
                    NONE => Dom.NTab.ins (a,bset) abtab2
                  | SOME oldbset => Dom.NTab.update (a,Cod.NSet.union_merge bset oldbset) abtab2
              ) aset abtab)
        |> update_codf
        (fn batab => 
            Cod.NSet.fold
              (fn b => fn batab2 => 
                  case Cod.NTab.lookup batab2 b of
                    NONE => Cod.NTab.ins (b,aset) batab2
                  | SOME oldaset => Cod.NTab.update (b,Dom.NSet.union_merge aset oldaset) batab2
              ) bset batab);

fun del_many aset bset rel = 
    rel |> update_domf 
        (fn abtab => 
            Dom.NSet.fold
              (fn a => fn abtab2 => 
                  case Dom.NTab.lookup abtab2 a of
                    NONE => abtab2
                  | SOME oldbset => 
                    let val newbset = Cod.NSet.subtract oldbset bset 
                    in if Cod.NSet.is_empty newbset then Dom.NTab.delete a abtab2
                       else Dom.NTab.update (a,newbset) abtab2 end)
              aset abtab)
        |> update_codf
        (fn batab => 
            Cod.NSet.fold
              (fn b => fn batab2 => 
                  case Cod.NTab.lookup batab2 b of
                    NONE => batab2
                  | SOME oldaset => 
                    let val newaset = Dom.NSet.subtract oldaset aset 
                    in if Dom.NSet.is_empty newaset then Cod.NTab.delete b batab2
                       else Cod.NTab.update (b,newaset) batab2 end)
              bset batab);


(* 1-many relations *)
(* implicit: rel *)
fun add_to_dom a bset = add_many (Dom.NSet.single a) bset;
fun add_to_cod b aset = add_many aset (Cod.NSet.single b);
fun del_from_dom a bset = del_many (Dom.NSet.single a) bset;
fun del_from_cod b aset = del_many aset (Cod.NSet.single b);

(* 1-1 relations *)
(* implicit: rel *)
fun add1 a b = add_many  (Dom.NSet.single a) (Cod.NSet.single b);
fun del1 a b = del_many (Dom.NSet.single a) (Cod.NSet.single b);

(* convert to/from lists *)
val list_of = Dom.NTab.list_of o get_domtab;
fun of_list l = fold (fn (a,bset) => add_to_dom a bset) l empty;

val rawlist_of = maps (fn (a,bset) => map (pair a) (Cod.NSet.list_of bset))
                 o Dom.NTab.list_of o get_domtab;
fun of_rawlist l = fold (fn (a,b) => add1 a b) l empty;

(* deleting all entries *)
fun del_dom a1 rel = 
    (case Dom.NTab.lookup (get_domtab rel) a1 of 
       NONE => rel
     | SOME changedbs => 
       rel |> update_domf (Dom.NTab.delete a1)
       |> update_codf (fn batab => 
                          Cod.NSet.fold 
                            (Cod.NTab.map_entry (Dom.NSet.delete a1))
                            changedbs batab))
fun del_cod b1 rel = 
    (case Cod.NTab.lookup (get_codtab rel) b1 of 
       NONE => rel
     | SOME changedas => 
       rel |> update_codf (Cod.NTab.delete b1)
           |> update_domf (fn abtab => 
                              Dom.NSet.fold 
                                (Dom.NTab.map_entry (Cod.NSet.delete b1))
                                changedas abtab));

(* renaming *)
fun rename_dom a1 a2 rel = 
    let val changedbs = Dom.NTab.get (get_domtab rel) a1
    in rel |> update_domf (Dom.NTab.rename a1 a2)
           |> update_codf 
           (fn batab => 
               Cod.NSet.fold 
                 (Cod.NTab.map_entry (Dom.NSet.rename a1 a2))
                 changedbs batab)
    end;

fun rename_cod b1 b2 rel = 
    let val changedas = Cod.NTab.get (get_codtab rel) b1
    in rel |> update_codf (Cod.NTab.rename b1 b2)
           |> update_domf 
           (fn abtab => 
               Dom.NSet.fold 
                 (Dom.NTab.map_entry (Cod.NSet.rename b1 b2))
                 changedas abtab)
    end;

(* pretty printing *)
fun pretty rel = 
    Pretty.chunks 
    [Pretty.str "Rel{",
     Pretty.block [Pretty.str "Dom->Cod:",  
                   Dom.NTab.pretty Cod.NSet.pretty (get_domtab rel)],
     Pretty.block [Pretty.str "Cod->Dom:",  
                   Cod.NTab.pretty Dom.NSet.pretty (get_codtab rel)],
     Pretty.str "}"];
val print = Pretty.writeln o pretty;

end;


