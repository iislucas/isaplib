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
    (objects with ordered and fresh construction operations)
    e.g. two way linked dependencies on names

    POSSIBLE IMPROVEMENT: lazy updates? 
*)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
signature BINREL 
= sig

structure A : NAMES (* argument 1 name type *)
structure B : NAMES (* argument 2 name type *)
type T

val empty : T

(* getting related sets of names *)
val get_arels : T -> A.name -> B.NSet.T (* is empty is none *)
val get_brels : T -> B.name -> A.NSet.T (* is empty is none *)

(* add and delete many binary relations *)
val add_many : A.NSet.T -> B.NSet.T -> T -> T
val del_many : A.NSet.T -> B.NSet.T -> T -> T

(* 1 to many relations *)
val add_toA : A.name -> B.NSet.T -> T -> T
val add_toB : B.name -> A.NSet.T -> T -> T

val del_fromA : A.name -> B.NSet.T -> T -> T
val del_fromB : B.name -> A.NSet.T -> T -> T

(* 1-1 relations *)
val add1_to : A.name -> B.name -> T -> T
val del1_from : A.name -> B.name -> T -> T

(* rename *)
val renameA : A.name -> A.name -> T -> T
val renameB : B.name -> B.name -> T -> T

(* pretty printing *)
val pretty : T -> Pretty.T
val print : T -> unit

(* internal: is NONE if no entry *)
val lookup_arels : T -> A.name -> B.NSet.T option
val lookup_brels : T -> B.name -> A.NSet.T option

end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
functor BinRelFun(structure A : NAMES and B : NAMES) 
: BINREL
= struct

structure A = A; (* argument 1 name type *)
structure B = B; (* argument 2 name type *)

(* datatype for meta variable environments *)
(* invarient: *)
datatype T = BinRel of {
         AtoB : B.NSet.T A.NTab.T,
         BtoA : A.NSet.T B.NTab.T
};

val empty = BinRel {AtoB = A.NTab.empty,
                    BtoA = B.NTab.empty};

fun get_AtoB (BinRel rep) = #AtoB rep;
fun get_BtoA (BinRel rep) = #BtoA rep;

fun update_AtoB f (BinRel rep) = 
    BinRel {AtoB = f (#AtoB rep), BtoA = (#BtoA rep)}

fun update_BtoA f (BinRel rep) = 
    BinRel {AtoB = (#AtoB rep), BtoA = f (#BtoA rep)}

(* implicit: rel n *)
val lookup_arels = A.NTab.lookup o get_AtoB;
val lookup_brels = B.NTab.lookup o get_BtoA;

fun get_arels rel n = 
    case lookup_arels rel n of NONE => B.NSet.empty

fun get_brels rel n = 
    case lookup_brels rel n of NONE => A.NSet.empty

(* add and remove things from the binary relation. *)
fun add_many aset bset rel = 
    rel |> update_AtoB 
        (fn abtab => 
            A.NSet.fold
              (fn a => fn abtab2 => 
                  case A.NTab.lookup abtab2 a of
                    NONE => A.NTab.ins (a,bset) abtab2
                  | SOME oldbset => A.NTab.update (a,B.NSet.union_merge bset oldbset) abtab2
              ) aset abtab)
        |> update_BtoA
        (fn batab => 
            B.NSet.fold
              (fn b => fn batab2 => 
                  case B.NTab.lookup batab2 b of
                    NONE => B.NTab.ins (b,aset) batab2
                  | SOME oldaset => B.NTab.update (b,A.NSet.union_merge aset oldaset) batab2
              ) bset batab);

fun del_many aset bset rel = 
    rel |> update_AtoB 
        (fn abtab => 
            A.NSet.fold
              (fn a => fn abtab2 => 
                  case A.NTab.lookup abtab2 a of
                    NONE => abtab2
                  | SOME oldbset => 
                    let val newbset = B.NSet.subtract oldbset bset 
                    in if B.NSet.is_empty newbset then A.NTab.delete a abtab2
                       else A.NTab.update (a,newbset) abtab2 end)
              aset abtab)
        |> update_BtoA
        (fn batab => 
            B.NSet.fold
              (fn b => fn batab2 => 
                  case B.NTab.lookup batab2 b of
                    NONE => batab2
                  | SOME oldaset => 
                    let val newaset = A.NSet.subtract oldaset aset 
                    in if A.NSet.is_empty newaset then B.NTab.delete b batab2
                       else B.NTab.update (b,newaset) batab2 end)
              bset batab);


(* 1-many relations *)
(* implicit: rel *)
fun add_toA a bset = add_many (A.NSet.single a) bset;
fun add_toB b aset = add_many aset (B.NSet.single b);
fun del_fromA a bset = del_many (A.NSet.single a) bset;
fun del_fromB b aset = del_many aset (B.NSet.single b);

(* 1-1 relations *)
(* implicit: rel *)
fun add1_to a b = add_many  (A.NSet.single a) (B.NSet.single b);
fun del1_from a b = del_many (A.NSet.single a) (B.NSet.single b);

(* renaming *)
fun renameA a1 a2 rel = 
    let val changedbs = A.NTab.get (get_AtoB rel) a1
    in rel |> update_AtoB (A.NTab.rename a1 a2)
           |> update_BtoA 
           (fn batab => 
               B.NSet.fold 
                 (B.NTab.map_entry (A.NSet.rename a1 a2))
                 changedbs batab)
    end;

fun renameB b1 b2 rel = 
    let val changedas = B.NTab.get (get_BtoA rel) b1
    in rel |> update_BtoA (B.NTab.rename b1 b2)
           |> update_AtoB 
           (fn abtab => 
               A.NSet.fold 
                 (A.NTab.map_entry (B.NSet.rename b1 b2))
                 changedas abtab)
    end;

(* pretty printing *)
fun pretty rel = 
    Pretty.chunks 
    [Pretty.str "Rel{",
     Pretty.block [Pretty.str "A->B:",  
                   A.NTab.pretty B.NSet.pretty (get_AtoB rel)],
     Pretty.block [Pretty.str "B->A:",  
                   B.NTab.pretty A.NSet.pretty (get_BtoA rel)],
     Pretty.str "}"];
val print = Pretty.writeln o pretty;

end;


