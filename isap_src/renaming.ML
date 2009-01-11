(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  Title:      renaming.ML
    Author:     Lucas Dixon
                lucas.dixon@ed.ac.uk
                30 May 2008   *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  DESCRIPTION:

Note: This is essentially isomorphisms on a finite set of names.  We
do not store the inverse name function and thus this does not allow
efficient renaming. Efficient renaming can be done using the
change_names structure.

Generic renaming with a set of ignored names, a set of names to be
avoided, and a table of renamings. 

*)
signature RENAMING
= sig

type name (* name *)
type nset (* set of names *)
type 'a nmap (* mapping from names *)
type T (* renaming *)

exception invalid_renaming_exp of string

val empty : T

val union_ignores : nset -> T -> T 
val union_avoids : nset -> T -> T

val mk : nset (* do not create new fresh names for these *)
         -> nset (* avoid these names for new names *)
         -> name nmap (* use this renaming map *)
         -> T

(* avoid these *)
val mk_from_avoidset : nset -> T

(* info about renaming *)
val get_newnames : T -> nset
val set_newnames : nset -> T -> T

(* lookup if we have been renamed *)
val lookup : T -> name -> name option;

(* for doing renaming / creating an isomorphism *)
val rename1 : name (* old name *)
             -> T (* renaming so far *)
             -> (name option) * T (* clash avoiding new name and extended renaming *)

(* for alpha-equivlance checking / isomorphism checking *)
val try_rename1 : name (* old name *)
                  -> name (* new name *)
             -> T (* renaming so far *)
             -> T option (* (if possible) extended renaming *)

(* internals of renaming *)
val get_ignored : T -> nset
val get_avoids : T -> nset
val get_nmap : T -> name nmap

(* pretty *)
val pretty : T -> Pretty.T
val print : T -> unit;

end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
functor RenamingFun(structure NTab : BASIC_NAME_TAB
                    and Namer : NAMER 
                    sharing type Namer.name = NTab.name) 
: RENAMING
= struct

exception invalid_renaming_exp of string (* used in table and set renaming *)

structure NSet = NTab.NSet;

type name = NTab.name;
type nset = NTab.NSet.T;
type 'a nmap = 'a NTab.T;

(* Invarients: 
   ignore <= avoids - (dom(rn) + range(rn)); 
   new <= avoids - dom(rn); 
   new = range(rn); 
   dom(rn) <= avoids; 
   range(rn) <= avoids 
*)
datatype T = Renaming of 
         {ignore : NSet.T, (* considered to already be renamed, so not renamed again *)
          new : NSet.T, (* these are the newly generated names, range of nmap *)
          avoids : NSet.T, (* Nothing is allowed to be renamed to these *)
          nmap : name NTab.T}; (* mapping from old to new names *)


(* empty renaming *)
val empty =
    Renaming {ignore = NSet.empty,
              new = NSet.empty,
              avoids = NSet.empty,
              nmap = NTab.empty};

fun update_newnames f (Renaming rep) =
    Renaming {ignore = #ignore rep,
              new = f (#new rep),
              avoids = #avoids rep,
              nmap = #nmap rep};

val set_newnames = update_newnames o K;

(* *)
fun union_ignores ignores' (Renaming rep) = 
    Renaming {ignore = NSet.union_merge ignores' (#ignore rep),
              new = #new rep,
              avoids = NSet.union_merge ignores' (#avoids rep),
              nmap = #nmap rep};

fun union_avoids avoids' (Renaming rep) = 
    Renaming {ignore = #ignore rep,
              new = #new rep,
              avoids = NSet.union_merge avoids' (#avoids rep),
              nmap = #nmap rep};

(* IMPROVE: make new an argument... have other constructors *)
fun mk ignore avoids nmap =
    Renaming {ignore = ignore,
              new = NTab.fold (fn (_,n) => NSet.add n) nmap NSet.empty,
              avoids = avoids,
              nmap = nmap};

(* make a renaming *)
fun mk_from_avoidset avoids = union_avoids avoids empty;

(* get info/status of renaming *)
fun get_avoids (Renaming rep) = (#avoids rep);
fun get_newnames (Renaming rep) = (#new rep);
fun get_nmap (Renaming rep) = (#nmap rep);
fun get_ignored (Renaming rep) = (#ignore rep);


val lookup = NTab.lookup o get_nmap;

(* rename an old name *)
fun rename1 oldn (rn as Renaming rep) = 
    let val ignore = (#ignore rep) in
      if NSet.contains ignore oldn then (NONE, rn) (* ignore it, already renamed *)
      else 
        let val nmap = (#nmap rep) in 
          (case NTab.lookup nmap oldn of
             NONE => 
             let val avoids = (#avoids rep) in
               if NSet.contains avoids oldn then (* rename it! *)
                 let val (n2,avoids2) = (NSet.add_new oldn avoids)
                 in (SOME n2, (* oldname clashed, so it was renamed *)
                     Renaming {ignore = ignore,
                               avoids = avoids2,
                               new = NSet.add n2 (#new rep),
                               nmap = NTab.ins (oldn,n2) nmap}) 
                 end
               else (NONE, (* old name was not renamed to something different,
                            so we can ignore all future occurences, and avoid 
                            later changing any other name to clash with this one. *)
                     Renaming {ignore = NSet.ins_fresh oldn ignore, 
                               new = #new rep,
                               avoids = NSet.ins_fresh oldn avoids, 
                               nmap = nmap})
             end
           | SOME n2 => (SOME n2, rn))
        end
    end;


(* rename an old name *)
(* THINK: maybe check if newn = oldn, then add name to ignores and it is not new or in nmap *)
fun try_rename1 oldn newn (rn as Renaming rep) =
    let val nmap = (#nmap rep) in 
      case NTab.lookup nmap oldn of
        NONE => 
        let val avoids = (#avoids rep) in
          if NSet.contains avoids newn then NONE
          else 
            (SOME 
               (Renaming {ignore = #ignore rep, 
                          new = NSet.add newn (#new rep),
                          avoids = NSet.ins_fresh oldn avoids, 
                          nmap = NTab.ins (oldn,newn) nmap }))
        end
      | SOME n2 => if Namer.eq (n2, newn) then SOME rn
                   else NONE
    end;


(* composition of two renamings; equivalent of do rn1 then do rn2 *)
(* fun compose rn1 rn2 = 
    let val ignore = 
            NSet.union_merge (get_ignored rn1, get_ignored rn2)
        val avoids = 
            
    in
      
    end;
*)

(* pretty *)
fun pretty (Renaming rep) = 
    Pretty.chunks 
      [Pretty.str "Renaming:",
       Pretty.block [Pretty.str "ignore: ", NSet.pretty (#ignore rep)],
       Pretty.block [Pretty.str "avoids: ", NSet.pretty (#avoids rep)],
       Pretty.block [Pretty.str "NMap: ", NTab.pretty Namer.pretty (#nmap rep)]];

val print = Pretty.writeln o pretty;

end;






(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
signature RAW_RENAMING 
= sig

structure NTab : BASIC_NAME_TAB

type T

exception invalid_renaming_exp of string

val empty : T

val union_ignores : NTab.NSet.T -> T -> T 
val union_avoids : NTab.NSet.T -> T -> T

val mk : NTab.NSet.T (* do not create new fresh names for these *)
         -> NTab.NSet.T (* avoid these names for new names *)
         -> NTab.name NTab.T (* use this renaming map *)
         -> T

val mk_from_avoids : NTab.NSet.T (* avoid these *)
                       -> T

(* info about renaming *)
val get_newnames : T -> NTab.NSet.T
val set_newnames : NTab.NSet.T -> T -> T

(* lookup if we have been renamed *)
val lookup : T -> NTab.name -> NTab.name option;

(* for doing renaming / creating an isomorphism *)
val rename1 : NTab.name (* old name *)
             -> T (* renaming so far *)
             -> (NTab.name option) * T (* clash avoiding new name and extended renaming *)

(* for alpha-equivlance checking / isomorphism checking *)
val try_rename1 : NTab.name (* old name *)
                  -> NTab.name (* new name *)
             -> T (* renaming so far *)
             -> T option (* (if possible) extended renaming *)


(* compose 2 renamings *)
(* val compose : T -> T -> T *)

(* internals of renaming *)
val get_ignored : T -> NTab.NSet.T
val get_avoids : T -> NTab.NSet.T
val get_nmap : T -> NTab.name NTab.T

(* pretty *)
val pretty : T -> Pretty.T
val print : T -> unit;

end;
