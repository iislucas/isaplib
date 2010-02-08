(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  Title:      basic_renaming.ML
    Author:     Lucas Dixon
                lucas.dixon@ed.ac.uk
                10 Jan 2010   *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  DESCRIPTION:

Note: This is essentially isomorphisms on a finite set of names.  We
do not store the inverse name function and thus this does not allow
efficient renaming. Efficient renaming can be done using the
change_names structure.

Generic renaming with a set of ignored names, a set of names to be
avoided, and a table of renamings. 

*)
signature BASIC_RENAMING
= sig

type name (* name *)
type nset (* set of names *)
(* type nmap *) (* mapping from names to names *)
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

(* rename these *)
val mk_from_nmap : name nmap -> T

(* lookup if we have been renamed *)
val lookup : T -> name -> name option;

(* for doing renaming / creating an isomorphism *)
val rename1 : name (* old name *)
             -> T (* renaming so far *)
             (* some new name (if clashed) and extended renaming *)
             -> (name option) * T 

(* for alpha-equivlance checking / isomorphism checking *)
(* rename renaming ignores names that don't occur *)
val try_rename1_renaming : name (* old name *)
                  -> name (* new name *)
             -> T (* renaming so far *)
             -> T option (* (if possible) renamed renaming *)

(* composition extends renaming if renameing wasn't already present and 
   if possible *)
val try_compose_rename1 : name (* old name *)
                  -> name (* new name *)
             -> T (* renaming so far *)
             -> T option (* (if possible) extended renaming *)

(* cached info about new names created during renaming *)
val get_newnames : T -> nset
val set_newnames : nset -> T -> T

(* internals of renaming *)
val get_ignored : T -> nset
val set_ignored : nset -> T -> T
val get_avoids : T -> nset
val get_nmap : T -> name nmap

(* edit internal mapping *)
val update_nmap : (name nmap -> name nmap) -> T -> T

(* internal representation *)
val constr : {ignore : nset, (* considered to already be renamed, so not renamed again *)
          new : nset, (* these are the newly generated names, range of nmap *)
          avoids : nset, (* Nothing is allowed to be renamed to these *)
          nmap : name nmap} (* mapping from old to new names *)
         -> T
val destr : T -> {ignore : nset, new : nset, avoids : nset, nmap : name nmap} 


(* pretty *)
val pretty : T -> Pretty.T
val print : T -> unit;

end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
functor BasicRenamingFun(structure NTab : BASIC_NAME_TAB
                    and Namer : NAMER 
                    sharing type Namer.name = NTab.name) 
: BASIC_RENAMING
= struct

exception invalid_renaming_exp of string (* used in table and set renaming *)

structure NSet = NTab.NSet;

type name = NTab.name;
type nset = NTab.NSet.T;
(* type nmap = name NTab.T; *)
type 'a nmap = 'a NTab.T;

(* Invarients: 
   ignore <= avoids - (dom(rn) + range(rn)); 
   new <= avoids - dom(rn); 
   new <= range(rn); 
   dom(rn) <= avoids; 
   range(rn) <= avoids 
*)
datatype T = Renaming of 
         {ignore : NSet.T, (* part of context: ignored and not renamed *)
          new : NSet.T, (* these are the newly generated names, 
                           a subset of the range of nmap *)
          avoids : NSet.T, (* nothing is new allowed to be renamed to these *)
          nmap : name NTab.T}; (* mapping from old to new names *)

val constr = Renaming;
fun destr (Renaming rep) = rep;

(* empty renaming *)
val empty =
    Renaming {ignore = NSet.empty,
              new = NSet.empty,
              avoids = NSet.empty,
              nmap = NTab.empty };

fun mk_from_nmap nmap = 
    Renaming {ignore = NSet.empty,
              new = NSet.empty,
              avoids = NTab.get_nameset nmap,
              nmap = nmap};

fun update_newnames f (Renaming rep) =
    Renaming {ignore = #ignore rep,
              new = f (#new rep),
              avoids = #avoids rep,
              nmap = #nmap rep };

fun update_nmap f (Renaming rep) =
    Renaming {ignore = #ignore rep,
              new = #new rep,
              avoids = #avoids rep,
              nmap = f (#nmap rep) };

fun update_ignored f (Renaming rep) =
    Renaming {ignore = f (#ignore rep),
              new = #new rep,
              avoids = #avoids rep,
              nmap = #nmap rep };

val set_newnames = update_newnames o K;
val set_ignored = update_ignored o K;

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

(* rename a renaming *)
fun try_rename1_renaming n1 n2 (Renaming rep) = 
    if NSet.contains (#avoids rep) n2
    then NONE
    else SOME (Renaming {ignore = the (NSet.try_rename1 n1 n2 (#ignore rep)),
                         avoids = the (NSet.try_rename1 n1 n2 (#avoids rep)),
                         new = the (NSet.try_rename1 n1 n2 (#new rep)),
                         nmap = the (NTab.try_rename1 n1 n2 (#nmap rep))});


fun try_compose_rename1 oldn newn (rn as Renaming rep) =
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
                   else NONE (* maybe should raise a name clash here? *)
    end;


(* pretty *)
fun pretty (Renaming rep) = 
    Pretty.chunks 
      [Pretty.str "Renaming:",
       Pretty.block [Pretty.str "ignore: ", NSet.pretty (#ignore rep)],
       Pretty.block [Pretty.str "avoids: ", NSet.pretty (#avoids rep)],
       Pretty.block [Pretty.str "NMap: ", NTab.pretty Namer.pretty (#nmap rep)]];

val print = Pretty.writeln o pretty;

end;
