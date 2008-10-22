(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  Title:      change_names.ML
    Author:     Lucas Dixon
                lucas.dixon@ed.ac.uk
                30 May 2008   *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  DESCRIPTION:

This changes names in an old type of name space to names in a new type
of name space. This is a generalisation of renaming, but is a little
less efficient, so is written as it's own structure. 

Renaming = CHANGE_NAMES
           where OldN = NewN, 
           and translate_name = identify function. 

Uses a set of ignored names (not renamed), a set of names to be
avoided (don't make any new name one of these), 
and a table of renamings (the name translation used),
and holds the set of newly generated names. 

*)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
signature CHANGE_NAMES 
= sig

structure OldN : BASIC_NAMES
structure NewN : BASIC_NAMES

(* when OldN and NewN are not the same, a translation function is needed *)
val translate_name : OldN.name -> NewN.name 

(* type of renaming *)
type T

val mk : OldN.NSet.T (* ignore these : don't rename *)
         -> NewN.NSet.T (* avoid these : don't name anything one of these *)
         -> NewN.name OldN.NTab.T (* use this renaming map *)
         -> T

val init_with_avoids : NewN.NSet.T (* avoid these *)
                       -> T

val change1 : OldN.name (* old name *)
             -> T (* renaming so far *)
             -> (NewN.name option) * T (* new name and extended renaming *)


val union_ignores : OldN.NSet.T -> T -> T 
val union_avoids : NewN.NSet.T -> T -> T


(* compose 2 renamings *)
(* val compose : T -> T -> T *)

(* info about renaming *)
val get_newnames : T -> NewN.NSet.T
val get_ignored : T -> OldN.NSet.T
val get_avoids : T -> NewN.NSet.T
val get_nmap : T -> NewN.name OldN.NTab.T

(* pretty *)
val pretty : T -> Pretty.T
val print : T -> unit;

end;


functor ChangeNamesFun(structure OldN : BASIC_NAMES 
                       structure NewN : BASIC_NAMES 
                       val translate_name : OldN.name -> NewN.name) 
: CHANGE_NAMES
= struct

structure OldN = OldN;
structure NewN = NewN;
val translate_name = translate_name;

(* Invarients: 
   ignore <= avoids - (dom(rn) + range(rn)); 
   new <= avoids - dom(rn); 
   new = range(rn); 
   dom(rn) <= avoids; 
   range(rn) <= avoids 
*)
datatype T = ChangeNames of 
         {ignore : OldN.NSet.T, (* These names that are not renamed *)
          new : NewN.NSet.T, (* these are the newly generated names, range of nmap *)
          avoids : NewN.NSet.T, (* Nothing is allowed to be renamed to these *)
          nmap : NewN.name OldN.NTab.T}; (* mapping from old to new names *)

(* IMPROVE: make new an argument... have other constructors *)
fun mk ignore avoids nmap =
    ChangeNames 
      {ignore = ignore,
       new = OldN.NTab.fold (fn (_,n) => NewN.NSet.add n) nmap NewN.NSet.empty,
       avoids = avoids,
       nmap = nmap};

(* make a renaming *)
fun init_with_avoids avoids =
    ChangeNames {ignore = OldN.NSet.empty,
              new = NewN.NSet.empty,
              avoids = avoids,
              nmap = OldN.NTab.empty};

fun union_ignores ignores' (ChangeNames rep) = 
    ChangeNames 
      {ignore = OldN.NSet.union_merge ignores' (#ignore rep),
       new = #new rep,
       (* adding ignored names also adds the image of the ignored 
          names to the avoids *)
       avoids = OldN.NSet.fold (fn oldn => NewN.NSet.add (translate_name oldn)) 
                               ignores' (#avoids rep),
       nmap = #nmap rep};

fun union_avoids avoids' (ChangeNames rep) = 
    ChangeNames {ignore = #ignore rep,
              new = #new rep,
              avoids = NewN.NSet.union_merge avoids' (#avoids rep),
              nmap = #nmap rep};

(* get info/status of renaming *)
fun get_avoids (ChangeNames rep) = (#avoids rep);
fun get_newnames (ChangeNames rep) = (#new rep);
fun get_nmap (ChangeNames rep) = (#nmap rep);
fun get_ignored (ChangeNames rep) = (#ignore rep);

(* rename an old name *)
fun change1 oldn (rn as ChangeNames rep) = 
    let val ignore = (#ignore rep) in
      if OldN.NSet.contains ignore oldn then (NONE, rn) (* ignore it *)
      else 
        let val nmap = (#nmap rep) in 
          (case OldN.NTab.lookup nmap oldn of
             NONE => 
             let 
               val avoids = (#avoids rep)
               val maybe_newn = (translate_name oldn)
             in
               if NewN.NSet.contains avoids maybe_newn then (* rename it! *)
                 let val (n2,avoids2) = (NewN.NSet.add_new maybe_newn avoids)
                 in (SOME n2, 
                     ChangeNames {ignore = ignore, avoids = avoids2,
                               new = NewN.NSet.add n2 (#new rep),
                               nmap = OldN.NTab.ins (oldn,n2) nmap}) 
                 end
               else (NONE, 
                     ChangeNames {ignore = OldN.NSet.ins_fresh oldn ignore, 
                               new = #new rep,
                               avoids = NewN.NSet.ins_fresh maybe_newn avoids, 
                               nmap = OldN.NTab.ins (oldn,maybe_newn) nmap})
             end
           | SOME n2 => (SOME n2, rn))
        end
    end;


(* composition of two renamings; equivalent of do rn1 then do rn2 *)
(* fun compose rn1 rn2 = 
    let val ignore = 
            N.NSet.union_merge (get_ignored rn1, get_ignored rn2)
        val avoids = 
            
    in
      
    end;
*)

(* pretty *)
fun pretty (ChangeNames rep) = 
    Pretty.chunks 
      [Pretty.str "ChangeNames:",
       Pretty.block [Pretty.str "ignore: ", OldN.NSet.pretty (#ignore rep)],
       Pretty.block [Pretty.str "new: ", NewN.NSet.pretty (#new rep)],
       Pretty.block [Pretty.str "avoids: ", NewN.NSet.pretty (#avoids rep)],
       Pretty.block [Pretty.str "nmap: ", OldN.NTab.pretty NewN.pretty_name (#nmap rep)]];

val print = Pretty.writeln o pretty;

end;
