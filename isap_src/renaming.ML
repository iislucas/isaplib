(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  Title:      renaming.ML
    Author:     Lucas Dixon
                lucas.dixon@ed.ac.uk
                30 May 2008   *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  DESCRIPTION:

Generic renaming with a set of ignored names, a set of names to be
avoided, and a table of renamings. 

*)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
signature RENAMING 
= sig

structure N : BASIC_NAMES

type T

val mk : N.NSet.T (* ignore these *)
         -> N.NSet.T (* avoid these *)
         -> N.name N.NTab.T (* use this renaming map *)
         -> T

val init_with_avoids : N.NSet.T (* avoid these *)
                       -> T

val rename1 : N.name (* old name *)
             -> T (* renaming so far *)
             -> (N.name option) * T (* new name and extended renaming *)


val union_ignores : N.NSet.T -> T -> T 
val union_avoids : N.NSet.T -> T -> T


(* compose 2 renamings *)
(* val compose : T -> T -> T *)

(* info about renaming *)
val get_newnames : T -> N.NSet.T
val get_ignored : T -> N.NSet.T
val get_avoids : T -> N.NSet.T
val get_nmap : T -> N.name N.NTab.T

(* pretty *)
val pretty : T -> Pretty.T
val print : T -> unit;

end;


functor RenamingFun(N : BASIC_NAMES) 
: RENAMING
= struct

structure N = N;

(* Invarients: 
   ignore <= avoids - (dom(rn) + range(rn)); 
   new <= avoids - dom(rn); 
   new = range(rn); 
   dom(rn) <= avoids; 
   range(rn) <= avoids 
*)
datatype T = Renaming of 
         {ignore : N.NSet.T, (* These names that are not renamed *)
          new : N.NSet.T, (* these are the newly generated names, range of nmap *)
          avoids : N.NSet.T, (* Nothing is allowed to be renamed to these *)
          nmap : N.name N.NTab.T}; (* mapping from old to new names *)

(* IMPROVE: make new an argument... have other constructors *)
fun mk ignore avoids nmap =
    Renaming {ignore = ignore,
              new = N.NTab.fold (fn (_,n) => N.NSet.add n) nmap N.NSet.empty,
              avoids = avoids,
              nmap = nmap};

(* make a renaming *)
fun init_with_avoids avoids =
    Renaming {ignore = N.NSet.empty,
              new = N.NSet.empty,
              avoids = avoids,
              nmap = N.NTab.empty};

(* *)
fun union_ignores ignores' (Renaming rep) = 
    Renaming {ignore = N.NSet.union_merge ignores' (#ignore rep),
              new = #new rep,
              avoids = N.NSet.union_merge ignores' (#avoids rep),
              nmap = #nmap rep};

fun union_avoids avoids' (Renaming rep) = 
    Renaming {ignore = #ignore rep,
              new = #new rep,
              avoids = N.NSet.union_merge avoids' (#avoids rep),
              nmap = #nmap rep};

(* get info/status of renaming *)
fun get_avoids (Renaming rep) = (#avoids rep);
fun get_newnames (Renaming rep) = (#new rep);
fun get_nmap (Renaming rep) = (#nmap rep);
fun get_ignored (Renaming rep) = (#ignore rep);

(* rename an old name *)
fun rename1 oldn (rn as Renaming rep) = 
    let val ignore = (#ignore rep) in
      if N.NSet.contains ignore oldn then (NONE, rn) (* ignore it *)
      else 
        let val nmap = (#nmap rep) in 
          (case N.NTab.lookup nmap oldn of
             NONE => 
             let val avoids = (#avoids rep) in
               if N.NSet.contains avoids oldn then (* rename it! *)
                 let val (n2,avoids2) = (N.NSet.add_new oldn avoids)
                 in (SOME n2, 
                     Renaming {ignore = ignore, avoids = avoids2,
                               new = N.NSet.add n2 (#new rep),
                               nmap = N.NTab.ins (oldn,n2) nmap}) 
                 end
               else (NONE, 
                     Renaming {ignore = N.NSet.ins_fresh oldn ignore, 
                               new = #new rep,
                               avoids = N.NSet.ins_fresh oldn avoids, 
                               nmap = nmap})
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
fun pretty (Renaming rep) = 
    Pretty.chunks 
      [Pretty.str "Renaming:",
       Pretty.block [Pretty.str "ignore: ", N.NSet.pretty (#ignore rep)],
       Pretty.block [Pretty.str "avoids: ", N.NSet.pretty (#avoids rep)],
       Pretty.block [Pretty.str "NMap: ", N.NTab.pretty N.pretty_name (#nmap rep)]];

val print = Pretty.writeln o pretty;

end;
