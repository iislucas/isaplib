(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  Title:      name_iso.ML
    Author:     Lucas Dixon
                lucas.dixon@ed.ac.uk
                30 May 2008   *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  DESCRIPTION:

Note: This is essentially isomorphisms between finite sets of 
      (possibly different) names (Dom and Cod).

The structure changes names in an old type of name space to names in a
new type of name space. This is a roughly a generalisation of
renaming, but is a little less efficient, amd with a different
treatment of new names, so renaming is written as it's own structure.

Renaming =aprox= 
  CHANGE_NAMES where Dom = Cod, 
               and translate_name = identify function. 

ChangeNames are less efficient than renaming in that we also store in
the inverse name mapping. This allows renaming of the domain and
codomain elements, but slightly slows down the adding of names to the
name change type.

The change-name operation uses a set of ignored names (not renamed), a
set of names to be avoided (don't make any new name one of these), and
a table of renamings (the name translation used).

*)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
signature NAME_ISO_SHARING = (* all types for sharing *)
sig
  structure Dom : NAMES_SHARING
  structure Cod : NAMES_SHARING
  type T
end;


signature NAME_ISO 
= sig

structure Dom : NAMES
structure Cod : NAMES

(* when Dom and Cod are not the same, a translation function is needed *)
val translate_name : Dom.name -> Cod.name 

(* type of name-change / isomorphism *)
type T

(* constructors *)
val empty : T;
val is_empty : T -> bool;

(* other operations on the change name space *)
val union_ignores : Dom.NSet.T -> T -> T 
val union_avoids : Cod.NSet.T -> T -> T
val avoid_ignores_image : T -> T;

(* assumes two mappings are disjoint/compatible *)
val union_merge : T -> T -> T

(* for doing renaming/creating an isomorphism *)
val change1 : Dom.name (* old name *)
             -> T (* renaming so far *)
             -> (Cod.name option) * T (* new name and extended renaming *)

(* for alpha-equivlance/isomorphism-checking *)
val try_change1 : Dom.name (* old name *)
                  -> Cod.name (* new name *)
                  -> T (* renaming so far *)
                  -> T option (* new name and extended renaming *)
(* as above, but raises exception *)
exception add_exp of Dom.name * Cod.name * T
val add : Dom.name -> Cod.name -> T -> T

(* renaming names *)
val try_rename1_dom : Dom.name -> Dom.name -> T -> T option
val try_rename1_cod : Cod.name -> Cod.name -> T -> T option


(* 
val rename_dom : Dom.renaming -> T -> Dom.renaming * T
val rename_cod : Cod.renaming -> T -> Cod.renaming * T
*)

(* lookuping up renamings *)
val domf : T -> Dom.name -> Cod.name
val codf : T -> Cod.name -> Dom.name
val lookup_domf : T -> Dom.name -> Cod.name option
val lookup_codf : T -> Cod.name -> Dom.name option

(* removing entries *)
(* delete an element from the domain (and the corresponding one from cod) *)
val del_dom : Dom.name -> T -> T
val del_cod : Cod.name -> T -> T

(* info about renaming *)
val get_cod_nset : T -> Cod.NSet.T
val get_dom_nset : T -> Dom.NSet.T

val get_ignored : T -> Dom.NSet.T
val get_avoids : T -> Cod.NSet.T

val get_domtab : T -> Cod.name Dom.NTab.T
val get_codtab : T -> Dom.name Cod.NTab.T

(* pretty printing *)
val pretty : T -> Pretty.T
val print : T -> unit;

(* internal representation *)
val constr : {ignore : Dom.NSet.T, (* These names are not renamed/changed *)
          avoids : Cod.NSet.T, (* Nothing is allowed to be renamed to these *)
          domf : Cod.name Dom.NTab.T, (* mapping from old to new names *)
          codf : Dom.name Cod.NTab.T} -> T
val destr : T -> {ignore : Dom.NSet.T, 
          avoids : Cod.NSet.T, 
          domf : Cod.name Dom.NTab.T,
          codf : Dom.name Cod.NTab.T} 

(* sharing *)
structure Sharing : NAME_ISO_SHARING
sharing type Sharing.T = T
sharing Sharing.Dom = Dom.Sharing
sharing Sharing.Cod = Cod.Sharing

end;


(* *)
functor NameIsoFun(structure Dom : NAMES 
  structure Cod : NAMES 
  val translate_name : Dom.name -> Cod.name) 
: NAME_ISO
= struct

structure Dom = Dom;
structure Cod = Cod;
val translate_name = translate_name;

(* Invarients: 
   ignore <= avoids - (dom(cn) + range(cn)); 
   cod <= avoids - dom(cn); 
   cod = range(cn); 
   dom(cn) <= avoids; 
   range(cn) <= avoids;
   cod = dom-1
*)
(* THINK: maybe have a set of old/newnames for which the translation
was direct and didn't result in a name clash: like ignores for
renaming? *)
datatype T = ChangeNames of 
         {ignore : Dom.NSet.T, (* These names are not renamed/changed *)
          avoids : Cod.NSet.T, (* Nothing is allowed to be renamed to these *)
          domf : Cod.name Dom.NTab.T, (* mapping from old to new names *)
          codf : Dom.name Cod.NTab.T}; (* mapping from new names to their old name *)


val constr = ChangeNames;
fun destr (ChangeNames rep) = rep;

(* the empty changing of names that does nothing *)
val empty =
    ChangeNames {ignore = Dom.NSet.empty,
                 avoids = Cod.NSet.empty,
                 domf = Dom.NTab.empty,
                 codf = Cod.NTab.empty};

fun union_ignores ignores' (ChangeNames rep) = 
    ChangeNames 
      {ignore = Dom.NSet.union_merge ignores' (#ignore rep),
       avoids = #avoids rep,
       domf = #domf rep, 
       codf = #codf rep};

fun union_avoids avoids' (ChangeNames rep) = 
    ChangeNames 
      {ignore = #ignore rep,
       avoids = Cod.NSet.union_merge avoids' (#avoids rep),
       domf = #domf rep, 
       codf = #codf rep};

(* get info/status of renaming *)
fun get_avoids (ChangeNames rep) = (#avoids rep);
fun get_domtab (ChangeNames rep) = (#domf rep);

val is_empty = Dom.NTab.is_element_empty o get_domtab;

fun get_codtab (ChangeNames rep) = (#codf rep);
fun get_ignored (ChangeNames rep) = (#ignore rep);
val get_cod_nset = Cod.NTab.get_nameset o get_codtab;
val get_dom_nset = Dom.NTab.get_nameset o get_domtab;

val domf = Dom.NTab.get o get_domtab;
val codf = Cod.NTab.get o get_codtab;

val lookup_domf = Dom.NTab.lookup o get_domtab;
val lookup_codf = Cod.NTab.lookup o get_codtab;

fun update_avoids f (ChangeNames rep) = 
    ChangeNames 
      {ignore = #ignore rep, avoids = f (#avoids rep),
       domf = #domf rep, codf = #codf rep};
       
(* implicit: T *)
fun avoid_ignores_image cn = 
    update_avoids
      (Dom.NSet.fold (fn oldn => Cod.NSet.add (translate_name oldn))
                     (get_ignored cn))
      cn;

(* assumes compatable renamings; raises exception if not *)
fun union_merge cn1 cn2 = 
    ChangeNames 
      {ignore = Dom.NSet.union_merge (get_ignored cn1) (get_ignored cn2), 
       avoids = Cod.NSet.union_merge (get_avoids cn1) (get_avoids cn2),
       domf = Dom.NTab.merge_disjoint (get_domtab cn1) (get_domtab cn2), 
       codf = Cod.NTab.merge_disjoint (get_codtab cn1) (get_codtab cn2)};
      
(* add an old name to be changed to a new name *)
fun change1 oldn (cn as ChangeNames rep) = 
    let val ignore = (#ignore rep) in
      if Dom.NSet.contains ignore oldn then (NONE, cn) (* ignore it *)
      else 
        let val domf = (#domf rep) in 
          (case Dom.NTab.lookup domf oldn of
             NONE => 
             let 
               val avoids = (#avoids rep)
               val maybe_newn = (translate_name oldn)
             in
               if Cod.NSet.contains avoids maybe_newn then (* rename it! *)
                 let val (n2,avoids2) = (Cod.NSet.add_new maybe_newn avoids)
                 in (SOME n2, 
                     ChangeNames {ignore = ignore, 
                                  avoids = avoids2,
                                  domf = Dom.NTab.ins (oldn,n2) domf,
                                  codf = Cod.NTab.ins (n2,oldn) (#codf rep) }) 
                 end
               else (NONE, 
                     ChangeNames {ignore = ignore, 
                                  avoids = Cod.NSet.ins_fresh maybe_newn avoids, 
                                  domf = Dom.NTab.ins (oldn,maybe_newn) domf,
                                  codf = Cod.NTab.ins (maybe_newn,oldn) (#codf rep) })
             end
           | SOME n2 => (SOME n2, cn))
        end
    end;


(* rename an old name *)
fun try_change1 oldn newn (cn as ChangeNames rep) =
    let val domf = (#domf rep) in 
      case Dom.NTab.lookup domf oldn of
        NONE => 
        let val avoids = (#avoids rep) in 
          if Cod.NSet.contains avoids newn then NONE
          else 
            (SOME 
               (ChangeNames {ignore = (#ignore rep), 
                             avoids = Cod.NSet.ins_fresh newn avoids, 
                             domf = Dom.NTab.ins (oldn,newn) domf,
                             codf = Cod.NTab.ins (newn,oldn) (#codf rep) }))
        end
      | SOME n2 => if Cod.name_eq (n2, newn) then SOME cn
                   else NONE
    end;

(* as above but raise exception - for when we know that oldn/newn
   are not already mapped to/from *)
exception add_exp of Dom.name * Cod.name * T
fun add oldn newn cn =
    (case try_change1 oldn newn cn of 
       NONE => raise add_exp (oldn, newn, cn)
     | SOME cn2 => cn2);

(* rename names *)
fun try_rename1_dom n1 n2 (cn as ChangeNames rep) = 
    (case Dom.NTab.try_rename1 n1 n2 (#domf rep)
     of SOME dom2 =>
     (case Dom.NSet.try_rename1 n1 n2 (#ignore rep)
       of SOME ignore2 => 
         let val cod2 = 
                 case Dom.NTab.lookup dom2 n2 of 
                   NONE => (#codf rep)
                 | SOME codn => (Cod.NTab.update (codn, n2) (#codf rep))
         in
           SOME (ChangeNames {ignore = ignore2, 
                              avoids = #avoids rep, 
                              domf = dom2,
                              codf = cod2 })
         end
       | NONE => NONE)
      | NONE => NONE);

fun try_rename1_cod n1 n2 (cn as ChangeNames rep) = 
    (case Cod.NTab.try_rename1 n1 n2 (#codf rep)
     of SOME cod2 =>
     (case Cod.NSet.try_rename1 n1 n2 (#avoids rep)
       of SOME avoids2 => 
         let val dom2 = 
                 case Cod.NTab.lookup cod2 n2 of 
                   NONE => (#domf rep)
                 | SOME domn => Dom.NTab.update (domn,n2) (#domf rep)
         in
           SOME (ChangeNames {ignore = #ignore rep,
                              avoids = avoids2,
                              domf = dom2,
                              codf = cod2 })
         end
        | NONE => NONE)
      | NONE => NONE);


(* delete an element from the domain (and the corresponding one from cod) *)
fun del_dom n (cn as ChangeNames rep) = 
    let val dom1 = (#domf rep) val cod1 = (#codf rep)
    in 
      case Dom.NTab.lookup dom1 n of 
        NONE => cn
      | SOME codn => 
        ChangeNames {ignore = Dom.NSet.delete n (#ignore rep),
                     avoids = Cod.NSet.delete codn (#avoids rep),
                     domf = Dom.NTab.delete n dom1,
                     codf = Cod.NTab.delete codn cod1}
    end;

fun del_cod n (cn as ChangeNames rep) = 
    let val dom1 = (#domf rep) val cod1 = (#codf rep)
    in 
      case Cod.NTab.lookup cod1 n of 
        NONE => cn
      | SOME domn => 
        ChangeNames {ignore = Dom.NSet.delete domn (#ignore rep),
                     avoids = Cod.NSet.delete n (#avoids rep),
                     domf = Dom.NTab.delete domn dom1,
                     codf = Cod.NTab.delete n cod1}
    end;


(* pretty *)
fun pretty (ChangeNames rep) = 
    Pretty.chunks 
      [Pretty.str "NameIso {",
       Pretty.block [Pretty.str "  ignore: ", Dom.NSet.pretty (#ignore rep)],
       Pretty.block [Pretty.str "  avoids: ", Cod.NSet.pretty (#avoids rep)],
       Pretty.block [Pretty.str "  isof: ", Dom.NTab.pretty Cod.pretty_name (#domf rep)],
       Pretty.str "}"];

val print = Pretty.writeln o pretty;


(* *)
structure Sharing = struct
  structure Dom = Dom.Sharing
  structure Cod = Cod.Sharing
  type T = T;
end;

end;
