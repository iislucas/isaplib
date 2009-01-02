(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  Title:      change_names.ML
    Author:     Lucas Dixon
                lucas.dixon@ed.ac.uk
                30 May 2008   *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  DESCRIPTION:

Note: This is essentially isomorphisms between finite sets of (different) names.

The structure changes names in an old type of name space to names in a
new type of name space. This is a roughly a generalisation of
renaming, but is a little less efficient, amd with a different
treatment of new names, so renaming is written as it's own structure.

Renaming =aprox= CHANGE_NAMES where Dom = Cod, and translate_name = identify function. 

ChangeNames are less efficient than renaming in that we also store in
the inverse name mapping. This allows renaming of the domain and
codomain elements, but slightly slows down the adding of names to the
name change type.

The change-name operation uses a set of ignored names (not renamed), a
set of names to be avoided (don't make any new name one of these), and
a table of renamings (the name translation used).

*)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
signature CHANGE_NAMES 
= sig

structure Dom : BASIC_NAMES
structure Cod : BASIC_NAMES

(* when Dom and Cod are not the same, a translation function is needed *)
val translate_name : Dom.name -> Cod.name 

(* type of name-change / isomorphism *)
type T

(* constructors *)
val empty : T;

(* other operations on the change name space *)
val union_ignores : Dom.NSet.T -> T -> T 
val union_avoids : Cod.NSet.T -> T -> T
val avoid_ignores_image : T -> T;

(* for doing renaming/creating an isomorphism *)
val change1 : Dom.name (* old name *)
             -> T (* renaming so far *)
             -> (Cod.name option) * T (* new name and extended renaming *)

(* for alpha-equivlance/isomorphism-checking *)
val try_change1 : Dom.name (* old name *)
                  -> Cod.name (* new name *)
                  -> T (* renaming so far *)
                  -> T option (* new name and extended renaming *)

(* rename an element in the domain *)
val rename_dom : Dom.name -> Dom.name -> T -> T
val rename_cod : Cod.name -> Cod.name -> T -> T

(* lookuping up renamings *)
val domf : T -> Dom.name -> Cod.name
val codf : T -> Cod.name -> Dom.name
val lookup_domf : T -> Dom.name -> Cod.name option
val lookup_codf : T -> Cod.name -> Dom.name option

(* removing entries *)
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

end;


(* *)
functor ChangeNamesFun(structure Dom : BASIC_NAMES 
                       structure Cod : BASIC_NAMES 
                       val translate_name : Dom.name -> Cod.name) 
: CHANGE_NAMES
= struct

structure Dom = Dom;
structure Cod = Cod;
val translate_name = translate_name;

(* Invarients: 
   ignore <= avoids - (dom(rn) + range(rn)); 
   cod <= avoids - dom(rn); 
   cod = range(rn); 
   dom(rn) <= avoids; 
   range(rn) <= avoids;
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
fun avoid_ignores_image rn = 
    update_avoids
      (Dom.NSet.fold (fn oldn => Cod.NSet.add (translate_name oldn))
                     (get_ignored rn))
      rn;

(* add an old name to be changed to a new name *)
fun change1 oldn (rn as ChangeNames rep) = 
    let val ignore = (#ignore rep) in
      if Dom.NSet.contains ignore oldn then (NONE, rn) (* ignore it *)
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
           | SOME n2 => (SOME n2, rn))
        end
    end;


(* rename an old name *)
fun try_change1 oldn newn (rn as ChangeNames rep) =
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
      | SOME n2 => if Cod.name_eq (n2, newn) then SOME rn
                   else NONE
    end;

(* internally rename *)
fun rename_dom n1 n2 (rn as ChangeNames rep) = 
    let val dom1 = (#domf rep); val cod1 = (#codf rep);
      val (dom2, cod2) = 
          case Dom.NTab.lookup dom1 n1 of 
            NONE => (dom1, cod1)
          | SOME codn => (Dom.NTab.rename n1 n2 dom1, 
                          Cod.NTab.update (codn, n2) cod1)
    in
      ChangeNames {ignore = Dom.NSet.rename n1 n2 (#ignore rep), 
                   avoids = #avoids rep, 
                   domf = dom2,
                   codf = cod2 }
    end;

fun rename_cod n1 n2 (rn as ChangeNames rep) = 
    let val dom1 = (#domf rep); val cod1 = (#codf rep);
      val (dom2, cod2) = 
          case Cod.NTab.lookup cod1 n1 of 
            NONE => (dom1, cod1)
          | SOME domn => (Dom.NTab.update (domn,n2) dom1, 
                          Cod.NTab.rename n1 n2 cod1)
    in
      ChangeNames {ignore = #ignore rep,
                   avoids = Cod.NSet.rename n1 n2 (#avoids rep),
                   domf = dom2,
                   codf = cod2 }
    end;


(* delete an element from the domain *)
fun del_dom n (rn as ChangeNames rep) = 
    let val dom1 = (#domf rep) val cod1 = (#codf rep)
    in 
      case Dom.NTab.lookup dom1 n of 
        NONE => rn
      | SOME codn => 
        ChangeNames {ignore = Dom.NSet.delete n (#ignore rep),
                     avoids = Cod.NSet.delete codn (#avoids rep),
                     domf = Dom.NTab.delete n dom1,
                     codf = Cod.NTab.delete codn cod1}
    end;

fun del_cod n (rn as ChangeNames rep) = 
    let val dom1 = (#domf rep) val cod1 = (#codf rep)
    in 
      case Cod.NTab.lookup cod1 n of 
        NONE => rn
      | SOME domn => 
        ChangeNames {ignore = Dom.NSet.delete domn (#ignore rep),
                     avoids = Cod.NSet.delete n (#avoids rep),
                     domf = Dom.NTab.delete domn dom1,
                     codf = Cod.NTab.delete n cod1}
    end;


(* pretty *)
fun pretty (ChangeNames rep) = 
    Pretty.chunks 
      [Pretty.str "ChangeNames{",
       Pretty.block [Pretty.str "ignore: ", Dom.NSet.pretty (#ignore rep)],
       Pretty.block [Pretty.str "avoids: ", Cod.NSet.pretty (#avoids rep)],
       Pretty.block [Pretty.str "domf: ", Dom.NTab.pretty Cod.pretty_name (#domf rep)],
       Pretty.str "}"];

val print = Pretty.writeln o pretty;

end;
