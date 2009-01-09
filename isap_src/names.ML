(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  Title:      names.ML
    Author:     Lucas Dixon
                lucas.dixon@ed.ac.uk
                30 May 2008                                            *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  DESCRIPTION:

Names + renaming for them

Avoids very common error: see "n" is not in avoid set, so you don't
change it. But! you also don't want another renaming, call it "m"
*not* to result in renaming "m" to "n". So you need both an avoid set
as well as a ignore set. "n" gets added to the ignore set as well as
the avoid set, thus "m" doesn't clobber "n" and all future occurances
of "n" are ignored. 

*)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)

(* names + renaming *)
signature NAMES
= sig
  type name
  structure NTab : NAME_TAB;
  sharing type NTab.name = name;

  structure NSet : NAME_SET;
  sharing NTab.NSet = NSet;

  structure Tab : TABLE;
  sharing NTab.Tab = Tab;

  structure Rnm : RENAMING 
  where type name = name 
  and type nset = NSet.T 
  and type 'a nmap = 'a NTab.T

  sharing type NTab.renaming = Rnm.T 
  sharing type NSet.renaming = Rnm.T;

  val default_name : name
  val pretty_name : name -> Pretty.T
  val print_name : name -> unit
  val name_eq : name * name -> bool
  val name_ord : name * name -> General.order

  type renaming  
  sharing type Rnm.T = renaming

  val empty_renaming : renaming (* avoid these names *)
  val mk_renaming : NSet.T  (* ignore these *)
                    -> NSet.T (* avoid these names *) 
                    -> name NTab.T (* Use this renaming map *)
                    -> renaming
  val renaming_of_avoidset 
      : NSet.T (* avoid these names *)
        -> renaming

  (* for renaming *)
  val rename1 : name (* old name *)
                -> renaming (* renaming so far *)
                -> (name option) (* new name *)
                   * renaming (* extended renaming *)

  (* for alpha conversion *)
  val try_rename1 : name (* old name *)
                    -> name (* new name *)
                    -> renaming (* renaming so far *)
                    -> renaming option (* (optional) extended renaming *)

  (* add more stuff to renaming *)
  val union_avoids_to_renaming : NSet.T -> renaming -> renaming
  (* ignores are auto also added to avoids *)
  val union_ignores_to_renaming : NSet.T -> renaming -> renaming
  
  (* gets from renamings *)
  val newnames_of_renaming : renaming -> NSet.T
  val ignored_of_renaming : renaming -> NSet.T
  val avoids_of_renaming : renaming -> NSet.T
  val nmap_of_renaming : renaming -> name NTab.T

  val pretty_renaming : renaming -> Pretty.T
  val print_renaming : renaming -> unit
end;

(* Safe String Names; isomorphic strings, named hidden under a
constructor so the underlying string type is not normally revealed,
thus harder to confuse name types. *)

signature SNAMES
= sig
include NAMES
type external_name
val mk : external_name -> name
val dest : name -> external_name
end;

functor NamesFun(Nm : BASIC_NAMES) : NAMES
= struct
  open Nm;
  structure NSet = NameSetFun(Nm);
  structure NTab = NameTabFun(Nm);

  type renaming = Rnm.T
  val empty_renaming = Rnm.empty;
  val mk_renaming = Rnm.mk;
  val renaming_of_avoidset = Rnm.mk_from_avoidset;

  val rename1 = Rnm.rename1;
  val try_rename1 = Rnm.try_rename1;

  val union_ignores_to_renaming = Rnm.union_ignores; 
  val union_avoids_to_renaming = Rnm.union_avoids; 

  val newnames_of_renaming = Rnm.get_newnames;
  val ignored_of_renaming = Rnm.get_ignored;
  val avoids_of_renaming = Rnm.get_avoids;
  val nmap_of_renaming = Rnm.get_nmap;

  val pretty_renaming = Rnm.pretty;
  val print_renaming = Rnm.print;
end;

signature foofoo =
sig
structure N1 : NAMES
structure N2 : NAMES
sharing N1 = N2
end;

signature STR_NAMES = NAMES where type name = string;

(* strings as names *)
structure StrName :> STR_NAMES = NamesFun(BasicStrName);

signature SSTR_NAMES = SNAMES where type external_name = string;

(* safe strings as names *)
structure SStrName :> SSTR_NAMES
= struct
open StrName;
type external_name = string;
val mk = I;
val dest = I;
end;


signature STR_INT_NAMES = NAMES where type name = string * int;
structure StrIntName :> STR_INT_NAMES = NamesFun(BasicStrIntName);

(* safe (string * int) names *)
signature SSTR_INT_NAMES = SNAMES where type external_name = string * int;
structure SStrIntName :> SSTR_INT_NAMES
= struct
open StrIntName;
type external_name = string * int;
val mk = I;
val dest = I;
end;

signature INT_NAMES = NAMES where type name = int;
structure IntName :> INT_NAMES = NamesFun(BasicIntName);

(* safe int names *)
signature SINT_NAMES = SNAMES where type external_name = int;
structure SIntName :> SINT_NAMES
= struct
open IntName;
type external_name = int;
val mk = I;
val dest = I;
end;

(* done in basic names  
install_pp (make_pp ["IntName", "NSet", "T"] (Pretty.pprint o IntName.NSet.pretty));
*)

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
(* Lifting into FreshNamers *)
structure IntNSet = IntName.NSet;
structure StrNSet = StrName.NSet;
structure StrIntNSet = StrIntName.NSet;


structure IntNTab = IntName.NTab;
structure StrNTab = StrName.NTab;
structure StrIntNTab = StrIntName.NTab;

structure IntTab = IntNTab.Tab;
structure StrTab = StrNTab.Tab;
structure StrIntTab = StrIntNTab.Tab;
