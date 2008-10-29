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
  include BASIC_NAMES
  type renaming  
  structure Rnm : RENAMING
  sharing type Rnm.T = renaming

  val mk_renaming : NSet.T  (* ignore these *)
                    -> NSet.T (* avoid these names *) 
                    -> name NTab.T (* Use this renaming map *)
                    -> renaming
  val init_renaming : NSet.T (* avoid these names *)
                      -> renaming
  val rename1 : name (* old name *)
                -> renaming (* renaming so far *)
                -> (name option) (* new name *)
                   * renaming (* extended renaming *)

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

signature STR_NAMES = NAMES where type name = string;

(* strings as names *)
structure StrName :> STR_NAMES
= struct
  open BasicStrName
  structure Renaming = RenamingFun(BasicStrName);
  structure Rnm = Renaming;
  type renaming = Renaming.T
  val mk_renaming = Renaming.mk;
  val init_renaming = Renaming.init_with_avoids;

  val rename1 = Renaming.rename1;

  val union_ignores_to_renaming = Renaming.union_ignores; 
  val union_avoids_to_renaming = Renaming.union_avoids; 

  val newnames_of_renaming = Renaming.get_newnames;
  val ignored_of_renaming = Renaming.get_ignored;
  val avoids_of_renaming = Renaming.get_avoids;
  val nmap_of_renaming = Renaming.get_nmap;

  val pretty_renaming = Renaming.pretty;
  val print_renaming = Renaming.print;
end;

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

(* strings as names *)
structure StrIntName :> STR_INT_NAMES
= struct
  open BasicStrIntName
  structure Renaming = RenamingFun(BasicStrIntName);
  structure Rnm = Renaming;
  type renaming = Renaming.T
  val mk_renaming = Renaming.mk;
  val init_renaming = Renaming.init_with_avoids;

  val rename1 = Renaming.rename1;

  val union_ignores_to_renaming = Renaming.union_ignores; 
  val union_avoids_to_renaming = Renaming.union_avoids; 

  val newnames_of_renaming = Renaming.get_newnames;
  val ignored_of_renaming = Renaming.get_ignored;
  val avoids_of_renaming = Renaming.get_avoids;
  val nmap_of_renaming = Renaming.get_nmap;

  val pretty_renaming = Renaming.pretty;
  val print_renaming = Renaming.print;
end;

signature SSTR_INT_NAMES = SNAMES where type external_name = string * int;

(* safe (string * int) names *)
structure SStrIntName :> SSTR_INT_NAMES
= struct
open StrIntName;
type external_name = string * int;
val mk = I;
val dest = I;
end;


signature INT_NAMES = NAMES where type name = int;

(* ints as names *)
structure IntName :> INT_NAMES
= struct
  open BasicIntName
  structure Renaming = RenamingFun(BasicIntName);
  structure Rnm = Renaming;
  type renaming = Renaming.T
  val mk_renaming = Renaming.mk;
  val init_renaming = Renaming.init_with_avoids;

  val rename1 = Renaming.rename1;

  val union_ignores_to_renaming = Renaming.union_ignores; 
  val union_avoids_to_renaming = Renaming.union_avoids; 

  val newnames_of_renaming = Renaming.get_newnames;
  val ignored_of_renaming = Renaming.get_ignored;
  val avoids_of_renaming = Renaming.get_avoids;
  val nmap_of_renaming = Renaming.get_nmap;

  val pretty_renaming = Renaming.pretty;
  val print_renaming = Renaming.print;
end;

signature SINT_NAMES = SNAMES where type external_name = int;

(* safe (string * int) names *)
structure SIntName :> SINT_NAMES
= struct
open IntName;
type external_name = int;
val mk = I;
val dest = I;
end;
