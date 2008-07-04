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


(* strings as names *)
structure StrName :> NAMES where type name = string
= struct
  open BasicStrName
  structure Renaming = RenamingFun(BasicStrName);
  type renaming = Renaming.T
  val mk_renaming = Renaming.mk;
  val init_renaming = Renaming.init_with_avoids;
  val rename1 = Renaming.rename1;
  val ignored_of_renaming = Renaming.get_ignored;
  val avoids_of_renaming = Renaming.get_avoids;
  val nmap_of_renaming = Renaming.get_nmap;
  val pretty_renaming = Renaming.pretty;
  val print_renaming = Renaming.print;
end;


(* safe strings as names *)
structure SStrName :> SNAMES where type external_name = string
= struct
open StrName;
type external_name = string;
val mk = I;
val dest = I;
end;


(* strings as names *)
structure StrIntName :> NAMES where type name = string * int
= struct
  open BasicStrIntName
  structure Renaming = RenamingFun(BasicStrIntName);
  type renaming = Renaming.T
  val mk_renaming = Renaming.mk;
  val init_renaming = Renaming.init_with_avoids;
  val rename1 = Renaming.rename1;
  val ignored_of_renaming = Renaming.get_ignored;
  val avoids_of_renaming = Renaming.get_avoids;
  val nmap_of_renaming = Renaming.get_nmap;
  val pretty_renaming = Renaming.pretty;
  val print_renaming = Renaming.print;
end;


(* safe (string * int) names *)
structure SStrIntName :> SNAMES where type external_name = string * int
= struct
open StrIntName;
type external_name = string * int;
val mk = I;
val dest = I;
end;



(* ints as names *)
structure IntName :> NAMES where type name = int
= struct
  open BasicIntName
  structure Renaming = RenamingFun(BasicIntName);
  type renaming = Renaming.T
  val mk_renaming = Renaming.mk;
  val init_renaming = Renaming.init_with_avoids;
  val rename1 = Renaming.rename1;
  val ignored_of_renaming = Renaming.get_ignored;
  val avoids_of_renaming = Renaming.get_avoids;
  val nmap_of_renaming = Renaming.get_nmap;
  val pretty_renaming = Renaming.pretty;
  val print_renaming = Renaming.print;
end;


(* safe (string * int) names *)
structure SIntName :> SNAMES where type external_name = int
= struct
open IntName;
type external_name = int;
val mk = I;
val dest = I;
end;
