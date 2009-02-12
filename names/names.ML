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

  structure Namer : NAMER
  sharing type Namer.name = name;

  structure NTab : NAME_TAB;
  sharing type NTab.name = name;

  structure NSet : NAME_SET;
  sharing NTab.NSet = NSet;

  structure Tab : TABLE;
  sharing NTab.Tab = Tab;

  structure Rnm : RENAMING 
  sharing type Rnm.name = name 
  sharing type Rnm.nset = NSet.T 
  sharing type Rnm.nmap = NTab.T (* note compound type... *)

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
type src
val mk : src -> name
val dest : name -> src
end;

functor NamesFun(
  structure Namer : NAMER 
  val default_name : Namer.name
) : NAMES
= struct
  (* create basic names *)
  structure Nm : BASIC_NAMES = 
    BasicNameFun(
      structure Namer = Namer 
      val default_name = default_name);

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

(* add data to a names type - makes new names type with extra data *)
functor AddDataNameFun(
  structure Nm : NAMES 
  type data
  val ord_data : data * data -> order
  val pretty : data * Nm.name -> Pretty.T
  val default_name : data * Nm.name
)
: NAMES
= struct 
  structure Namer2 = 
    AddDataNamerFun(
      structure N = Nm.Namer;
      type data = data
      val ord_data = ord_data
      val pretty = pretty);
  structure Names = NamesFun(
    structure Namer = Namer2
    val default_name = default_name);
  open Names;
end;

(* IMRPOVE: stop using StrNames and use SStrNames - can be more
   efficient and is safer *)
signature STR_NAMES = NAMES where type name = string;
structure StrName : STR_NAMES = 
  NamesFun(structure Namer = StrNamer val default_name = "a");

(* FIXME: seriously improve SNAMER's by internally using (string * int),
and parsing the last alphanum to the int representation. This would
give much faster suc, pred, and ord relationships. pretty printing and
parsing from string would still be slowish, but would not really
matter. *)
signature SSTR_NAMES = SNAMES where type src = string;

(* safe strings as names *)
structure SStrName :> SSTR_NAMES
= struct
structure S = NamesFun(structure Namer = StrSNamer val default_name = StrSNamer.StrName("a",0));
open S;
type src = string;
val mk = StrSNamer.mk;
val dest = StrSNamer.dest;
end;


signature STR_INT_NAMES = NAMES where type name = string * int;
structure StrIntName : STR_INT_NAMES = 
  NamesFun(structure Namer = StrIntNamer val default_name = ("a",0));

(* safe (string * int) names *)
signature SSTR_INT_NAMES = SNAMES where type src = string * int;
structure SStrIntName :> SSTR_INT_NAMES
= struct
  open StrIntName;
  type src = string * int;
  val mk = I;
  val dest = I;
end;

signature INT_NAMES = NAMES where type name = int;
structure IntName : INT_NAMES = 
  NamesFun(structure Namer = IntNamer 
           val default_name = 0);

(* safe int names *)
signature SINT_NAMES = SNAMES where type src = int;
structure SIntName :> SINT_NAMES
= struct
open IntName;
type src = int;
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
