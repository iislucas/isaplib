(*  Title:      basic_names.ML
    Author:     Lucas Dixon
                lucas.dixon@ed.ac.uk
                30 May 2008   *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  DESCRIPTION:

Basic things you want from names...

*)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)

(* names with an ordering, sets of names, and tables from names to objects *)
signature BASIC_NAMES
= sig
  type name

  structure NTab : NAME_TAB 
  sharing type NTab.name = name;
  structure NSet : NAME_SET
  sharing NTab.NSet = NSet;
  structure Tab : TABLE
  sharing NTab.Tab = Tab;

  val default_name : name
  val pretty_name : name -> Pretty.T
  val print_name : name -> unit
  val name_eq : name * name -> bool
  val name_ord : name * name -> General.order
end;


(* strings as names *)
structure BasicStrName : BASIC_NAMES where type name = string
= struct
  type name = string
  structure NTab = StrNTab;
  structure NSet = StrNSet;
  structure Tab = StrTab;
  val default_name = "a";
  val pretty_name = Pretty.str;
  val print_name = Pretty.writeln o pretty_name;
  val name_ord = String.compare;
  fun name_eq (ab as (a,b)) = case (name_ord ab) of EQUAL => true | _ => false;
end;

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
structure BasicStrIntName : BASIC_NAMES where type name = (string * int)
  = struct
  type name = string * int
  structure NTab = StrIntNTab;
  structure NSet = StrIntNSet;
  structure Tab = StrIntTab;
  val default_name = ("a",0);
  val pretty_name = StrIntNTab.pretty_name;
  val print_name = Pretty.writeln o pretty_name;
  val name_ord = StrIntNTab.ord;;
  fun name_eq (ab as (a,b)) = case (name_ord ab) of EQUAL => true | _ => false;
end;

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
structure BasicIntName : BASIC_NAMES where type name = int
  = struct
  type name =  int
  structure NTab = IntNTab;
  structure NSet = IntNSet;
  structure Tab = IntTab;
  val default_name = 0;
  val pretty_name = IntNTab.pretty_name;
  val print_name = Pretty.writeln o pretty_name;
  val name_ord = IntNTab.ord;;
  fun name_eq (ab as (a,b)) = case (name_ord ab) of EQUAL => true | _ => false;
end;
