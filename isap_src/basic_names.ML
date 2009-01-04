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

  structure NTab : BASIC_NAME_TAB 
  sharing type NTab.name = name;
  structure NSet : BASIC_NAME_SET
  sharing NTab.NSet = NSet;
  structure Tab : TABLE
  sharing NTab.Tab = Tab;

  val default_name : name
  val pretty_name : name -> Pretty.T
  val print_name : name -> unit
  val name_eq : name * name -> bool
  val name_ord : name * name -> General.order
end;




(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
(* Lifting into FreshNamers *)
structure BasicIntNSet = BasicNameSetFun(IntNamer);
structure BasicStrNSet = BasicNameSetFun(StrNamer);
structure BasicStrIntNSet = BasicNameSetFun(StrIntNamer);

install_pp (make_pp ["BasicIntNSet", "T"] (Pretty.pprint o BasicIntNSet.pretty));
install_pp (make_pp ["BasicStrNSet", "T"] (Pretty.pprint o BasicStrNSet.pretty));
install_pp (make_pp ["BasicStrIntNSet", "T"] (Pretty.pprint o BasicStrIntNSet.pretty));


structure BasicIntNTab = BasicNameTabFun(BasicIntNSet);
structure BasicStrNTab = BasicNameTabFun(BasicStrNSet);
structure BasicStrIntNTab = BasicNameTabFun(BasicStrIntNSet);

structure BasicIntTab = BasicIntNTab.Tab;
structure BasicStrTab = BasicStrNTab.Tab;
structure BasicStrIntTab = BasicStrIntNTab.Tab;


(* strings as names *)
structure BasicStrName : BASIC_NAMES where type name = string
= struct
  type name = string
  structure NTab = BasicStrNTab;
  structure NSet = BasicStrNSet;
  structure Tab = BasicStrTab;
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
  structure NTab = BasicStrIntNTab;
  structure NSet = BasicStrIntNSet;
  structure Tab = BasicStrIntTab;
  val default_name = ("a",0);
  val pretty_name = BasicStrIntNTab.pretty_name;
  val print_name = Pretty.writeln o pretty_name;
  val name_ord = BasicStrIntNTab.ord;;
  fun name_eq (ab as (a,b)) = case (name_ord ab) of EQUAL => true | _ => false;
end;

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
structure BasicIntName : BASIC_NAMES where type name = int
  = struct
  type name =  int
  structure NTab = BasicIntNTab;
  structure NSet = BasicIntNSet;
  structure Tab = BasicIntTab;
  val default_name = 0;
  val pretty_name = BasicIntNTab.pretty_name;
  val print_name = Pretty.writeln o pretty_name;
  val name_ord = BasicIntNTab.ord;;
  fun name_eq (ab as (a,b)) = case (name_ord ab) of EQUAL => true | _ => false;
end;



(* 
install_pp (make_pp ["StrNTab", "'a T"] 
                    (Pretty.pprint o StrNTab.direct_pretty));
install_pp (make_pp ["StrIntNTab", "'a T"] 
                    (Pretty.pprint o StrIntNTab.direct_pretty));
*)
