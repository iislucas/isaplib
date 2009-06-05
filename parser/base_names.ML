(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
(*  Basic kinds for names - this is to help avoid
    accidentally mixing up the different kinds of names. *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)

(* datatype names *)
structure Dn :> SSTR_NAMES = SStrName;
structure DnTab = Dn.NTab;
(* consturctor names: or-choice names *)
structure Cn :> SSTR_NAMES = SStrName;
structure CnTab = Cn.NTab;
(* element names: and-choice names *)
structure En :> SSTR_NAMES = SStrName;
structure EnTab = En.NTab;


fun pprint_of_prettyf f depth pretty = 
  PolyML.PrettyString o Pretty.string_of o f;

PolyML.addPrettyPrinter (pprint_of_prettyf Dn.pretty_name);
PolyML.addPrettyPrinter (pprint_of_prettyf En.pretty_name);
PolyML.addPrettyPrinter (pprint_of_prettyf Cn.pretty_name);

(* Agenda element names *)
structure An :> SINT_NAMES = SIntName;
PolyML.addPrettyPrinter (pprint_of_prettyf An.pretty_name);


fun print_tab (depth : int) (printEl: 'a * int -> PolyML.pretty) (x : 'a An.NTab.T) =
		PolyML.PrettyString "?'a An.NTab.T";

structure AnTab = An.NTab;
PolyML.addPrettyPrinter print_tab;          

