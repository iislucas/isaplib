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

install_pp (make_pp ["Dn", "name"] (Pretty.pprint o Dn.pretty_name));
install_pp (make_pp ["Cn", "name"] (Pretty.pprint o Cn.pretty_name));
install_pp (make_pp ["En", "name"] (Pretty.pprint o En.pretty_name));


(* Agenda element names *)
structure An :> SINT_NAMES = SIntName;
install_pp (make_pp ["An", "name"] (Pretty.pprint o An.pretty_name));
structure AnTab = An.NTab;
