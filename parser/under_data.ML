(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
(*  Underlying kinds for datatypes - this is to help avoid
    accidentally mixing up the different kinds of names. *)
signature UNDER_DATA
= sig

type dname (* names of datatypes *)
type cname (* names of constructors *)
type ename (* names of record element labels *)

structure DnTab : FNAME_TAB where type name = dname;
structure CnTab : FNAME_TAB where type name = cname;
structure EnTab : FNAME_TAB where type name = ename;

val dname : string -> dname
val cname : string -> cname
val ename : string -> ename

val pretty_dname : dname -> Pretty.T
val pretty_cname : cname -> Pretty.T
val pretty_ename : ename -> Pretty.T

val dname_eq : dname -> dname -> bool
val cname_eq : cname -> cname -> bool
val ename_eq : ename -> ename -> bool

val dname_ord : dname * dname -> General.order
val cname_ord : cname * cname -> General.order
val ename_ord : ename * ename -> General.order

end;

structure DB_UnderData 
= struct 

type dname = string;
type cname = string;
type ename = string;

structure EnTab :> FNAME_TAB where type name = string = StrNameTab;
structure CnTab :> FNAME_TAB where type name = string = StrNameTab;
structure DnTab :> FNAME_TAB where type name = string = StrNameTab;

val dname = (I : string -> dname);
val cname = (I : string -> cname);
val ename = (I : string -> ename);

val dname_ord = DnTab.ord;
val cname_ord = CnTab.ord;
val ename_ord = EnTab.ord;

fun dname_eq a b = case (dname_ord (a, b)) of EQUAL => true | _ => false;
fun cname_eq a b = case (dname_ord (a, b)) of EQUAL => true | _ => false;
fun ename_eq a b = case (ename_ord (a, b)) of EQUAL => true | _ => false;

val pretty_dname = Pretty.str;
val pretty_cname = Pretty.str;
val pretty_ename = Pretty.str;

end;

structure UnderData :> UNDER_DATA = DB_UnderData;

install_pp (make_pp ["UnderData", "DnTab", "name"] 
                    (Pretty.pprint o UnderData.pretty_dname));
install_pp (make_pp ["UnderData", "dname"] 
                    (Pretty.pprint o UnderData.pretty_dname));

install_pp (make_pp ["UnderData", "EnTab", "name"] 
                    (Pretty.pprint o UnderData.pretty_ename));
install_pp (make_pp ["UnderData", "ename"] 
                    (Pretty.pprint o UnderData.pretty_ename));

install_pp (make_pp ["UnderData", "CnTab", "name"] 
                    (Pretty.pprint o UnderData.pretty_cname));
install_pp (make_pp ["UnderData", "cname"] 
                    (Pretty.pprint o UnderData.pretty_cname));


structure DnTab = UnderData.DnTab;
structure CnTab = UnderData.CnTab;
structure EnTab = UnderData.EnTab;
