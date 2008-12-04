(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
(*  Generic notion of And-Or-Datatypes  *)
(* IMPROVE: Allow subtypes of and-types *)
signature DATA = 
sig

(* atomic data names are BName.name *)

(* names for data, constructors and elements *)
type dname = Dn.name; (* datatype names *)
type cname = Cn.name; (* constructors names *)
type ename = En.name; (* record element names *)

(* should have unique constructors, one non-looping
branch, and has been check w.r.t. context (certified). *) 
(* for proposing datatypes *)
(* Ands [] = succeed, Ors [] = fail / invalid *)
datatype datatyp = 
         BType of BName.name
       | Ands of dname EnTab.T (* (ename => sub dname) *)
       | Ors of dname CnTab.T; (* (cname => sub dname) *)

(* helper functions - do not gaurentee correctness of datatype *)
val mk_ors_dtyp : (string * string) list -> datatyp
val mk_ands_dtyp : (string * string) list -> datatyp

(* data with depenendcy / usage information, and can also be a data var *)
type data;

(* datatype context: table/graph of dnames -> datatypes *)
type dcontext;

(* exceptions *)
exception bug_exp of dname * datatyp * dcontext;
exception named_data_is_var_exp of string * dname;
exception data_is_var_exp of string;
exception data_is_not_ors_exp of string;
exception get_exp of dcontext * dname;

(* context with only basic types *)
val mk_base_dcx : (string * BName.name) list -> dcontext
(* val base_dcx : dcontext; *)

val unit_id : string; (* id is an escaped string *) 
val unit_dname : dname;

(* make from string *)
val dname : string -> dname
val cname : string -> cname
val ename : string -> ename

val dname_ord : dname * dname -> General.order
val cname_ord : cname * cname -> General.order
val ename_ord : ename * ename -> General.order

(* contexts examination *)
val lookup : dcontext -> dname -> data option
val get : dcontext -> dname -> data
val lookup_datatyp : dcontext -> dname -> datatyp option
val get_datatyp : dcontext -> dname -> datatyp
val datatyp_of_data : data -> datatyp
val vars_of_dcx : dcontext -> Dn.NSet.T

(* checking and adding new data *)
exception bad_datatype_exp of string;
val certify_datatyp :  dcontext -> dname * datatyp -> bool
val add_datatyp : dname * datatyp -> dcontext -> dcontext
val addfresh_datatyp : dname * datatyp -> dcontext -> dname * dcontext
val del_data : dname -> dcontext -> dcontext

(* val drop_datatype : dname -> dcontext -> dcontext *)


(* parents of dnames *)
val parents_of_data : data -> Dn.NSet.T
val add_dname_subposes_to_ptab : dcontext -> dname 
                                 -> DataPos.Set.T DnTab.T
                                 -> DataPos.Set.T DnTab.T
val mk_ptab : dcontext -> DataPos.Set.T DnTab.T

(* super-types based on or-types *)
type suppos;
structure SupTySet : COLLECTION where type obj = suppos;
val parentn_of_suppos : suppos -> dname
val constr_list_of_suppos : suppos -> cname list
val pretty_suppos : suppos -> Pretty.T
val print_suppos : suppos -> unit
val pretty_supset : SupTySet.T -> Pretty.T
val print_supset : SupTySet.T -> unit

(* data holds all it's super types with it: constant time operation.
   All or-types which we are a member of. *)
val supers_of_data : data -> SupTySet.T;

(* sub-types based on or-types *)
type subpos;
structure SubTySet : COLLECTION where type obj = subpos;
val childn_of_subpos : subpos -> dname
val constr_list_of_subpos : subpos -> cname list
val pretty_subpos : subpos -> Pretty.T
val print_subpos : subpos -> unit
val pretty_subset : SubTySet.T -> Pretty.T
val print_subset : SubTySet.T -> unit

(* inversion with respect to referrer *)
val suppos_of_subpos : dname -> subpos -> suppos
val subpos_of_suppos : dname -> suppos -> subpos

(* positions in and-type *)
type andpos; (* parent name and consturctors to get to and-child *)
structure AndSet : COLLECTION where type obj = andpos;
val parentn_of_andpos : andpos -> dname;
val field_of_andpos : andpos -> ename;
val optsubpos_of_andpos : andpos -> subpos option;
val add_dname_subposes_to_andptab 
    : dcontext -> dname 
      -> AndSet.T DnTab.T
      -> AndSet.T DnTab.T;
val add_andpos_to_andptab : dname -> andpos -> AndSet.T DnTab.T 
                            -> AndSet.T DnTab.T
val mk_andptab : dcontext -> AndSet.T DnTab.T
val pretty_andpos : andpos -> Pretty.T
val print_andpos : andpos -> unit
val pretty_andptab : AndSet.T DnTab.T -> Pretty.T
val print_andptab : AndSet.T DnTab.T -> unit

(* internal use only: adding to set of chached variable names *)
val add_varname : Dn.name -> dcontext -> dcontext;
val add_varnames : Dn.NSet.T -> dcontext -> dcontext;
val del_varname : Dn.name -> dcontext -> dcontext;


(* pretty stuff *)
val pretty_dtyp : datatyp -> Pretty.T
val print_dtyp : datatyp -> unit
val pretty_data : data -> Pretty.T
val print_data : data -> unit
val pretty_full_data : data -> Pretty.T
val print_full_data : data -> unit
val pretty_dname_in_dcx : dcontext -> dname -> Pretty.T
val print_dname_in_dcx : dcontext -> dname -> unit
val pretty_dcx : dcontext -> Pretty.T
val print_dcx : dcontext -> unit

(* val pretty_latex_data : data -> Pretty.T
val print_latex_data : data -> unit *)

end;




(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
(* 
functor DB_DataFUN(
   structure BaseData : BASE_DATA
   structure UnderData : UNDER_DATA) 
*)
structure DB_Data
= struct

(* basic primitive types that exist in an empty context:
BName.name *)


(* handy quantifiers over CnTab and EnTab values *)
fun forall_in_cntab n = 
    List.all n o CnTab.values;
fun exists_in_cntab n = 
    List.exists n o CnTab.values;
fun forall_in_entab n = 
    List.all n o EnTab.values;
fun exists_in_entab n = 
    List.exists n o EnTab.values;

(* names *)
type dname = Dn.name; (* datatype names *)
type cname = Cn.name; (* constructors names *)
type ename = En.name; (* record element names *)

val dname_ord = Dn.name_ord;
val cname_ord = Cn.name_ord;
val ename_ord = En.name_ord;

(* make names from strings *)
val dname = Dn.mk;
val cname = Cn.mk;
val ename = En.mk;

(* equality of names *)
val dname_eq = Dn.name_eq;
val cname_eq = Cn.name_eq;
val ename_eq = En.name_eq;

(* datatypes *)

(* Ands [] = succeed, Ors [] = fail *)
(* for proposing datatypes and examining it *)
datatype datatyp = BType of BName.name
                 | Ands of dname EnTab.T (* (ename => sub dname) *)
                 | Ors of dname CnTab.T; (* (cname => sub dname) *)

(* these rename consturctors and element names as needed to be fresh *)
fun mk_ors_dtyp l = 
    Ors (fold (fn (c,d) => CnTab.doadd (cname c, dname d)) 
         l CnTab.empty) : datatyp;

fun mk_ands_dtyp l = 
    Ands (fold (fn (e,d) => EnTab.doadd (ename e, dname d)) 
          l EnTab.empty) : datatyp;

fun children_of_dtyp (BType _) = []
  | children_of_dtyp (Ands etab) = EnTab.values etab
  | children_of_dtyp (Ors ctab) = CnTab.values ctab;


(* positions in or-data, used for creating super-type sets *)
(* we keep tha path so that we know the consturctor trace to get to
the child type from the parent *)
datatype suppos = SupTyPos of 
         (dname * (* name of a super type of us *)
          (* constructor choices to get from super-type to us *) 
          cname list);
fun parentn_of_suppos (SupTyPos (n, cL)) = n;
fun constr_list_of_suppos (SupTyPos (n, cL)) = cL;
fun inc_suppos c (SupTyPos (d,cL)) = (SupTyPos (d,c::cL));

fun suppos_ord (SupTyPos p, SupTyPos p2) = 
    Library.prod_ord Dn.name_ord 
                     (Library.list_ord Cn.name_ord)
                     (p,p2);
(* an SupTySet is used to hold the set of super types of a type *)
structure SupTySet :> COLLECTION where type obj = suppos
  = CollectionFUN(type key = suppos val ord = suppos_ord); 
fun inc1_suppos_set c = 
  SupTySet.map (fn SupTyPos (d,cL) => SupTyPos (d, c::cL));

(* pretty printing for suppos, supsets and suptabs *)
fun pretty_suppos (SupTyPos (n, cL)) = 
    Pretty.block 
      [Pretty.str "+(", Dn.pretty_name n,
       Pretty.str ", ", 
       Pretty.list "[" "]" (map Cn.pretty_name cL),
       Pretty.str ")"];
fun pretty_supset supset = 
    Pretty.list "{" "}" (map pretty_suppos (SupTySet.dest supset));
val print_suppos = Pretty.writeln o pretty_suppos;
val print_supset = Pretty.writeln o pretty_supset;

(* we keep tha path so that we know the consturctor trace to get to
the sub type from us *)
datatype subpos = SubTyPos of 
         (dname * (* subtype name *)
          (* constructor choices to get from us to sub-type *) 
          cname list);
(* Note: we don't allow the list of constructors to be empty *)
fun childn_of_subpos (SubTyPos (n, cL)) = n;
fun constr_list_of_subpos (SubTyPos (n, cL)) = cL;
fun inc_subpos c (SubTyPos (d,cL)) = (SubTyPos (d,c::cL));

fun subpos_ord (SubTyPos s, SubTyPos s2) = 
    Library.prod_ord Dn.name_ord 
                     (Library.list_ord Cn.name_ord)
                     (s,s2);
(* SubSet is used to hold the set of sub types of a type *)
structure SubTySet :> COLLECTION where type obj = subpos
  = CollectionFUN(type key = subpos val ord = subpos_ord); 
fun inc1_subpos_set c = 
  SubTySet.map (fn SubTyPos (d,cL) => SubTyPos (d, c::cL));
fun inc_subpos_set cL2 = 
  SubTySet.map (fn SubTyPos (d,cL) => SubTyPos (d, cL2 @ cL));

(* pretty printing for subpos, supsets and suptabs *)
fun pretty_subpos (SubTyPos (n, cL)) = 
    Pretty.block 
      [Pretty.str "-(", Dn.pretty_name n,
       Pretty.str ", ", 
       Pretty.list "[" "]" (map Cn.pretty_name cL),
       Pretty.str ")"];
fun pretty_subset sbset = 
    Pretty.list "{" "}" (map pretty_subpos (SubTySet.dest sbset));
val print_subpos = Pretty.writeln o pretty_subpos;
val print_subset = Pretty.writeln o pretty_subset;


fun suppos_of_subpos pn (SubTyPos (dn,l)) = SupTyPos (pn, rev l);
fun subpos_of_suppos pn (SupTyPos (dn,l)) = SubTyPos (pn, rev l);


(* data = datatyp with variables we depend on *)
(* invarient: (subs = {}) | (vardeps <= subs) *)
datatype datatrm = 
         DataVar 
       | DataTyp of {dtyp: datatyp, 
                     vardeps : Dn.NSet.T, (* (cached) vars we refer to *)
                     subs : SubTySet.T (* (recursive/cached) sub-types of us *)
                    };
(* invarient: parents => supers *)
datatype data = 
         Data of {dtrm: datatrm, 
                  parents: Dn.NSet.T, (* (cached) everything we occur in *)
                  supers: SupTySet.T (* (recursive/cached) super-types of us *)
                 };

fun dtrm_of_data (Data rep) = #dtrm rep;
fun supers_of_data (Data rep) = #supers rep;
fun parents_of_data (Data rep) = #parents rep;
fun isvar_of_data d = 
    (case dtrm_of_data d of DataVar => true | _ => false);
exception data_is_var_exp of string;
fun datatyp_of_data d = 
    (case dtrm_of_data d of 
       DataVar => raise data_is_var_exp "datatyp_of_data"
     | DataTyp rep => #dtyp rep);
exception data_is_not_ors_exp of string;
fun subs_of_dtrm DataVar = SubTySet.empty
  | subs_of_dtrm (DataTyp rep) = (#subs rep);
val subs_of_data = subs_of_dtrm o dtrm_of_data;

fun vardeps_of_data d = 
    (case dtrm_of_data d of 
       DataVar => raise data_is_var_exp "vardeps_of_data"
     | DataTyp rep => #vardeps rep);
fun children_of_data d = 
    (case dtrm_of_data d 
      of DataVar => []
       | DataTyp rep => children_of_dtyp (#dtyp rep));
fun data_has_vardep d = 
    (case dtrm_of_data d 
      of DataVar => false
       | DataTyp rep => Dn.NSet.is_empty (#vardeps rep));

fun update_vardeps_in_dtrm f DataVar = 
    raise data_is_var_exp "update_vardeps_in_dtrm"
  | update_vardeps_in_dtrm f (DataTyp rep) = 
    DataTyp {dtyp = #dtyp rep, vardeps = f (#vardeps rep), subs = #subs rep};
fun update_subs_in_dtrm f DataVar = 
    raise data_is_var_exp "update_ors_in_dtrm"
  | update_subs_in_dtrm f (DataTyp rep) = 
    DataTyp {dtyp = #dtyp rep, vardeps = #vardeps rep, subs = f (#subs rep)};

fun update_dtrm_in_data f (Data rep) =
    Data {dtrm = f (#dtrm rep), parents = #parents rep, supers = #supers rep};
val update_vardeps_in_data = 
    update_dtrm_in_data o update_vardeps_in_dtrm;
val remove_vardep_in_data = 
    update_vardeps_in_data o Dn.NSet.delete;
val add_vardep_in_data = 
    update_vardeps_in_data o Dn.NSet.add;

val set_dtrm_in_data = update_dtrm_in_data o K;

fun update_parents_in_data f (Data rep) =
    Data {dtrm = #dtrm rep, parents = f (#parents rep), supers = #supers rep};
(* implicit n and data *)
val add_parent_in_data = update_parents_in_data o Dn.NSet.add;
val del_parent_in_data = update_parents_in_data o Dn.NSet.delete;
val set_parents_in_data = update_parents_in_data o K;

fun update_supers_in_data f (Data rep) =
    Data {dtrm = #dtrm rep, parents = #parents rep, supers = f (#supers rep)};
val del_super_in_data = update_supers_in_data o SupTySet.delete;
val set_supers_in_data = update_supers_in_data o K;

(* only add supers if we are not an and-type *)
fun add_parent_and_supers_in_data (n,supers) (d as Data rep) = 
    Data {dtrm = #dtrm rep, 
          parents = Dn.NSet.add n (#parents rep), 
          supers = SupTySet.union_set supers (#supers rep)};

(* only add subs if we are not an and-type *)
fun add_subs_in_data subs2 d = 
    (case (datatyp_of_data d) of 
       Ands _ => d
     | _ => update_dtrm_in_data 
              (update_subs_in_dtrm (SubTySet.union_set subs2)) d);

(* contains only certified datatype with unique constructors, and one
non-looping case, and where names in the datatype are within the
context they are certified for. *)
(* FIXME/IMPROVE: vars should be table to all data that references them *) 
(* invars: (vars = Dn.NSet.empty) <=> forall data, isvar is false *)
datatype dcontext = DCx of {data: data DnTab.T, vars: Dn.NSet.T};

fun datas_of_dcx (DCx rep) = #data rep;
fun vars_of_dcx (DCx rep) = #vars rep;

fun update_dcx_data f (DCx rep) = 
    DCx {data = f (#data rep), vars = #vars rep};
fun update_dcx_vars f (DCx rep) = 
    DCx {data = #data rep, vars = f (#vars rep)};
val set_dcx_data_entry = update_dcx_data o DnTab.update

val add_varname = update_dcx_vars o Dn.NSet.add;
val add_varnames = update_dcx_vars o Dn.NSet.union_merge;
val del_varname = update_dcx_vars o Dn.NSet.delete;

fun lookup dcx n = DnTab.lookup (datas_of_dcx dcx) n;
fun contains dcx n = 
    (case lookup dcx n of NONE => false | SOME _ => true);
exception get_exp of dcontext * dname;
fun get dcx n = (case lookup dcx n of NONE => raise get_exp (dcx,n)
                                    | SOME v => v);

(* exception to be raised when data name is known, and data is var but should not be. *)
exception named_data_is_var_exp of string * dname;

val get_datatyp = datatyp_of_data oo get; 
fun lookup_datatyp dcx n = 
  case lookup dcx n 
  of NONE => NONE 
   | SOME data => SOME (datatyp_of_data data)
     handle data_is_var_exp _ => 
            raise named_data_is_var_exp ("lookup_datatyp",n);

fun update_dcx_data_entry f n (DCx rep) = 
    DCx {data = DnTab.map_entry f n (#data rep), vars = #vars rep};

fun del_dcx_data_entry n (DCx rep) = 
    DCx {data = DnTab.delete n (#data rep), vars = #vars rep};

fun data_is_var d = 
    (case dtrm_of_data d of DataVar => true | DataTyp rep => false);

fun get_supers dcx n =
    (case lookup dcx n of NONE => SupTySet.empty
                        | SOME d => supers_of_data d);

fun get_subs dcx n =  
    (case lookup dcx n of NONE => SubTySet.empty
                        | SOME d => subs_of_data d);


(* add more parents and supers *)
val add_parent_in_dcx = update_dcx_data_entry o add_parent_in_data;
val del_parent_in_dcx = update_dcx_data_entry o del_parent_in_data;

fun add_parent_and_supers_in_dcx pn_supers n dcx = 
    update_dcx_data_entry (add_parent_and_supers_in_data (pn_supers))
                          n dcx;

fun add_subs_in_dcx subs n dcx = 
    update_dcx_data_entry (add_subs_in_data subs) n dcx;

fun remove_vardep_in_dcx varn n dcx = 
    update_dcx_data_entry (remove_vardep_in_data varn) n dcx;

fun add_vardep_in_dcx varn n dcx = 
    update_dcx_data_entry (add_vardep_in_data varn) n dcx;


(* if not in dcx, raises exception from get *)
fun is_var dc n = isvar_of_data (get dc n);
fun is_ground dc n =  not (is_var dc n);

(* add base data and unit data to context *)
fun mk_basedata (s,dtyp) = 
    (dname s, Data{dtrm = DataTyp {dtyp = BType dtyp, 
                                   vardeps = Dn.NSet.empty,
                                   subs = SubTySet.empty},
                   parents = Dn.NSet.empty,
                   supers = SupTySet.empty});
val unit_id = "unit";
val unit_dname = dname unit_id;
val unit_data = 
    Data{dtrm = DataTyp {dtyp = Ands EnTab.empty, 
                         vardeps = Dn.NSet.empty,
                         subs = SubTySet.empty
                        },
         parents = Dn.NSet.empty,
         supers = SupTySet.empty
        };

fun mk_base_dcx basetype_names = 
    DCx {data = DnTab.empty 
                  |> DnTab.update (unit_dname, unit_data)
                  |> (fold (DnTab.update o mk_basedata)
                           basetype_names),
         vars = Dn.NSet.empty};

(* n has at least one var child *)
fun has_var_child dc n = 
    List.exists (is_var dc) (children_of_data (get dc n));

(* args: dc n : n has a var dep in dc *)
val has_var_dep = data_has_vardep oo get;


(* make new a data entry *)
fun new_data_var dcx = 
    Data{dtrm = DataVar, 
         parents = Dn.NSet.empty, 
         supers = SupTySet.empty
        };
(*          (Dn.NSet.fold (fn pn => Dn.NSet.union_merge (get_supers dcx pn))
               parents Dn.NSet.empty)
*)

(* some sub-branch in "qn" has "n" *)
(* fun is_made_from dc n qn = 
    dname_eq n qn 
    orelse 
    (case get dc qn of 
       (Ands tb) => exists_in_entab (is_made_from dc n) tb
     | (Ors tb) = exists_in_cntab (is_made_from dc n) tb
     | (BType _) = false);
*)

(* IDEA: for improved efficiency of loop checking: cache the number of
non-super-types in each or-choice for each super type. We can then
just check the non-supers integer to see number of non-looping entries
for each supertype. type addition/removal then require updating of
ints as appropriate. Addition and deletion speeds would then depend of
number of entries for the type. *)

(* IDEA2: Even better than above: mark the (recursive) subs, which are
stored with each node, that make a type non-loopy.  *)

(* checks that not-well-founded: every path in "qn" leads to a
"n". Assumes dc is not loopy. Note: the seenset is needed for this to
terminate: else you'll look down all finite data branches *)
fun data_loopy_in dcx n seenset d =  
    if isvar_of_data d then 
       false (* not yet defined, might not loop? *)
    else 
      (case datatyp_of_data d of 
         (Ands tb) => exists_in_entab (dname_loopy_in dcx n seenset) tb
       | (Ors tb) => forall_in_cntab (dname_loopy_in dcx n seenset) tb
       | (BType _) => false)
and dname_loopy_in dcx n seenset qn = 
    (Dn.name_eq (n, qn))
     orelse 
     (not (Dn.NSet.contains seenset qn) (* avoid stuff seen before *)
      andalso
      data_loopy_in dcx n (Dn.NSet.add qn seenset) (get dcx qn));


(* at least one non-looping in n case: well-founded *)
fun is_finite_over dcx loopy_in_n seenset m = 
    not (dname_loopy_in dcx loopy_in_n seenset m);

fun dnames_of_dtyp (BType _) = []
  | dnames_of_dtyp (Ands etb) = EnTab.values etb
  | dnames_of_dtyp (Ors ctb) = CnTab.values ctb;

(* the dname is loopy *)
fun is_loopy dc n = 
    let val data = (get dc n) in
      if data_has_vardep data then false
      else data_loopy_in dc n Dn.NSet.empty data
    end;

exception bad_datatype_exp of string;

(* raise exception if loopy *)
fun assert_is_not_loopy dc n = 
    if is_loopy dc n then 
      raise bad_datatype_exp 
              ("The datatype: " 
               ^ (Pretty.string_of (Dn.pretty_name n)) 
               ^ " is loopy.")
    else ();
(* recursively check ansectors are not loopy, 
   building a table of the checked data to avoid ancestor checking loops *)
fun assert_has_no_loopy_ancestor dc n checked0 = 
    let 
      val parents_list = Dn.NSet.dest (parents_of_data (get dc n))
    in 
      fold (fn pn => fn checked => 
            if Dn.NSet.contains checked pn then 
              checked
            else 
              (let val () = assert_is_not_loopy dc pn; in 
                 assert_has_no_loopy_ancestor dc pn (Dn.NSet.add pn checked)
               end))
           parents_list
           checked0
    end;

exception bug_exp of dname * datatyp * dcontext;

(* raised when a bad datatype is given, or if the datatype name is
   already used. Must have unique constructors, and one non-looping
   case, type names not in the context dc will be made into new
   variable-types. *)
fun add_datatyp (n,md) dcx = 
    let 
      val _ = writeln ("making " ^ (Pretty.string_of (Dn.pretty_name n)))

      (* lookup if an undef entry exists and get the names of typs
      this is used by in order to check that any finished types are
      well-founded *)
      val (supers, parents, wasvar) = 
          (case lookup dcx n of 
            SOME d => 
            if isvar_of_data d 
            then (supers_of_data d, parents_of_data d, true)
            else raise bad_datatype_exp 
                      ("A defined datatype with that name already exists:" 
                       ^ Pretty.string_of (Dn.pretty_name n))
          | NONE => (SupTySet.empty, Dn.NSet.empty, false));

      (* get children dnames and make sure the type is not empty
      THINK/FIXME: I think we may need to disallow/manage specially
      singleton types: these are just renamings. Probably the or-case
      is fine. The And case may need something more. *)
      val childdnames = 
          case md 
           of Ands etab => 
              (* this restriction limits to only 1 unit type *)
              (* an alternative is to allow many unit types *)
              (if EnTab.is_element_empty etab then
                 raise bad_datatype_exp ("empty Ands")
               else EnTab.values etab)
            | Ors ctab => 
              (if CnTab.is_element_empty ctab then
                 raise bad_datatype_exp ("empty Ors")
               else CnTab.values ctab)
            | BType _ => 
              raise bad_datatype_exp "Cannot add base types as new types.";
      
      (* figure out new (variable) and old children *)
      val (new_cdnames,old_cdnames) = 
          List.partition (fn n2 => not (contains dcx n2)
                                   andalso not (Dn.name_eq (n, n2)))
                         childdnames;
          

      (* add new variables for non-existent children *)
      val new_var_data = new_data_var dcx;
      val dcx2 = 
          dcx |> (fold (fn dn => set_dcx_data_entry (dn, new_var_data))
                       new_cdnames)
              |> fold add_varname new_cdnames
              |> (if wasvar then del_varname n else I);
      
      val supers2 = SupTySet.union1 (SupTyPos (n,[])) supers;

      (* make the dtyp and the subs (things this is a sub of) for it *)
      val ((subs, dcx3), dtyp) = 
          case md of Ands etab => 
                     ((SubTySet.empty, 
                       dcx2 |> (EnTab.fold (add_parent_in_dcx n o snd) etab)), 
                      Ands etab)
                   | Ors ctab => 
                     (CnTab.fold 
                        (fn (c,n2) => fn (subs, dcx') => 
                         if (Dn.name_eq (n,n2)) then (subs, dcx')
                         else
                           (subs |> SubTySet.union1 (SubTyPos (n2,[c]))
                                 |> (SubTySet.union_set 
                                       (inc1_subpos_set c (get_subs dcx' n2))), 
                            dcx' |> (add_parent_and_supers_in_dcx
                                       (n, inc1_suppos_set c supers2) n2)
                        ))
                        ctab (SubTySet.empty, dcx2),
                      Ors ctab)
                   | BType _ => 
                     raise bad_datatype_exp "Cannot add base types as new types.";

      (* make the new data object *)
      val data = Data{dtrm = DataTyp {dtyp = dtyp, 
                                      vardeps = Dn.NSet.of_list new_cdnames,
                                      subs = subs},
                      parents = parents,
                      supers = supers
                     };

      (* IMPROVE: use trees instead of consturctor lists, avoid rev op *)
      val dcx4 = 
         dcx3 
           (* add us to the data table *)
           |> set_dcx_data_entry (n, data)
           (* add subs to each super of children *)
           |> (SupTySet.fold
                 (fn SupTyPos (n,cL) => 
                     add_subs_in_dcx (inc_subpos_set (rev cL) subs) n)
                 supers);

      (* remove any var-deps from parents *)
      val dcx5 = if wasvar then 
                  Dn.NSet.fold (remove_vardep_in_dcx n) parents dcx4
                else dcx4;

      (* check no loops are introduced: will raise exception on 
         creation of loopy data *)
      val _ = assert_is_not_loopy dcx5 n;
      val _ = assert_has_no_loopy_ancestor dcx5 n (Dn.NSet.of_list [n]);
    in dcx5 end
    handle bad_datatype_exp s => raise bad_datatype_exp s
         | _ => raise bug_exp (n,md,dcx);


fun addfresh_datatyp (n,md) dcx = 
    let val n2 = Dn.NSet.new n (DnTab.get_names (datas_of_dcx dcx))
    in (n2, add_datatyp (n2, md) dcx) end

(* remove a references to "n" as a parent of the sub-datatypes of the
   dtyp from the datacontext. *)
fun remove_dtyp_childrens_parent_ref n (BType _) dcx = dcx
  | remove_dtyp_childrens_parent_ref n (Ands etab) dcx = 
    En.NTab.fold (del_parent_in_dcx n o snd) etab dcx
  | remove_dtyp_childrens_parent_ref n (Ors ctab) dcx = 
    Cn.NTab.fold (del_parent_in_dcx n o snd) ctab dcx;

(* remove all entries to suppos's from superset of data entry for n,
   if n is var and no longer has any parents, it is removed. *)
fun remove_from_supset_refs suppos n dcx = 
    let val d = get dcx n
        val d2 = d |> del_super_in_data suppos
    in 
      case dtrm_of_data d2
       of DataVar => (* if var *)
          (* and no more parents remove entry *)
          if Dn.NSet.is_empty (parents_of_data d2) then 
            update_dcx_data (DnTab.delete n) dcx
          else (* else just update data *)
            set_dcx_data_entry (n,d2) dcx
        (* if not var, update data. *)
        | _ => set_dcx_data_entry (n,d2) dcx
    end;

(* delete a data entry; make it a var if it's referenced from other
   places. Also remove any children vars that have no other parent *)
fun del_data n dcx =
    let val d = get dcx n;
        val parents = parents_of_data d;
    in case dtrm_of_data d 
        of DataVar => 
           if Dn.NSet.is_empty parents then 
             update_dcx_data (DnTab.delete n) dcx
           else dcx (* nothing to do, already var (deleted) ignore *)
         | DataTyp rep => 
           let 
             val subs = (#subs rep);
             val vardeps = (#vardeps rep);
           in
             dcx 
               (* start by making it a var, no, so it has no further child deps *)
               |> update_dcx_data_entry (set_dtrm_in_data DataVar) n
               (* remove parent and supertype refs for 
                  all subtypes (of or-types) who have n as a parent.
                  (If the subtype is a var has no other supertypes, 
                   it's also removed) *)
               |> (SubTySet.fold (fn subpos => remove_from_supset_refs 
                                                  (suppos_of_subpos n subpos)
                                                  (childn_of_subpos subpos))
                                 (#subs rep))
               (* remove any references to us as a parent - 
                  vars are the parents of no one! *)
               |> remove_dtyp_childrens_parent_ref n (#dtyp rep)
               |> (if Dn.NSet.is_empty parents then 
                     (* if we now have no parents, remove it *)
                     update_dcx_data (DnTab.delete n)
                   else (* else note that it's a var *)
                     add_varname n o
                     Dn.NSet.fold (add_vardep_in_dcx n) parents
                  )
           end
    end;


fun certify_datatyp dc md = 
    (add_datatyp md dc; true) handle bad_datatype_exp _ => false;

(* val drop_datatype = DnTab.delete; *)

(* add a dpos to a position set table *)
fun add_pos_to_ptab n dpos ptab = 
    let 
      val newpset = 
          (case DnTab.lookup ptab n 
            of SOME oldpset => 
               DataPos.Set.union1 dpos oldpset
             | NONE => DataPos.Set.of_list [dpos])
    in DnTab.update (n,newpset) ptab end

(* locations in a datatype *)
fun add_dname_subposes_to_ptab dcx n ptab =
    let 
      fun dtypf (BType _) dptab = ptab
        | dtypf (Ands etab) ptab = 
          let val enames = EnTab.get_names etab in
            EnTab.fold
              (fn (e2,n2) => 
                  add_pos_to_ptab n2 (DataPos.mk_atands (n, (e2,enames))))
              etab ptab
          end
        | dtypf (Ors ctab) ptab = 
          let val cnames = CnTab.get_names ctab in 
            CnTab.fold
              (fn (c2,n2) => 
                  add_pos_to_ptab n2 (DataPos.mk_ators (n, (c2,cnames))))
              ctab ptab
          end
      val dtyp = datatyp_of_data (get dcx n)
                 handle data_is_var_exp _ => 
                        raise named_data_is_var_exp ("add_dname_subposes_to_ptab",n)
    in dtypf dtyp ptab end;

fun mk_ptab dcx =
    fold (add_dname_subposes_to_ptab dcx) 
         (DnTab.keys (datas_of_dcx dcx)) DnTab.empty;

(* position in an and datatype *)
datatype andpos = AndPos of 
         ( (* parent name *) 
           (dname *  
           (* field to get form parent to child *) 
           ename) * 
           (* optionally, we are a subtype in andposition of supertype *)
           subpos option 
         );
fun parentn_of_andpos (AndPos ((n, e), s)) = n;
fun field_of_andpos (AndPos ((n, e), s)) = e;
fun optsubpos_of_andpos (AndPos ((n, e), s)) = s;

fun pretty_andpos (AndPos ((parent_dname, ename), subpos)) = 
    Pretty.block 
      ([Pretty.str "p_dname:",
        Dn.pretty_name parent_dname, 
        Pretty.str "; ename: ",
        En.pretty_name ename]
       @ (case subpos 
           of NONE => []
            | SOME p => 
              [Pretty.str "; subpos: ",
               pretty_subpos p]));

val print_andpos = Pretty.writeln o pretty_andpos;


(* an ordering on orpos: not don't need to consider path, just parent
name and path and constructor: if these are the same, then the path
must also be the same. *)
fun andpos_ord (AndPos p, AndPos p2) = 
    Library.prod_ord
      (Library.prod_ord Dn.name_ord En.name_ord)
      (Library.option_ord subpos_ord)
      (p,p2);

(* an AndSet is used to hold the set of positions of a type in some
parent type. *)
structure AndSet :> COLLECTION where type obj = andpos
  = CollectionFUN(type key = andpos val ord = andpos_ord); 


(* add a andpos to a table going from types to andpos they can occur in *)
fun add_andpos_to_andptab n andpos ptab = 
    let 
      val newpset = 
          (case DnTab.lookup ptab n 
            of SOME oldpset => 
               AndSet.union1 andpos oldpset
             | NONE => AndSet.of_list [andpos])
    in DnTab.update (n,newpset) ptab end;



fun add_dname_subposes_to_andptab dcx n andtab =
    let 
      fun dtypf (BType _) andtab = andtab
        | dtypf (Ands etab) andtab = 
          let val enames = EnTab.get_names etab in
            EnTab.fold
              (fn (e2,n2) => 
                  SubTySet.fold (* add for each subtype *)
                    (fn sbpos => 
                        add_andpos_to_andptab 
                          (childn_of_subpos sbpos) 
                          (AndPos ((n, e2), SOME sbpos)))
                    (get_subs dcx n2)
                    (* add this as direct sub-and type (n2) *)
                  o (add_andpos_to_andptab n2 (AndPos ((n, e2), NONE)))
              )
              etab andtab
          end
        | dtypf (Ors ctab) andtab = andtab
      val dtyp = datatyp_of_data (get dcx n)
          handle data_is_var_exp _ => 
                 raise named_data_is_var_exp ("add_dname_subposes_to_andptab",n)
    in dtypf dtyp andtab end;

(* andptab is used to hold all the locations in and data types that a
datatype can occur in. *) 
fun mk_andptab dcx =
    fold (add_dname_subposes_to_andptab dcx) 
         (DnTab.keys (datas_of_dcx dcx)) DnTab.empty;


(* pretty printing for andpos, andsets and andptabs *)
fun pretty_andpos (AndPos ((parentn, e),NONE)) = 
    Pretty.block 
      [Pretty.str "*(", Dn.pretty_name parentn,
       Pretty.str ", ", 
       En.pretty_name e,
       Pretty.str ")"]
  | pretty_andpos (AndPos ((parentn, e),SOME subpos)) = 
    Pretty.block 
      [Pretty.str "*(", Dn.pretty_name parentn,
       Pretty.str ", ", 
       En.pretty_name e,
       Pretty.str " : ",
       Dn.pretty_name (childn_of_subpos subpos),
       Pretty.str ")"];

fun pretty_andset andset = 
    Pretty.list "{" "}" (map pretty_andpos (AndSet.dest andset));

fun pretty_andptab andptab = 
    Pretty.chunks 
      (map (fn (n,andset) => 
               Pretty.block
                 [Dn.pretty_name n,
                  Pretty.str " <= ",
                  pretty_andset andset]) 
           (DnTab.dest andptab));

val print_andptab = Pretty.writeln o pretty_andptab;


(* mapfromsnd f [x1, x2, ..., xn]  ===>  [x1, f x2, ..., f xn] *)
fun mapfromsnd f (x :: (xs as _ :: _)) = x :: map f xs
  | mapfromsnd f xs = xs;

(* mapfromsnd f [x1, x2, ..., xn]  ===>  [x1, f x2, ..., f xn] *)
fun apfst_mapfromsnd f1 f2 (x :: (xs as _ :: _)) = f1 x :: map f2 xs
  | apfst_mapfromsnd f1 f2 [a] = [f1 a]
  | apfst_mapfromsnd f1 f2 [] = [];

(* pretty stuff *)
fun pretty_dtyp (BType btype) = 
    Pretty.block [Pretty.str " = BType(", BName.pretty_name btype,
                  Pretty.str ")"]
  | pretty_dtyp (Ands tb) = 
    Pretty.chunks (apfst_mapfromsnd 
                     (fn p => Pretty.block [Pretty.str " = ", p])
                     (fn p => Pretty.block [Pretty.str " * ", p])
                     (map (fn (e,d) => Pretty.block 
                                         [En.pretty_name e,
                                          Pretty.str " : ", 
                                          Dn.pretty_name d])
                          (rev (EnTab.dest tb))))
  | pretty_dtyp (Ors tb) = 
    Pretty.chunks (apfst_mapfromsnd 
                     (fn p => Pretty.block [Pretty.str " = ", p])
                     (fn p => Pretty.block [Pretty.str " | ", p])
                     (map (fn (e,d) => Pretty.block 
                                         [Cn.pretty_name e,
                                          Pretty.str " of ", 
                                          Dn.pretty_name d])  
                         (rev (CnTab.dest tb))));

val print_dtyp = Pretty.writeln o pretty_dtyp;

fun pretty_full_data d = 
    let 
      val supers = supers_of_data d
      val subs = subs_of_data d
      val dtrm = dtrm_of_data d
      val parents = parents_of_data d
    in 
      Pretty.chunks 
        ((if isvar_of_data d then [Pretty.str " ?"]
          else 
            [pretty_dtyp (datatyp_of_data d)]
            @ [Pretty.block 
                 [Pretty.str "vardeps: ",
                  Pretty.list 
                    "[" "]" (map Dn.pretty_name 
                                 (Dn.NSet.dest 
                                    (vardeps_of_data d)))]])
         @ [Pretty.block 
              [Pretty.str "parents: ",
               Pretty.list 
                 "[" "]" (map Dn.pretty_name 
                              (Dn.NSet.dest parents))]]
         @ [Pretty.block 
                 [Pretty.str "supers: ",
                  pretty_supset supers]]
         @ [Pretty.block 
                 [Pretty.str "subs: ",
                  pretty_subset subs]]
        )
    end;


fun pretty_data d = 
    let 
      val dtrm = dtrm_of_data d
      val parents = parents_of_data d
    in 
      Pretty.chunks 
        ((if isvar_of_data d then [Pretty.str " = ?"]
          else 
            [pretty_dtyp (datatyp_of_data d)]
            @ [Pretty.block 
                 [Pretty.str "vardeps: ",
                  Pretty.list 
                    "[" "]" (map Dn.pretty_name 
                                 (Dn.NSet.dest 
                                    (vardeps_of_data d)))]])
         @ [Pretty.block 
              [Pretty.str "parents: ",
               Pretty.list 
                 "[" "]" (map Dn.pretty_name 
                              (Dn.NSet.dest parents))]]
        )
    end;
    
val print_data = Pretty.writeln o pretty_data;
val print_full_data = Pretty.writeln o pretty_full_data;

fun pretty_dname_in_dcx dc n = 
    Pretty.chunks 
      [Dn.pretty_name n,
       Pretty.indent 2 (Pretty.block [pretty_data (get dc n)])];

val print_dname_in_dcx = Pretty.writeln oo pretty_dname_in_dcx;



fun pretty_dcx dc = 
    Pretty.chunks 
    [Pretty.block 
       [Pretty.str "DataContext;",
        Pretty.str "(with Vars:", 
        Dn.NSet.pretty (vars_of_dcx dc),
        Pretty.str ") { "],
     Pretty.indent 
       2 (Pretty.chunks
            (map (pretty_dname_in_dcx dc) (DnTab.keys (datas_of_dcx dc)))),
     Pretty.str "}"];
val print_dcx = Pretty.writeln o pretty_dcx;




(* 
val pretty_latex_data : data -> Pretty.T
val print_latex_data : data -> unit
*)


fun each_pos_in_ands f dt = 
    List.filter (f o snd) (CnTab.dest dt)

fun each_pos_in_ors f dt = 
    List.filter (f o snd) (EnTab.dest dt)

(*     
fun deriv_wrt x (Type name) = if dname_eq x name then NONE else SOME
  | deriv_wrt x (Ands and_table) = 
    Ands (fold 
            (new_and_case_constructor x and_table))
            (each_pos_in_ands (rec_in x) and_table)
            CnTab.empty
  | deriv_wrt x (Ors (or_table, subs)) = 
    Ands (fold 
            (new_or_case_constructor x and_table))
            (each_pos_in_ors (rec_in x) or_table)
            CnTab.empty

and new_and_case_constructor x and_table (n, subdata) dand_table =
    (case deriv_wrt x subdata 
      of NONE => 
         CnTab.new (n, CnTab.remove n and_table) 
                   d_and_table
       | SOME d_subdata => 
         CnTab.new (n, CnTab.replace (n, d_subdata) and_table)
                   d_and_table)
and new_or_case_constructor x and_table (n, subdata) dand_table
*)

end;


structure Data : DATA = DB_Data;
