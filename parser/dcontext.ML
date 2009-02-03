(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
(*  Generic notion of And-Or-Datatype Contexts  *)
signature DCONTEXT = 
sig

(* data with depenendcy / usage information, and can also be a data var *)
type data;

(* datatype context: table/graph of dnames -> datatypes *)
type T;

(* should have unique constructors, one non-looping
branch, and has been check w.r.t. context (certified). *) 
(* for proposing datatypes *)
(* Ands [] = succeed, Ors [] = fail / invalid *)
datatype datatyp = 
         Free of Dn.name
       | Ands of An.NSet.T (* Set of elements *)
       | Ors of An.NSet.T; (* Set of constructors *)

(* helper functions - do not gaurentee correctness of datatype *)
val mk_ors_dtyp : T -> string list -> datatyp
val mk_ands_dtyp : T -> string list -> datatyp

(* context with these free/base types *)
val mk_base_dcx : Dn.name list -> T

val unit_id : string; (* id is an escaped string *) 
val unit_dname : dname;

(* contexts examination *)
val lookup : T -> dname -> data option
val get : T -> dname -> data

(* checking and adding new data *)
exception bad_datatype_exp of string;
val certify :  T -> dname * datatyp -> bool
val add : dname * datatyp -> T -> T
val addfresh : dname * datatyp -> T -> dname * T
val del : dname -> T -> T
val abstract_to_free : dname -> T -> T

(* internal use only: adding to set of chached variable names *)
val add_freename : Dn.name -> T -> T;
val add_freenames : Dn.NSet.T -> T -> T;
val del_freename : Dn.name -> T -> T;

(* parents of dnames *)
val all_parents_of_data : data -> Dn.NSet.T
val and_parents_of_data : data -> En.name Dn.NTab.T
val add_dname_subposes_to_ptab : T -> dname 
                                 -> DataPos.Set.T DnTab.T
                                 -> DataPos.Set.T DnTab.T
val mk_ptab : T -> DataPos.Set.T DnTab.T

(* All or-types which we are a member of. *)
val supers_of_data : data -> SupPathSet.T Dn.NTab.T;

(* inversion with respect to referrer *)
(* 
val suppos_of_subpos : dname -> subpos -> suppos
val subpos_of_suppos : dname -> suppos -> subpos
*)

(* positions in and-type *)
type andpos; (* parent name and consturctors to get to and-child *)
val mk_andptab : T -> AndSet.T DnTab.T
val pretty_andpos : andpos -> Pretty.T
val print_andpos : andpos -> unit
val pretty_andptab : AndSet.T DnTab.T -> Pretty.T
val print_andptab : AndSet.T DnTab.T -> unit

(* pretty stuff *)
val pretty_data : data -> Pretty.T
val print_data : data -> unit
val pretty_full_data : data -> Pretty.T
val print_full_data : data -> unit
val pretty_dname_in_dcx : T -> dname -> Pretty.T
val print_dname_in_dcx : T -> dname -> unit
val pretty_dcx : T -> Pretty.T
val print_dcx : T -> unit

(* val pretty_latex_data : data -> Pretty.T
val print_latex_data : data -> unit *)

end;




(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
(* 
functor DB_DataFUN(
   structure BaseData : BASE_DATA
   structure UnderData : UNDER_DATA) 
*)

(* Note: depend on property of naming that new names from same
starting point are greater than or equal to starting point. -- order
is important for parsing and-types. *)
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

(* Ands [] = succeed, Ors [] = fail *)
(* for proposing datatypes and examining it *)

(* Note: we use ordering on En and Cn to define ordering of
constructors/elements - these are internal names; the automorphism is
the nice name for it *)
datatype datatyp = Free of unit (* *)
                 | Ands of En.NSet.T (* elements internal name *)
                           * EnAuto.T (* internal name -> nice name *)
                 | Ors of Cn.NSet.T (* constructors *)
                          * CnAuto.T; (* internal name -> nice name *) 

(* these rename consturctors and element names as needed to be fresh *)
fun mk_ors_dtyp dcx l =
    get_cnameset dcx
    Ors (fold (fn (c,d) => CnTab.doadd (Cn.mk c, Dn.mk d)) 
         l CnTab.empty) : datatyp;
fun mk_ands_dtyp dcx l = 
    Ands (fold (fn (e,d) => EnTab.doadd (En.mk e, Dn.mk d)) 
          l EnTab.empty) : datatyp;

(* 
fun try_rename1_in_datatrm n1 n2 DataVar = (NONE, DataVar)
  | try_rename1_in_datatrm n1 n2 (DataTyp rep) =   
*)

(* invarient: parents => supers *)
datatype data = datatyp;

fun isfree_of_data (Free _) => true 
  | isfree_of_data _ = false;


structure CMap = NameMapFun(structure Dom = Cn and Cod = Dn);
structure EMap = NameMapFun(structure Dom = En and Cod = Dn);

(* contains only certified datatype with unique constructors, and one
non-looping case, and where names in the datatype are within the
context they are certified for. *)
(* invars: (frees = Dn.NSet.empty) <=> forall data, isfree is false *)
datatype T = DCx of 
         {dtyps : data DnTab.T, 
          emap : EMap.T,
          cmap : CMap.T,
          (* vars -> dnames using them *)
          frees : Dn.NSet.T
         };

fun try_rename1 n1 n2 (DCx rep) =
    DCx { dtyps = Dn.NTab.try_rename1 n1 n2 (#dtyps rep), 
          emap = EMap.try_rename1_cod (#emap rep),
          cmap = CMap.try_rename1_cod (#cmap rep),
          frees = Dn.NSet.try_rename1 (#frees rep) };
fun get_dnset (DCx rep) = DnTab.get_nameset (#dtyps rep);

structure CR = CompoundRenamingFun(
structure Nm : Dn
type obj = T
val get_nameset = get_dnset (* get set of all names *)
val try_rename1 = try_rename1 (* rename 1 name *)
end);
open CR;
(* 
adds: 
val rename : renaming -> obj -> renaming * obj
val mk_renaming : obj -> renaming -> renaming
val do_renaming : renaming -> obj -> obj
*)

fun get_dtyps (DCx rep) = #dtyps rep;
fun get_frees (DCx rep) = #frees rep;
fun get_cmap (DCx rep) = #cmap rep;
fun get_emap (DCx rep) = #emap rep;

fun update_dtyps f (DCx rep) = 
    DCx { dtyps = f (#dtyps rep), 
          emap = #emap rep,
          cmap = #cmap rep,
          frees = #frees rep };

(* internal manipulation of dtyps *)
(* implicit args: f n dcx *)
val update_dtyp_entry = update_dtyps oo Dn.NTab.map_entry;
(* implicit args: n dcx *)
val del_dtyp_entry = update_dtyps o Dn.NTab.delete;
(* implicit args: data n dcx *)
val set_dtyp_entry = update_dtyps o update_dtyp_entry o K;


fun update_frees f (DCx rep) = 
    DCx {dtyps = #dtyps rep, 
         emap = #emap rep,
         cmap = #cmap rep,         
         frees = f (#frees rep)};

val add_freename = update_frees o Dn.NSet.add;
val add_freenames = update_frees o Dn.NSet.union_merge;
val del_freename = update_frees o Dn.NSet.delete;

(* lookup in dtyps *)
fun lookup dcx n = DnTab.lookup (get_dtyps dcx) n;
fun contains dcx n = Dn.NSet.contains (get_dnset dcx) n;

exception get_exp of T * dname;
fun get dcx n = (case lookup dcx n of NONE => raise get_exp (dcx,n)
                                    | SOME v => v);

val is_free = Dn.NSet.contains d o get_frees 
fun is_ground = 

(* add base data and unit data to context *)
val unit_id = "unit";
val unit_dname = Dn.mk unit_id;
val unit_data = 
    Data{dtyp = DataTyp {dtyp = Ands EnTab.empty, 
                         freedeps = Dn.NSet.empty,
                         subs = SubPathSet.empty
                        },
         parents = Dn.NSet.empty,
         supers = SupPathSet.empty
        };

fun mk_base_dcx basetype_names = 
    DCx {data = DnTab.empty 
                  |> DnTab.update (unit_dname, Free())
                  |> (fold (DnTab.update o mk_basedata)
                           basetype_names),
         frees = Dn.NSet.empty};

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
    if isfree_of_data d then 
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
      if data_has_freedep data then false
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
      val parents_list = Dn.NSet.list_of (parents_of_data (get dc n))
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

exception bug_exp of dname * datatyp * T;

(* raised when a bad datatype is given, or if the datatype name is
   already used. Must have unique constructors, and one non-looping
   case, type names not in the context dc will be made into new
   freeiable-types. *)
fun add_datatyp (n,md) dcx = 
    let 
      (* val _ = writeln ("making " ^ (Pretty.string_of (Dn.pretty_name n))) *)

      (* lookup if an undef entry exists and get the names of typs
      this is used by in order to check that any finished types are
      well-founded *)
      val (supers, parents, wasfree) = 
          (case lookup dcx n of 
            SOME d => 
            if isfree_of_data d 
            then (supers_of_data d, parents_of_data d, true)
            else raise bad_datatype_exp 
                      ("A defined datatype with that name already exists:" 
                       ^ Pretty.string_of (Dn.pretty_name n))
          | NONE => (SupPathSet.empty, Dn.NSet.empty, false));

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
      
      (* figure out new (freeiable) and old children *)
      val (new_cdnames,old_cdnames) = 
          List.partition (fn n2 => not (contains dcx n2)
                                   andalso not (Dn.name_eq (n, n2)))
                         childdnames;
          

      (* add new freeiables for non-existent children *)
      val new_free_data = new_data_free dcx;
      val dcx2 = 
          dcx |> (fold (fn dn => set_dcx_data_entry (dn, new_free_data))
                       new_cdnames)
              |> fold add_freename new_cdnames
              |> (if wasfree then del_freename n else I);
      
      val supers2 = SupPathSet.union1 (SupTyPos (n,[])) supers;

      (* make the dtyp and the subs (things this is a sub of) for it *)
      val ((subs, dcx3), dtyp) = 
          case md of Ands etab => 
                     ((SubPathSet.empty, 
                       dcx2 |> (EnTab.fold (add_parent_in_dcx n o snd) etab)), 
                      Ands etab)
                   | Ors ctab => 
                     (CnTab.fold 
                        (fn (c,n2) => fn (subs, dcx') => 
                         if (Dn.name_eq (n,n2)) then (subs, dcx')
                         else
                           (subs |> SubPathSet.union1 (SubTyPath (n2,[c]))
                                 |> (SubPathSet.union_set 
                                       (inc1_subpos_set c (get_subs dcx' n2))), 
                            dcx' |> (add_parent_and_supers_in_dcx
                                       (n, inc1_suppos_set c supers2) n2)
                        ))
                        ctab (SubPathSet.empty, dcx2),
                      Ors ctab)
                   | BType _ => 
                     raise bad_datatype_exp "Cannot add base types as new types.";

      (* make the new data object *)
      val data = Data{dtyp = DataTyp {dtyp = dtyp, 
                                      freedeps = Dn.NSet.of_list new_cdnames,
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
           |> (SupPathSet.fold
                 (fn SupTyPos (n,cL) => 
                     add_subs_in_dcx (inc_subpos_set (rev cL) subs) n)
                 supers);

      (* remove any free-deps from parents *)
      val dcx5 = if wasfree then 
                  Dn.NSet.fold (remove_freedep_in_dcx n) parents dcx4
                else dcx4;

      (* check no loops are introduced: will raise exception on 
         creation of loopy data *)
      val _ = assert_is_not_loopy dcx5 n;
      val _ = assert_has_no_loopy_ancestor dcx5 n (Dn.NSet.of_list [n]);
    in dcx5 end
    handle bad_datatype_exp s => raise bad_datatype_exp s
         | _ => raise bug_exp (n,md,dcx);


fun addfresh_datatyp (n,md) dcx = 
    let val n2 = Dn.NSet.new (DnTab.get_nameset (datas_of_dcx dcx)) n
    in (n2, add_datatyp (n2, md) dcx) end

(* remove a references to "n" as a parent of the sub-datatypes of the
   dtyp from the datacontext. *)
fun remove_dtyp_childrens_parent_ref n (BType _) dcx = dcx
  | remove_dtyp_childrens_parent_ref n (Ands etab) dcx = 
    En.NTab.fold (del_parent_in_dcx n o snd) etab dcx
  | remove_dtyp_childrens_parent_ref n (Ors ctab) dcx = 
    Cn.NTab.fold (del_parent_in_dcx n o snd) ctab dcx;

(* remove all entries to suppos's from superset of data entry for n,
   if n is free and no longer has any parents, it is removed. *)
fun remove_from_supset_refs suppos n dcx = 
    let val d = get dcx n
        val d2 = d |> del_super_in_data suppos
    in 
      case dtyp_of_data d2
       of DataFree => (* if free *)
          (* and no more parents remove entry *)
          if Dn.NSet.is_empty (parents_of_data d2) then 
            update_dcx_data (DnTab.delete n) dcx
          else (* else just update data *)
            set_dcx_data_entry (n,d2) dcx
        (* if not free, update data. *)
        | _ => set_dcx_data_entry (n,d2) dcx
    end;

(* delete a data entry; make it a free if it's referenced from other
   places. Also remove any children frees that have no other parent *)
fun del_data n dcx =
    let val d = get dcx n;
        val parents = parents_of_data d;
    in case dtyp_of_data d 
        of DataFree => 
           if Dn.NSet.is_empty parents then 
             update_dcx_data (DnTab.delete n) dcx
           else dcx (* nothing to do, already free (deleted) ignore *)
         | DataTyp rep => 
           let 
             val subs = (#subs rep);
             val freedeps = (#freedeps rep);
           in
             dcx 
               (* start by making it a free, no, so it has no further child deps *)
               |> update_dcx_data_entry (set_dtyp_in_data DataFree) n
               (* remove parent and supertype refs for 
                  all subtypes (of or-types) who have n as a parent.
                  (If the subtype is a free has no other supertypes, 
                   it's also removed) *)
               |> (SubPathSet.fold (fn subpos => remove_from_supset_refs 
                                                  (suppos_of_subpos n subpos)
                                                  (childn_of_subpos subpos))
                                 (#subs rep))
               (* remove any references to us as a parent - 
                  frees are the parents of no one! *)
               |> remove_dtyp_childrens_parent_ref n (#dtyp rep)
               |> (if Dn.NSet.is_empty parents then 
                     (* if we now have no parents, remove it *)
                     update_dcx_data (DnTab.delete n)
                   else (* else note that it's a free *)
                     add_freename n o
                     Dn.NSet.fold (add_freedep_in_dcx n) parents
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
          let val enames = EnTab.get_nameset etab in
            EnTab.fold
              (fn (e2,n2) => 
                  add_pos_to_ptab n2 (DataPos.mk_atands (n, (e2,enames))))
              etab ptab
          end
        | dtypf (Ors ctab) ptab = 
          let val cnames = CnTab.get_nameset ctab in 
            CnTab.fold
              (fn (c2,n2) => 
                  add_pos_to_ptab n2 (DataPos.mk_ators (n, (c2,cnames))))
              ctab ptab
          end
      val dtyp = datatyp_of_data (get dcx n)
                 handle data_is_free_exp _ => 
                        raise named_data_is_free_exp ("add_dname_subposes_to_ptab",n)
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
          let val enames = EnTab.get_nameset etab in
            EnTab.fold
              (fn (e2,n2) => 
                  SubPathSet.fold (* add for each subtype *)
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
          handle data_is_free_exp _ => 
                 raise named_data_is_free_exp ("add_dname_subposes_to_andptab",n)
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
           (DnTab.list_of andptab));

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
                          (rev (EnTab.list_of tb))))
  | pretty_dtyp (Ors tb) = 
    Pretty.chunks (apfst_mapfromsnd 
                     (fn p => Pretty.block [Pretty.str " = ", p])
                     (fn p => Pretty.block [Pretty.str " | ", p])
                     (map (fn (e,d) => Pretty.block 
                                         [Cn.pretty_name e,
                                          Pretty.str " of ", 
                                          Dn.pretty_name d])  
                         (rev (CnTab.list_of tb))));

val print_dtyp = Pretty.writeln o pretty_dtyp;

fun pretty_full_data d = 
    let 
      val supers = supers_of_data d
      val subs = subs_of_data d
      val dtyp = dtyp_of_data d
      val parents = parents_of_data d
    in 
      Pretty.chunks 
        ((if isfree_of_data d then [Pretty.str " ?"]
          else 
            [pretty_dtyp (datatyp_of_data d)]
            @ [Pretty.block 
                 [Pretty.str "freedeps: ",
                  Pretty.list 
                    "[" "]" (map Dn.pretty_name 
                                 (Dn.NSet.list_of 
                                    (freedeps_of_data d)))]])
         @ [Pretty.block 
              [Pretty.str "parents: ",
               Pretty.list 
                 "[" "]" (map Dn.pretty_name 
                              (Dn.NSet.list_of parents))]]
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
      val dtyp = dtyp_of_data d
      val parents = parents_of_data d
    in 
      Pretty.chunks 
        ((if isfree_of_data d then [Pretty.str " = ?"]
          else 
            [pretty_dtyp (datatyp_of_data d)]
            @ [Pretty.block 
                 [Pretty.str "freedeps: ",
                  Pretty.list 
                    "[" "]" (map Dn.pretty_name 
                                 (Dn.NSet.list_of 
                                    (freedeps_of_data d)))]])
         @ [Pretty.block 
              [Pretty.str "parents: ",
               Pretty.list 
                 "[" "]" (map Dn.pretty_name 
                              (Dn.NSet.list_of parents))]]
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
        Pretty.str "(with Frees:", 
        Dn.NSet.pretty (frees_of_dcx dc),
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
    List.filter (f o snd) (CnTab.list_of dt)

fun each_pos_in_ors f dt = 
    List.filter (f o snd) (EnTab.list_of dt)

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
