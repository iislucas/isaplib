(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *)
(*  unification data  *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
(* FIXME/TODO: name/move this file somewhere more appropriate. This has more to
   do with unification. *)
signature ORD_DATA
= sig
  type data
  val pretty_data : data -> Pretty.T
  val print_data : data -> unit
  val data_ord : data * data -> General.order
  val data_eq : data * data -> bool
end;

signature BASIC_SUBST =
sig
  type subst (* substitution within data *)
  val empty_subst : subst
  (* like f o g; this should just composes substitutions *)
  val compose_subst : subst * subst -> subst
  val pretty_subst : subst -> Pretty.T
  val print_subst : subst -> unit
end;

signature UNIFIABLE_DATA_SHARING
= sig
  type data
  type subst
end;

signature UNIFIABLE_DATA
= sig
  include ORD_DATA;
  include BASIC_SUBST;
  val unify_data : data * data -> subst -> subst option
  val match_data : data * data -> subst -> subst option
  val subst_in_data : subst -> data -> data
  val default_data : data

  structure Sharing : UNIFIABLE_DATA_SHARING
  sharing type Sharing.data = data
  sharing type Sharing.subst = subst
end;

(* *)
functor IgnoredUnifiableData(
  structure S : BASIC_SUBST
  structure D : ORD_DATA
  val default_data : D.data)
: UNIFIABLE_DATA
=
struct
  open D;
  open S;
  fun unify_data d s = SOME s;
  fun match_data d s = SOME s;
  fun subst_in_data s d = d;
  val default_data = default_data

  structure Sharing = struct
    type data = data
    type subst = subst
  end;
end;

(* to simplify sharing constraints *)
signature UNIFIABLE_DATA_NAME_SHARING =
sig
  include NAME_SHARING
  structure Map : NAME_MAP_SHARING
  structure InjEndo : NAME_INJ_SHARING
  sharing InjEndo.Dom = InjEndo.Cod
  sharing Map.Dom = InjEndo.Dom
  sharing Map.Cod = InjEndo.Dom

  (* it would be nice to have a syntax for 'this' structure,
     save myself the lines below... I guess resolving recursion would need a little thought.
     This issue is a bit deeper really: we need a language to specify
     signatures and their relations sensibly.

     Below is essentially: "this.NAME_SHARING = Map.Dom"
  *)
  sharing type name = Map.Dom.name;
  sharing type table = Map.Dom.table;
  sharing type nset = Map.Dom.nset
  sharing type ntab = Map.Dom.ntab
  sharing type renaming = Map.Dom.renaming;

end;

(* to simplify sharing constraints *)
signature NAME_AND_DATA_SHARING =
sig
  include UNIFIABLE_DATA_NAME_SHARING;
  type data
end;

signature NAME_AND_UNIFIABLE_DATA_SHARING =
sig
  include NAME_AND_DATA_SHARING
  type subst
end;

(* string based names with maps and inj endos *)
signature SSTR_NAME_AND_MAPS
= sig
  include SSTR_NAME
  structure Map : NAME_MAP;
  structure InjEndo : NAME_INJENDO;
  sharing InjEndo.Dom.Sharing = InjEndo.Cod.Sharing
  sharing Map.Dom.Sharing = InjEndo.Dom.Sharing
  sharing Map.Cod.Sharing = InjEndo.Dom.Sharing
  sharing Sharing = Map.Dom.Sharing;
end;

signature NAME_AND_DATA
= sig
  include SSTR_NAME_AND_MAPS
  include ORD_DATA
  structure SharingWithData : NAME_AND_DATA_SHARING
  where type data = data
  sharing Sharing = SharingWithData;
  (* TODO: push Map and InjEndo into names? generally very useful *)
  sharing SharingWithData.Map = Map.Sharing;
  sharing SharingWithData.InjEndo = InjEndo.Sharing;
end;



structure UnitSubst : BASIC_SUBST =
struct
  type subst = unit (* substitution within data *)
  val empty_subst = ()
  fun compose_subst ((),()) = ()
  fun pretty_subst () = Pretty.str "unit";
  val print_subst = Pretty.writeln o pretty_subst;
end;



(*
PolyML Bug: very srange location of error message!

signature NAME_AND_UNIFIABLE_DATA
= sig
  include SSTR_NAME_AND_MAPS
  include UNIFIABLE_DATA
  val default_data : data
  structure SharingWithData : NAME_AND_UNIFIABLE_DATA_SHARING
    where type data = data and type subst = subst
  structure SharingWithoutData : UNIFIABLE_DATA_NAME_SHARING
    sharing SharingWithoutData = SharingWithData; (* note NOT symmetric *)
  sharing Sharing = SharingWithData;
  sharing SharingWithData.Map = Map.Sharing;
  sharing SharingWithData.InjEndo = InjEndo.Sharing;
end;
*)
