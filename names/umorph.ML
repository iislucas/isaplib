(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  unification name morphisms: morphisms of data that respect/construct an 
    injective endomorphism on names  *) 
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
signature ORD_DATA
= sig
  type data
  val pretty_data : data -> Pretty.T
  val print_data : data -> unit
  val data_ord : data * data -> General.order
  val data_eq : data * data -> bool
end;

signature UNIFIABLE_DATA
= sig
  include ORD_DATA
  type subst (* substitution within data *)
  val empty_subst : subst
  val unify_data : data * data -> subst -> subst option
  val match_data : data * data -> subst -> subst option
  val subst_in_data : subst -> data -> data

  (* like f o g; this should just composes substitutions *)
  val compose_subst : subst * subst -> subst

  val pretty_subst : subst -> Pretty.T
  val print_subst : subst -> unit
end;

(* to simplify sharing constraints *)
signature NAME_AND_DATA_SHARING =
sig
  include NAMES_SHARING
  type data
  structure Map : NAME_MAP_SHARING
  structure InjEndo : NAME_INJ_SHARING
  sharing InjEndo.Dom = InjEndo.Cod
  sharing Map.Dom = InjEndo.Dom
  sharing Map.Cod = InjEndo.Dom
  
  (* it would be nice to have a syntax for 'this' sturcture, 
     save myself the lines below... I guess resolving recursion would need a little thought. 
     This issue is a bit deeper really: we need a language to specify signatures and their relations sensibly. 
  
     Below is essentially: "this.NAMES_SHARING = Map.Dom"
  *)
  sharing type name = Map.Dom.name;
  sharing type table = Map.Dom.table;
  sharing type nset = Map.Dom.nset
  sharing type ntab = Map.Dom.ntab
  sharing type renaming = Map.Dom.renaming;

end;

signature NAME_AND_UNIFIABLE_DATA_SHARING =
sig
  include NAME_AND_DATA_SHARING
  type subst
end;

signature NAME_AND_DATA
= sig
  include SSTR_NAMES
  include ORD_DATA
  val default_data : data
  structure SharingWithData : NAME_AND_DATA_SHARING where type data = data
  sharing Sharing = SharingWithData;
  structure Map : NAME_MAP;
  sharing SharingWithData.Map = Map.Sharing;
  structure InjEndo : NAME_INJENDO;
  sharing SharingWithData.InjEndo = InjEndo.Sharing;
end;

signature NAME_AND_UNIFIABLE_DATA
= sig
  include SSTR_NAMES
  include UNIFIABLE_DATA
  val default_data : data
  structure SharingWithData : NAME_AND_UNIFIABLE_DATA_SHARING where type data = data
  sharing Sharing = SharingWithData;
  structure Map : NAME_MAP;
  sharing SharingWithData.Map = Map.Sharing;
  structure InjEndo : NAME_INJENDO;
  sharing SharingWithData.InjEndo = InjEndo.Sharing;
end;


signature UMORPH
= sig
  type T (* unification morphism *)

  (* Entity: internal names and their data *)
  structure Entity : NAME_AND_UNIFIABLE_DATA
  
  (* get unification (vertex & edge) data associated with graph automorphism *)
  val get_subst : T -> Entity.subst
  
  (* morphism of variables in internal data *)
  val get_injendo : T -> Entity.InjEndo.T
  
  (* for alpha-equivalance/isomorphism checking *)
  (* try to add mapping,  *)
  val try_unify : Entity.name * Entity.data
                    -> Entity.name * Entity.data
                    -> T (* renaming so far *)
                    -> T option (* new name and extended renaming *)

  val try_match : Entity.name * Entity.data
                    -> Entity.name * Entity.data
                    -> T (* renaming so far *)
                    -> T option (* new name and extended renaming *)

  exception subst_exp of unit
  (* as above, but raises exception *)
  val unify : Entity.name * Entity.data
              -> Entity.name * Entity.data 
              -> T -> T
  (* as above, but raises exception *)
  val match : Entity.name * Entity.data
              -> Entity.name * Entity.data 
              -> T -> T

end;

functor UmorphFun(Entity : NAME_AND_UNIFIABLE_DATA) : UMORPH
= struct

  structure Entity = Entity;

  datatype T = UMorph of Entity.subst * Entity.InjEndo.T;
  
  fun get_subst (UMorph (u,m)) = u;
  fun get_injendo (UMorph (u,m)) = m;
  fun update_subst f (UMorph (u,m)) = UMorph (f u,m);
  fun update_injendo f (UMorph (u,m)) = UMorph (u,f m);
  val set_subst = update_subst o K;
  val set_injendo = update_injendo o K;
  
  fun try_unify (n1,d1) (n2,d2) um = 
      (case Entity.InjEndo.try_change1 n1 n2 (get_injendo um) of
         NONE    => NONE
       | SOME cn => (case Entity.unify_data (d1,d2) (get_subst um) of
                       NONE    => NONE
                     | SOME un => SOME (UMorph (un, cn))));

  fun try_match (n1,d1) (n2,d2) um = 
      (case Entity.InjEndo.try_change1 n1 n2 (get_injendo um) of
         NONE    => NONE
       | SOME cn => (case Entity.match_data (d1,d2) (get_subst um) of
                       NONE    => NONE
                     | SOME un => SOME (UMorph (un, cn))));

  exception subst_exp of unit

  fun unify (nd1 as (n1,d1)) (nd2 as (n2,d2)) cn =
    (case try_unify nd1 nd2 cn of 
       NONE => raise subst_exp ()
     | SOME cn2 => cn2);

  fun match (nd1 as (n1,d1)) (nd2 as (n2,d2)) cn =
    (case try_match nd1 nd2 cn of 
       NONE => raise subst_exp ()
     | SOME cn2 => cn2);
                            
end;