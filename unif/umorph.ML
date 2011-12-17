(* unif data and name morphisms: morphisms of data that respect/construct an 
    injective endomorphism on names *)    
signature UMORPH
= sig
  type T (* unification morphism *)

  (* Entity: internal names and their data *)
  structure EData : UNIFIABLE_DATA
  structure Nm : SSTR_NAME_AND_MAPS
  
  (* get unification (vertex & edge) data associated with graph automorphism *)
  val get_subst : T -> EData.subst
  
  (* morphism of variables in internal data *)
  val get_injendo : T -> Nm.InjEndo.T
  
  (* for alpha-equivalance/isomorphism checking *)
  (* try to add mapping,  *)
  val try_unify : Nm.name * EData.data
                    -> Nm.name * EData.data
                    -> T (* renaming so far *)
                    -> T option (* new name and extended renaming *)

  val try_match : Nm.name * EData.data
                    -> Nm.name * EData.data
                    -> T (* renaming so far *)
                    -> T option (* new name and extended renaming *)

  exception subst_exp of unit
  (* as above, but raises exception *)
  val unify : Nm.name * EData.data
              -> Nm.name * EData.data 
              -> T -> T
  (* as above, but raises exception *)
  val match : Nm.name * EData.data
              -> Nm.name * EData.data 
              -> T -> T

end;


functor UmorphFun(structure Nm : SSTR_NAME_AND_MAPS and EData : UNIFIABLE_DATA) : UMORPH
= struct

  structure Nm = Nm;
  structure EData = EData;

  datatype T = UMorph of EData.subst * Nm.InjEndo.T;
  
  fun get_subst (UMorph (u,m)) = u;
  fun get_injendo (UMorph (u,m)) = m;
  fun update_subst f (UMorph (u,m)) = UMorph (f u,m);
  fun update_injendo f (UMorph (u,m)) = UMorph (u,f m);
  val set_subst = update_subst o K;
  val set_injendo = update_injendo o K;
  
  fun try_unify (n1,d1) (n2,d2) um = 
      (case Nm.InjEndo.try_change1 n1 n2 (get_injendo um) of
         NONE    => NONE
       | SOME cn => (case EData.unify_data (d1,d2) (get_subst um) of
                       NONE    => NONE
                     | SOME un => SOME (UMorph (un, cn))));

  fun try_match (n1,d1) (n2,d2) um = 
      (case Nm.InjEndo.try_change1 n1 n2 (get_injendo um) of
         NONE    => NONE
       | SOME cn => (case EData.match_data (d1,d2) (get_subst um) of
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