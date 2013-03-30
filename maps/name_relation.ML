(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  Title:      isaplib/maps/name_relation.ML
    Author:     Aleks Kissinger
                aleks0@gmail.com
                Initially written 31 Mar 2013                          *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  DESCRIPTION:
    A generic many-to-many relation for names. All other name-to-name
    mappings inherit this signature.
    
    While NAME_TABLE does not directly inherit this signature, it uses
    the same naming conventions where possible.
*)

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)

signature NAME_RELATION =
sig
  type dom
  type cod
  structure DomSet : NAME_SET sharing type DomSet.name = dom
  structure CodSet : NAME_SET sharing type CodSet.name = cod
  
  (* the internal representation *)
  type rep
  type T = (dom, cod, rep) Map.amap
  val empty: T
  val is_empty : T -> bool
  
  val size : T -> int (* number of mapped pairs (a,b). i.e. max(domsize,codsize) *)
  
  val get_dom_set : DomSet.T (* set of elements in domain *)
  val get_cod_set : CodSet.T (* set of elements in codomain *)
  
  (*** FORWARD LOOKUP FUNCTIONS ***)
  
  (* these will both throw multiple_values_exp if multiple values returned *)
  val get : T -> dom -> cod (* throws no_such_elem_exp if not found *)
  val get_opt : T -> dom -> cod option (* returns NONE if not found *)
  
  (* these will always succeed, but may return an empty set *)
  val img : T -> dom -> CodSet.T
  val img_of_set : T -> DomSet.T -> CodSet.T
  
  (*** INVERSE LOOKUP FUNCTIONS ***)
  
  (* these will both throw multiple_values_exp if multiple values returned *)
  val inv_get : T -> cod -> dom (* throws no_such_elem_exp if not found *)
  val inv_get_opt : T -> cod -> dom option (* returns NONE if not found *)
  
  (* these will always succeed, but may return an empty set *)
  val inv_img : T -> cod -> DomSet.T
  val inv_img_of_set : T -> CodSet.T -> DomSet.T
  
  (*** RELATION LOOKUP ***)
  
  val is_mapped : T -> (dom * cod) -> bool
  
  (*** UPDATERS ***)
  
  (* NOTE: for relations, set = add, but add is preferred *)
  
  (* for single-valued functions, overwrite if dom exists *)
  val set : (dom * cod) -> T -> T
  
  (* for single-valued functions, raise duplicate_exp dom exists *) 
  val add : (dom * cod) -> T -> T
  
  (* delete an element of the domain. no-op if not found *)
  val delete : dom -> T -> T
  
  (* delete an element of the codomain. no-op if not found *)
  val inv_delete : cod -> T -> T
  
  (* remove a single pair from the relation. for single-valued functions, use
     of delete or inv_delete is preferred *)
  val unmap : dom * cod -> T -> T
  
  (*** ABSTRACT MAP FUNCTIONS ***)
  
  (* inherited from Map *)
  val fold : (dom * cod -> 'a -> 'a) -> T -> 'a -> 'a
  val exists : (dom * cod -> bool) -> T -> bool
  val forall : (dom * cod -> bool) -> T -> bool
  val find : (dom * cod -> bool) -> T -> (dom * cod) option
  
  (* composition with other abstract maps *)
  val compose  : ('a,cod,'y) Map.amap * (dom,'a,'x) Map.amap -> T
  
  (* shorthand for compose o (I X compose) *)
  val compose3 : ('b,cod,'z) Map.amap * ('a,'b,'y) Map.amap * (dom,'a,'x) Map.amap -> T
  
  (* try to convert an amap to the given type of relation, exception if unsuccessful. *)
  val coerce : (dom,cod,'x) Map.amap -> T
  
  val pretty : T -> Pretty.T

end