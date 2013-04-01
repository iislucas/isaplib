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
  
  (* the main type. To enable coercions, this should be
     (dom,cod,rep) Map.amap and you should use transparent
     ascription *)
  type T(* = (dom, cod, rep) Map.amap*)
  val empty: T
  val is_empty : T -> bool
  
  val get_dom_set : T -> DomSet.T (* set of elements in domain *)
  val get_cod_set : T -> CodSet.T (* set of elements in codomain *)
  
  val dom_size : T -> int (* cardinality(dom_set) *)
  val cod_size : T -> int (* cardinality(cod_set) *)
  
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


functor NameRelation(
  structure Dom : NAME
  structure Cod : NAME
) : NAME_RELATION =
struct

  type dom = Dom.name
  type cod = Cod.name
  structure DomSet = Dom.NSet
  structure CodSet = Cod.NSet
  
  structure Tab = Table(type key = dom val ord = Dom.name_ord)
  structure ITab = Table(type key = cod val ord = Cod.name_ord)
  
  (* do gets directly on a table, throw exception if not found *)
  fun get_from_tab tab k =
    case Tab.lookup tab k of SOME v => v
       | NONE => raise Map.no_such_elem_exp ()
  fun get_from_itab tab k =
    case ITab.lookup tab k of SOME v => v
       | NONE => raise Map.no_such_elem_exp ()
  
  (* the internal representation *)
  type rep = {
    tab : CodSet.T Tab.table,
    itab : DomSet.T ITab.table,
    dom_set : DomSet.T,
    cod_set : CodSet.T
  }
  
  type T = (dom, cod, rep) Map.amap
  
  (* record accessors *)
  local
    fun update_rep_tab     f (r:rep) = {tab=f(#tab r),itab= #itab r,dom_set= #dom_set r,cod_set= #cod_set r}
    fun update_rep_itab    f (r:rep) = {tab= #tab r,itab=f(#itab r),dom_set= #dom_set r,cod_set= #cod_set r}
    fun update_rep_dom_set f (r:rep) = {tab= #tab r,itab= #itab r,dom_set=f(#dom_set r),cod_set= #cod_set r}
    fun update_rep_cod_set f (r:rep) = {tab= #tab r,itab= #itab r,dom_set= #dom_set r,cod_set=f(#cod_set r)}
  in
    fun update_tab     f (mp:T) = Map.update_rep (update_rep_tab     f) mp
    fun update_itab    f (mp:T) = Map.update_rep (update_rep_itab    f) mp
    fun update_dom_set f (mp:T) = Map.update_rep (update_rep_dom_set f) mp
    fun update_cod_set f (mp:T) = Map.update_rep (update_rep_cod_set f) mp
    
    fun get_tab     (mp:T) = #tab     (Map.get_rep mp)
    fun get_itab    (mp:T) = #itab    (Map.get_rep mp)
    fun get_dom_set (mp:T) = #dom_set (Map.get_rep mp)
    fun get_cod_set (mp:T) = #cod_set (Map.get_rep mp)
    
    fun set_tab     x = update_tab     (K x)
    fun set_itab    x = update_itab    (K x)
    fun set_dom_set x = update_dom_set (K x)
    fun set_cod_set x = update_cod_set (K x)
  end
  
  fun graph_of_rep (r:rep) = let
    fun di x = case Tab.lookup (#tab r) x of SOME ys => CodSet.list_of ys | NONE => []
    fun dom () = DomSet.list_of (#dom_set r)
  in Map.mk_graph (dom, di)
  end
  
  val is_empty = DomSet.is_empty o get_dom_set
  val dom_size = DomSet.cardinality o get_dom_set
  val cod_size = CodSet.cardinality o get_cod_set
  
  fun get_opt mp x =
    case Tab.lookup (get_tab mp) x
      of SOME ys => (case CodSet.tryget_singleton ys of SOME y => SOME y
                        | NONE => raise Map.multiple_values_exp ())
       | NONE => NONE
  
  fun get mp x = case get_opt mp x of SOME y => y
                    | NONE => raise Map.no_such_elem_exp ()
  
  fun img mp x = case Tab.lookup (get_tab mp) x of SOME ys => ys | NONE => CodSet.empty
  fun img_of_set mp xs = DomSet.fold (CodSet.union_merge o (img mp)) xs CodSet.empty
  
  fun inv_get_opt mp x =
    case ITab.lookup (get_itab mp) x
      of SOME ys => (case DomSet.tryget_singleton ys of SOME y => SOME y
                        | NONE => raise Map.multiple_values_exp ())
       | NONE => NONE
    
  fun inv_get mp y = case inv_get_opt mp y of SOME x => x
                        | NONE => raise Map.no_such_elem_exp ()
  
  fun inv_img mp y = case ITab.lookup (get_itab mp) y of SOME xs => xs | NONE => DomSet.empty
  fun inv_img_of_set mp ys = CodSet.fold (DomSet.union_merge o (inv_img mp)) ys DomSet.empty
  
  fun is_mapped mp (x,y) = CodSet.contains (img mp x) y
  
  fun add (x,y) mp =
    mp |> update_dom_set (DomSet.add x)
       |> update_cod_set (CodSet.add y)
       |> update_tab (Tab.update (x,CodSet.add y (img mp x)))
       |> update_itab (ITab.update (y,DomSet.add x (inv_img mp y)))
  
  val set = add
  
  fun unmap (x,y) mp = let
    val ix = img mp x |> CodSet.delete y
    val iy = inv_img mp y |> DomSet.delete x
  in mp |> (if CodSet.is_empty ix
            then update_tab (Tab.delete x) o update_dom_set (DomSet.delete x)
            else update_tab (Tab.update (x,ix)))
        |> (if DomSet.is_empty iy
            then update_itab (ITab.delete y) o update_cod_set (CodSet.delete y)
            else update_itab (ITab.update (y,iy)))
  end
  
  fun delete x mp = CodSet.fold (fn y => unmap (x,y)) (img mp x) mp
  fun inv_delete y mp = DomSet.fold (fn x => unmap (x,y)) (inv_img mp y) mp
  
  val empty_rep : rep = { tab = Tab.empty, itab = ITab.empty,
                          dom_set = DomSet.empty, cod_set = CodSet.empty }
  val empty = Map.MAP (empty_rep, graph_of_rep)
  
  fun of_graph gr = Map.fold_graph add gr empty
  
  val fold = Map.fold
  val forall = Map.forall
  val exists = Map.exists
  val find = Map.find
  
  fun coerce abs_map = fold add abs_map empty
  handle Map.duplicate_exp () => raise Map.bad_coercion_exp ()
  
  fun compose ms = coerce (Map.compose ms)
  fun compose3 ms = coerce (Map.compose3 ms)
  
  val pretty = Map.pretty "NameFunction" Dom.pretty_name Cod.pretty_name
end



