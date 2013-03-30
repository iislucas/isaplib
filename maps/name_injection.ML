(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  Title:      isaplib/maps/name_relation.ML
    Author:     Aleks Kissinger
                aleks0@gmail.com
                Initially written 31 Mar 2013                          *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  DESCRIPTION:
    An injective function with fast inverse-lookup and map inversion.
    Aka. a bidirectional map, or BidiMap.
    
    These are the main workhorse of renamers, where Dom=Cod.
*)

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)

signature NAME_INJECTION =
sig
  include NAME_RELATION
  type irep
  val mk_inverse : (cod,dom,irep) Map.amap -> T
end

functor NameInjection(
  structure Dom : NAME
  structure Cod : NAME
) =
struct

  type dom = Dom.name
  type cod = Cod.name
  structure DomSet = Dom.NSet
  structure CodSet = Cod.NSet
  
  structure Tab = Table(type key = dom val ord = Dom.name_ord)
  structure ITab = Table(type key = cod val ord = Cod.name_ord)
  
  (* the internal representation *)
  type rep = {
    tab : cod Tab.table,
    itab : dom ITab.table,
    dom_set : DomSet.T,
    cod_set : CodSet.T
  }
  
  fun update_tab f (r : rep) = {tab=f(#tab r),itab= #itab r,dom_set= #dom_set r,cod_set= #cod_set r}
  fun update_itab f (r : rep) = {tab= #tab r,itab=f(#itab r),dom_set= #dom_set r,cod_set= #cod_set r}
  fun update_dom_set f (r : rep) = {tab= #tab r,itab= #itab r,dom_set=f(#dom_set r),cod_set= #cod_set r}
  fun update_cod_set f (r : rep) = {tab= #tab r,itab= #itab r,dom_set= #dom_set r,cod_set=f(#cod_set r)}

  fun get_tab     (r : rep) = #tab r
  fun get_itab    (r : rep) = #itab r
  fun get_dom_set (r : rep) = #dom_set r
  fun get_cod_set (r : rep) = #cod_set r

  val set_tab        = update_tab     o K
  val set_itab       = update_itab    o K
  val set_dom_set    = update_dom_set o K
  val set_cod_set    = update_cod_set o K


  
  type irep = {
    itab : cod Tab.table,
    tab : dom ITab.table,
    cod_set : DomSet.T,
    dom_set : CodSet.T
  }
  
  type T = (dom, cod, rep) Map.amap
  

end
