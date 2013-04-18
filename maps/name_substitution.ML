(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  Title:      isaplib/maps/name_relation.ML
    Author:     Aleks Kissinger
                aleks0@gmail.com
                Initially written 31 Mar 2013                          *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  DESCRIPTION:
    An injective endofunction with fast inverse-lookup and constant-time
    map inversion. These are used for renaming.
    
    Note there is a great deal of code replication with name injections.
    This seems to be difficult to get around without two-level functors
    since Tab != Tab. As substition is BY FAR the most used case,
    it seems wasteful to obfuscate the code to share more with injections.
    
    NOTE: 'set (x,y)' will overwrite a value if x is already in the
    domain, and raise duplicate_exp if mapping x to y would break
    injectivity. 'add (x,y)' is the preferred way to add elements,
    as it will throw duplicate_exp if x is in the domain *or* if y
    is in the codomain.
*)

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)

signature NAME_SUBSTITUTION =
sig
  include NAME_RELATION
  sharing type dom = cod
  sharing DomSet = CodSet
  val inverse_of : T -> T
  val extend_to : DomSet.T -> T -> T
end

functor NameSubstitution(
  structure Name : NAME
) : NAME_SUBSTITUTION =
struct

  type dom = Name.name
  type cod = Name.name
  structure DomSet = Name.NSet
  structure CodSet = Name.NSet
  
  structure Tab = Table(type key = dom val ord = Name.name_ord)
  
  (* do gets directly on a table, throw exception if not found *)
  fun get_from_tab tab k =
    case Tab.lookup tab k of SOME v => v
       | NONE => raise Map.no_such_elem_exp ()
  
  (* the internal representation *)
  type rep = {
    tab : cod Tab.table,
    itab : dom Tab.table,
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
    fun di x = case Tab.lookup (#tab r) x of SOME y => [y] | NONE => []
    fun dom () = DomSet.list_of (#dom_set r)
  in Map.mk_graph (dom, di)
  end
  
  val is_empty = DomSet.is_empty o get_dom_set
  val dom_size = DomSet.cardinality o get_dom_set
  val cod_size = CodSet.cardinality o get_cod_set (* should always = dom_size *)
  
  val get = get_from_tab o get_tab
  val get_opt = Tab.lookup o get_tab
  fun img mp x = case get_opt mp x of SOME y => CodSet.single y | NONE => CodSet.empty
  fun img_of_set mp xs = DomSet.fold (CodSet.union_merge o (img mp)) xs CodSet.empty
  val inv_get = get_from_tab o get_itab
  val inv_get_opt = Tab.lookup o get_itab
  fun inv_img mp x = case inv_get_opt mp x of SOME y => DomSet.single y | NONE => DomSet.empty
  fun inv_img_of_set mp xs = CodSet.fold (DomSet.union_merge o (inv_img mp)) xs DomSet.empty
  
  fun is_mapped mp (x,y) =
    case get_opt mp x of SOME y' => Name.name_eq(y,y') | NONE => false
  
  (* throws an exception if setting value would make map no longer injective *)
  fun set (x,y) mp = let
    val oldy_opt = get_opt mp x
  in
    if (CodSet.contains (get_cod_set mp) y) then raise Map.duplicate_exp ()
    else mp |> update_dom_set (DomSet.add x)
            |> update_cod_set (case oldy_opt of SOME oldy => CodSet.delete oldy | NONE => I)
            |> update_cod_set (CodSet.add y)
            |> update_tab (Tab.update (x,y))
            |> update_itab (case oldy_opt of SOME oldy => Tab.delete oldy | NONE => I)
            |> update_itab (Tab.update (y,x))
  end
  
  fun add (x,y) mp = 
    if (DomSet.contains (get_dom_set mp) x) then raise Map.duplicate_exp ()
    else set (x,y) mp
  
  local
    fun delete_pair (x,y) mp =
      mp |> update_dom_set (DomSet.delete x)
         |> update_tab (Tab.delete x)
         |> update_cod_set (CodSet.delete y)
         |> update_itab (Tab.delete y)
  in
    fun delete x mp = case get_opt mp x of NONE => mp | SOME y => delete_pair (x,y) mp
    fun inv_delete y mp = case inv_get_opt mp y of NONE => mp | SOME x => delete_pair (x,y) mp
  end
  
  fun unmap (x,y) mp = case get_opt mp x 
                         of SOME y1 => if Name.name_eq (y,y1) then delete x mp else mp
                          | NONE => mp
  
  val empty_rep : rep = { tab = Tab.empty, itab = Tab.empty,
                          dom_set = DomSet.empty, cod_set = CodSet.empty }
  val empty = Map.MAP (empty_rep, graph_of_rep)
  
  fun of_graph gr = Map.fold_graph add gr empty
  
  val fold = Map.fold
  val forall = Map.forall
  val exists = Map.exists
  val find = Map.find
  
  fun coerce abs_map = fold add abs_map empty
  handle _ => raise Map.bad_coercion_exp ()
  
  fun compose ms = coerce (Map.compose ms)
  fun compose3 ms = coerce (Map.compose3 ms)
  
  val pretty = Map.pretty "NameSubstitution" Name.pretty_name Name.pretty_name
  
  (*type irep = {
    tab : dom Tab.table,
    itab : cod Tab.table,
    dom_set : CodSet.T,
    cod_set : DomSet.T
  }
  
  type iT = (cod,dom,irep) Map.amap*)
  
  (*
   Constant-time inversion.
   *)
  fun inverse_of mp =
    mp |> set_cod_set (get_dom_set mp)
       |> set_dom_set (get_cod_set mp)
       |> set_tab (get_itab mp)
       |> set_itab (get_tab mp)
  
  (* extend domain to include new_dom, mapping items to themselves if not mapped *)
  fun extend_to new_dom mp = DomSet.fold
    (fn x => if (DomSet.contains (get_dom_set mp) x) then I else add (x, x)) new_dom mp
  
  structure Sharing =
  struct
    type dom = dom
    type cod = cod
    type rep = rep
    structure DomSet = DomSet.Sharing
    structure CodSet = CodSet.Sharing
  end
end
