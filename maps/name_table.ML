(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  Title:      isaplib/maps/name_table.ML
    Author:     Aleks Kissinger, Lucas Dixon
                aleks0@gmail.com, lucas.dixon@ed.ac.uk
                Initially written Oct 2005, Updated 27 Mar 2013        *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  DESCRIPTION:
    Table with names as keys and polymorphic value types. Note we don't
    even require values to have a notion of equality, so things like
    inverse image don't make sense.
    
    The underlying type is an ABS_MAP, so it supports coercion and
    composition with other map types.
*)

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)

signature NAME_TABLE =
sig
  type name
  structure NSet : NAME_SET sharing type NSet.name = name
  
  type 'a rep
  type 'a T = (name, 'a, 'a rep) Map.amap
  val empty: 'a T
  
  val size : 'a T -> int
  val is_empty : 'a T -> bool
  
  (* forward lookup *)
  val get : 'a T -> name -> 'a
  val get_opt : 'a T -> name -> 'a option
  (*val img : T -> dom -> CodSet.T*)
  (*val img_of_set : T -> DomSet.T -> CodSet.T*)
  
  (* inverse lookup *)
  (*val inv_get : T -> cod -> dom*)
  (*val inv_get_opt : T -> cod -> dom option*)
  (*val inv_img : T -> cod -> DomSet.T*)
  (*val inv_img_of_set : T -> CodSet.T -> DomSet.T*)
  
  (* relation lookup *)
  (*val is_mapped : T -> (dom * cod) -> bool*)
  
  (* updaters *)
  val set : (name * 'a) -> 'a T -> 'a T
  val add : (name * 'a) -> 'a T -> 'a T
  val store : 'a -> 'a T -> (name * 'a T)
  val store_anon : 'a -> 'a T -> 'a T
  val delete : name -> 'a T -> 'a T
  (*val inv_delete : cod -> T -> T*)
  (*val unmap : dom * cod -> T -> T*)
  
  (* inherited from Map.Util *)
  val fold : (name * 'a -> 'b -> 'b) -> 'a T -> 'b -> 'b
  val exists : (name * 'a -> bool) -> 'a T -> bool
  val forall : (name * 'a -> bool) -> 'a T -> bool
  val find : (name * 'a -> bool) -> 'a T -> (name * 'a) option
  
  (* composition with other abstract maps *)
  val compose  : ('a,'b,'y) Map.amap * (name,'a,'x) Map.amap -> 'b T
  val compose3 : ('b,'c,'z) Map.amap * ('a,'b,'y) Map.amap * (name,'a,'x) Map.amap -> 'c T
  
  (* try to convert an amap to a name table. Throws an exception if unsuccessful. *)
  val coerce : (name,'a,'b) Map.amap -> 'a T
  
  val pretty : ('a -> Pretty.T) -> 'a T -> Pretty.T
end

functor NameTable(
  structure Dom : NAME
) : NAME_TABLE =
struct

  structure NSet = Dom.NSet
  type name = Dom.name
  fun ord (a, b) = Dom.name_ord (a,b)
  fun name_eq (a, b) = (ord (a,b) = EQUAL)

  structure Tab = Table(type key = name val ord = ord);
  
  (* do gets directly on a table, throw exception if not found *)
  fun get_from_tab tab k =
    case Tab.lookup tab k of SOME v => v
       | NONE => raise Map.no_such_elem_exp ()

  datatype 'a rep = Rep of {names : NSet.T, tab : 'a Tab.table}
  
  local
    fun update_rep_names f (Rep r) = Rep {names=f(#names r),tab= #tab r}
    fun update_rep_tab f (Rep r) = Rep {names= #names r,tab=f(#tab r)}
    fun get_rep_names (Rep r) = #names r
    fun get_rep_tab   (Rep r) = #tab r
  in
  
    fun update_names f = Map.update_rep (update_rep_names f)
    fun update_tab f   = Map.update_rep (update_rep_tab f)
    fun set_names x    = update_names (K x)
    fun set_tab x      = update_tab (K x)
    fun get_names m    = get_rep_names (Map.get_rep m)
    fun get_tab m      = get_rep_tab (Map.get_rep m)
         
    fun graph_of_rep rep = let
      fun di x = [get_from_tab (get_rep_tab rep) x]
      fun dom () = NSet.list_of (get_rep_names rep)
    in Map.mk_graph (dom, di)
    end
  
  end
  
  
  

  type 'a T = (name, 'a, 'a rep) Map.amap
  
  fun size mp = NSet.cardinality (get_names mp)
  fun is_empty mp = NSet.is_empty (get_names mp)
  
  fun get_opt mp = Tab.lookup (get_tab mp)
  fun get mp = get_from_tab (get_tab mp)

  fun set (k,v) mp =
    mp |> update_names (NSet.add k)
       |> update_tab (Tab.update (k,v))
  
  fun add (k,v) m =
    if (NSet.contains (get_names m) k) then raise Map.duplicate_exp ()
    else set (k,v) m
  
  fun store v mp = let
    val (fresh,names) = NSet.add_new Dom.default_name (get_names mp)
  in (fresh, mp |> set_names names |> update_tab (Tab.update (fresh,v)))
  end
  
  fun store_anon v mp = snd (store v mp)
  
  fun delete n mp =
    mp |> update_names (NSet.delete n)
       |> update_tab (Tab.delete_safe n)
  
  val empty_rep = Rep { names=NSet.empty, tab=Tab.empty }
  val empty = Map.MAP (empty_rep, graph_of_rep)
  fun of_graph gr = Map.fold_graph add gr empty
  
  val fold = Map.fold
  val forall = Map.forall
  val exists = Map.exists
  val find = Map.find
  
  fun coerce abs_map = fold add abs_map empty
  handle Map.duplicate_exp () => raise Map.multiple_values_exp ()
  
  fun compose ms = coerce (Map.compose ms)
  fun compose3 ms = coerce (Map.compose3 ms)
  
  fun pretty pretty_val = Map.pretty "NameTable" Dom.pretty_name pretty_val
  
end

