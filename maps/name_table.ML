signature NAME_TABLE =
sig
  type name
  structure NSet : NAME_SET sharing type NSet.name = name
  
  type 'a rep
  type 'a T = (name, 'a, 'a rep) Map.amap
  val empty: 'a T
  
  val lookup : 'a T -> name -> 'a option
  val get : 'a T -> name -> 'a
  
  val update : (name * 'a) -> 'a T -> 'a T
  val update_no_overwrite : (name * 'a) -> 'a T -> 'a T
  val add : 'a -> 'a T -> (name * 'a T)
  val doadd : 'a -> 'a T -> 'a T
  
  (* inherited from Map.Util *)
  val fold : (name * 'a -> 'b -> 'b) -> 'a T -> 'b -> 'b
  val exists : (name * 'a -> bool) -> 'a T -> bool
  val forall : (name * 'a -> bool) -> 'a T -> bool
  val find : (name * 'a -> bool) -> 'a T -> (name * 'a) option
  
  (* composition with other abstract maps *)
  val compose  : ('a,'b,'y) Map.amap -> (name,'a,'x) Map.amap -> 'b T
  val compose3 : ('b,'c,'z) Map.amap -> ('a,'b,'y) Map.amap -> (name,'a,'x) Map.amap -> 'c T
  
  (* try to convert an amap to a name table. Throws an exception if unsuccessful. *)
  val coerce : (name,'a,'b) Map.amap -> 'a T
end

functor NameTable(
  structure NSet : NAME_SET
  structure Namer : NAMER
  sharing type Namer.name = NSet.name
  val default_name: Namer.name
) =
struct

  structure NSet = NSet
  type name = NSet.name
  val pretty_name  = Namer.pretty
  val string_of_name = Pretty.string_of o pretty_name;
  fun ord (a, b) = Namer.ord (a,b)
  fun name_eq (a, b) = (ord (a,b) = EQUAL)

  structure Tab = Table(type key = name val ord = ord);

  datatype 'a rep = Rep of {names : NSet.T, tab : 'a Tab.table}
  
  fun update_rep_names f (Rep r) = Rep {names=f(#names r),tab= #tab r}
  fun update_rep_tab f (Rep r) = Rep {names= #names r,tab=f(#tab r)}
  fun get_rep_names (Rep r) = #names r
  fun get_rep_tab   (Rep r) = #tab r
  val update_names = Map.update_rep o update_rep_names
  val update_tab   = Map.update_rep o update_rep_tab
  val set_names    = update_names o K
  val set_tab      = update_tab o K
  val get_names    = get_rep_names o Map.get_rep
  val get_tab      = get_rep_tab o Map.get_rep
  
  val empty_rep = Rep { names=NSet.empty, tab=Tab.empty }
  
  (* private: do gets directly on a table, throw exception if not found *)
  fun get_from_tab tab k =
    case Tab.lookup tab k of SOME v => v
       | NONE => raise Map.no_such_elem_exp ()

  type 'a T = (name, 'a, 'a rep) Map.amap
  
  val lookup = Tab.lookup o get_tab
  val get = get_from_tab o get_tab

  fun update (k,v) m =
    m |> update_names (NSet.add k)
      |> update_tab (Tab.update (k,v))
  
  fun update_no_overwrite (k,v) m =
    if (NSet.contains (get_names m) k) then raise Map.duplicate_exp ()
    else update (k,v) m
  
  fun add v m = let
    val (fresh,names) = NSet.add_new default_name (get_names m)
  in (fresh, m |> set_names names |> update_tab (Tab.update (fresh,v)))
  end
  
  fun doadd v m = snd (add v m)
  
  fun graph_of_rep rep = let
    fun di x = [get_from_tab (get_rep_tab rep) x]
    fun dom () = NSet.list_of (get_rep_names rep)
    fun cod () = map (get_from_tab (get_rep_tab rep)) (dom ())
  in Map.mk_graph (dom, cod, di)
  end
  
  val empty = Map.mk (empty_rep, graph_of_rep)
  fun of_graph gr = Map.fold_graph update_no_overwrite gr empty
  
  open Map.Util
  
  val coerce = of_graph o Map.graph_of
  
  fun compose m2 m1 = of_graph (Map.compose_graphs (Map.graph_of m2) (Map.graph_of m1))
  fun compose3 m3 m2 m1 = of_graph (
    Map.compose_graphs (Map.graph_of m2)
      (Map.compose_graphs (Map.graph_of m2) (Map.graph_of m1)))
  
end

