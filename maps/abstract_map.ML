(*
 * An abstract structure supporting direct and inverse images, along with fast inversion
 * and composition. Other functors should work with amap's with more efficient/useful
 * representations.
 *)

signature ABS_MAP =
sig

  (* the graph of a mapping *)
  type ('a,'b) amapgraph
  val mk_graph : (unit -> 'a list) * (unit -> 'b list) * ('a -> 'b list) -> ('a,'b) amapgraph

  (* store a representation and a means of converting to/from graphs *)
  type ('a,'b,'c) amap
  val mk : 'c * ('c -> ('a,'b) amapgraph) -> ('a,'b,'c) amap
  
  val update_rep : ('c -> 'c) -> ('a,'b,'c) amap -> ('a,'b,'c) amap
  val get_rep    : ('a,'b,'c) amap -> 'c
  val set_rep    : 'c -> ('a,'b,'c) amap -> ('a,'b,'c) amap

  exception no_such_elem_exp of unit
  exception multiple_values_exp of unit
  exception duplicate_exp of unit

  val graph_of : ('a,'b,'c) amap -> ('a,'b) amapgraph
  val fold_graph : ('a * 'b -> 'c -> 'c) -> ('a,'b) amapgraph -> 'c -> 'c
  val compose_graphs : ('b,'c) amapgraph -> ('a,'b) amapgraph -> ('a,'c) amapgraph
  
  structure Util : sig
    val fold : ('a * 'b -> 'd -> 'd) -> ('a,'b,'c) amap -> 'd -> 'd
    val exists : ('a * 'b -> bool) -> ('a,'b,'c) amap -> bool
    val forall : ('a * 'b -> bool) -> ('a,'b,'c) amap -> bool
    val find : ('a * 'b -> bool) -> ('a,'b,'c) amap -> ('a * 'b) option
  end
end


structure Map :> ABS_MAP =
struct

  exception no_such_elem_exp of unit
  exception multiple_values_exp of unit
  exception duplicate_exp of unit

  datatype ('a,'b) amapgraph = AMG of {
    dom: unit -> 'a list, cod: unit -> 'b list, di: 'a -> 'b list }
  fun mk_graph (dom,cod,di) = AMG { dom=dom, cod=cod, di=di }

  datatype ('a,'b,'c) amap = AM of { rep : 'c, gf: 'c -> ('a,'b) amapgraph }
  fun mk (rep,gf) = AM { rep=rep, gf=gf }
  
  fun get_rep (AM m) = #rep m
  fun update_rep f (AM {rep,gf}) = AM {rep=f(rep),gf=gf}
  fun set_rep rep1 (AM {rep=_,gf=gf}) = AM {rep=rep1,gf=gf}
  (*val set_rep = update_rep o K*)

  fun graph_of (AM {rep,gf}) = gf rep

  fun fold_graph f g base = let
    val AMG { dom, di, ... } = g
  in fold (fn x => fold (fn y => f (x,y)) (di x)) (dom ()) base
  end
  
  structure Util =
  struct
    fun fold f = fold_graph f o graph_of
    
    fun find (f : 'a * 'b -> bool) m = let
      exception found_exp of 'a * 'b
    in (
         fold (fn (x,y) => fn () => if (f (x,y)) then raise found_exp (x,y) else ()) m ();
         NONE
       ) handle found_exp (x,y) => SOME (x,y)
    end
    
    fun forall f m = case (find (not o f) m) of SOME _ => false | NONE => true
    fun exists f m = case (find f m) of SOME _ => true | NONE => false
  end

  fun compose_graphs g2 g1 =
  let
    val AMG { dom=dom1, cod=cod1, di=di1 } = g1
    val AMG { dom=dom2, cod=cod2, di=di2 } = g2
  in AMG { dom = dom1, cod = cod2, di = maps di2 o di1 }
  end

end

