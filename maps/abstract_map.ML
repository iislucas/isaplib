(*
 * An abstract structure supporting direct and inverse images, along with fast inversion
 * and composition. Other functors should work with amap's with more efficient/useful
 * representations.
 *)

signature ABS_MAP =
sig

  (* the graph (and inverse graph) of a mapping *)
  datatype ('a,'b) amapgraph = MG of {
      dom: unit -> 'a list, cod: unit -> 'b list, di: 'a -> 'b list }

  (* store a representation and a means of converting to/from graphs *)
  type ('a,'b,'c) amap = 'c * ('c -> ('a,'b) amapgraph)

  exception no_such_elem_exp of unit
  exception multiple_values_exp of unit
  exception duplicate_exp of unit

  val graph_of : ('a,'b,'c) amap -> ('a,'b) amapgraph
  val fold_graph : ('a * 'b -> 'c -> 'c) -> ('a,'b) amapgraph -> 'c -> 'c
  val compose_graphs : ('b,'c) amapgraph -> ('a,'b) amapgraph -> ('a,'c) amapgraph

  val f : ('a,'b,'c) amap -> 'a -> 'b

end


structure Map :> ABS_MAP =
struct

  exception no_such_elem_exp of unit
  exception multiple_values_exp of unit
  exception duplicate_exp of unit

  datatype ('a,'b) amapgraph = MG of {
    dom: unit -> 'a list, cod: unit -> 'b list, di: 'a -> 'b list }

  type ('a,'b,'c) amap = 'c * ('c -> ('a,'b) amapgraph)

  fun graph_of (r,gf) = gf r

  fun fold_graph f g base = let
    val MG { dom, di, ... } = g
  in fold (fn x => fold (fn y => f (x,y)) (di x)) (dom ()) base
  end

  fun compose_graphs g2 g1 =
  let
    val MG { dom=dom1, cod=cod1, di=di1 } = g1
    val MG { dom=dom2, cod=cod2, di=di2 } = g2
  in MG { dom = dom1, cod = cod2, di = maps di2 o di1 }
  end

  fun f m x =
  let
    val MG { di, ... } = graph_of m
    in case di x of [y]   => y
                  | [] => raise no_such_elem_exp ()
                  | ys => raise multiple_values_exp ()
  end

end

