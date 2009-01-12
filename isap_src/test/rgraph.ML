local  
(* An instance of Basic Graph: strings at vertices and edges *)
structure DB_StrRGraph = BasicRGraphFun(
  structure Vertex = StrVertex
  and Edge = StrEdge
);
(* structure StrRGraph :> BASIC_RGRAPH where 
type Vertex.data = StrVertex.data 
 and type Edge.data = StrEdge.data
 and type Vertex.name = string
 and type Edge.name = string
= DB_StrRGraph;
*)

structure StrRGraph : BASIC_RGRAPH 
where type Vertex.name = string
  and type Vertex.data = string
  and type Edge.name = string
  and type Edge.data = string
= DB_StrRGraph;

val g = StrRGraph.empty;
val g = g |> StrRGraph.new_named_vertex ("a") ("");
val g = g |> StrRGraph.new_named_vertex ("b") ("");
val g = g |> StrRGraph.new_named_vertex ("c") ("");
val _ = StrRGraph.print g; 
val g = g |> StrRGraph.new_named_edge ("R0") ("") 
        (StrRGraph.Vertex.NSet.of_list (["a","b"]));
val g = g |> StrRGraph.new_named_edge ("R1") ("")
        (StrRGraph.Vertex.NSet.of_list (["b","c"]));;

val _ = StrRGraph.print g; 
val _ = StrRGraph.shortest_path g ("a") ("c"); 

val _ = StrRGraph.get_connected_vertices g ("a");


structure DB_ParamRGraph = BasicRGraphFun(
  structure Vertex = UnitVertex
  and Edge = UnitEdge
);

structure ParamRGraph : BASIC_RGRAPH 
where type Vertex.name = string
  and type Vertex.data = unit
  and type Edge.name = string
  and type Edge.data = unit
= DB_ParamRGraph;

val g = ParamRGraph.empty;
val g = g |> ParamRGraph.new_named_vertex ("a") ();
val g = g |> ParamRGraph.new_named_vertex ("b") ();
val g = g |> ParamRGraph.new_named_vertex ("c") ();
val _ = ParamRGraph.print g; 
val g = g |> ParamRGraph.new_named_edge ("R0") () 
        (ParamRGraph.Vertex.NSet.of_list (["a","b"]));
val g = g |> ParamRGraph.new_named_edge ("R1") ()
        (ParamRGraph.Vertex.NSet.of_list (["b","c"]));;
val _ = ParamRGraph.print g; 
val _ = ParamRGraph.shortest_path g ("a") ("c"); 
val _ = ParamRGraph.get_connected_vertices g ("a");

in 
  Pretty.writeln (Pretty.str "test/rgraph.ML: tests passed.") (* big let *)
end (* let *)
    handle _ => raise ERROR "test/rgraph.ML: test failed! horrible monsters!";
end;
