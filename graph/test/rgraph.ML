local 

local  
(* An instance of Basic Graph: strings at vertices and edges *)
structure StrRGraph = BasicRGraphFun(
  structure Vertex = StrVertex
  and Edge = StrEdge
);
structure V = StrRGraph.Vertex;
structure E = StrRGraph.Edge;
in val _ = let 
val g = StrRGraph.empty;
val g = g |> StrRGraph.new_named_vertex (V.mk "a") ("");
val g = g |> StrRGraph.new_named_vertex (V.mk "b") ("");
val g = g |> StrRGraph.new_named_vertex (V.mk "c") ("");
val _ = StrRGraph.print g; 
val g = g |> StrRGraph.new_named_edge (E.mk "R0") ("") 
        (StrRGraph.Vertex.NSet.of_list ([V.mk "a",V.mk "b"]));
val g = g |> StrRGraph.new_named_edge (E.mk "R1") ("")
        (StrRGraph.Vertex.NSet.of_list ([V.mk "b",V.mk "c"]));;

val _ = StrRGraph.print g; 
val _ = StrRGraph.shortest_path g (V.mk "a") (V.mk "c"); 

val _ = StrRGraph.get_connected_vertices g (V.mk "a");
in () end handle _ => raise ERROR "test/rgraph.ML: test1 failed! horrible monsters!";
end;

local 
structure V = ParamRGraph.Vertex;
structure E = ParamRGraph.Edge;
in val _ = let 
val g = ParamRGraph.empty;
val g = g |> ParamRGraph.new_named_vertex (V.mk "a") ();
val g = g |> ParamRGraph.new_named_vertex (V.mk "b") ();
val g = g |> ParamRGraph.new_named_vertex (V.mk "c") ();
val _ = ParamRGraph.print g; 
val g = g |> ParamRGraph.new_named_edge (E.mk "R0") () 
        (ParamRGraph.Vertex.NSet.of_list ([V.mk "a",V.mk "b"]));
val g = g |> ParamRGraph.new_named_edge (E.mk "R1") ()
        (ParamRGraph.Vertex.NSet.of_list ([V.mk "b",V.mk "c"]));;
val _ = ParamRGraph.print g; 
val _ = ParamRGraph.shortest_path g (V.mk "a") (V.mk "c"); 
val _ = ParamRGraph.get_connected_vertices g (V.mk "a");
in () end handle _ => raise ERROR "test/rgraph.ML: test2 failed! horrible monsters!";
end

in 
 val _ = Pretty.writeln (Pretty.str "test/rgraph.ML: tests passed.") 
end (* local *)
    
