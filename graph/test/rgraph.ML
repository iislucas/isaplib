local 

local  
(* An instance of Basic Graph: strings at vertices and edges *)
structure StrRGraph = BasicRGraphFun(
  structure VData = StrData
  and EData = StrData
);
structure VNm = StrRGraph.VNm;
structure ENm = StrRGraph.ENm;
in val _ = let 
val g = StrRGraph.empty;
val g = g |> StrRGraph.new_named_vertex (VNm.mk "a") ("");
val g = g |> StrRGraph.new_named_vertex (VNm.mk "b") ("");
val g = g |> StrRGraph.new_named_vertex (VNm.mk "c") ("");
val _ = StrRGraph.print g; 
val g = g |> StrRGraph.new_named_edge (ENm.mk "R0") ("") 
        (VNm.NSet.of_list ([VNm.mk "a",VNm.mk "b"]));
val g = g |> StrRGraph.new_named_edge (ENm.mk "R1") ("")
        (VNm.NSet.of_list ([VNm.mk "b",VNm.mk "c"]));;

val _ = StrRGraph.print g; 
val _ = StrRGraph.shortest_path g (VNm.mk "a") (VNm.mk "c"); 

val _ = StrRGraph.get_connected_vertices g (VNm.mk "a");
in () end handle _ => raise ERROR "test/rgraph.ML: test1 failed! horrible monsters!";
end;

local 
structure VNm = ParamRGraph.VNm;
structure ENm = ParamRGraph.ENm;
in val _ = let 
val g = ParamRGraph.empty;
val g = g |> ParamRGraph.new_named_vertex (VNm.mk "a") ();
val g = g |> ParamRGraph.new_named_vertex (VNm.mk "b") ();
val g = g |> ParamRGraph.new_named_vertex (VNm.mk "c") ();
val _ = ParamRGraph.print g; 
val g = g |> ParamRGraph.new_named_edge (ENm.mk "R0") () 
        (VNm.NSet.of_list ([VNm.mk "a",VNm.mk "b"]));
val g = g |> ParamRGraph.new_named_edge (ENm.mk "R1") ()
        (VNm.NSet.of_list ([VNm.mk "b",VNm.mk "c"]));;
val _ = ParamRGraph.print g; 
val _ = ParamRGraph.shortest_path g (VNm.mk "a") (VNm.mk "c"); 
val _ = ParamRGraph.get_connected_vertices g (VNm.mk "a");
in () end handle _ => raise ERROR "test/rgraph.ML: test2 failed! horrible monsters!";
end

in 
 val _ = Pretty.writeln (Pretty.str "test/rgraph.ML: tests passed.") 
end (* local *)
    
