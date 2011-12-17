(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  
    Representation for (undirected) Relation Graphs with named vertexs (vars) 
    and edges (relations). 
*)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(* *)

signature BASIC_RGRAPH = 
sig

structure VNm : SSTR_NAME
structure ENm : SSTR_NAME

structure VData : UNIFIABLE_DATA
structure EData : UNIFIABLE_DATA

type T; (* a graph *)

exception no_such_vname_exp of string * VNm.name * T;
exception vname_already_exists_exp of VNm.name * T;

exception no_such_ename_exp of string * ENm.name * T;
exception ename_already_exists_exp of ENm.name * T;

(* Making Graphs *)
val empty : T
val is_empty : T -> bool
(* adding a vertex gives back unique name for it and the new graph
val add_vertex : VData.data -> T -> VNm.name * T 
val doadd_vertex : VData.data -> T -> T *)
(* VNm.name = suggested name *)
val add_named_vertex : VNm.name -> VData.data -> T -> VNm.name * T 
val new_named_vertex : VNm.name -> VData.data -> T -> T  (* can raise *)

(* VNm.names must already exist, else raises: no_such_vname_exp *)
val add_named_edge : ENm.name -> EData.data -> VNm.NSet.T -> T -> ENm.name * T 
val new_named_edge : ENm.name -> EData.data -> VNm.NSet.T -> T -> T (* can raise *)

(* delete raises an exception if there is no such edge/vertex *)
val delete_edge : ENm.name -> T -> T
val delete_vertex : VNm.name -> T -> T

(* replace all occurances of old name (1st arg) with new name (2nd arg) *)
(*  2nd name must not already exist and 1st must already exist, 
    else an exception is raised *)
val rename_vname : VNm.name -> VNm.name -> T -> T 
val rename_ename : ENm.name -> ENm.name -> T -> T 

(* update a vertex's data *)
val update_vertex : (VData.data -> VData.data) -> VNm.name -> T -> T
val update_edge : (EData.data -> EData.data) -> ENm.name -> T -> T
val set_vertex : VData.data -> VNm.name -> T -> T
val set_edge : EData.data -> ENm.name -> T -> T

(* getting stuff from graphs *)
val get_vertex : T -> VNm.name -> (VData.data * ENm.NSet.T)
val get_edge : T -> ENm.name -> (EData.data * VNm.NSet.T)
val lookup_vertex : T -> VNm.name 
									-> (VData.data * ENm.NSet.T) option
val lookup_edge : T -> ENm.name 
									-> (EData.data * VNm.NSet.T) option

(* working with names in the graph, no exceptions *)
val useup_vnames : VNm.NSet.T -> T -> T
val useup_enames : ENm.NSet.T -> T -> T

val get_connected_vertices : T -> VNm.name -> (ENm.name * VNm.name) list 

(* paths *)
val shortest_path : T -> VNm.name -> VNm.name -> (ENm.name * VNm.name) list option

(* getting graph internal representation... *)
val get_vertex_list : T -> (VNm.name * (VData.data * ENm.NSet.T)) (* edges leaving this vertex *)
                    list;
val get_edge_list : T -> (ENm.name * (EData.data * VNm.NSet.T)) (* to this vertex *)
                    list

val get_vertices : T -> (VData.data * ENm.NSet.T) (* relations we are in... *)
                       VNm.NTab.T;
val get_edges : T -> (EData.data * VNm.NSet.T) (* to this vertex *)
                     ENm.NTab.T

(* pretty printing *)
val pretty : T -> Pretty.T
val pretty_vertex : VData.data -> Pretty.T
val pretty_edge : EData.data -> Pretty.T

val print : T -> unit
val print_vertex : VData.data -> unit
val print_edge : EData.data -> unit

end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Basic Relational Graphs *)
functor BasicRGraphFun(
   structure VData : UNIFIABLE_DATA
   structure EData : UNIFIABLE_DATA
)
= struct

structure VNm = SStrName;
structure ENm = SStrName;

structure VData = VData;
structure EData = EData;

(* a graph *)
datatype T = RGraph of 
         {
          vertices : (VData.data * ENm.NSet.T) (* edges incident to this vertex *)
                    VNm.NTab.T,
          edges : (EData.data * VNm.NSet.T) (* vertices connected by this relation *) 
                    ENm.NTab.T
         };

val empty = RGraph { vertices = VNm.NTab.empty, edges = ENm.NTab.empty };
fun update_vertices f (RGraph rep) = 
    RGraph { vertices = f (#vertices rep), edges = #edges rep };
fun update_edges f (RGraph rep) = 
    RGraph { vertices = #vertices rep, edges = f (#edges rep)};
val set_vertices = update_vertices o K;
val set_edges = update_edges o K;
fun get_vertices (RGraph rep) = #vertices rep;
fun get_edges (RGraph rep) = #edges rep;
val get_vertex_list = VNm.NTab.list_of o get_vertices;
val get_edge_list = ENm.NTab.list_of o get_edges;

val is_empty = VNm.NTab.is_name_empty o get_vertices;

fun lookup_vertex g n = 
    case VNm.NTab.lookup (get_vertices g) n 
     of NONE => NONE | SOME x => SOME x;
fun lookup_edge g en = 
    case ENm.NTab.lookup (get_edges g) en 
     of NONE => NONE | SOME x => SOME x;


fun get_connected_vertices g n = 
    let 
      val vertices = get_vertices g
      val edges = get_edges g
    in
      case VNm.NTab.lookup vertices n of 
        SOME (nd,ens) =>
        ENm.NSet.fold (fn e => 
                           let val (ed,ns) = ENm.NTab.get edges e
                           in VNm.NSet.fold 
                                (fn n' => 
                                 fn nes => 
                                    if VNm.NTab.name_eq (n', n) then nes
                                    else (e,n')::nes)
                                ns end) ens []
      | NONE => []
    end;

exception no_such_vname_exp of string * VNm.name * T;
exception vname_already_exists_exp of VNm.name * T;
exception no_such_ename_exp of string * ENm.name * T;
exception ename_already_exists_exp of ENm.name * T;

fun get_vertex g n = 
    case lookup_vertex g n 
     of NONE => raise no_such_vname_exp ("get_vertex", n, g)
      | SOME x => x;
fun get_edge g en = 
    case lookup_edge g en 
     of NONE => raise no_such_ename_exp ("get_edge", en, g)
      | SOME x => x;

(* adding a vertex gives back unique name for it and the new graph *)
fun add_named_vertex n nd g =
    let val (n2,vertices2) = VNm.NTab.add (n,(nd,ENm.NSet.empty)) (get_vertices g)
    in (n2, set_vertices vertices2 g) end;
(* implicit args: nd g *)
(* val add_vertex = add_named_vertex Vertex.default_name;
val doadd_vertex = snd oo add_vertex;*)

(* implicit arg: g *)
fun new_named_vertex n nd = update_vertices (VNm.NTab.ins (n,(nd,ENm.NSet.empty)));

(* working with names in the graph, no exceptions *)
val useup_vnames = update_vertices o VNm.NTab.useup_names;
val useup_enames = update_edges o ENm.NTab.useup_names;

(* implicit args: g n *)
val has_vname = VNm.NTab.contains o get_vertices;
val has_ename = ENm.NTab.contains o get_edges;


fun add_named_edge en e ns g = 
    let 
      (* make sure the vertices exist *) 
      val _ = VNm.NSet.fold (fn n => if has_vname g n then I 
                                    else raise no_such_vname_exp 
                                                 ("add_named_edge", n, g))
                           ns ();

      (* add the new egde *)
      val (en2,edges2) = ENm.NTab.add (en,(e,ns)) (get_edges g)

      (* update the vertices: add edge name as appropriate to ins and outs *)
      val vertices2 = 
          VNm.NSet.fold
            (VNm.NTab.map_entry (fn (n,ees) => (n,ENm.NSet.ins_fresh en2 ees)))            
            ns
            (get_vertices g)
      (* give back new edge name and new graph *)
    in (en2, RGraph{vertices = vertices2, edges = edges2}) end;

fun new_named_edge en e ns g =
    let 
      (* make sure the vertices exist *) 
      val _ = VNm.NSet.fold (fn n => if has_vname g n then I 
                                    else raise no_such_vname_exp 
                                                 ("add_named_edge", n, g))
                           ns ();

      (* add the new egde *)
      val edges2 = ENm.NTab.ins (en,(e,ns)) (get_edges g)

      (* update the vertices: add edge name as appropriate to ins and outs *)
      val vertices2 = 
          VNm.NSet.fold
            (VNm.NTab.map_entry (fn (n,ees) => (n,ENm.NSet.ins_fresh en ees)))
            ns
            (get_vertices g)
      (* give back new edge name and new graph *)
    in RGraph{vertices = vertices2, edges = edges2} end;

(* deleting vertex also removes all it's edges. *)
fun delete_vertex n g = 
    let 
      fun delete_connecting_vertex n (ed,ns) = 
          (ed, VNm.NSet.delete n ns);

      val vertices = get_vertices g
      val (nd,ens) =
          case VNm.NTab.lookup vertices n of 
            NONE => raise no_such_vname_exp ("delete_vertex", n, g)
          | SOME x => x;

      val vertices2 = vertices |> VNm.NTab.delete n

      (* delete all edges going into or out of this vertex. *)
      val edges2 = 
          (get_edges g)
            |> ENm.NSet.fold 
            (ENm.NTab.map_entry (delete_connecting_vertex n)) ens

      (* give back new edge name and new graph *)
    in RGraph{vertices = vertices2, edges = edges2} end;
    

(* deleting an edge also removes all it's vertices if they are not 
   connected to anything else. *)
fun delete_edge e g = 
    let 
      fun delete_connecting_edge e (nd,ees) = 
          (nd, ENm.NSet.delete e ees);

      val edges = get_edges g
      val (ed,ns) =
          case ENm.NTab.lookup edges e of 
            NONE => raise no_such_ename_exp ("delete_edge", e, g)
          | SOME x => x;

      val edges2 = edges |> ENm.NTab.delete e

      (* delete all edges going into or out of this vertex. *)
      val vertices2 = 
          (get_vertices g)
            |> VNm.NSet.fold 
            (VNm.NTab.map_entry (delete_connecting_edge e)) ns

      (* give back new edge name and new graph *)
    in RGraph{vertices = vertices2, edges = edges2} end;



(* vname = suggested name *)
(* rename all occurances of old name (1st arg) with new name (2nd arg) 
   n2 must not exist already, n1 must exist *)
fun rename_vname n1 n2 g = 
    let 
      val vertices = (get_vertices g)
      (* gets vertex for n1, makes ure it exists *)
      val (nd,ens) = case VNm.NTab.lookup vertices n1 
                      of NONE => raise no_such_vname_exp ("rename_vname", n1, g)
                       | SOME x => x;

    (* update the vertex entry for n1, make sure that n2 doesn't already exist *)
      val vertices2 = 
          case (vertices |> VNm.NTab.delete n1
                      |> VNm.NTab.try_ins (n2,(nd,ens)))
           of NONE => raise vname_already_exists_exp (n2,g)
            | SOME vertices2 => vertices2;

      (* update any edges to use the new name instead of n1 *)
      val edges2 = 
          ENm.NSet.fold 
            (ENm.NTab.map_entry (fn (e,nns) => 
                                 (e, nns |> VNm.NSet.delete n1
                                         |> VNm.NSet.add n2))) 
            ens
            (get_edges g);

      (* give back new edge name and new graph *)
    in RGraph{vertices = vertices2, edges = edges2} end;


fun rename_ename en1 en2 g = 
    let 
      val edges = (get_edges g)
      (* gets vertex for n1, makes ure it exists *)
      val (e,nns) = case ENm.NTab.lookup edges en1
                     of NONE => raise no_such_ename_exp ("rename_vname", en1, g)
                      | SOME x => x;

      (* update the vertex entry for n1, we make sure that n2 doesn't already exist *)
      val edges2 = 
          case (edges |> ENm.NTab.delete en1
                      |> ENm.NTab.try_ins (en2,(e,nns)))
           of NONE => raise ename_already_exists_exp (en2,g)
            | SOME edges2 => edges2;

      (* update any edges to use the new name n1 instead of n1, using ins and outs lists *)
      val vertices2 = 
          VNm.NSet.fold 
            (VNm.NTab.map_entry (fn (nd,ens) => 
                                 (nd, ens |> ENm.NSet.delete en1
                                          |> ENm.NSet.add en2))) 
            nns
            (get_vertices g);

      (* give back new edge name and new graph *)
    in RGraph{vertices = vertices2, edges = edges2} end;

    

(* finding shortest path between two vertices, forward search, breadth first,
avoids any loops *) 
fun shortest_path g n1 n2 =
    let 
      fun search dest ptab [] = NONE
        | search dest ptab ((a,alist)::agenda) = 
          (if VNm.NTab.name_eq (a, dest) then SOME alist
           else if VNm.NSet.contains ptab a then 
             search dest ptab agenda
           else
             let 
               val agenda' = 
                   agenda @ (map (fn (e,b) => (b, (e,a)::alist))
                                 (get_connected_vertices g a))
               val ptab' = VNm.NSet.add a ptab
             in search dest ptab' agenda' end)
    in search n2 VNm.NSet.empty [(n1,[])] end;


(* implicit arg: g *)
fun update_vertex f n = 
    update_vertices (VNm.NTab.map_entry (fn (nd,ens) => (f nd,ens)) n);
fun update_edge f en = 
    update_edges (ENm.NTab.map_entry (fn (e,nns) => (f e,nns)) en);
val set_vertex = update_vertex o K;
val set_edge = update_edge o K;

(* pretty printing *)
val pretty_vname = VNm.pretty_name;
val pretty_ename = ENm.pretty_name;
val pretty_vertex = VData.pretty_data;
val pretty_edge = EData.pretty_data;

val print_vname = Pretty.writeln o pretty_vname;
val print_ename = Pretty.writeln o pretty_ename;
val print_vertex = Pretty.writeln o pretty_vertex;
val print_edge = Pretty.writeln o pretty_edge;

fun pretty g = 
    Pretty.chunks 
      [Pretty.str "RGraph{",
       Pretty.block 
         [Pretty.str "  ", (* indent *)
          Pretty.chunks 
          [Pretty.block 
             [Pretty.str "Vertices: ",
              Pretty.chunks 
                (map 
                   (fn (n, (nd,ens)) => 
                       Pretty.block 
                         [pretty_vname n,
                          Pretty.str "; ",
                          Pretty.list "{" "}" (map pretty_ename (ENm.NSet.list_of ens)),
                          Pretty.str "; ",
                          pretty_vertex nd
                   ])
                   (VNm.NTab.list_of (get_vertices g)))],
           Pretty.block
             [Pretty.str "Edges: ",
              Pretty.chunks 
                (map 
                   (fn (en, (ed,nns)) => 
                       Pretty.block 
                         [pretty_ename en,
                          Pretty.str "; ",
                          Pretty.list "{" "}" (map pretty_vname (VNm.NSet.list_of nns)),
                          Pretty.str "; ",
                          pretty_edge ed
                   ])
                   (ENm.NTab.list_of (get_edges g)))]]],
       Pretty.str "}"
      ];
     
val print = Pretty.writeln o pretty;

end;


(* basic parameter n-relation-graph *)
structure DB_ParamRGraph = BasicRGraphFun(
  structure VData = UnitData
  and EData = UnitData
);

 structure ParamRGraph : BASIC_RGRAPH 
(*  where type VNm.name = string
  and type VData.data = unit
  and type ENm.name = string
  and type EData.data = unit *)
= DB_ParamRGraph;

