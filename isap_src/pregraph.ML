(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*   Some preliminaries for Generic Graphs (Vertixes and Edges) *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
signature ORD_DATA
= sig
  type data
  val pretty_data : data -> Pretty.T
  val print_data : data -> unit
  val data_ord : data * data -> General.order
  val data_eq : data * data -> bool
end;

signature NAME_AND_DATA
= sig
include SSTR_NAMES
include ORD_DATA
val default_data : data
end;

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
signature VERTEX = NAME_AND_DATA;

signature EDGE = 
sig 
include NAME_AND_DATA;

(* my own bool type for clarity *)
datatype half = TargetEnd | SourceEnd;
val other_half : half -> half (* not *)

(* half edge name: half * name *)
structure Hen : NAMES
val edge_of_hen : Hen.name -> name;
val mk_hen : half -> name -> Hen.name;
val hen_is_src : Hen.name -> bool
val hen_is_tgt : Hen.name -> bool
val other_hen : Hen.name -> Hen.name (* swap: src <-> tgt *)

val mk_src_hen : name -> Hen.name;
val mk_tgt_hen : name -> Hen.name;

(* val src_henset : NSet.T -> Hen.NSet.T;
val tgt_henset : NSet.T -> Hen.NSet.T;

val src_hentab : NTab.T -> Hen.NTab.T;
val tgt_hentab : NTab.T -> Hen.NTab.T;
*)

end;


functor EdgeFun(Nm : NAME_AND_DATA)
: EDGE 
= struct
open Nm;

(* half = bool, using my own datatype for clarity/readability *)
datatype half = TargetEnd | SourceEnd;
fun other_half TargetEnd = SourceEnd
  | other_half SourceEnd = TargetEnd;

structure Hen : NAMES = 
  AddDataNameFun(
    structure Nm = Nm
    type data = half
    (* false = source, true = target *)
    fun ord_data (TargetEnd, TargetEnd) = EQUAL
      | ord_data (TargetEnd, SourceEnd) = GREATER
      | ord_data (SourceEnd, TargetEnd) = LESS
      | ord_data (SourceEnd, SourceEnd) = EQUAL
    fun pretty (TargetEnd,n) = Pretty.block [Pretty.str "->", Nm.pretty_name n]
      | pretty (SourceEnd,n) = Pretty.block [Pretty.str "<-", Nm.pretty_name n]
    val default_name = (TargetEnd, Nm.default_name));

fun edge_of_hen ((h,en) : Hen.name) = en;
fun hen_is_src ((h,en) : Hen.name) = (SourceEnd = h);
fun hen_is_tgt ((h,en) : Hen.name) = (TargetEnd = h);
fun other_hen ((h,en) : Hen.name) = (other_half h, en);

fun mk_hen h e = ((h,e) : Hen.name);

val mk_src_hen = mk_hen SourceEnd;
val mk_tgt_hen = mk_hen TargetEnd;

end;

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Kinds of data *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 

structure StrData : ORD_DATA where type data = string 
= struct 
type data = string
val pretty_data = Pretty.str;
val print_data = Pretty.writeln o pretty_data;
val data_ord = String.compare;
fun data_eq (a,b) = (a = (b : string));
end;

structure UnitData : ORD_DATA where type data = unit 
= struct 
type data = unit
val pretty_data = K (Pretty.str "");
val print_data = Pretty.writeln o pretty_data;
val data_ord = K EQUAL;
val data_eq = K true;
end;

structure StrNmStrData 
: NAME_AND_DATA where type data = string
= struct
open SStrName;
val default_name = mk "a";
open StrData;
val default_data = "";
end;

structure StrNmUnitData 
: NAME_AND_DATA where type data = unit
= struct
open SStrName;
val default_name = mk "a";
open UnitData;
val default_data = ();
end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Some instances of edges and vertices *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(* BUG BUG BUG!!! PolyML: 
if you make the signature constraint strict, you get: 
StrVertex.NSet.empty : StrVertex.Rnm.T
*)
structure StrVertex 
: VERTEX where type data = string
= struct
open StrNmStrData;
val default_name = mk "Va";
end;

structure UnitVertex 
: VERTEX where type data = unit
= struct
open StrNmUnitData;
val default_name = mk "Va";
end;

structure UnitEdge 
: EDGE where type data = unit
= EdgeFun(StrNmUnitData);

structure StrEdge 
: EDGE where type data = string
= EdgeFun(StrNmStrData);
