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

(* names for half edges and edge directions *)
signature EHN_NAMES
= sig

(* my own bool type for clarity, 
   Its used for two meaning: 1. direction, 2. half edge id. 
   TargetEnd = normal direction, Target-end
   SourceEnd = reversed direction, Source-end
*)
datatype halfid = TargetEnd | SourceEnd;
val other_half : halfid -> halfid (* not *)

type edgename
include NAMES

val get_ename : name -> edgename
val get_halfid : name -> halfid
val other : name -> name (* swap: src <-> tgt *)
val mk_src : edgename -> name;
val mk_tgt : edgename -> name;
val is_src : name -> bool
val is_tgt : name -> bool

end;

functor EhnFun(Nm : NAMES) 
: EHN_NAMES 
(* where type edgename = Nm.name *)
= struct 
  type edgename = Nm.name;
  (* half = bool, using my own datatype for clarity/readability *)
  datatype halfid = TargetEnd | SourceEnd;
  fun other_half TargetEnd = SourceEnd
    | other_half SourceEnd = TargetEnd;

  structure EhnN = AddDataNameFun(
    structure Nm = Nm
    type data = halfid
    (* false = source, true = target *)
    fun ord_data (TargetEnd, TargetEnd) = EQUAL
      | ord_data (TargetEnd, SourceEnd) = GREATER
      | ord_data (SourceEnd, TargetEnd) = LESS
      | ord_data (SourceEnd, SourceEnd) = EQUAL
    fun pretty (TargetEnd,n) = Pretty.block [Pretty.str "->", Nm.pretty_name n]
      | pretty (SourceEnd,n) = Pretty.block [Pretty.str "<-", Nm.pretty_name n]
    val default_name = (TargetEnd, Nm.default_name));
  open EhnN;

  fun get_ename ((h,en) : name) = en;
  fun get_halfid ((h,en) : name) = h;
  fun is_src ((h,en) : name) = (SourceEnd = h);
  fun is_tgt ((h,en) : name) = (TargetEnd = h);
  fun other ((h,en) : name) = (other_half h, en);
      
  fun mk h e = ((h,e) : name);
  
  val mk_src = mk SourceEnd;
  val mk_tgt = mk TargetEnd;
end;



(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
signature VERTEX = NAME_AND_DATA;

signature EDGE = 
sig 
include NAME_AND_DATA;

structure Ehn : EHN_NAMES where type edgename = name; 

val add_src_ehns : NSet.T -> Ehn.NSet.T -> Ehn.NSet.T;
val add_tgt_ehns : NSet.T -> Ehn.NSet.T -> Ehn.NSet.T;

end;




functor EdgeFun(Nm : NAME_AND_DATA)
 : EDGE
= struct
open Nm;

structure Ehn = EhnFun(Nm); 

val add_src_ehns = 
    NSet.fold (Ehn.NSet.add o Ehn.mk_src);
val add_tgt_ehns = 
    NSet.fold (Ehn.NSet.add o Ehn.mk_tgt);

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
