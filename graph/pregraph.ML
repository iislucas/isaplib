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

signature UNIFIABLE_DATA
= sig
  include ORD_DATA
  type unif
  val empty_unif : unif
  val unify_data : data * data -> unif -> unif option
  val subst_in_data : unif -> data -> data

  (* like f o g; this should just composes substitutions *)
  val compose_unif : unif * unif -> unif

  val pretty_unif : unif -> Pretty.T
  val print_unif : unif -> unit
end;

(* to simplify sharing constraints *)
signature NAME_AND_DATA_SHARING =
sig
include NAMES_SHARING
type data
structure Map : NAME_MAP_SHARING
structure InjEndo : NAME_INJ_SHARING
sharing InjEndo.Dom = InjEndo.Cod
sharing Map.Dom = InjEndo.Dom
sharing Map.Cod = InjEndo.Dom

(* it would be nice to have a syntax for 'this' sturcture, 
   save myself the lines below... I guess resolving recursion would need a little thought. 
   This issue is a bit deeper really: we need a language to specify signatures and their relations sensibly. 

   Below is essentially: "this.NAMES_SHARING = Map.Dom"
*)
sharing type name = Map.Dom.name;
sharing type table = Map.Dom.table;
sharing type nset = Map.Dom.nset
sharing type ntab = Map.Dom.ntab
sharing type renaming = Map.Dom.renaming;

end;

signature NAME_AND_UNIFIABLE_DATA_SHARING =
sig
  include NAME_AND_DATA_SHARING
  type unif
end;

signature NAME_AND_DATA
= sig
  include SSTR_NAMES
  include ORD_DATA
  val default_data : data
  structure SharingWithData : NAME_AND_DATA_SHARING where type data = data
  sharing Sharing = SharingWithData;
  structure Map : NAME_MAP;
  sharing SharingWithData.Map = Map.Sharing;
  structure InjEndo : NAME_INJENDO;
  sharing SharingWithData.InjEndo = InjEndo.Sharing;
end;

signature NAME_AND_UNIFIABLE_DATA
= sig
  include SSTR_NAMES
  include UNIFIABLE_DATA
  val default_data : data
  structure SharingWithData : NAME_AND_UNIFIABLE_DATA_SHARING where type data = data
  sharing Sharing = SharingWithData;
  structure Map : NAME_MAP;
  sharing SharingWithData.Map = Map.Sharing;
  structure InjEndo : NAME_INJENDO;
  sharing SharingWithData.InjEndo = InjEndo.Sharing;
end;

signature EHN_SHARING = 
sig include NAMES_SHARING; type halfid; end;

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

  type edgename;
  include NAMES;
  structure EhnSharing : EHN_SHARING where type halfid = halfid
  sharing Sharing = EhnSharing;

  val get_ename : name -> edgename
  val get_halfid : name -> halfid
  val other : name -> name (* swap: src <-> tgt *)
  val mk_start : edgename -> name;
  val mk_end : edgename -> name;
  val is_start : name -> bool
  val is_end : name -> bool

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
    fun pretty (TargetEnd,n) = Pretty.block [Pretty.str "tgt:", Nm.pretty_name n]
      | pretty (SourceEnd,n) = Pretty.block [Pretty.str "src:", Nm.pretty_name n]
    val default_name = (TargetEnd, Nm.default_name));
  open EhnN;

  structure EhnSharing = struct open Sharing; type halfid = halfid; end;
  
  fun get_ename ((h,en) : name) = en;
  fun get_halfid ((h,en) : name) = h;
  fun is_start ((h,en) : name) = (SourceEnd = h);
  fun is_end ((h,en) : name) = (TargetEnd = h);
  fun other ((h,en) : name) = (other_half h, en);
      
  fun mk h e = ((h,e) : name);
  
  val mk_start = mk SourceEnd;
  val mk_end = mk TargetEnd;
end;



(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
signature VERTEX = NAME_AND_UNIFIABLE_DATA;

(* Special Sharing sigs for Edges which include half-edge names *)

signature EDGE_SHARING = 
sig 
  include NAMES_SHARING; structure Ehn : EHN_SHARING; 
end;

(* a classic case of multiple ways to express this, and a nice one that uses multiple overlapping inheritence: 
include EDGE_SHARING + NAME_AND_DATA
*)
signature EDGE_AND_DATA_SHARING = 
sig 
  include NAME_AND_DATA_SHARING; structure Ehn : EHN_SHARING; 
end;

signature EDGE_AND_UNIFIABLE_DATA_SHARING = 
sig 
  include NAME_AND_UNIFIABLE_DATA_SHARING; structure Ehn : EHN_SHARING; 
end;

(* Edges *)
signature EDGE = 
sig 
include NAME_AND_UNIFIABLE_DATA;

structure Ehn : EHN_NAMES where type edgename = name; 

val add_start_ehns : NSet.T -> Ehn.NSet.T -> Ehn.NSet.T;
val add_end_ehns : NSet.T -> Ehn.NSet.T -> Ehn.NSet.T;

structure EdgeSharing : EDGE_SHARING 

sharing Ehn.EhnSharing = EdgeSharing.Ehn;
sharing Sharing = EdgeSharing;

structure EdgeSharingWithData : EDGE_AND_DATA_SHARING 
  where type data = data

sharing EdgeSharing = EdgeSharingWithData;
sharing Map.Sharing = EdgeSharingWithData.Map;
sharing InjEndo.Sharing = EdgeSharingWithData.InjEndo;

end;




functor EdgeFun(Nm : NAME_AND_UNIFIABLE_DATA)
 : EDGE
= struct
open Nm;

structure Ehn = EhnFun(Nm); 

val add_start_ehns = 
    NSet.fold (Ehn.NSet.add o Ehn.mk_start);
val add_end_ehns = 
    NSet.fold (Ehn.NSet.add o Ehn.mk_end);

structure EdgeSharing = struct 
  open Sharing; 
  structure Ehn = Ehn.EhnSharing; 
end;
structure EdgeSharingWithData = struct 
  open EdgeSharing; 
  type data = data;
  structure Map = Map.Sharing;
  structure InjEndo = InjEndo.Sharing;
end;

end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Kinds of data *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 

structure StrData : UNIFIABLE_DATA where type data = string 
= struct 
type data = string
val pretty_data = Pretty.str;
val print_data = Pretty.writeln o pretty_data;
val data_ord = String.compare;
fun data_eq (a,b) = (a = (b : string));
type unif = unit
val empty_unif = ();
fun compose_unif _ = ();
fun unify_data x _ = if data_eq x then SOME () else NONE;
fun subst_in_data u d = d;
fun pretty_unif _ = Pretty.str "()";
val print_unif = Pretty.writeln o pretty_unif;
end;

structure UnitData : UNIFIABLE_DATA where type data = unit 
= struct 
type data = unit
val pretty_data = K (Pretty.str "");
val print_data = Pretty.writeln o pretty_data;
val data_ord = K EQUAL;
val data_eq = K true;
type unif = unit
val empty_unif = ();
fun compose_unif _ = ();
fun unify_data x _ = if data_eq x then SOME () else NONE;
fun subst_in_data u d = d;
fun pretty_unif _ = Pretty.str "()";
val print_unif = Pretty.writeln o pretty_unif;
end;

structure StrNmStrData 
: NAME_AND_UNIFIABLE_DATA where type data = string
= struct
open SStrName;
val default_name = mk "a";
open StrData;
val default_data = "";
structure Map = NameMapFun(structure Dom = SStrName and Cod = SStrName);
structure InjEndo = NameInjEndoFun(SStrName);
structure SharingWithData = struct 
  open Sharing; 
  type data = data;
  type unif = unit;
  structure Map = Map.Sharing;
  structure InjEndo = InjEndo.Sharing;
  end;
end;

structure StrNmUnitData 
: NAME_AND_UNIFIABLE_DATA where type data = unit
= struct
open SStrName;
val default_name = mk "a";
open UnitData;
val default_data = ();
structure Map = NameMapFun(structure Dom = SStrName and Cod = SStrName);
structure InjEndo = NameInjEndoFun(SStrName);
structure SharingWithData = struct 
  open Sharing; 
  type data = data; 
  type unif = unit;
  structure Map = Map.Sharing;
  structure InjEndo = InjEndo.Sharing;
  end;
end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Some instances of edges and vertices *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(* BUG BUG BUG!!! PolyML: 
if you make the signature constraint strict, you get: 
StrVertex.NSet.empty : StrVertex.Rnm.T
*)
structure StrVertex 
:> VERTEX where type data = string
= struct
open StrNmStrData;
val default_name = mk "Va";
end;

structure UnitVertex 
:> VERTEX where type data = unit
= struct
open StrNmUnitData;
val default_name = mk "Va";
end;

structure UnitEdge 
:> EDGE where type data = unit
= struct
structure E = EdgeFun(StrNmUnitData);
open E;
val default_name = mk "Ea";
end;

structure StrEdge 
:> EDGE where type data = string
= struct
structure E = EdgeFun(StrNmStrData);
open E;
val default_name = mk "Ea";
end;

