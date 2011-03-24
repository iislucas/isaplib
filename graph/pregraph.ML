(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*   Some preliminaries for Generic Graphs (Vertixes and Edges)            *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(* 
  EHN is for Edge Half Names. I.e. an edge half is either the src end of the 
  edge, or the target end of the edge. 
*)
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
  val ord_half : halfid * halfid -> order (* Src < Tgt *)

  type edgename;
  include NAMES;
  structure EhnSharing : EHN_SHARING where type halfid = halfid
  sharing Sharing = EhnSharing;

  structure InjEndo : NAME_INJENDO;
  sharing InjEndo.Dom.Sharing = Sharing; 

  val get_ename : name -> edgename
  val get_halfid : name -> halfid

  val same_half : name -> name -> bool

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

  fun ord_half (TargetEnd, TargetEnd) = EQUAL
    | ord_half (TargetEnd, SourceEnd) = GREATER
    | ord_half (SourceEnd, TargetEnd) = LESS
    | ord_half (SourceEnd, SourceEnd) = EQUAL

  structure EhnN = AddDataNameFun(
    structure Nm = Nm
    type data = halfid
    val ord_data = ord_half;
    fun pretty (TargetEnd,n) = Pretty.block [Pretty.str "tgt:", Nm.pretty_name n]
      | pretty (SourceEnd,n) = Pretty.block [Pretty.str "src:", Nm.pretty_name n]
    val default_name = (TargetEnd, Nm.default_name));
  open EhnN;

  structure EhnSharing = struct open Sharing; type halfid = halfid; end;
  
  structure InjEndo = NameInjEndoFun(EhnN);

  fun get_ename ((h,en) : name) = en;
  fun get_halfid ((h,en) : name) = h;
  fun same_half ((h,en) : name) ((h2,en2) : name) = (ord_half (h,h2) = EQUAL);
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

signature EDGE_SHARING = sig 
  include NAMES_SHARING; structure Ehn : EHN_SHARING; 
end;

(* a classic case of multiple ways to express this, and a nice one that uses multiple overlapping inheritence: 
include EDGE_SHARING + NAME_AND_DATA
*)
signature EDGE_AND_DATA_SHARING = sig 
  include NAME_AND_DATA_SHARING; structure Ehn : EHN_SHARING; 
end;

signature EDGE_AND_UNIFIABLE_DATA_SHARING = sig 
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
  
  structure EdgeSharingWithData : EDGE_AND_UNIFIABLE_DATA_SHARING 
    where type data = data and type subst = subst
  
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
    type subst = subst;
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
  type subst = unit
  val empty_subst = ();
  fun compose_subst _ = ();
  fun unify_data x _ = if data_eq x then SOME () else NONE;
  fun match_data x _ = if data_eq x then SOME () else NONE;
  fun subst_in_data u d = d;
  fun pretty_subst _ = Pretty.str "()";
  val print_subst = Pretty.writeln o pretty_subst;
end;

structure UnitData : UNIFIABLE_DATA where type data = unit 
= struct 
  type data = unit
  val pretty_data = K (Pretty.str "");
  val print_data = Pretty.writeln o pretty_data;
  val data_ord = K EQUAL;
  val data_eq = K true;
  type subst = unit
  val empty_subst = ();
  fun compose_subst _ = ();
  fun unify_data x _ = if data_eq x then SOME () else NONE;
  fun match_data x _ = if data_eq x then SOME () else NONE;
  fun subst_in_data u d = d;
  fun pretty_subst _ = Pretty.str "()";
  val print_subst = Pretty.writeln o pretty_subst;
end;



structure SStrNameAndMaps 
(* : SSTR_NAMES_AND_MAPS *) 
= struct
  open SStrName;
  val default_name = mk "a";
  structure Map = NameMapFun(structure Dom = SStrName and Cod = SStrName);
  structure InjEndo = NameInjEndoFun(SStrName);
end;


(* *)
functor StrNmData(
  structure D : UNIFIABLE_DATA
  val default_data : D.data
) : NAME_AND_UNIFIABLE_DATA
= struct
  open SStrNameAndMaps;
  open D;
  val default_data = default_data;

  structure SharingWithData = struct 
    open Sharing; 
    type data = D.data;
    type subst = D.subst;
    structure Map = Map.Sharing;
    structure InjEndo = InjEndo.Sharing;
  end;
  (* signature constraint will remove what we want to hide *)
  structure SharingWithoutData = SharingWithData;
end;


structure StrNmStrData = 
StrNmData(structure D = StrData 
          val default_data = "");

structure StrNmUnitData = 
StrNmData(structure D = UnitData 
          val default_data = ());

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Some instances of edges and vertices *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
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

