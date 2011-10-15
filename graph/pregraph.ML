(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*   Some preliminaries for Generic Graphs (Vertixes and Edges)            *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(* 
  EHN is for Edge Half Names. I.e. an edge half is either the src end of the 
  edge, or the target end of the edge. 
*)
signature EHN_SHARING = 
sig include NAME_SHARING; type halfid; end;

(* names for half edges and edge directions *)
signature EHN_NAME
= sig

(* my own bool type for clarity, 
   Its used for two meaning: 1. direction, 2. half edge id. 
   TargetEnd = normal direction, Target-end
   SourceEnd = reversed direction, Source-end
   TODO: check is it used for direction? Refactor: start and end => src and tgt.
*)
  datatype halfid = TargetEnd | SourceEnd;
  val other_half : halfid -> halfid (* not *)
  val ord_half : halfid * halfid -> order (* Src < Tgt *)

  type edgename;
  include SNAME
    where type src = halfid * edgename; 
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

functor EhnFun(Nm : NAME) 
: EHN_NAME 
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
      
  type src = halfid * edgename;
  fun mk (h,e) = ((h,e) : name);
  fun dest ((h,e) : name) = (h,e);

  fun mk_start e = mk (SourceEnd,e);
  fun mk_end e = mk (TargetEnd,e);
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