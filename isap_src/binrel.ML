(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  Title:      binrel.ML
    Author:     Lucas Dixon, University of Edinburgh
                lucas.dixon@ed.ac.uk
    Updated:    10 Dec 2008
    Date:       10 Dec 2008
*)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  
    Binary Relations over finite name sets 
    (objects with ordered and fresh construction operations)
    e.g. two way linked dependencies on names

    POSSIBLE IMPROVEMENT: lazy updates? 
*)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
signature BINREL 
= sig

structure A : NAMES (* argument 1 name type *)
structure B : NAMES (* argument 2 name type *)
type T

val empty : T

(* getting related sets of names *)
fun get_arels : T -> A.name -> B.NSet.T (* is empty is none *)
fun get_brels : T -> B.name -> A.NSet.T (* is empty is none *)

(* 1-1 relations *)
val add1_toA : A.name -> B.name -> T -> T
val add1_toA : B.name -> A.name -> T -> T

val del1_fromA : A.name -> B.name -> T -> T
val del1_fromA : B.name -> A.name -> T -> T

(* rename *)
val renameA : A.name -> A.name -> T -> T
val renameB : B.name -> B.name -> T -> T

(* 1 to many relations *)
val add_toA : A.name -> B.NSet.T -> T -> T
val add_toB : B.name -> A.NSet.T -> T -> T

val del_fromA : A.name -> B.NSet.T -> T -> T
val del_fromB : B.name -> A.NSet.T -> T -> T

(* shorthand each thing in first set related to each thing in second *)
val add_many : A.NSet.T * B.NSet.T -> T -> T
val subtr_many : A.NSet.T * B.NSet.T -> T -> T

(* pretty printing *)
val pretty : T -> Pretty.T
val print : T -> unit

(* internal: is NONE is no entry *)
fun lookup_arels : T -> A.name -> B.NSet.T option
fun lookup_brels : T -> B.name -> A.NSet.T option


end;

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
functor BinRelFun(structure A : NAMES and B : NAMES) 
: BINREL
= struct

structure A : NAMES (* argument 1 name type *)
structure B : NAMES (* argument 2 name type *)
type T

(* datatype for meta variable environments *)
(* invarient: *)
datatype T = BinRel of {
         AtoB : B.NSet.T A.NTab.T,
         BtoA : A.NSet.T B.NTab.T
};

val empty = BinRel {AtoB = A.NTab.empty,
                    BtoA = B.NTab.empty};

fun get_AtoB (BinRel rep) = #AtoB rep;
fun get_BtoA (BinRel rep) = #BtoA rep;

fun update_AtoB f (BinRel rep) = 
    BinRel {AtoB = f (#AtoB rep), BtoA = (#BtoA rep)}

fun update_BtoA f (BinRel rep) = 
    BinRel {AtoB = (#AtoB rep), BtoA = f (#BtoA rep)}

(* implicit: rel n *)
val lookup_arels = A.NTab.lookup o get_AtoB;
val lookup_brels = B.NTab.lookup o get_BtoA;

fun get_arels rel n = 
    case lookup_arels rel n of NONE => B.NSet.empty

fun get_brels rel n = 
    case lookup_brels rel n of NONE => A.NSet.empty

(* 1 to many relations *)
fun add_toA a bset rel = 
    rel |> update_AtoB 
        (fn abtab => 
            case A.NTab.lookup abtab a of
              NONE => A.NTab.update (a,bset) abtab
            | SOME oldbset => 
              A.NTab.update 
                (a,B.NSet.union_merge bset oldbset) abtab )
        |> update_BtoA 
        (fn batab => 
            B.NSet.fold 
              (fn b => batab2 => 
                  case B.NTab.lookup batab2 a of
                    NONE => B.NTab.ins (b,A.NSet.single a) batab2
                  | SOME aset => B.NTab.update (b,A.NSet.add a aset) batab2
              ) bset batab );

(* copy of above with a/b replaces *)
fun add_toB b aset rel = 
    rel |> update_BtoA
        (fn batab => 
            case B.NTab.lookup batab b of
              NONE => A.NTab.update (b,aset) batab
            | SOME oldaset => 
              B.NTab.update 
                (b,A.NSet.union_merge aset oldaset) batab )
        |> update_AtoB
        (fn abtab => 
            A.NSet.fold 
              (fn a => abtab2 => 
                  case A.NTab.lookup abtab2 b of
                    NONE => A.NTab.ins (a,B.NSet.single b) batab2
                  | SOME bset => A.NTab.update (a,B.NSet.add b bset) abtab2
              ) aset abtab );



val del_fromA : A.name -> B.NSet.T -> T -> T
val del_fromB : B.name -> A.NSet.T -> T -> T


(* 1-1 relations *)
fun add1_toA an bn rel = 
    

: A.name -> B.name -> T -> T
val add1_toA : B.name -> A.name -> T -> T

val del1_fromA : A.name -> B.name -> T -> T
val del1_fromA : B.name -> A.name -> T -> T



end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
structure DKind : DEPKIND = 
struct 

type dep = string; (* a single dependent object *)
type T = string list; (* a collection of things that depend *)

val empty = [];
fun single x = [x];

val uni = curry Library.union_string;
val int = curry Library.inter_string;
val min : T -> T -> T = (Library.subtract (op =));

val add1 = uni o single;
val drop1 = min o single;

val of_list : dep list -> T = Library.distinct (op =);
val list_of : T -> dep list = I;

val compare = fast_string_ord;
val dep_eq = Library.is_equal o fast_string_ord;

val pretty_dep = Pretty.str;
val pretty = Pretty.list "[" "]" o map pretty_dep
val print_dep = Pretty.writeln o pretty_dep;
val print = Pretty.writeln o pretty;

end;


signature DEPENV
= sig
    structure DKind : DEPKIND 
    structure VarDeps : TABLE where type key = Trm.Var.name
    structure TVarDeps : TABLE where type key = Trm.TVar.name

    type vname = VarDeps.key
    type tvname = TVarDeps.key

    exception lookup_var_exp of string * vname;
    exception lookup_tvar_exp of string * tvname;

    (* dependency table, where 'r is the type for names of things that
    are dependencies are for, eg names of theorems *)
    type T

    val add : DKind.T * tvname list * vname list -> T -> T
    val add_tvar : tvname * DKind.T -> T -> T
    val add_var : vname * DKind.T -> T -> T
(*     val remove_tvar : tvname -> T -> T
    val remove_var : vname -> T -> T *)
    val empty : T

    val drop_tvdep : DKind.dep -> tvname -> T -> T 
    val drop_vdep : DKind.dep -> vname -> T -> T

    val remove_tvar : tvname -> T -> T 
    val remove_var : vname -> T -> T

    val try_remove_tvar : tvname -> T -> T 
    val try_remove_var : vname -> T -> T

    val union1_tvdep : DKind.dep -> tvname -> T -> T 
    val union1_vdep : DKind.dep -> vname -> T -> T

    val union_tvdep : DKind.T -> tvname -> T -> T 
    val union_vdep : DKind.T -> vname -> T -> T

    val lookup_deps : T -> vname -> DKind.T
    val lookup_tdeps : T -> tvname -> DKind.T

    val get_tvars : T -> tvname list
    val get_tvardeps : T -> (tvname * DKind.T) list
    val get_tvartab : T -> DKind.T TVarDeps.table

    val get_vars : T -> vname list
    val get_vardeps : T -> (vname * DKind.T) list
    val get_vartab : T -> DKind.T VarDeps.table

    val pretty : Context.theory -> T -> Pretty.T
end;


(* A variable environment structure that eagerly updates instantiations *)
functor DepEnvFUN(DKind : DEPKIND)
: DEPENV 
= struct 

structure DKind = DKind;

type vname = Trm.Var.name;
type tvname = Trm.TVar.name;
val vord = Trm.Var.NTab.ord;
val tvord = Trm.TVar.NTab.ord;

structure VarDeps : TABLE where type key = vname 
= TableFun(type key = vname val ord = vord);
structure TVarDeps : TABLE where type key = tvname 
= TableFun(type key = tvname val ord = tvord);

    exception lookup_var_exp of string * vname;
    exception lookup_tvar_exp of string * tvname;

(* datatype for meta variable environments *)
datatype T = Varenv of {
         vars : DKind.T VarDeps.table,
         tvars : DKind.T TVarDeps.table,
         
};

(* make var env for node i *)
val empty = Varenv {vars = VarDeps.empty,
                    tvars = TVarDeps.empty };

fun add_to_vdeps iL vars tab = 
    List.foldr 
      (fn (s,tab) => 
          (case VarDeps.lookup tab s of 
             NONE => VarDeps.update (s,iL) tab
           | SOME L => VarDeps.update (s,DKind.uni iL L) tab))
      tab vars;

fun add_to_tvdeps iL vars tab = 
    List.foldr 
      (fn (s,tab) => 
          (case TVarDeps.lookup tab s of 
             NONE => TVarDeps.update (s,iL) tab
           | SOME L => TVarDeps.update (s,DKind.uni iL L) tab))
      tab vars;

fun app_tvars f (Varenv rep) = 
    Varenv {vars = #vars rep,
            tvars = f (#tvars rep)};
fun app_vars f (Varenv rep) = 
    Varenv {vars = f (#vars rep),
            tvars = #tvars rep};

fun add (iL, tvars, vars) (Varenv rep) = 
    Varenv {vars = add_to_vdeps iL vars (#vars rep),
            tvars = add_to_tvdeps iL tvars (#tvars rep) };

fun add_var (var, iL) = 
    app_vars (add_to_vdeps iL [var]);
fun add_tvar (tvar, iL) = 
    app_tvars (add_to_tvdeps iL [tvar]);

fun remove_tvar v = app_tvars (TVarDeps.delete v);
fun remove_var v = app_vars (VarDeps.delete v);

fun rename_var 
fun rename_tvar
fun rename_dep 

fun try_remove_tvar v t = 
    app_tvars (TVarDeps.delete v) t handle TVarDeps.UNDEF _ => t;
fun try_remove_var v t = 
    app_vars (VarDeps.delete v) t handle VarDeps.UNDEF _ => t;

fun drop_tvdep i s = 
    app_tvars
      (fn tab => 
          case TVarDeps.lookup tab s of 
            NONE => tab
(* raise lookup_tvar_exp ("drop_tvdep: no such tvar", s) *)
          | SOME L => TVarDeps.update (s,DKind.drop1 i L) tab);

fun drop_vdep i s = 
    app_vars
      (fn tab => 
          case VarDeps.lookup tab s of 
            NONE => tab
(* raise lookup_var_exp ("drop_vdep: no such var", s) *)
          | SOME L => VarDeps.update (s,DKind.drop1 i L) tab);

fun union1_tvdep i s = 
    app_tvars
      (fn tab => 
          case TVarDeps.lookup tab s of 
            NONE => TVarDeps.update (s,DKind.single i) tab
          | SOME L => TVarDeps.update (s,DKind.add1 i L) tab);

fun union1_vdep i s = 
    app_vars
    (fn tab => 
        case VarDeps.lookup tab s of 
          NONE => VarDeps.update (s,DKind.single i) tab
        | SOME L => VarDeps.update (s,DKind.add1 i L) tab);


fun union_tvdep L2 s = 
    app_tvars
      (fn tab => 
          case TVarDeps.lookup tab s of 
            NONE => TVarDeps.update (s,L2) tab
          | SOME L => TVarDeps.update (s,DKind.uni L2 L) tab);

fun union_vdep L2 s = 
    app_vars
    (fn tab => 
        case VarDeps.lookup tab s of 
          NONE => VarDeps.update (s,L2) tab
        | SOME L => VarDeps.update (s,DKind.uni L2 L) tab);



fun get_vartab (Varenv rep) = #vars rep;
fun get_tvartab (Varenv rep) = #tvars rep;

fun lookup_tdeps venv s = 
    case TVarDeps.lookup (get_tvartab venv) s of 
      NONE => DKind.empty
(* raise lookup_tvar_exp ("lookup_tdeps: no such tvar", s) *)
    | SOME ds => ds;
fun lookup_deps venv s = 
    case VarDeps.lookup (get_vartab venv) s of 
      NONE => DKind.empty
(* raise lookup_var_exp ("lookup_deps: no such var", s) *)
    | SOME ds => ds;

(* use fold for speed (not dest) - avoid rev operation 
   in table implementation *)
fun get_vars venv = VarDeps.fold (cons o fst) (get_vartab venv) [];
fun get_tvars venv = TVarDeps.fold (cons o fst) (get_tvartab venv) [];

fun get_vardeps venv = VarDeps.fold cons (get_vartab venv) [];
fun get_tvardeps venv = TVarDeps.fold cons (get_tvartab venv) [];


fun pretty_deps vpf (v, deps) = 
    Pretty.block 
      [vpf v,
       Pretty.str "--" ,
       DKind.pretty deps];

fun pretty thry (Varenv rep) = 
    Pretty.chunks
      [
       Pretty.str "Type Variables: ",
       Pretty.indent 
         2 (Pretty.chunks 
              (map (pretty_deps Trm.TVar.pretty_name)
                   (TVarDeps.dest (#tvars rep)))),
       Pretty.str "Term Variables: ",
       Pretty.indent 
         2 (Pretty.chunks 
              (map (pretty_deps (Trm.Var.pretty_name))
                   (VarDeps.dest (#vars rep))))];

end;

structure DepEnv = DepEnvFUN(DKind);

(*  LocalWords:  sig
 *)
