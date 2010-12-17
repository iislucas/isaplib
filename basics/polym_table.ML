(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  Title:      libs/polym_table.ML
    Author:     Lucas Dixon, University of Edinburgh
                lucas.dixon@ed.ac.uk
*)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  DESCRIPTION:
    Polymorphic Tables
*)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 

signature POLYM_TABLE =
sig

  exception no_such_kind_exp of string;
  exception conflicting_kind_exp of string;

  (* Polymorphic object wrapper using exceptions *)
  datatype obj = Obj of exn

  (* kinds of data, each one is unique *)
  type kind 
  (* constructs a new unique kind with name given by string *)
  val new_kind : string -> kind 
  val name_of_kind : kind -> string
  (* raises conflicting_kind_exp is there are two different kinds with the same name being 
     compared. *)
  val kind_ord : kind * kind -> order 

  (* table of kinds *)
  structure KTab : TABLE where type key = kind;

  (* *)
  type T = obj KTab.table;

  (* *)
  val empty : T

  (* raises "no_such_kind_exp" if no such kind *)
  val get : kind -> (obj -> 'a) -> T -> 'a

  (* set inserts a new kind into table if it's not already there *)
  val set : kind -> (* kind of the object *)
                  ('a -> obj) -> (* data encoding exception *)
                  'a -> (* data *)
                  T -> T

  (* *)
  val update : kind -> (* kind of the object *)
                  (obj -> 'a) -> (* decoding *)
                  ('a -> obj) -> (* encoding exception *)
                  ('a -> 'a) -> (* update data function *)
                  T -> T

  (* tries to merge entries, if defined in both, takes the latter value *)
  (* IMPROVE: maybe parameterise by a notion of data merging? *)
  val merge : T -> T -> T

end;


structure PolymTable : POLYM_TABLE 
= struct
  exception no_such_kind_exp of string;
  exception conflicting_kind_exp of string;
  
  datatype obj = Obj of exn;

  datatype kind = Kind of string * stamp;
  fun new_kind name = Kind (name, stamp ());
  fun name_of_kind (Kind (name, _)) = name;
  fun kind_ord (Kind (name1,stamp1),Kind (name2,stamp2)) = 
      (case fast_string_ord (name1,name2) of EQUAL => 
        if stamp1 = stamp2 then EQUAL else 
        raise conflicting_kind_exp ("The kind '" ^ name1 ^ "'has been created twice; it should only exist once in each table")
      | ord => ord);

  structure KTab = Table(type key = kind val ord = kind_ord);

  (* kind is a string, and object is polymorphic via exceptions *)
  type T = obj KTab.table; 
  
  val empty = KTab.empty;
  
  (* *)
  fun get k decf tbl = 
      case KTab.lookup tbl k of 
        SOME ob => decf ob
      | NONE => raise no_such_kind_exp ("'" ^ (name_of_kind k) ^ "' does not exist in the table.");  
  
  (* sets/inserts a kind and value in the table *)
  fun set k encf ob tbl = 
      KTab.update (k,encf ob) tbl;
  
  (* for updating information using a function "f" once a kind exists *)
  fun update k decf encf f tbl = 
      KTab.map_entry k (fn ob => encf (f (decf ob))) tbl;

  (* merge entries. If defined in both tables, takes the latter value. 
     assumes disjointness of kinds (else raises exception) *)
  (* IMPROVE: maybe parameterise by a notion of data merging? *)
  fun merge tbl1 tbl2 = 
      KTab.fold (fn (k,ob) => fn tbl2' => case KTab.lookup tbl2' k
                                 of NONE => KTab.update (k,ob) tbl2'
                                  | SOME _ => tbl2')
                tbl1
                tbl2;


end;

(* Signature for basic things you might want to do with a particular kind of polymorphic table 
   entry *)
signature POLYM_ENTRY = 
sig
  val kind : PolymTable.kind
  type T
  val get : PolymTable.T -> T
  val set : T -> PolymTable.T -> PolymTable.T
  val update : (T -> T) -> PolymTable.T -> PolymTable.T
end;

(* construct a new polymorphic entry from a given type. Note: you should never have two different instances of this functor (polymorphic entries) with the same name in the same table: if you do, you'll get an exception (PolymTable.conflicting_kind_exp ...)  *)
functor PolymEntryFun(type T val name : string) : POLYM_ENTRY = 
struct 
  val kind = PolymTable.new_kind name;
  type T = T;
  exception DataWrapper of T;
  
  val encf = PolymTable.Obj o DataWrapper;
  (* an intresting trick: pattern match in a function on an exception constructor *) 
  val decf = (fn PolymTable.Obj (DataWrapper x) => x); 
  
  (* basic contextual information manipulations *)
  val get = PolymTable.get kind decf;
  val set = PolymTable.set kind encf;
  val update = PolymTable.update kind decf encf;
end;

