(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  Title:      isaplib/isap_src/nametab.ML (sym link in src/generic/)
    Author:     Lucas Dixon, University of Edinburgh
                lucas.dixon@ed.ac.uk
                Initially written Oct 2005, Updated 27 Dec 2008        *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  DESCRIPTION:
    Table with names as keys. 
    for holding things associated with nice fresh namers. 

    Each entry has a name - but we allow some names not to have
    entries: you can use up names without providing entries.

    Note: Currently 
*)

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
signature NAME_TAB =
sig

  type 'a T
  type name

  structure NSet : NAME_SET sharing type NSet.name = name;
  structure Tab : TABLE sharing type Tab.key = name;

  val empty : 'a T
  val is_name_empty : 'a T -> bool 
  val is_element_empty : 'a T -> bool (* this implies is_name_empty *)

  val is_name_singleton : 'a T -> bool 
  val is_element_singleton : 'a T -> bool (* this implies is_name_singleton *)
  val tryget_singleton : 'a T -> (name * 'a) option

  val try_ins : (name * 'a) -> 'a T -> 'a T option
  val ins : (name * 'a) -> 'a T -> 'a T (* raises on duplicate *)
  val add : (name * 'a) -> 'a T -> (name * 'a T)
  val doadd : (name * 'a) -> 'a T -> 'a T (* add with new name, don't tell me *)

  (* create new name and use it up: update must be used to set entry's data *)
  val new_name : name -> 'a T -> (name * 'a T)
  (* just use-up a list of names *)
  (* IMPROVE: avoid conversion to lists as uses up lots of memory:
     have a function in FNAMETAB to useup names from a name collection *)
  val useup_names : NSet.T -> 'a T -> 'a T

  val rename : name -> name -> 'a T -> 'a T (* raises on duplicate *)

  val lookup : 'a T -> name -> 'a option
  val contains : 'a T -> name -> bool
  val contains_name : 'a T -> name -> bool (* some names may be used 
                                              without having entries *)
  val forall : ((name * 'a) -> bool) -> 'a T -> bool
  val exists : ((name * 'a) -> bool) -> 'a T -> bool

  val get : 'a T -> name -> 'a (* raises Tab.UNDEF *)
  val next_smaller : 'a T -> name -> (name * 'a) option
  val next_bigger : 'a T -> name -> (name * 'a) option
  val pull_elem : 'a T -> ((name * 'a) * 'a T) option
  val map_entry : ('a -> 'a) -> name -> 'a T -> 'a T
  val map_all : ('a -> 'b) -> 'a T -> 'b T
  val map_all' : (name -> 'a -> 'b) -> 'a T -> 'b T
  val fold : ((name * 'a) -> 'b -> 'b) -> 'a T -> 'b -> 'b
  val update : (name * 'a) -> 'a T -> 'a T  (* no exp, replaces old name *)
  val update_new : (name * 'a) -> 'a T -> 'a T (* exp if not fresh for table *)

  val delete : name -> 'a T -> 'a T

  (* get the the biggest/smallest element (log time) *)
  val lookup_biggest : 'a T -> (name * 'a) option;
  val lookup_smallest : 'a T -> (name * 'a) option;

  (* pull out the biggest/smallest element (log time) *)
  val pull_biggest : 'a T -> ((name * 'a) * 'a T) option;
  val pull_smallest : 'a T -> ((name * 'a) * 'a T) option;

  val merge_disjoint : 'a T -> 'a T -> 'a T (* note: must be disjoint: raises Tab.DUP *)
  val merge_joint : (* exception on clash noneq elements *)
      ('a * 'a -> bool) (* return true if they are same (i.e. good) *)
      -> 'a T -> 'a T -> 'a T 

  (* values that are in the first but not the second *)
  val difference : 'a T -> 'a T -> 'a T 

  val list_of : 'a T -> (name * 'a) list
  val keys : 'a T -> name list
  val values : 'a T -> 'a list
  val get_nameset : 'a T -> NSet.T;

  val ord : name * name -> order
  val name_eq : name * name -> bool

  (* pretty *)
  val string_of_name : name -> string
  val pretty_name : name -> Pretty.T
  val pretty_ntree: 'a T -> Pretty.T
  val pretty_as_list : ('a -> Pretty.T) -> 'a T -> Pretty.T
  val pretty : ('a -> Pretty.T) -> 'a T -> Pretty.T
  val print : ('a -> Pretty.T) -> 'a T -> unit
(*   val direct_pretty : 'a T -> Pretty.T
  val direct_print : 'a T -> unit *)
end;


functor NameTabFun(NSet : NAME_SET)
: NAME_TAB 
= struct

  structure NSet = NSet;

  type name = NSet.N.name;

  val pretty_name  = NSet.N.pretty

(*   fun pretty_name (n,i) = 
      Pretty.block [N.pretty_name n, Pretty.str (Int.toString i)]; *)
  val string_of_name = Pretty.string_of o pretty_name;

  fun ord (a, b) = NSet.N.ord (a,b);

  fun name_eq (a, b) = (ord (a,b) = EQUAL);

(*       (Library.prod_ord NSet.ord Library.int_ord) (a,b); *)

  structure Tab = TableFun(type key = name val ord = ord );

  datatype 'a T = 
           Tab of {names : NSet.T,
                   tab : 'a Tab.table}

  fun get_tab (Tab rep) = #tab rep;
  fun get_nameset (Tab rep) = #names rep;

  val empty = Tab {names = NSet.empty,
                   tab = Tab.empty};

  fun is_name_empty (Tab {names, tab}) = NSet.is_empty names;
  fun is_element_empty (Tab {names, tab}) = Tab.is_empty tab;

  fun is_name_singleton (Tab {names, tab}) = NSet.is_singleton names;
  (* IMPROVE: could add this to table library *)
  fun tryget_singleton (Tab {names, tab}) = 
      (case NSet.tryget_singleton names 
        of NONE => NONE
         | SOME n => 
           (case Tab.lookup tab n of SOME x => SOME (n,x) 
                                    | NONE => NONE));
  fun is_element_singleton ntab = 
      (case tryget_singleton ntab of NONE => false | SOME _ => true);

  fun try_ins (x as (n,a)) (Tab rep) = 
      let val names = (#names rep) in
        case NSet.lookup names n
         of NONE => SOME (Tab{ names = NSet.add n names,
                               tab = Tab.update_new x (#tab rep) })
          | SOME _ => NONE
      end;

  (* NSet.ins_fresh and Tab.update_new raise exceptions on duplicates *) 
  fun ins (x as (n,a)) (Tab rep) = 
      (Tab{ names = NSet.ins_fresh n (#names rep),
            tab = Tab.update_new x (#tab rep) });

  fun map_entry f n (Tab rep) = 
      Tab{ names = #names rep,
           tab = Tab.map_entry n f (#tab rep) };

  fun map_all f (Tab rep) = 
      Tab{ names = #names rep,
           tab = Tab.map f (#tab rep) };

  fun map_all' f (Tab rep) = 
      Tab{ names = #names rep,
           tab = Tab.map' f (#tab rep) };

  fun fold f (Tab rep) = Tab.fold f (#tab rep);

  fun exists f (Tab rep) = Tab.exists f (#tab rep);
  fun forall f (Tab rep) = Tab.forall f (#tab rep);


  fun update (x as (n,a)) (Tab rep) = 
      Tab{ names = NSet.add n (#names rep),
           tab = Tab.update x (#tab rep) };

  fun update_new (x as (n,a)) (Tab rep) = 
      Tab{ names = NSet.add n (#names rep),
           tab = Tab.update_new x (#tab rep) };

  fun delete n (Tab rep) = 
      Tab{ names = NSet.delete n (#names rep),
           tab = Tab.delete n (#tab rep) };

  fun add (x as (n,a)) (Tab rep) =
      let val names = #names rep in
        case NSet.lookup names n
         of NONE => (n, Tab{ names = NSet.add n names,
                             tab = Tab.update_new x (#tab rep) })
          | SOME (_,t) => 
            let val n' = NSet.N.suc t in
              (n', Tab{ names = NSet.add n' names,
                        tab = Tab.update_new (n',a) (#tab rep) })
            end
      end;

  fun doadd x = snd o add x;

  fun new_name n (Tab rep) = 
      let val (n2,names2) = NSet.add_new n (#names rep) in 
        (n2, Tab{ names = names2, tab = #tab rep })
      end;

  fun useup_names names (Tab rep) = 
      Tab{ names = NSet.union_merge names (#names rep),
           tab = #tab rep};

  fun lookup (Tab rep) n =
      Tab.lookup (#tab rep) n;

  (* implicit name argument *)
  fun contains_name (Tab rep) = NSet.contains (#names rep);

  fun contains (Tab rep) n =
      (case Tab.lookup (#tab rep) n of NONE => false 
                                      | _ => true);

  (* rename an entry in a table *)
  fun rename n1 n2 (Tab rep) = 
      (case Tab.lookup (#tab rep) n1
        of NONE => 
           raise Tab.UNDEF n1
         | SOME x => 
           Tab{ names = (#names rep) |> NSet.rename n1 n2,
                tab = (#tab rep) |> Tab.delete n1
                                 |> Tab.update (n2,x) });

  fun get (Tab rep) n =
      case Tab.lookup (#tab rep) n 
       of NONE => 
          raise Tab.UNDEF n
        | SOME x => x;

  (* FIXME & IMPROVE: may raise an exception if tab contains used
  names without entries: fix by adding a get_smaller to table.ML *)
  fun next_smaller tab n = 
      (case NSet.next_smaller (get_nameset tab) n 
        of NONE => NONE
         | SOME n2 => SOME (n2,get tab n2));
  fun next_bigger tab n = 
      (case NSet.next_bigger (get_nameset tab) n 
        of NONE => NONE
         | SOME n2 => SOME (n2,get tab n2));

(* assumes names = content *)
  fun pull_elem (tab as Tab rep) = 
      (case NSet.pull_local_bot (#names rep) of 
         NONE => NONE 
       | SOME (n,names2) => 
         SOME ((n,get tab n), 
               Tab {names = names2, tab = Tab.delete n (#tab rep)}));

  (* pull out the biggest/smallest element (log time) *)
  fun lookup_biggest (tb as Tab rep) = 
      (case Tab.max_key (#tab rep) 
        of NONE => NONE
         | SOME n => SOME (n,get tb n));

  fun lookup_smallest (tb as Tab rep) = 
      (case Tab.min_key (#tab rep) 
        of NONE => NONE
         | SOME n => SOME (n,get tb n));

  (* pop off the biggest/smallest element (log time) *)
  fun pull_biggest tb = 
      (case lookup_biggest tb 
        of NONE => NONE
         | SOME (v as (n,_)) => SOME (v, delete n tb));
  fun pull_smallest (tb as Tab rep) = 
      (case lookup_smallest tb 
        of NONE => NONE
         | SOME (v as (n,_)) => SOME (v, delete n tb));

  (* getting all entries... *)
  fun list_of (Tab rep) = Tab.fold cons (#tab rep) [];
  fun keys (Tab rep) = Tab.fold (cons o fst) (#tab rep) [];
  fun values (Tab rep) = Tab.fold (cons o snd) (#tab rep) [];

  (* ? more efficient way? *)
  fun difference tab1 tab2 = 
      Basics.fold (fn k => if contains tab2 k then delete k else I) 
           (keys tab1) tab1;

  (* merging *)
  fun merge_disjoint t1 t2 =
      Tab.fold
        (fn (v as (n,a)) => fn t =>
            case try_ins v t
             of NONE => raise Tab.DUP n
              | SOME t2 => t2)
        (get_tab t1) t2;

  fun merge_joint chkf t1 t2 =
      Tab.fold
        (fn (v as (n,a)) => fn t => 
            case lookup t n
             of NONE => update_new (n,a) t
              | SOME a2 => 
                if chkf (a,a2) then t else 
                raise Tab.DUP n)
        (get_tab t1) t2;

  (* pretty printing *)
  fun pretty f t = 
      Pretty.chunks 
        (map 
           (fn (k,v) => 
               Pretty.block 
                 [ pretty_name k, 
                   Pretty.str " => ",
                   f v ])
           (Tab.dest (get_tab t)));

  (* pretty printing *)
  fun pretty_as_list f t = 
      Pretty.list "{" "}" 
        (map 
           (fn (k,v) => 
               Pretty.block 
                 [ pretty_name k, 
                   Pretty.str " => ",
                   f v ])
           (Tab.dest (get_tab t)));


  fun pretty_ntree t = 
      Pretty.chunks
      [Pretty.str "Names: ",
       NSet.pretty (get_nameset t)];

  fun print pf = Pretty.writeln o (pretty pf)

(* 
  fun direct_pretty x = pretty (Pretty.str o PolyML.makestring) x;
  fun direct_print x = Pretty.writeln (direct_pretty x)
*)

end;


structure IntNTab = NameTabFun(IntNSet);
structure StrNTab = NameTabFun(StrNSet);
structure StrIntNTab = NameTabFun(StrIntNSet);

structure IntTab = IntNTab.Tab;
structure StrTab = StrNTab.Tab;
structure StrIntTab = StrIntNTab.Tab;

(* 
install_pp (make_pp ["StrNTab", "'a T"] 
                    (Pretty.pprint o StrNTab.direct_pretty));
install_pp (make_pp ["StrIntNTab", "'a T"] 
                    (Pretty.pprint o StrIntNTab.direct_pretty));
*)
