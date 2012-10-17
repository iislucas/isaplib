(*This signature specifies a structure designed to help parallelise operations across trees.
It is designed for lazy trees (trees which produce lazy sequences) although is applicably to any
tree flow in the program. Trees should have many leaves, and the depth of the tree should be high.
The structure will process elements of the tree, returning the leaves left first (nodes are not returned).
As should be obvious, if the tree is very left or right deep, evaluation will be inefficient as it will be forced
into a sequential like process.
It automatically parallelises the process!

Warning: in the implementation there is a "node_limit" to prevent excessively large numbers
of children ever being processed at once. However, if the tree is extremely large
(or very right biased and a little smaller, but still large),
it is possible for this process to run out of memory.*)
signature MJB_PAR_TREE =
sig
  (*This is the type of returned elements
  from the functions (keep reading for this to make sense)*)
  datatype ('a,'b) elem = Node of 'b | Leaf of 'a
  
  (*The type of a parallel tree!*)
  type par_tree;
  
  (*Create a new parallelising tree. You must supply the future data that it
  should use.*)
  val new : MJB_Simple_Future.future_data (*Future data for the tree to use*)
    -> par_tree
    
  (*Get the future data in use by this parallel tree*)
  val get_future_data : par_tree
    -> MJB_Simple_Future.future_data
    
  (*Set the time that a process should take to run before being assigned
  to a core. The default (ATTOW - at the time of writing) is 4000
  microseconds. A large value will keep the groups assigned to cores
  large, so that they can run for longer on independent problems,
  improving the efficiency. If the value is too large, then the tree
  may not be big enough to enable such groups to be created.*)
  val set_time : par_tree
    -> Time.time option (* The time to use. If NONE is supplied, it will use a default*)
    -> par_tree
    
  (*Return the time to be used by the compactor. It will return the default
  time it is using if none was set.*)
  val get_time : par_tree
    -> Time.time
    
  (*Set an estimate for the current number of functions that will need to
  be combined to reach the required time specified above*)
  val set_estimate : par_tree
    -> int option (*The estimate. If NONE is supplied, a default is used*)
    -> par_tree
    
  (*Return the estimated number of groups, or a default
  if no estimate has been set*)
  val get_estimate : par_tree
    -> int
  
  (*Set the number of groups of functions that should be created
  during parallelisation. Only use this if you know how it works!
  Essentially, groups taking the time specified above (set_time) to process
  will be assigned to futures. The length is how many of these groups
  should be created before assigning the tasks to cores. Typically,
  10*the number of cores is a good way to overload the future's library.*)
  val set_no_groups : par_tree
    -> int option (*How many groupsshould be created for parallelisation.
                    If NONE is supplied, it will use a default*)
    -> par_tree
    
  (*Return the number of groups to be created before parallelisation.
  If nothing was set, it will return the default that it is going to use.*)
  val get_no_groups : par_tree
    -> int
    
  (*Set whether the order in which leaves are returned is important. Setting
  this to false means that if a leaf is discovered in the tree
  early (i.e: not the "first" one - leftmost) then the algorithm
  can stop and return that. Hence, it is more likely to take less time
  and memory when pulling leaves from the tree with this set to false.
  
  Note that nodes are still computed in order, since it is generally
  considered that leaves are more likely to appear deep in a tree.*)
  val set_order_matters : par_tree
    -> bool     (*True if the order of the leaves is important, or false otherwise*)
    -> par_tree
    
  (*Return whether or not the order the leaves are returned in is considered
  important*)
  val get_order_matters : par_tree
    -> bool
    
  (*Set the maximum number of nodes the algorithm is allowed to gather in the tree
  before parallelisation. Passing NONE will ensure there is no limit at all. A limit
  below 1 is not allowed. Typcially, a setting should be in the thousands. This can help
  to prevent the algorithm running out of memory.*)
  val set_node_limit : par_tree
    -> int option (* NONE indicates that there should be no limit. Otherwise, the maximum
                     number of nodes that may be gathered at one time. *)
    -> par_tree
    
  (*Return the current node limit in use by the tree.*)
  val get_node_limit : par_tree
    -> int option
  
  (*This function is the main work of the structure. It will evaluate the tree in a lazy way while parallelising
  as much as possible.
  
  Note: the function which returns a list of nodes or leaves can really change for each node if the node itself
        holds the function to be applied too, so this is a very generic type signature
      
  Note2: while the children returned form a lazy sequence, this will be at least mostly evaluated
  
  Note3: the node function permits nodes to have no children, so you can
         avoid worrying about this case.
  *)
  val compute_tree : par_tree
    -> ('a,'b) elem                       (*This is the root of the tree*)
    -> ('b -> ((('a,'b) elem) List.list)) (*The function which, when applied to nodes, will produce the children*)
    -> ('a Seq.seq)                       (*A resulting lazy sequence of leaves*)
    
  (*This function is very similar to compute tree, except that it enables you to control
  how deep the nodes can be before they are parallelised. This can also be helpful
  in preventing too much memory from being used, but is particularly useful
  if you know a great deal about the structure of the algorithm
  being passed in. You supply a minimum and maximum depth at which parallelisation
  can occur.
  
  Note: a separate compute funciton is used, because adding to the complexity
  inside the parallel tree was not desirable. Hence, what really happens is that
  the function used to compute the elements is slightly modified to pretend to finish
  at certain depths. More specific information on when and how parallelisation
  should occur should be incorporated into the node function itself.
  *)
  val compute_tree_with_depths : par_tree
    -> ('a,'b) elem                       (*This is the root of the tree*)
    -> ('b -> ((('a,'b) elem) List.list)) (*The function which, when applied to nodes, will produce the children*)
    -> (int option * int option)          (*The (min,max) values for the depth where parallelisation can occur*)
    -> ('a Seq.seq)                       (*A resulting lazy sequence of leaves*)
  
  
  
end;

structure MJB_Par_Tree :> MJB_PAR_TREE =
struct
  local
  structure Compactor = MJB_Compactor;
  structure Par_Seq = MJB_Safe_Parallel_Seq
  structure U = Unsynchronized;
  
  (*This is a singly linked list with a tail pointer, which permits
  O(1) append, cons, pull and is_empty. Note that pulling without
  checking for empty can throw an error (this is for efficiency)*)
  structure Linked_List =
  struct
    (*The type of the linked list!*)
    datatype 'a cell =
        (*An element in the list. Points to the next element
        and houses the value in this element*)
        Elem of (('a cell U.ref) * 'a)
      | Dummy (*For the end of the list*)
    
    (*Has a pointer to the first cell and the last one
    Stores the size of list to avoid having to destroy lists during append*)
    type 'a linked_list = (('a cell) * ('a cell))
    
    (*Create a new empty linked list*)
    val empty = (Dummy,Dummy)
    
    (*Attach an element to the front of the linked list*)
    (*val cons : 'a -> 'a linked_list -> 'a linked_list*)
    fun cons v (Dummy,_) = (*Dummy tail*)
        let val c = Elem ((U.ref Dummy),v) in (c,c) end
      | cons v (tl,next) = (*Attach to the front*)
        let val c = Elem ((U.ref next),v) in (tl,c) end
    
    (*Append one list to another. Both lists should be considered
    modified and so not used individually again!*)
    (*val cons : 'a linked_list -> 'a linked_list -> 'a linked_list*)
    fun append (Dummy,_) ls2 = ls2
      | append ls1 (Dummy,_) = ls1 (*Empty list cases*)
      | append (Elem (c1,_),next1) (tl2,next2) =
        let val _ = c1:=next2 in (*Join the first tail to the second head*)
          (tl2,next1)
        end
    
    (*Check to see if the list is empty*)
    fun is_empty (Dummy,_) = true
      | is_empty _ = false    
    (*Pull an element from the list*)
    fun pull (_,Dummy) = NONE
      | pull (tl,Elem (c1,v1)) = case (!c1) of
          Dummy => SOME (v1,(Dummy,Dummy))(*Last element*)
        | _ => SOME (v1,(tl,!c1)) (*Just fish for the front element*)
    
    (*Convert the contents of the singly linked list to a regular list*)
    fun list_of (Dummy,_) = []
      | list_of (_,next) =
        let
          fun conv Dummy = []
            | conv (Elem (c,v)) = v::(conv (!c))
        in
          conv next
        end
        
    (*Convert from a list of values to a linked list of the same values*)
    fun of_list [] = empty
      | of_list (v::vs) = cons v (of_list vs)
        
    (*Some local correctness tests*)
    val _ = let
      val ls = empty;
      val _ = if ([]=(list_of ls)) then () else
              raise ERROR "Linked list failure with empty";
      val _ = if (is_empty ls) then () else
              raise ERROR "Linked list failure with empty check";
      val ls = cons 1 ls;
      val _ = if ([1]=(list_of ls)) then () else
              raise ERROR "Linked list failure with singleton";
      val ls = cons 2 ls;
      val _ = if ([2,1]=(list_of ls)) then () else
              raise ERROR "Linked list failure with two";
      val ls2 = cons 4 (cons 3 empty);
      (*Remember that appending breaks the old lists*)
      val new_ls = append ls ls2
      val _ = if ([2,1,4,3]=(list_of new_ls)) then () else
              raise ERROR "Linked list failure with two";
      (*Try pulling from the list...*)
      val (v,rest) = case (pull new_ls) of NONE =>
              raise ERROR "Linked list pulled empty 1" | SOME (v,rest) => (v,rest)
      val _ = if (2=v) then () else
              raise ERROR "Linked list failure with pull 1";
      val _ = if ([1,4,3]=(list_of rest)) then () else
              raise ERROR "Linked list failure with pull 2";
      val (v,rest) = case (pull rest) of NONE =>
              raise ERROR "Linked list pulled empty 2" | SOME (v,rest) => (v,rest)
      val _ = if (1=v) then () else
              raise ERROR "Linked list failure with pull 3";
      val _ = if ([4,3]=(list_of rest)) then () else
              raise ERROR "Linked list failure with pull 4";
      val (v,rest) = case (pull rest) of NONE =>
              raise ERROR "Linked list pulled empty 3" | SOME (v,rest) => (v,rest)
      val _ = if (4=v) then () else
              raise ERROR "Linked list failure with pull 5";
      val _ = if ([3]=(list_of rest)) then () else
              raise ERROR "Linked list failure with pull 6";
      val (v,rest) = case (pull rest) of NONE =>
              raise ERROR "Linked list pulled empty 4" | SOME (v,rest) => (v,rest)
      val _ = if (3=v) then () else
              raise ERROR "Linked list failure with pull 7";
      val _ = if ([]=(list_of rest)) then () else
              raise ERROR "Linked list failure with pull 8";
      val _ = if (NONE=(pull rest)) then () else
              raise ERROR "Linked list failure with pull 9";
      in () end
      
    
  end;
  
  (*This is an implementation using difference lists!! This
  results in faster append O(1). Laziness is not useful here, since the list
  of child nodes must have been formulated by the function already.*)
  structure Difference_List =
  struct
    (*The type of the difference list*)
    datatype 'a DList = DList of ('a list -> 'a list)
    
    (*Has a pointer to the first cell and the last one
    Stores the size of list to avoid having to destroy lists during append*)
    type 'a difference_list = 'a DList
    
    (*Create a new empty linked list*)
    val empty = DList (fn x => x)
    
    (*Attach an element to the front of a difference list*)
    fun cons x (DList f) = DList (fn ls => x::(f ls))
    
    (*Attach an element to the end of the list*)
    fun snoc (DList f) x = DList (fn ls => (f (x::ls)))
    
    (*Append one list to another. Both lists should be considered
    modified and so not used individually again!*)
    fun append (DList f) (DList g) = DList (f o g)
    
    (*Convert the contents of the difference list to a regular list*)
    fun list_of (DList f) = f []
    
    (*Convert from a list of values to a difference list with the same values*)
    fun of_list [] = empty
      | of_list (x::xs) = cons x (of_list xs)
        
    (*Some local correctness tests*)
    val _ = let
      val ls = empty;
      val _ = if ([]=(list_of ls)) then () else
              raise ERROR "Difference list failure with empty";
      val ls = cons 1 ls;
      val _ = if ([1]=(list_of ls)) then () else
              raise ERROR "Difference list failure with singleton";
      val ls = cons 2 ls;
      val _ = if ([2,1]=(list_of ls)) then () else
              raise ERROR "Difference list failure with two";
      val ls2 = cons 4 (cons 3 empty);
      (*Remember that appending breaks the old lists*)
      val new_ls = append ls ls2
      val _ = if ([2,1,4,3]=(list_of new_ls)) then () else
              raise ERROR "Difference list failure with append";
              (*
      fun myfunc 1 = [Par_Tree.Leaf 1,Par_Tree.Node 2,Par_Tree.Leaf 2,Par_Tree.Node 2,Par_Tree.Leaf 3]
        | myfunc 2 = [Par_Tree.Node 3,Par_Tree.Node 3]
        | myfunc _ = [];
      val result = Seq.list_of (Par_Tree.compute_tree par_tree (Par_Tree.Node 1) myfunc);
      val _ = if (result=[1,2,3]) then () else
              raise ERROR "Par_Tree: failed empty node 4";*)
      val res_ls = append (append (append (cons 1 empty) (append empty (append empty empty))) (append (cons 2 empty) (append empty (append empty empty)))) (cons 3 empty)
      val _ = if ([1,2,3]=(list_of res_ls)) then () else
              raise ERROR "Difference list failure with append 2";
      in () end
      
    
  end;
  
  in
  (*This is the type of returned elements
  from the functions (keep reading for this to make sense)*)
  datatype ('a,'b) elem = Node of 'b | Leaf of 'a
  
  (*
  IMPLEMENTATION FOR ESTIMATES:
  
  The compactor with the parallel lazy sequence will be used to apply the function
  over a sequence of nodes. The time aimed at is enough such that the compactor produces
  a sufficiently large number of groups such that the parallel lazy sequence can make use of
  these groups. this is quite a long time, so for small procedures, it is quite likely
  that the entire tree will be consumed.
  
  Tests suggest that a length of a minimum of (no.cores * 10) is close to optimal, but
  longer is also close to optimal. Hence, this is the minimum target for the number
  of nodes passed in.
  
  The functions should take a minimum of about 1200 microseconds,
  but a time of 20000 microseconds is recommended (per group) since
  this keeps the estimates from the compactor more stable (stronger average).
  The compactor will be left to figure out the how to group functions,
  and its final estimate for the group size will
  be passed back to it as an estimate for the next attempt. It will be allowed
  to prime each time, but only for a short amount of time.
  
  The length of the resulting sequence will constantly be increased until the compactor
  is producing enough groups. Hopefully this will be efficient enough.
  *)
  val recommended_time = Time.fromMicroseconds 20000; (*Number of microseconds for groups to aim for*)
  val num_cores = Thread.numProcessors();
  val recommended_length = num_cores * 40;
  val prime_length = 10;
  val recommended_est = 1;
  val node_limit = 16000; (*This is a limit on the number of nodes that may be gathered*)
  val order_matters = true; (*Safe as a default*)
  
  (*The type of a parallel tree!
  It remembers the time to aim for, the number of groups which should
  be created, the future data to use, and an estimate for the number
  of functions.
  The boolean value specifies if the order that leaves are returned
  in is relevant
  The final integer option is the node limit, if it exists.*)
  type par_tree = Time.time * int * MJB_Simple_Future.future_data * int * bool * (int option)
  
  (*Create a new parallelising tree. You must supply the future data that it
  should use.*)
  fun new fd = (recommended_time,recommended_length,fd,recommended_est,order_matters,SOME node_limit)
  
  (*Get the future data in use by this parallel tree*)
  fun get_future_data (_,_,fd,_,_,_) = fd
  
  (*Set an estimate for the current number of functions that will need to
  be combined to reach the required time specified above*)
  fun set_estimate (t,l,fd,_,ord,nlimit) est = case est of
      NONE => (t,l,fd,recommended_est,ord,nlimit)
    | SOME e => (t,l,fd,e,ord,nlimit)
    
  (*Return the estimated number of groups, or a default
  if no estimate has been set*)
  fun get_estimate (_,_,_,est,_,_) = est
  
  (*Set the time that a process should take to run before being assigned
  to a core. The default (ATTOW - at the time of writing) is 4000
  microseconds. A large value will keep the groups assigned to cores
  large, so that they can run for longer on independent problems,
  improving the efficiency. If the value is too large, then the tree
  may not be big enough to enable such groups to be created.*)
  fun set_time (_,length,fd,est,ord,nlimit) time = case time of
      NONE => (recommended_time,length,fd,est,ord,nlimit)
    | SOME t => (t,length,fd,est,ord,nlimit)
    
  (*Return the time to be used by the compactor. It will return the default
  time it is using if none was set.*)
  fun get_time (t,_,_,_,_,_) = t
  
  (*Set the number of groups of functions that should be created
  during parallelisation. Only use this if you know how it works!
  Essentially, groups taking the time specified above (set_time) to process
  will be assigned to futures. The length is how many of these groups
  should be created before assigning the tasks to cores. Typically,
  10*the number of cores is a good way to overload the future's library.*)
  fun set_no_groups (t,_,fd,est,ord,nlimit) l =
    case l of
        NONE => (t,recommended_length,fd,est,ord,nlimit)
      | SOME length => (t,length,fd,est,ord,nlimit)
    
  (*Return the number of groups to be created before parallelisation.
  If nothing was set, it will return the default that it is going to use.*)
  fun get_no_groups (_,l,_,_,_,_) = l
  
  (*Set whether the order in which leaves are returned is important. Setting
  this to false means that if a leaf is discovered in the tree
  early (i.e: not the "first" one - leftmost) then the algorithm
  can stop and return that. Hence, it is more likely to take less time
  and memory when pulling leaves from the tree with this set to false.
  
  Note that nodes are still computed in order, since it is generally
  considered that leaves are more likely to appear deep in a tree.*)
  fun set_order_matters (t,l,fd,est,_,nlimit) ord = (t,l,fd,est,ord,nlimit)
  
  (*Return whether or not the order the leaves are returned in is considered
  important*)
  fun get_order_matters (_,_,_,_,ord,_) = ord
  
  (*Set the maximum number of nodes the algorithm is allowed to gather in the tree
  before parallelisation. Passing NONE will ensure there is no limit at all. A limit
  below 1 is not allowed. Typcially, a setting should be in the thousands. This can help
  to prevent the algorithm running out of memory.*)
  fun set_node_limit (t,l,fd,est,ord,_) nlimit = (t,l,fd,est,ord,nlimit)
    
  (*Return the current node limit in use by the tree.*)
  fun get_node_limit (_,_,_,_,_,nlimit) =  nlimit
  
  (*A complicated part of the algorithm is forming a sequence of unevaluated nodes, joined with sequences
  of results. The sequence will consist of a dummy head and a dummy last element.*)
  
  (*The type of a result. Points to another result to form a sequence*)
  type 'a result = 'a Difference_List.difference_list
  (*The type of an unevaluated element*)
  datatype ('a,'b) tree_elem =
                      (*Forms the head of the sequence. It only holds a sequence of results*)
                      Dummy_Head of ('a list)
                      (*This forms the end of the sequence, and does nothing...*)
                    | Dummy_Tail
                      (*The main part of the sequence. Results held are considered to be results obtained
                      to the right of this node in the tree (so AFTER it). It holds the node it should evaluate*)
                    | Not_Done of ('b * 'a result)
  (*The type of a tree sequence*)
  type ('a,'b) tree_seq = (('a,'b) tree_elem) Seq.seq
  (*
  The sequence evaluation works like this.
  Upon pulling, see if the dummy head has any results to return. If so, return the result. If not,
  it will compute an appropriate number of nodes. These nodes will return sequences of children, and the overall
  result is a sequence of sequences of new nodes or results. These need to be reattached to the tree sequence.
  
  This is done as follows. Firstly, when the unevaluated group is taken from the tree sequence, the tree sequence
  is not modified. Hence, the sequence of sequences produced is in the same order as the sequence of unevaluated
  elements in the tree sequence.
  
  When encountering an element in the tree sequence, this will: pull from the sequence of sequences to get the next,
  result. If:
  
  (a) The children are all results, then the resulting elements will be stuck onto the previous unevaluated
      element's results.
  (b) If the children contain unevaluated elements, then the last unevaluated element will point to the next element
      in the tree sequence. Similarly, the previous element in the tree sequence will be made to point to the first
      unevaluated element in these children.
  
  IMPLEMENTATION:
  
  Important: the next element also returns what the next item to point to is.
  
  Important: Note that append does potentially incur a linear overhead at the moment,
             since for an unluckily shaped tree, many leaves may have to be appended.
             Since append is lazy, the operation below is genuinely O(1), but
             pulls could be linear (although an extremely low coefficient). For
             a truly O(1) combine, an O(1) would be necessary (doubly linked list
             for example).
  *)
  
  fun add_to_tree_seq a b
  (*
    (*The theoretical type signature...*)
       ('a,'b) tree_seq (*The (remaining) tree sequence*)
    -> ((('a,'b) elem) List.list) Seq.seq (*The sequence of children*)
    -> (('a,'b) tree_seq) (*The resulting tree sequence combined with the next
    unevaluated element to point to and the results to be added to the previous element
    (The next element to point is that tree sequence!)*)*)
  =
    let
      (*Converted from a sequence to a list, so this required a modification*)
      fun add tree_sq child_sq =
        case Seq.pull child_sq of
            NONE => (tree_sq,Difference_List.empty)(*Must be finished*)
          | SOME (children,child_sq) =>
        (*Get the next tree element to replace...*)
        case Seq.pull tree_sq of
            (*Cannot happen...*)
            NONE => raise ERROR "par_tree: Ran out of tree elements - semantic error"
          | SOME (Dummy_Head _,_) => raise ERROR "par_tree: Pulled the head from the tree sequence - semantic error"
          | SOME (Dummy_Tail,_) => raise ERROR "par_tree: Pulled the tail out of the sequence for modification - semantic error"
          (*This is the node to replace!*)
          | SOME (Not_Done (_,res),tq) =>
            let
              (*Get the results from the rest of the sequence first...*)
              val (new_tree_sq,late_results) = add tq child_sq
              (*Gather the first sequence of results...*)
              fun first_res c_sq = case c_sq of
                  [] => (c_sq,Difference_List.empty)
                | ((Node _)::_) => (c_sq,Difference_List.empty)
                | ((Leaf v)::c_sq2) =>
                  let
                    val (sq2,res_sq) = first_res c_sq2
                  in
                    (sq2,Difference_List.cons v res_sq)
                  end
              val (first_uneval,first_results) = first_res children
              (*Check if a sequence is empty...*)
              fun seq_is_empty sq = case (Seq.pull sq) of
                  NONE => true
                | _ => false
              (*Now start gathering recursively all of the results...*)
              fun forge_seq c_sq =
                case (c_sq) of
                    [] => Seq.empty
                  | ((Leaf _)::_) => raise ERROR
                    "par_seq: should not be able to pull a leaf in forge_seq - semantic error"
                  | ((Node n)::c_sq2) =>
                let
                  fun is_empty [] = true
                    | is_empty _ = false;
                  val (uneval,results) = first_res c_sq2
                in
                  (*The last one should have the old_results
                  attached to the end...*)
                  Seq.cons (Not_Done (n,
                    if (is_empty uneval) then (Difference_List.append results (Difference_List.append res late_results))
                    else results)) (forge_seq uneval)
                end
              (*This forms the new sequence to be inserted...*)
              val to_insert = forge_seq first_uneval
            in
              (*Now insert the resulting sequence in!*)
              (Seq.append to_insert new_tree_sq,
              if (seq_is_empty to_insert) then (*This was just a sequence of results...*)
                Difference_List.append first_results (Difference_List.append res late_results) else first_results)
            end
    in
      add a b
    end
  
  (*
  fd is the future data that the leaf_sequence should use
  tree_sq is the sequence of nodes and leaves
  tgt is the target number of groups
  est is the compactor's previous estimate for how many functions need to be combined
  to make a single group.
  f is the function which, when applied to a node, will produce the children of
  that node.
  *)
  fun leaf_sequence fd tree_sq est tgt time f nlimit = Seq.make (fn () => (
    (*PolyML.print("Starting to find leaves");*)
    (*tree_sq must have a dummy head and tail*)
    case (Seq.pull tree_sq) of
        NONE => raise ERROR "par_tree: missing the dummy head"
      | SOME (Dummy_Tail,_) => raise ERROR "par_tree: pulled dummy tail by accident"
      | SOME (Not_Done _,_) => raise ERROR "par_tree: pulled node by accident"
      | SOME (Dummy_Head leaves,tr_sq) => case (leaves) of
          (leaf::rest_of_leaves) => ((*PolyML.print("Found a leaf");*)
          SOME (leaf,leaf_sequence fd (Seq.cons (Dummy_Head rest_of_leaves) tr_sq) est tgt time f nlimit))
        | [] => (*The hard case...*)
          case (Seq.pull tr_sq) of SOME (Dummy_Tail,_) => (*Finished*) (
          (*PolyML.print("No more nodes to compute");*)NONE) | _ =>
          ((*PolyML.print("Did not find a leaf");*)
          (*First, lets pull the current estimate*)
          let
            val to_gather = (((est : LargeInt.int)*(tgt : LargeInt.int)) : LargeInt.int);
            val to_gather = case nlimit of
              NONE => to_gather
            | SOME n => if (to_gather>n) then n else to_gather; (*Limit*)
            fun gather (0 : LargeInt.int) _ = ((*PolyML.print("Gathered nodes, found enough: " ^ (Int.toString(to_gather)));*)Seq.empty)
              | gather n sq = case (Seq.pull sq) of
                  SOME (Dummy_Tail,_) => ((*PolyML.print("Gathered nodes, hit tail");*)Seq.empty) (*finished*)
                | SOME (Dummy_Head _,_) => raise ERROR "par_tree: pulled head while gathering nodes"
                | SOME (Not_Done (v,_),sq2) => Seq.cons v (gather (n-(1 : LargeInt.int)) sq2)
                | NONE => raise ERROR "par_tree: reached end of sequence without seeing tail";
            (*Now compute the sequence of children...*)
            val nodes = gather to_gather tr_sq; (*Avoid feeding the head*)
            (*The compactor to use...*)
            val comp = Compactor.new_dynamic time;
            (*Let the compactor go static, because it will be used again*)
            val comp = Compactor.prime comp (SOME prime_length) true;
            val comp = Compactor.set_estimated_number comp (SOME est);
            (*Now parallelise over this sequence...
            Each group produces a sequence of sequences of children
            Hence, the resulting sequence is a sequence of sequences of sequences!*)
            val result =
              Par_Seq.sequential_seq_of (
              Par_Seq.map (fn _ => fn (f,n) => (f(),n)) (
              Par_Seq.of_sequential_seq fd (
              Compactor.compact_with_seq comp (
              Seq.map (fn x => fn sq => Seq.cons (f x) sq) nodes) Seq.empty)))
            (*val _ = PolyML.print("Computed all of the children");*)
            (*Check the final estimate and flatten the results*)
            fun get_est sq = case (Seq.pull sq) of
                NONE => (Seq.empty,~1)
              | SOME ((v,n),sq2) => let
                val (res_sq,res_est) = get_est sq2
              in (Seq.append v res_sq,if (res_est>=0) then res_est else n) end;
            val (children,new_est) = get_est result;
            (*val _ = PolyML.print("Retrieved estimate");*)
            (*Now combine all of the results...*)
            val (new_tree_sq,extra_leaves) = add_to_tree_seq tr_sq children;
            val new_tree_sq = Seq.cons (Dummy_Head (Difference_List.list_of extra_leaves)) new_tree_sq
          in
            (*Recur now on the rest*)
            Seq.pull (leaf_sequence fd new_tree_sq new_est tgt time f nlimit)
          end)))
  
  (*********************************************************************************)
  (*This part of the algorithm is for when the order of the leaves does not matter
  For efficiency, it is reimplemented since many of the complexities of the previous
  algorithm can be avoided (ordering the leaves and storing them by nodes)*)
  (*********************************************************************************)
  
  (*This is the datatype to hold all of the nodes together!
  The computed leaves are in the first argument. The nodes to be
  computed appear, in order, in the second*)
  type ('a,'b) node_list = ('a List.list * 'b List.list)
  
  (*Run the function!*)
  fun leaf_sequence_without_order fd n_list est tgt time f nlimit = Seq.make (fn() =>
    case (n_list) of
      ([],[]) => NONE (*Must be done*)
    | (leaf::leaves,nodes) => SOME (leaf, leaf_sequence_without_order fd (leaves,nodes) est tgt time f nlimit)
    | ([],nodes) => (*The hard case...*)
      let
        (*How many nodes to combine together...*)
        val to_gather = (((est : LargeInt.int)*(tgt : LargeInt.int)) : LargeInt.int);
        val to_gather = case nlimit of
            NONE => to_gather
          | SOME n => if (to_gather>n) then n else to_gather; (*Limit*)
        (*Now divide the list into its two halves...nodes to be
        computed and those left alone*)
        fun divide 0 xs = ([],xs)
          | divide _ [] = ([],[])
          | divide n (x::xs) = let val (h1,h2) = divide (n-1) xs in
              (x::h1,h2)
            end;
        (*Now the two halves*)
        val (nodes_to_compute,kept_nodes) = divide to_gather nodes;
        (*The compactor to use...*)
        val comp = Compactor.new_dynamic time;
        (*Let the compactor go static, because it will be used again*)
        val comp = Compactor.prime comp (SOME prime_length) true;
        val comp = Compactor.set_estimated_number comp (SOME est);
        (*Now parallelise over this sequence...
        Each group produces a sequence of sequences of children
        Hence, the resulting sequence is a sequence of sequences of sequences!*)
        val result =
          Par_Seq.sequential_seq_of (
          Par_Seq.map (fn _ => fn (f,n) => (f(),n)) (
          Par_Seq.of_sequential_seq fd (
          Compactor.compact_with_seq comp (
          Seq.map (fn x => fn sq => Seq.cons (f x) sq) (Seq.of_list nodes_to_compute)) Seq.empty)));
        (*Check the final estimate and flatten the results*)
        fun get_est sq = case (Seq.pull sq) of
            NONE => ([],~1) (*Guaranteed to be at least one element*)
          | SOME ((v,n),sq2) => let       
            val (res_sq,res_est) = get_est sq2
          in ((Seq.list_of v)@res_sq,if (res_est>=0) then res_est else n) end;
        (*The children are the resulting mix of leaves and nodes, currently
        in the order they were produced*)
        val (children,new_est) = get_est result;
        (*Next, split this up into nodes and leaves...*)
        fun find_leaves [] = ([],[])
          | find_leaves (c::cs) = let val (leaves,nodes) = find_leaves cs in
            case c of
              Leaf l => (l::leaves,nodes)
            | Node n => (leaves,n::nodes)
          end;
        (*The new leaves, and the new nodes*)
        val (new_leaves,new_nodes) = find_leaves (flat children);
        (*Construct the new list of nodes...*)
        val new_nodes = new_nodes@kept_nodes;
        (*Now see if we have anything to return...*)
      in
        case new_leaves of
          [] => (*Still no leaves found...*)
            Seq.pull (leaf_sequence_without_order fd ([],new_nodes) new_est tgt time f nlimit)
        | (l::ls) => (*Leaves!! Return the first one...*)
            SOME (l,leaf_sequence_without_order fd (ls,new_nodes) new_est tgt time f nlimit)
      end)
      
  
  
  (*This function is the main work of the structure. It will evaluate the tree in a lazy way while parallelising
  as much as possible.
  
  Note: the function which returns a list of nodes or leaves can really change for each node if the node itself
        holds the function to be applied too, so this is a very generic type signature
      
  Note2: while the children returned form a lazy sequence, this will be at least mostly evaluated*)
  fun compute_tree (time,length,fd,est,ord,nlimit) root f = case root of
        Leaf v => Seq.single v (*Special case*)
      | Node v => (*Decide which algorithm to use based on whether or
                  not the order matters*)
                  if (ord) then (
                    leaf_sequence fd (Seq.cons (Dummy_Head [])
                    (Seq.cons (Not_Done (v,Difference_List.empty)) 
                    (Seq.single Dummy_Tail))) est length time f nlimit
                  ) else (
                    leaf_sequence_without_order fd ([],[v]) est length time f nlimit
                  )
                  
  
  (*This function is very similar to compute tree, except that it enables you to control
  how deep the nodes can be before they are parallelised. This can also be helpful
  in preventing too much memory from being used, but is particularly useful
  if you know a great deal about the structure of the algorithm
  being passed in. You supply a minimum and maximum depth at which parallelisation
  can occur.
  
  If the depth is set as a maximum, the algorithm is simple. It will parallelise
  across all nodes and they will pretend to finish if they are
  at the specified depth, or mark the depth of their children otherwise.
  
  If the depth is set as a minimum, the problem is harder. The function will compute
  all children sequentially up to the specified depth. Then it will combine the resulting
  nodes and leaves into the relevant format, and finally parallelise across all of these.
  *)
  fun compute_tree_with_depths par_tree root f (min,max) = Seq.empty
    
  
  
  end
end;
