(*
This is a signature for a parallel sequence. It is exactly the same as for an ordinary
sequence, except for four functions which can convert to and from
the sequential sequence to the parallel sequence. The one other exception
is the format of "make" explained in the signature below

The functions have been written out in full here since I wanted to write a description
for each. You can reduce the size of this by using "include SEQ;" if you like.

If an element in the sequence raises an exception, this should only be raised
when the element is pulled from the sequence.
*)
signature MJB_PARALLEL_SEQ =
sig
  (*An empty sequence exception. This is raised when you demand the head or
  tail of an empty sequence*)
  exception Empty_Parallel_Sequence of string
  (*The type of a parallel sequence*)
  type 'a seq
  (*Add a function for future evaluation to the sequence.
  The format is different to the form of make in the original sequential sequence
  because the functions you pass in are modified. Hence, passing in a function
  of the form (unit -> 'a * 'a seq) is not helpful because the returned 'a cannot
  be modified without evaluating the entire function.
  
  (A workaround is to create a sequential sequence and then convert it into
  a parallel sequence, but I suggest you avoid this...)*)
  val make : (MJB_Simple_Future.future_data -> 'a) -> 'a seq -> 'a seq
  (*Create the empty sequence.*)
  val empty : 'a seq
  (*Get the next element from the sequence. Returns NONE
  if a next element does not exist. This requires future data which will
  be passed to the function which evaluates the next element.
  Pulling from the same sequence twice (to return exactly the same element)
  can be especially painful since further values in the sequence may be
  re-evaluated too, so avoid this!!*)
  val pull : MJB_Simple_Future.future_data -> 'a seq -> ('a * 'a seq) option
  (*Attach an element to the front of the sequence*)
  val cons : 'a -> 'a seq -> 'a seq
  (*Create a sequence with a single element*)
  val single : 'a -> 'a seq
  (*Attempt to apply a function to an element. If it succeeds, it will return
  a singleton sequence containing the answer. If it fails an empty sequence
  will be returned*)
  val try : ('a -> 'b) -> 'a -> 'b seq
  (*Return the first element of the sequence.
  Raises empty sequence exception if there is no head
  Calling this on a sequence more than once may force evaluation of the head
  multiple times (dependent on whether or not it has begun evaluation).*)
  val hd : MJB_Simple_Future.future_data -> 'a seq -> 'a
  (*Return the tail of the sequence
  Raises empty sequence exception if there is no tail
  As with hd, calling this multiple times may incur a penalty.
  This is much less likel with tl, and if no form of removal
  of the elements was applied, then it is basically free since
  the element dropped will not be evaluated unnecessarily.*)
  val tl : 'a seq -> 'a seq
  (*Return a list of the first n elements (evaluated) paired with the rest*)
  val chop : MJB_Simple_Future.future_data -> int -> 'a seq -> (('a list) * ('a seq))
  (*Take the first n elements of the sequence. (Forget the rest)*)
  val take : int -> 'a seq -> 'a seq
  (*Convert the sequence to a list of values.
  WARNING: This forces evaluation of the entire sequence!!*)
  val list_of : MJB_Simple_Future.future_data -> 'a seq -> 'a list
  (*Convert a list back into a sequence.*)
  val of_list : 'a list -> 'a seq
  (*Join two sequences together (first appears in front)*)
  val append : 'a seq -> 'a seq -> 'a seq
  (*Map a function over a sequence. This is done lazily like all of the functions*)
  val map : ('a -> 'b) -> 'a seq -> 'b seq
  (*Apply a function to the first sequence, and append the result
  to the next sequence*)
  val mapp : ('a -> 'b) -> 'a seq -> 'b seq -> 'b seq
  (*Interleave two sequences together, starting with the first
  If one runs out, the other is appended to the end.*)
  val interleave: 'a seq * 'a seq -> 'a seq
  (*Filter the values in a sequence. This is still lazy to a point, which
  is surprising if you think about how elements are processed in parallel.
  (See implementation details)*)
  val filter : MJB_Simple_Future.future_data -> ('a -> bool) -> 'a seq -> 'a seq
  (*Flatten (concatenate) a sequence of sequences. This too maintains
  laziness across the sequence of sequences and in each individually sequence*)
  val flat : MJB_Simple_Future.future_data -> 'a seq seq -> 'a seq
  (*Maps a function which produces a sequence
  for every element in the original sequence. It automatically
  appends all of the results*)
  val maps : MJB_Simple_Future.future_data -> ('a -> 'b seq) -> 'a seq -> 'b seq
  (*Maps a filter function over the sequence. If the filter returns
  none, the element is dropped. If the filter returns SOME b then
  b is attached to the sequence.*)
  val map_filter : MJB_Simple_Future.future_data -> ('a -> 'b option) -> 'a seq -> 'b seq
  (*Apply a function over the sequence. The second element
  is paired with this function for every single value*)
  val lift : ('a -> 'b -> 'c) -> 'a seq -> 'b -> 'c seq
  (*Similar to lift but appends the results of the function*)
  val lifts : MJB_Simple_Future.future_data -> ('a -> 'b -> 'c seq) -> 'a seq -> 'b -> 'c seq
  (*Creates a singleton sequence element with the given value
  and future data*)
  val succeed : 'a -> 'a seq
  (*Returns an empty sequence regardless of the element*)
  val fail : 'a -> 'b seq
  (*Turn a parallel lazy sequence into a sequential lazy sequence.
  This is really for typing. The elements will still be processed
  in parallel if they were originally added to a parallel sequence.
  Elements added later are processed sequentially.*)
  val sequential_seq_of : MJB_Simple_Future.future_data -> 'a seq -> 'a Seq.seq
  (*Turn a parallel lazy sequence into a sequential lazy sequence
  with immediate effect. That means that, other than futures already started,
  further pulls to the sequence will not cause parallel evaluation generally.
  
  Warning: this is not guaranteed if there are inner parallelised parts
  of the sequence, or if you filtered across it.*)
  val truly_sequential_seq_of : MJB_Simple_Future.future_data -> 'a seq -> 'a Seq.seq
  (*Turn a sequential lazy sequence into a parallel lazy sequence.
  Elements that were originally in the lazy sequence will continue
  to be processed sequentially, unless you apply any function across the sequence.
  The sequential elements must be processed sequentially,
  but the function applied will be applied in parallel*)
  val of_sequential_seq : 'a Seq.seq -> 'a seq
  (*Apply a function to a singleton list to produce a sequence
  of singleton lists, which will be turned back into an ordinary
  sequence containing the element in each resulting singleton list*)
  val singleton: ('a list -> 'b list seq) -> 'a -> 'b seq
  (*Print a sequence, up to the n elements as supplied*)
  val print: MJB_Simple_Future.future_data ->
    (int -> 'a -> unit) -> int -> 'a seq -> unit
  (*Accumulating a function over a sequence; this is lazy.
  Apply the function to an element of the first
  sequence and the sequence produced by "this" to produce a new
  sequence.*)
  val it_right: MJB_Simple_Future.future_data ->
    ('a * 'b seq -> 'b seq) -> 'a seq * 'b seq -> 'b seq
  (*Apply a function f to an element x to produce a sequence.
  Then apply g to every element in this sequence to produce
  a new sequence for each individual result, and then
  append the results*)
  val THEN: ('a -> 'b seq) * ('b -> 'c seq) ->
    MJB_Simple_Future.future_data -> 'a -> 'c seq
  (*Apply a function f to an element x to produce a sequence
  If the sequence is initially empty, then return g x instead*)
  val ORELSE: ('a -> 'b seq) * ('a -> 'b seq) -> 
    'a -> 'b seq
  (*Apply a function f to an element x to generate a sequence.
  Then apply a function g to the same element to generate a second
  sequence and append the first to the second*)
  val APPEND: ('a -> 'b seq) * ('a -> 'b seq) ->
    'a -> 'b seq
  (*For every function in the list, apply
  that function to each element in the sequence generated so far,
  and append the resulting sequences*)
  val EVERY: MJB_Simple_Future.future_data ->
    ('a -> 'a seq) list -> 'a -> 'a seq
  (*Wait for the first function to return a non-empty sequence
  and return that sequence*)
  val FIRST: ('a -> 'b seq) list -> 'a -> 'b seq
  (*Return the sequence generated by f applied to the given element,
  or if that returns an empty sequence, return a singleton sequence
  containing the given element instead*)
  val TRY: ('a -> 'a seq) -> 'a -> 'a seq
  (*f is applied to an element x to produce a sequence. If f produces the empty
  sequence, a singleton sequence with x will be returned. Otherwise,
  f is then applied to every element in the produced sequence.
  This recurs.*)
  val REPEAT: MJB_Simple_Future.future_data ->
    ('a -> 'a seq) -> 'a -> 'a seq
  (*Apply the given function to the sequence first.
  Then repeat it across the resulting sequence.*)
  val REPEAT1: MJB_Simple_Future.future_data ->
    ('a -> 'a seq) -> 'a -> 'a seq
  (*If the first integer is less than the second, this returns
  a singleton sequence with the element supplied. Otherwise,
  the function f is applied to the second integer and the element.
  This sequence is appended to a sequence formed recursively
  by reapplying Interval, reducing the second integer by 1, to
  every element produced in the first sequence.*)
  val INTERVAL: MJB_Simple_Future.future_data ->
    (int -> 'a -> 'a seq) -> int -> int -> 'a -> 'a seq
  (*If the function returns the empty sequence, this will
  return the empty sequence. Otherwise it will return the frst
  element only of the resulting sequence*)
  val DETERM: MJB_Simple_Future.future_data ->
    ('a -> 'b seq) -> 'a -> 'b seq
end;

(*
A structure implementing the parallel sequence. This essentially works
by using a normal sequence of elements which are either classified as either:

1. "Waiting" - indicates evaluation of this element has not started.
2. "Started" - indicates a future call has been performed on this element,
so it has either been evaluated or is being evaluated.

When you pull, if the element you are pulling is "Waiting", then a number
of elements are really pulled and futures are created for each of them. They
are then pushed back into the sequence and the first one is actually returned.

The number of elements pre-evaluated is proportional to the number of cores (see
in the code for the exact number).

This code contains a version of pulling that includes a time check, which
means that the code checks to see if pulling is expensive and will
parallelise accordingly if it is expensive. Otherwise, it will
behave more sequentially. The overhead of system calls for the time
is still significant however, so this parallel list should still
be used carefully.
*)
structure MJB_Parallel_Seq :> MJB_PARALLEL_SEQ =
struct
  structure Future = MJB_Simple_Future;
  structure Stopwatch = MJB_Stopwatch;
  (*The type of an element in the sequence*)
  datatype 'a elem = Waiting of (Future.future_data -> 'a)
                   | Started of ('a Future.future)
                   | Done of 'a (*For speed*)
  (*An empty sequence exception. This is raised when you demand the head or
  tail of an empty sequence*)
  exception Empty_Parallel_Sequence of string
  (*The data type for lazy parallel sequences.
  Elements are either waiting to be started by a future,
  or have already been started and will have their result captured.
  (Exceptions are captured so that exceptions are released
  at the time element when the element is pulled)
  Futures capture their elements automatically!*)
  datatype 'a seq = Seq of unit -> (('a elem * 'a seq) option);
  (*Add a function for future evaluation to the sequence.
  The format is different to the form of make in the original sequential sequence
  because the functions you pass in are modified. Hence, passing in a function
  of the form (unit -> 'a * 'a seq) is not helpful because the returned 'a cannot
  be modified without evaluating the entire function.
  
  (A workaround is to create a sequential sequence and then convert it into
  a parallel sequence, but I suggest you avoid this...)*)
  fun make f sq = Seq (fn () => SOME (Waiting f,sq));
  (*Create the empty sequence.*)
  val empty = Seq (fn () => NONE);
  (*The number of cores available with this machine*)
  val num_cores = Thread.numProcessors();
  (*This is the number of elements further down the sequence that will attempt
  to be evaluated after a pull*)
  val look_ahead = num_cores * 10;
  (*How far I am willing to look ahead to find the first unsta*)
  val buffer = num_cores;
  
  (*Pull just the element, without evaluating, and return the rest of
  the sequence. Thisis considered "cheap" since the value in the element
  is not computed by this.*)
  fun pull_elem (Seq sq) = sq ()
  
  (*This is needed to prevent lazy
  evaluation when attempting to call futures further
  down the list.*)
  fun form elem sq = Seq (fn () => SOME (elem,sq))
  
  (*This will look ahead a number of elements in the sequence
  and start them off in futures.*)
  fun pull_ahead _ sq 0 _ = ((*TextIO.print("Terminated\n");*)sq) (*Looked at enough already*)
    | pull_ahead fd sq n buff = ((*TextIO.print("Looking " ^ Int.toString(n) ^ " ahead\n");*)case (pull_elem sq) of
        NONE => ((*TextIO.print("Was empty!\n");*)empty) (*The sequence is empty anyway, so return it*)
      | SOME (Waiting f,sq2) => (*This needs to begin evaluation!*)
          form (Started ((*TextIO.print("Parallel!\n");*)
          Future.future_group_data fd f)) ((*TextIO.print("To the next\n");*)
          pull_ahead fd sq2 (n-1) buff)
      | SOME (Started f,sq2) => ((*TextIO.print("Not parallel\n");*)
      if (buff=0) then sq else (Seq (fn() => (SOME (Started f,pull_ahead fd sq2 n (buff-1))))))
      (*Already started. Only look ahead again when
      the first chunk is evaluated. This creates bursts of processing, which for a program
      (which is constantly running) is the preferable way to make the most of the available
      cores. Also, this prevents long failed look aheads.*)
      | SOME (Done f,sq2) => ((*TextIO.print("Done already\n");*)
      if (buff=0) then sq else (Seq (fn() => (SOME (Done f,pull_ahead fd sq2 n (buff-1)))))));
  
  (*Pull a single element from the sequence.
  This will evaluate the element pulled.*)
  fun pull_one fd sq = case (pull_elem sq) of
      NONE => NONE (*The sequence is empty so this fails*)
    | (SOME (Waiting f,sq2)) => SOME (f fd,sq2) (*Force evaluation now.
    Generally the Waiting case will not come up, since pull_ahead will have been called
    before it.*)
    | (SOME (Started v,sq2)) => SOME (Future.join v,sq2) (*Join on the existing
    future, which has hopefully already finished.*)
    | (SOME (Done v,sq2)) => SOME (v,sq2);
  
  (*Untimed version - performs very badly by comparison*)
  (*
  fun pull fd sq = pull_one fd (pull_ahead fd sq look_ahead)
  *)
  (*If pulling takes longer than this to compute, the next
  series of values will be pulled in parallel.
  This reduces the severity of the worst case.*)
  (*Time in microseconds*)
  val time_boundary = 1;
  
  
  (*Get the next element from the sequence. Returns NONE
  if a next element does not exist. This requires future data which will
  be passed to the function which evaluates the next element.
  Pulling from the same sequence twice (to return exactly the same element)
  can be especially painful since further values in the sequence may be
  re-evaluated too, so avoid this!!*)
  fun pull fd sq =
    let
      val sw = Stopwatch.start (Stopwatch.new);
      val res = pull_one fd sq;
      val t = Time.toMicroseconds (fst (Stopwatch.stop sw));
    in
      case res of
        NONE => NONE
      | SOME (v,sq2) => if (t>time_boundary) then (*Decide whether or not to parallelise*)
        (PolyML.print("Decided to parallelise in Par_Seq");
        SOME (v,pull_ahead fd sq2 look_ahead buffer)) else res
    end
  
  (*Attach an element to the front of the sequence*)
  fun cons v sq = Seq (fn () => SOME (Done v,sq))
  
  (*Create a sequence with a single element*)
  fun single v = cons v empty;
  
  (*Return the first element of the sequence.
  Raises empty sequence exception if there is no head
  Calling this on a sequence more than once may force evaluation of the head
  multiple times (dependent on whether or not it has begun evaluation).*)
  fun hd fd sq = case (pull fd sq) of
      NONE => raise Empty_Parallel_Sequence "Cannot return the head of the empty sequence."
    | SOME (v,_) => v
  
  (*Return the tail of the sequence
  Raises empty sequence exception if there is no tail
  As with hd, calling this multiple times may incur a penalty.*)
  fun tl sq = case (pull_elem sq) of
      NONE => raise Empty_Parallel_Sequence "Cannot return the tail of the empty sequence."
    | SOME (_,sq2) => sq2
  
  (*Attempt to apply a function to an element. If it succeeds, it will return
  a singleton sequence containing the answer. If it fails an empty sequence
  will be returned*)
  fun try f x =
    (case Basics.try f x of
      SOME y => single y
    | NONE => empty);
  
  (*Return a list of the first n elements (evaluated) paired with the rest*)
  fun chop fd n xq =
    if n <= (0 : int) then ([], xq)
    else
      (case pull fd xq of
        NONE => ([], xq)
      | SOME (x, xq') => apfst (Basics.cons x) (chop fd (n - 1) xq'));
  
  (*Take the first n elements of the sequence. (Forget the rest)*)
  fun take n (Seq sq) =
    if n <= (0 : int) then empty
    else 
    Seq (fn () => case (sq()) of
        NONE => NONE
      | SOME (v,sq2) => SOME (v, take (n-1) sq2))
  
  (*Convert the sequence to a list of values.
  WARNING: This forces evaluation of the entire sequence!!*)
  fun list_of fd xq =
    (case pull fd xq of
      NONE => []
    | SOME (x, xq') => x :: list_of fd xq');
  
  (*Convert a list back into a sequence.*)
  fun of_list xs = fold_rev cons xs empty;
  
  (*Join two sequences together (first appears in front)*)
  fun append xq yq =
    let
      fun copy sq = Seq (fn () =>
        case (pull_elem sq) of
          NONE => pull_elem yq
        | SOME (v,sq2) => (SOME (v,copy sq2)))
    in copy xq end;
  
  (*Map a function over a sequence. This is done lazily like all of the functions*)
  fun map f xq = Seq (fn () =>
    case (pull_elem xq) of
      NONE => NONE (*Nothing to do with the empty sequence*)
      (*Waiting: create a new waiting which incorporates the work of the map function*)
    | SOME (Waiting g,sq2) => SOME (Waiting (fn fd => (f (g fd))),map f sq2)
      (*Started: don't waste the current future that is processing. Instead, create a new
      Waiting element which will join on this future when pulled.*)
    | SOME (Started g,sq2) => SOME (Waiting (fn _ => (f (Future.join g))),map f sq2)
      (*Done: just use the value - even cheaper than started!*)
    | SOME (Done v,sq2) => SOME (Waiting (fn _ => (f v)),map f sq2));
    
  (*Apply a function to the first sequence, and append the result
  to the next sequence*)
  fun mapp f xq yq = append (map f xq) yq;
  
  (*Interleave two sequences together, starting with the first
  If one runs out, the other is appended to the end.*)
  fun interleave (xq, yq) =
    Seq (fn () => case (pull_elem xq) of
        NONE => pull_elem yq (*Return the remaining sequence yq*)
      | SOME (v,sq2) => SOME (v,interleave (yq,sq2)))
        (*Swap sequences and return the element that was found*)
  
  (*An auxiliary function for filter. Note that this is forced to evaluate
  the filter condition!*)
  fun filter' fd pred xq =
    let
      fun copy sq = Seq (fn () =>
        case (pull fd sq) of
            NONE => NONE
          | SOME (v,sq2) => if (pred v) then
              SOME (Done v,copy sq2)
            else
              pull_elem (copy sq2))
    in copy xq end;
  
  (*Filter the values in a sequence. This is still lazy to a point, which
  is surprising if you think about how elements are processed in parallel.
  (See implementation details)
  
  Implementation detail:
  
  The filter function is simplified by turning the filter function into
  a function which returns SOME x if it is true for x, and NONE otherwise.
  A new filter removes all of the NONE values and then a final map converts
  SOME x to x. This allows the possibly heavy duty predicate function to be processed
  in parallel, and drastically simplifes the filter function so you won't notice
  that it forces evaluation on pulling up to the next "true" element
  *)
  fun filter fd pred xq =
    map (fn x => the x) (*Convert SOME x => x*)
      (filter' fd (fn x => not (is_none x)) (*Filter out the nones*)
      (map (fn x => if (pred x) then SOME x else NONE) xq)); (*The map function
      based on the filter function.*)
  
  (*Flatten (concatenate) a sequence of sequences. This too maintains
  laziness across the sequence of sequences and in each individually sequence
  
  Note: when the sequence of sequences experiences a "pull_elem"
  it does not force parallel pulling on the sequence
  it pulls out. When a pull then occurs on this sequence,
  it will be parallelised. This is for a number of reasons:
  
  1. pull_elem is supposed to be as cheap as possible for the parallel
  processing to be as useful as possible.
  2. If the sequence is very short, then pulling a single element
  like this will be cheaper. If it is very long, the next
  pull to the flattened sequence will cause parallelised pulling anyway.
  3. Avoiding parallel pulling here increases the coarseness of the concurrency,
  which for now is considered a good thing.
  *)
  fun flat fd sqq = Seq (fn () =>
    case (pull fd sqq) of (*I'll need the next sequence in the sequence of sequences.
    This is likely to be cheap, so it just pulls one element*)
        NONE => NONE (*Must have finished flattening*)
        (*I will need to pull the element out of this sequence, and append it to whatever
        the result of flatten is.*)
      | SOME (xq,xqq) => (*case (pull fd (append xq (flat fd xqq))) of
                  NONE => NONE
                | SOME (v,sq2) => SOME (Done v,sq2)*)
                pull_elem (append xq (flat fd xqq))
        )
  
  (*Maps a function which produces a sequence
  for every element in the original sequence. It automatically
  appends all of the results*)
  fun maps fd f sq = Seq (fn () =>
    case (pull_elem sq) of
        NONE => NONE (*Finished*)
      | SOME (Waiting g,sq2) => pull_elem (append (f (g fd)) (maps fd f sq2))
      | SOME (Started g,sq2) => pull_elem (append (f (Future.join g)) (maps fd f sq2))
      | SOME (Done v,sq2) => pull_elem (append (f v) (maps fd f sq2)))
  
  (*Maps a filter function over the sequence. If the filter returns
  none, the element is dropped. If the filter returns SOME b then
  b is attached to the sequence.
  
  Note to self:
  while this is simple and not too expensive, it is probably best to filter and then map...possibly...
  *)
  fun map_filter fd f = maps fd (fn x => (case f x of NONE => empty | SOME y => single y));
  
  (*Apply a function over the sequence. The second element
  is paired with this function for every single value*)
  fun lift f xq y = map (fn x => f x y) xq;
  
  (*Similar to lift but appends the results of the function*)
  fun lifts fd f xq y = maps fd (fn x => f x y) xq;
  
  (*Creates a singleton sequence element with the given value
  and future data*)
  fun succeed x = single x;
  
  (*Returns an empty sequence regardless of the element*)
  fun fail _ = empty;
  
  (*Turn a parallel lazy sequence into a sequential lazy sequence
  with immediate affect. That means that, other than futures already started,
  further pulls to the sequence will not cause parallel evaluation.*)
  fun truly_sequential_seq_of fd sq = let
    fun seq_of sq = Seq.make (fn () =>
      case (pull_elem sq) of
          NONE => NONE
        | SOME (e,sq2) => SOME (e,seq_of sq2)) in
    Seq.map (fn
      (Waiting g) => (g fd)
    | (Started g) => (Future.join g)
    | (Done v) => v
    ) (seq_of sq) end
  
  (*Turn a parallel lazy sequence into a sequential lazy
  sequence. This is still lazy*)
  fun sequential_seq_of fd sq = Seq.make (fn() =>
      case (pull fd sq) of
          NONE => NONE
        | SOME (e,sq2) => SOME (e,sequential_seq_of fd sq2))
  
  (*Turn a sequential lazy sequence into a parallel lazy sequence.
  Elements that were originally in the lazy sequence will continue
  to be processed sequentially, unless you apply any function across the sequence.
  The sequential elements must be processed sequentially,
  but the function applied will be applied in parallel*)
  fun of_sequential_seq sq = Seq (fn () =>
    case (Seq.pull sq) of
        NONE => NONE
      | SOME (v,sq2) =>
        SOME (Done v,of_sequential_seq sq2));
  
  (*Apply a function to a singleton list to produce a sequence
  of singleton lists, which will be turned back into an ordinary
  sequence containing the element in each resulting singleton list*)
  fun singleton f x = f [x] |> map (fn [y] => y | _ => raise Empty);
  
  (*Print a sequence, up to the n elements as supplied*)
  fun print fd print_elem count =
    let
      fun prnt (k: int) xq =
        if k > count then ()
        else
          (case pull fd xq of
            NONE => ()
          | SOME (x, xq') => (print_elem k x; writeln ""; prnt (k + 1) xq'));
    in prnt 1 end;
  
  (*Accumulating a function over a sequence; this is lazy.
  Apply the function to an element of the first
  sequence and the sequence produced by "this" to produce a new
  sequence.*)
  fun it_right fd f (xq, yq) =
    let
      fun its s = Seq (fn () => case (pull fd s) of
          NONE => pull_elem yq
        | SOME (v,sq2) => pull_elem (f (v,its sq2)))
    in its xq end;
  
  (*Apply a function f to an element x to produce a sequence.
  Then apply g to every element in this sequence to produce
  a new sequence for each individual result, and then
  append the results*)
  fun op THEN (f, g) fd x = maps fd g (f x);
  
  (*Apply a function f to an element x to produce a sequence
  If the sequence is initially empty, then return g x instead*)
  fun op ORELSE (f, g) x = Seq (fn () => case (pull_elem (f x)) of
      NONE => pull_elem (g x)
    | some => some);
  (*Apply a function f to an element x to generate a sequence.
  Then apply a function g to the same element to generate a second
  sequence and append the first to the second*)
  fun op APPEND (f, g) x = append
    (Seq (fn () => pull_elem (f x)))
    (Seq (fn () => pull_elem (g x)));
  
  (*For every function in the list, apply
  that function to each element in the sequence generated so far,
  and append the resulting sequences*)
  fun EVERY fd fs = fold_rev (fn a => fn b => THEN (a,b) fd) fs succeed;
  
  (*Wait for the first function to return a non-empty sequence
  and return that sequence*)
  fun FIRST fs = fold_rev (curry op ORELSE) fs fail;
  
  (*Return the sequence generated by f applied to the given element,
  or if that returns an empty sequence, return a singleton sequence
  containing the given element instead*)
  fun TRY f = ORELSE (f, succeed);
    
  (*f is applied to an element x to produce a sequence. If f produces the empty
  sequence, a singleton sequence with x will be returned. Otherwise,
  f is then applied to every element in the produced sequence.
  This recurs.*)
  fun REPEAT fd f =
    let
      fun rep qs x = Seq (fn() =>
        (case pull fd (f x) of
          NONE => SOME (Done x, repq qs)
        | SOME (x', q) => pull_elem (rep (q :: qs) x')))
      and repq [] = Seq (fn () => NONE)
        | repq (q :: qs) = Seq (fn() =>
            (case pull fd q of
              NONE => (pull_elem (repq qs))
            | SOME (x, q) => pull_elem (rep (q :: qs) x)));
    in fn x => rep [] x end;
  
  (*Apply the given function to the sequence first.
  Then repeat it across the resulting sequence.*)
  fun REPEAT1 fd f = THEN (f, REPEAT fd f) fd;
  
  (*If the first integer is less than the second, this returns
  a singleton sequence with the element supplied. Otherwise,
  the function f is applied to the second integer and the element.
  This sequence is appended to a sequence formed recursively
  by reapplying Interval, reducing the second integer by 1, to
  every element produced in the first sequence.*)
  fun INTERVAL fd f (i: int) j x =
    if i > j then single x
    else op THEN (f j, INTERVAL fd f i (j - 1)) fd x;
  
  (*If the fnction returns the empty sequence, this will
  return the empty sequence. Otherwise it will return the frst
  element only of the resulting sequence*)
  fun DETERM fd f x = Seq (fn () => pull_elem (
    (case pull fd (f x) of
      NONE => empty
    | SOME (x', _) => cons x' empty)));
end;

(*This signature is extremely similar to that of PARALLEL_SEQ so please
refer to that for details on what the functions do. The difference
with this is that the signature requires passing of future data
to all functions worked on within the sequence. This is considered better
management, since if you ever cancelled the sequence, you'd want any futures
it triggered to be cancelled too. This signature makes you "think" about this
(It has no additional power compared to the original signature for parallel
lazy sequences).
It also cuts out seemingly unnecessary future data requirements.

IMPORTANT: when a function involves two sequences, the future data of the first is given to the
resulting sequence. In flattening, the outer future data is kept*)
signature MJB_SAFE_PARALLEL_SEQ =
sig
  exception Empty_Parallel_Sequence of string
  type 'a seq
  (*Return the future data in use by this sequence*)
  val get_future_data : 'a seq -> MJB_Simple_Future.future_data
  (*Set the future data to be used by this sequence (generally avoid doing this)*)
  val set_future_data : MJB_Simple_Future.future_data -> 'a seq -> 'a seq
  val make : (MJB_Simple_Future.future_data -> 'a) -> 'a seq -> 'a seq
  (*Create a new empty sequence, which will begin by using the spassed in future data*)
  val empty : MJB_Simple_Future.future_data -> 'a seq
  val pull : 'a seq -> ('a * 'a seq) option
  val cons : 'a -> 'a seq -> 'a seq
  val single : MJB_Simple_Future.future_data -> 'a -> 'a seq
  val try : MJB_Simple_Future.future_data -> (MJB_Simple_Future.future_data -> 'a -> 'b) -> 'a -> 'b seq
  val hd : 'a seq -> 'a
  val tl : 'a seq -> 'a seq
  val chop : int -> 'a seq -> (('a list) * ('a seq))
  val take : int -> 'a seq -> 'a seq
  val list_of : 'a seq -> 'a list
  val of_list : MJB_Simple_Future.future_data -> 'a list -> 'a seq
  val append : 'a seq -> 'a seq -> 'a seq
  val map : (MJB_Simple_Future.future_data -> 'a -> 'b) -> 'a seq -> 'b seq
  val mapp : (MJB_Simple_Future.future_data -> 'a -> 'b) -> 'a seq -> 'b seq -> 'b seq
  val interleave: 'a seq * 'a seq -> 'a seq
  val filter : (MJB_Simple_Future.future_data -> 'a -> bool) -> 'a seq -> 'a seq
  val flat : 'a seq seq -> 'a seq
  val maps : (MJB_Simple_Future.future_data -> 'a -> 'b seq) -> 'a seq -> 'b seq
  val map_filter : (MJB_Simple_Future.future_data -> 'a -> 'b option) -> 'a seq -> 'b seq
  val lift : (MJB_Simple_Future.future_data -> 'a -> 'b -> 'c) -> 'a seq -> 'b -> 'c seq
  val lifts : (MJB_Simple_Future.future_data -> 'a -> 'b -> 'c seq) -> 'a seq -> 'b -> 'c seq
  val succeed : MJB_Simple_Future.future_data -> 'a -> 'a seq
  val fail : MJB_Simple_Future.future_data -> 'a -> 'b seq
  val sequential_seq_of : 'a seq -> 'a Seq.seq
  val truly_sequential_seq_of : 'a seq -> 'a Seq.seq
  val of_sequential_seq : MJB_Simple_Future.future_data -> 'a Seq.seq -> 'a seq
  val singleton: MJB_Simple_Future.future_data -> (MJB_Simple_Future.future_data -> 'a list -> 'b list seq) -> 'a -> 'b seq
  val print: (MJB_Simple_Future.future_data -> int -> 'a -> unit) -> int -> 'a seq -> unit
  val it_right: (MJB_Simple_Future.future_data -> 'a * 'b seq -> 'b seq) -> 'a seq * 'b seq -> 'b seq
  val THEN: (MJB_Simple_Future.future_data -> 'a -> 'b seq) * (MJB_Simple_Future.future_data -> 'b -> 'c seq) -> MJB_Simple_Future.future_data -> 'a -> 'c seq
  val ORELSE: (MJB_Simple_Future.future_data -> 'a -> 'b seq) * (MJB_Simple_Future.future_data -> 'a -> 'b seq) -> MJB_Simple_Future.future_data -> 'a -> 'b seq
  val APPEND: (MJB_Simple_Future.future_data -> 'a -> 'b seq) * (MJB_Simple_Future.future_data -> 'a -> 'b seq) -> MJB_Simple_Future.future_data -> 'a -> 'b seq
  val EVERY: MJB_Simple_Future.future_data -> (MJB_Simple_Future.future_data -> 'a -> 'a seq) list -> 'a -> 'a seq
  val FIRST: MJB_Simple_Future.future_data -> (MJB_Simple_Future.future_data -> 'a -> 'b seq) list -> 'a -> 'b seq
  val TRY: MJB_Simple_Future.future_data -> (MJB_Simple_Future.future_data -> 'a -> 'a seq) -> 'a -> 'a seq
  val REPEAT: MJB_Simple_Future.future_data -> (MJB_Simple_Future.future_data -> 'a -> 'a seq) -> 'a -> 'a seq
  val REPEAT1: MJB_Simple_Future.future_data -> (MJB_Simple_Future.future_data -> 'a -> 'a seq) -> 'a -> 'a seq
  val INTERVAL: MJB_Simple_Future.future_data -> (MJB_Simple_Future.future_data -> int -> 'a -> 'a seq) -> int -> int -> 'a -> 'a seq
  val DETERM: MJB_Simple_Future.future_data -> (MJB_Simple_Future.future_data -> 'a -> 'b seq) -> 'a -> 'b seq
end;

(*The safer way to use the parallel sequence, which manages the future data considerably for you.
It does force you to supply functions expecting future data, but this can of course be
escaped with fn fd => f

The implementations in here are pretty obvious so do not contain the comments
like Parallel_Seq. Essentially, it will pass the future data around when needed

Warning: an adjustment to the functions passed in to EVERY and FIRST means
a simple map must be computed over the list, so it is not entirely lazy.
A direct implementation of what is below might be able to avoid this, but it is unlikely
the mapping will take any significant amount of time.*)
structure MJB_Safe_Parallel_Seq :> MJB_SAFE_PARALLEL_SEQ =
struct
  exception Empty_Parallel_Sequence of string
  structure Future = MJB_Simple_Future;
  structure Seq = MJB_Parallel_Seq;
  type 'a seq = ('a Seq.seq) * Future.future_data
  (*Return the future data in use by this sequence*)
  fun get_future_data (_,fd) = fd
  (*Set the future data to be used by this sequence (generally avoid doing this)*)
  fun set_future_data fd (sq,_) = (sq,fd)
  
  (*All the functions below make use of the sequence passed in. A few complications
  do arise. In particular, if a function requires a function which returns a sequence,
  then this returned sequence is not of type Seq.seq. Hence, the function has to
  be modified. The extreme case of this is in FIRST and EVERY, where a list of functions
  have to be adjusted.*)
  fun make f (sq,fd) = (Seq.make f sq,fd)
  fun empty fd = (Seq.empty,fd)
  fun pull (sq,fd) = case (Seq.pull fd sq) of
      NONE => NONE
    | SOME (v,sq2) => SOME (v,(sq2,fd))
  fun cons v (sq,fd) = (Seq.cons v sq,fd)
  fun single fd v = (Seq.single v,fd)
  fun try fd f v = (Seq.try (f fd) v,fd)
  fun hd (sq,fd) = Seq.hd fd sq
  fun tl (sq,fd) = (Seq.tl sq,fd)
  fun chop n (sq,fd) = let val (ls,sq2) = Seq.chop fd n sq in (ls,(sq2,fd)) end
  fun take n (sq,fd) = (Seq.take n sq,fd)
  fun list_of (sq,fd) = Seq.list_of fd sq
  fun of_list fd ls = (Seq.of_list ls,fd)
  fun append (sq,fd) (sq2,_) = (Seq.append sq sq2,fd)
  fun map f (sq,fd) = (Seq.map (f fd) sq,fd)
  fun mapp f (sq,fd) (sq2,_) = (Seq.mapp (f fd) sq sq2,fd)
  fun interleave  ((sq,fd),(sq2,_)) = (Seq.interleave (sq,sq2),fd)
  fun filter f (sq,fd) = (Seq.filter fd (f fd) sq,fd)
  fun flat (sqq,fd) = (Seq.flat fd (Seq.map fst sqq),fd)
  fun maps f (sq,fd) = (Seq.maps fd (fn a => fst (f fd a)) sq,fd)
  fun map_filter f (sq,fd) = (Seq.map_filter fd (f fd) sq,fd)
  fun lift f (sq,fd) b = (Seq.lift (f fd) sq b,fd)
  fun lifts f (sq,fd) b = (Seq.lifts fd (fn a => fn b' => fst (f fd a b')) sq b,fd)
  fun succeed fd v = (Seq.succeed v,fd)
  fun fail fd v = (Seq.fail v,fd)
  fun sequential_seq_of (sq,fd) = Seq.sequential_seq_of fd sq
  fun truly_sequential_seq_of (sq,fd) =  Seq.truly_sequential_seq_of fd sq
  fun of_sequential_seq fd sq = (Seq.of_sequential_seq sq,fd)
  fun singleton fd f v = (Seq.singleton (fn a => fst (f fd a)) v,fd)
  fun print f n (sq,fd) = Seq.print fd (f fd) n sq
  fun it_right f ((sq,fd),(sq2,_)) = (Seq.it_right fd (fn (a,b) => fst (f fd (a,(b,fd)))) (sq,sq2),fd)
  fun THEN (f,g) fd v = (Seq.THEN (fn a => fst (f fd a),fn b => fst (g fd b)) fd v,fd)
  fun ORELSE (f,g) fd v = (Seq.ORELSE (fn a => fst (f fd a),fn b => fst (g fd b)) v,fd)
  fun APPEND (f,g) fd v = (Seq.APPEND (fn a => fst (f fd a),fn b => fst (g fd b)) v,fd)
  fun EVERY fd fs v = (Seq.EVERY fd (List.map (fn f => (fn a => fst (f fd a))) fs) v,fd)
  fun FIRST fd fs v = (Seq.FIRST (List.map (fn f => (fn a => fst (f fd a))) fs) v,fd)
  fun TRY fd f v = (Seq.TRY (fn a => fst (f fd a)) v,fd)
  fun REPEAT fd f v = (Seq.REPEAT fd (fn a => fst (f fd a)) v,fd)
  fun REPEAT1 fd f v = (Seq.REPEAT1 fd (fn a => fst (f fd a)) v,fd)
  fun INTERVAL fd f i j v = (Seq.INTERVAL fd (fn n => fn a => fst (f fd n a)) i j v,fd)
  fun DETERM fd f v = (Seq.DETERM fd (fn a => fst (f fd a)) v,fd)
end;

