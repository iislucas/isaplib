local
  (*************************************************)
  (****************CORRECTNESS TESTS****************)
  (*************************************************)
  
  (*Some fake future data*)
  fun fd() = MJB_Simple_Future.new_future_data NONE;
  
  structure Par_Seq = MJB_Safe_Parallel_Seq;
  (*Here are the unit tests for the parallel lazy sequence. These check that
  the results are always the same as with the regular sequential lazy sequence, which is assumed
  to have the correct desired behaviour*)
  
  (*Test list conversion*)
  val _ = Testing.test "Par_Seq: Converting empty list" (fn ls =>
     if (Par_Seq.list_of (Par_Seq.of_list (fd()) ls)=
        (Seq.list_of (Seq.of_list ls))) then () else
     raise ERROR "Par_Seq: Empty list conversion failed")
     []
  
  val _ = Testing.test "Par_Seq: Converting populated list" (fn ls =>
     if (Par_Seq.list_of (Par_Seq.of_list (fd()) ls)=
        (Seq.list_of (Seq.of_list ls))) then () else
     raise ERROR "Par_Seq: Populated list conversion failed")
     [1]
  
  (*Test the make capability*)
  val _ = Testing.test "Par_Seq: Make test" (fn () =>
    if (Par_Seq.list_of (Par_Seq.make (fn _ => 1) (Par_Seq.empty (fd())))=
        Seq.list_of (Seq.make (fn () => SOME (1,Seq.empty)))) then ()
        else
    raise ERROR "Par_Seq: make test failed")
    ()
    
  (*Test the pulling...*)
  val _ = Testing.test "Par_Seq: Pull on empty test" (fn () =>
    if (is_none (Par_Seq.pull (Par_Seq.of_list (fd()) [])) andalso
        is_none (Seq.pull (Seq.of_list []))) then ()
        else
    raise ERROR "Par_Seq: pull on empty failed")
    ()
  
  val _ = Testing.test "Par_Seq: Pull on populated test" (fn () =>
    let
      val (p1,sq) = the (Par_Seq.pull (Par_Seq.of_list (fd()) [1,2]));
      val (p2,sq) = the (Par_Seq.pull sq);
      val _ = if (is_none (Par_Seq.pull sq)) then () else raise
              ERROR "Par_Seq: pull on populated did not find none";
      val (s1,sq) = the (Seq.pull (Seq.of_list [1,2]));
      val (s2,sq) = the (Seq.pull sq);
      val _ = if (is_none (Seq.pull sq)) then () else raise
              ERROR "Par_Seq: fault in seq - pull on populated did not find none";
    in
      if ([p1,p2]=[s1,s2]) then () else
      raise ERROR "Par_Seq: pull on populated failed"
    end) ()
    
  (*Test cons*)
  val _ = Testing.test "Par_Seq: cons test" (fn x =>
    if (Par_Seq.list_of (Par_Seq.cons x (Par_Seq.empty (fd())))=
        Seq.list_of (Seq.cons x Seq.empty)) then ()
        else
    raise ERROR "Par_Seq: cons failed")
    1
  
  (*Test single*)
  val _ = Testing.test "Par_Seq: cons test" (fn x =>
    if (Par_Seq.list_of (Par_Seq.single (fd()) x)=
        Seq.list_of (Seq.single x)) then ()
        else
    raise ERROR "Par_Seq: single failed")
    1
    
  (*Test empty*)
  val _ = Testing.test "Par_Seq: empty test" (fn () =>
    if (Par_Seq.list_of (Par_Seq.empty (fd()))=Seq.list_of Seq.empty) then ()
    else raise ERROR "Par_Seq: empty failed")
    
  (*Test the try function...*)
  val _ = Testing.test "Par_Seq: try test" (fn () =>
    let
      val res_s1 = Seq.list_of (Seq.try (fn a => if (a=1) then raise ERROR "" else a) 1);
      val res_s2 = Seq.list_of (Seq.try (fn a => if (a=1) then raise ERROR "" else a) 2);
      val res_p1 = Par_Seq.list_of (Par_Seq.try (fd()) (fn _ => fn a => if (a=1) then raise ERROR "" else a) 1);
      val res_p2 = Par_Seq.list_of (Par_Seq.try (fd()) (fn _ => fn a => if (a=1) then raise ERROR "" else a) 2);
    in
      if (res_s1=res_p1) then (
      if (res_s2=res_p2) then ()
      else raise ERROR "Par_Seq: try test failed without exception"
      )
      else raise ERROR "Par_Seq: try test failed after exception"
    end) ()
  
  (*Test head and tail*)
  val _ = Testing.test "Par_Seq: head and tail test" (fn () =>
    let
      val p_empty = Par_Seq.of_list (fd()) [];
      val p_single = Par_Seq.of_list (fd()) [1];
      val s_single = Seq.of_list [1];
      val p_long = Par_Seq.of_list (fd()) [1,2];
      val s_long = Seq.of_list [1,2];
      val _ = (Par_Seq.hd p_empty;raise ERROR ("Par_Seq:" ^
        " expected exception when taking the head of an empty sequence")) handle _ => ()
      val _ = (Par_Seq.tl p_empty;raise ERROR ("Par_Seq:" ^
        " expected exception when taking the tail of an empty sequence")) handle _ => ()
      (*Sequential results*)
      val s_single_hd = (Seq.hd s_single);
      val s_single_tl = Seq.list_of (Seq.tl s_single);
      val s_long_hd = (Seq.hd s_long);
      val s_long_tl = Seq.list_of (Seq.tl s_long);
      (*Parallel results*)
      val p_single_hd = (Par_Seq.hd p_single);
      val p_single_tl = Par_Seq.list_of (Par_Seq.tl p_single);
      val p_long_hd = (Par_Seq.hd p_long);
      val p_long_tl = Par_Seq.list_of (Par_Seq.tl p_long)
    in
      if (s_single_hd=p_single_hd) then
      if (s_single_tl=p_single_tl) then
      if (s_long_hd=p_long_hd) then
      if (s_long_tl=p_long_tl) then ()
      else raise ERROR "Par_Seq: long list tails did not match"
      else raise ERROR "Par_Seq: long list heads did not match"
      else raise ERROR "Par_Seq: single list tails did not match"
      else raise ERROR "Par_Seq: single list heads did not match"
    end) ()
    
  (*Test chop*)
  val _ = Testing.test "Par_Seq: chop test" (fn () =>
    let
      (*Ordinary chop*)
      val (p_res,p_sq) = (Par_Seq.chop 2 (Par_Seq.of_list (fd()) [1,2,3,4]));
      val (s_res,s_sq) = (Seq.chop 2 (Seq.of_list [1,2,3,4]));
      val p_norm = (p_res,Par_Seq.list_of p_sq);
      val s_norm = (s_res,Seq.list_of s_sq);
      (*Chopping too much*)
      val (p_res2,p_sq2) = (Par_Seq.chop 2 (Par_Seq.of_list (fd()) []));
      val (s_res2,s_sq2) = (Seq.chop 2 (Seq.of_list []));
      val p_over = (p_res2,Par_Seq.list_of p_sq2);
      val s_over = (s_res2,Seq.list_of s_sq2);
    in
      if (p_norm=s_norm) then
      if (p_over=s_over) then
      ()
      else raise ERROR "Par_Seq: chopping over the length of the list failed"
      else raise ERROR "Par_Seq: chopping ordinarily failed"
    end) ()
    
  (*Test take (similar test to chop)*)
  val _ = Testing.test "Par_Seq: take test" (fn () =>
    let
      (*Ordinary chop*)
      val p_norm = Par_Seq.list_of (Par_Seq.take 2 (Par_Seq.of_list (fd()) [1,2,3,4]));
      val s_norm = Seq.list_of (Seq.take 2 (Seq.of_list [1,2,3,4]));
      (*Chopping too much*)
      val p_over = Par_Seq.list_of (Par_Seq.take 2 (Par_Seq.of_list (fd()) []));
      val s_over = Seq.list_of (Seq.take 2 (Seq.of_list []));
    in
      if (p_norm=s_norm) then
      if (p_over=s_over) then
      ()
      else raise ERROR "Par_Seq: taking over the length of the list failed"
      else raise ERROR "Par_Seq: taking ordinarily failed"
    end) ()
    
  (*Test append*)
  val _ = Testing.test "Par_Seq: append empty test" (fn ls =>
    let
      val p_empty = Par_Seq.list_of (Par_Seq.append (Par_Seq.empty (fd())) (Par_Seq.of_list (fd()) ls));
      val s_empty = Seq.list_of (Seq.append Seq.empty (Seq.of_list ls));
      val p_pop = Par_Seq.list_of (Par_Seq.append (Par_Seq.of_list (fd()) [1] ) (Par_Seq.of_list (fd()) ls));
      val s_pop = Seq.list_of (Seq.append (Seq.of_list [1]) (Seq.of_list ls))
    in
      if (p_empty=s_empty) then
      if (p_pop=s_pop) then ()
      else raise ERROR "Par_Seq: appending [...]++[] failed"
      else raise ERROR "Par_Seq: appending []++[] failed"
    end) []
    
  val _ = Testing.test "Par_Seq: append populated test" (fn ls =>
    let
      val p_empty = Par_Seq.list_of (Par_Seq.append (Par_Seq.empty (fd())) (Par_Seq.of_list (fd()) ls));
      val s_empty = Seq.list_of (Seq.append Seq.empty (Seq.of_list ls));
      val p_pop = Par_Seq.list_of (Par_Seq.append (Par_Seq.of_list (fd()) [1] ) (Par_Seq.of_list (fd()) ls));
      val s_pop = Seq.list_of (Seq.append (Seq.of_list [1]) (Seq.of_list ls))
    in
      if (p_empty=s_empty) then
      if (p_pop=s_pop) then ()
      else raise ERROR "Par_Seq: appending [...]++[...] failed"
      else raise ERROR "Par_Seq: appending []++[...] failed"
    end) []
    
  (*Test map*)
  val _ = Testing.test "Par_Seq: map test" (fn () =>
    let
      fun add1 x = x+1;
      val p_empty = Par_Seq.list_of (Par_Seq.map (fn _ => add1) (Par_Seq.empty (fd())));
      val s_empty = Seq.list_of (Seq.map add1 Seq.empty);
      val p_pop = Par_Seq.list_of (Par_Seq.map (fn _ => add1) (Par_Seq.of_list (fd()) [1,2]));
      val s_pop = Seq.list_of (Seq.map add1 (Seq.of_list [1,2]))
    in
      if (p_empty=s_empty) then
      if (p_pop=s_pop) then ()
      else raise ERROR "Par_Seq: map over populated failed"
      else raise ERROR "Par_Seq: map over empty failed"
    end) ()
  
  (*Test mapp (brief, since the implementation is very simple)*)
  val _ = Testing.test "Par_Seq mapp test" (fn () =>
    let
      fun change 1 = "a"
        | change 2 = "b"
        | change _ = "c";
    in
      if (Par_Seq.list_of (Par_Seq.mapp (fn _ => change) (Par_Seq.of_list (fd()) [1,2]) (Par_Seq.of_list (fd()) ["c","d"]))=
        Seq.list_of (Seq.mapp change (Seq.of_list [1,2]) (Seq.of_list ["c","d"]))) then
        ()
        else raise ERROR "Par_Seq: mapp failed"
    end) ()
    
  (*Test interleave...*)
  val _ = Testing.test "Par_Seq: interleave test" (fn () =>
    let
      (*Parallel results*)
      val p_empty_empty = Par_Seq.list_of (Par_Seq.interleave (Par_Seq.empty (fd()),Par_Seq.empty (fd())));
      val p_empty_pop = Par_Seq.list_of (Par_Seq.interleave (Par_Seq.empty (fd()),Par_Seq.of_list (fd()) [1,2,3]));
      val p_pop_empty = Par_Seq.list_of (Par_Seq.interleave (Par_Seq.of_list (fd()) [1,2,3],Par_Seq.empty (fd())));
      val p_pop_pop = Par_Seq.list_of (Par_Seq.interleave (Par_Seq.of_list (fd()) [1,2,3],Par_Seq.of_list (fd()) [4,5,6]));
      (*Sequential results*)
      val s_empty_empty = Seq.list_of (Seq.interleave (Seq.empty,Seq.empty));
      val s_empty_pop = Seq.list_of (Seq.interleave (Seq.empty,Seq.of_list [1,2,3]));
      val s_pop_empty = Seq.list_of (Seq.interleave (Seq.of_list [1,2,3],Seq.empty));
      val s_pop_pop = Seq.list_of (Seq.interleave (Seq.of_list [1,2,3],Seq.of_list [4,5,6]))
    in
      if (s_empty_empty=p_empty_empty) then
      if (s_empty_pop=p_empty_pop) then
      if (s_pop_empty=p_pop_empty) then
      if (s_pop_pop=p_pop_pop) then ()
      else raise ERROR "Par_Seq: interleave pop-pop failed"
      else raise ERROR "Par_Seq: interleave pop-empty failed"
      else raise ERROR "Par_Seq: interleave empty-pop failed"
      else raise ERROR "Par_Seq: interleave empty-empty failed"
    end) ()
    
    (*Test filter*)
    val _ = Testing.test "Par_Seq: filter test" (fn () =>
      let
        val p_empty = Par_Seq.list_of (Par_Seq.filter (fn _ => fn x => x>2) (Par_Seq.empty (fd())));
        val p_pop = Par_Seq.list_of (Par_Seq.filter (fn _ => fn x => x>2) (Par_Seq.of_list (fd()) [1,2,3,4]));
        val s_empty = Seq.list_of (Seq.filter (fn x => x>2) (Seq.empty));
        val s_pop = Seq.list_of (Seq.filter (fn x => x>2) (Seq.of_list [1,2,3,4]))
      in
        if (p_empty=s_empty) then
        if (p_pop=s_pop) then ()
        else raise ERROR "Par_Seq: filter pop failed"
        else raise ERROR "Par_Seq: filter empty failed"
      end) ()
      
    (*Convert a list of lists, to a sequence of sequences...*)
    fun to_par_seq [] = Par_Seq.empty (fd())
      | to_par_seq (x::xs) = Par_Seq.cons (Par_Seq.of_list (fd()) x) (to_par_seq xs)
    fun to_seq [] = Seq.empty
      | to_seq (x::xs) = Seq.cons (Seq.of_list x) (to_seq xs)
      
    (*Testing flatten*)
    val _ = Testing.test "Par_Seq: flat test" (fn () =>
      let
        (*Parallel results*)
        val p_empty_empty = Par_Seq.list_of (Par_Seq.flat (to_par_seq [[]]));
        val p_empty = Par_Seq.list_of (Par_Seq.flat (to_par_seq []));
        val p_pop = Par_Seq.list_of (Par_Seq.flat (to_par_seq [[1],[2,3]]));
        (*Sequential results*)
        val s_empty_empty = Seq.list_of (Seq.flat (to_seq [[]]));
        val s_empty = Seq.list_of (Seq.flat (to_seq []));
        val s_pop = Seq.list_of (Seq.flat (to_seq [[1],[2,3]]))
      in
        if (p_empty_empty=s_empty_empty) then
        if (p_empty=s_empty) then
        if (p_pop=s_pop) then ()
        else raise ERROR "Par_Seq: flat test on [...[...]...] failed"
        else raise ERROR "Par_Seq: flat test on [] failed"
        else raise ERROR "Par_Seq: flat test on [[]] failed"
      end) ()
      
    (*Testing maps*)
    val _ = Testing.test "Par_Seq: maps test" (fn () =>
      let
        fun make_p_seq 1 = Par_Seq.of_list (fd()) [1,2,3]
          | make_p_seq 2 = Par_Seq.of_list (fd()) []
          | make_p_seq _ = Par_Seq.of_list (fd()) [4];
        fun make_seq 1 = Seq.of_list [1,2,3]
          | make_seq 2 = Seq.of_list []
          | make_seq _ = Seq.of_list [4];
        val p_empty = Par_Seq.list_of (Par_Seq.maps (fn _ => make_p_seq) (Par_Seq.of_list (fd()) []));
        val p_pop = Par_Seq.list_of (Par_Seq.maps (fn _ => make_p_seq) (Par_Seq.of_list (fd()) [1,2,3]));
        val s_empty = Seq.list_of (Seq.maps (make_seq) (Seq.of_list []));
        val s_pop = Seq.list_of (Seq.maps (make_seq) (Seq.of_list [1,2,3]))
      in
        if (p_empty=s_empty) then
        if (p_pop=s_pop) then ()
        else raise ERROR "Par_Seq: maps test failed on populated"
        else raise ERROR "Par_Seq: maps test failed on empty"
      end) ()
      
    (*Testing map_filter*)
    val _ = Testing.test "Par_Seq: map_filter test" (fn () =>
      let
        fun filter_func 1 = SOME "a"
          | filter_func 2 = SOME "b"
          | filter_func _ = NONE;
        val p_empty = Par_Seq.list_of (Par_Seq.map_filter (fn _ => filter_func) (Par_Seq.of_list (fd()) []));
        val p_pop = Par_Seq.list_of (Par_Seq.map_filter (fn _ => filter_func) (Par_Seq.of_list (fd()) [1,2,3,2,1]));
        val s_empty = Seq.list_of (Seq.map_filter (filter_func) (Seq.of_list []));
        val s_pop = Seq.list_of (Seq.map_filter (filter_func) (Seq.of_list [1,2,3,2,1]))
      in
        if (p_empty=s_empty) then
        if (p_pop=s_pop) then ()
        else raise ERROR "Par_Seq: map_filter test failed on populated"
        else raise ERROR "Par_Seq: map_filter test failed on empty"
      end) ()
      
    (*Testing lift - brief since defined directly from map*)
    val _ = Testing.test "Par_Seq: lift test" (fn () =>
      let
        fun lift_func "a" x = 1+x
          | lift_func "b" x = 2+x
          | lift_func _ x = x;
        val p_empty = Par_Seq.list_of (Par_Seq.lift (fn _ => lift_func) (Par_Seq.of_list (fd()) []) 1);
        val p_pop = Par_Seq.list_of (Par_Seq.lift (fn _ => lift_func) (Par_Seq.of_list (fd()) ["a","b","c","b"]) 1);
        val s_empty = Seq.list_of (Seq.lift (lift_func) (Seq.of_list []) 1);
        val s_pop = Seq.list_of (Seq.lift (lift_func) (Seq.of_list ["a","b","c","b"]) 1)
      in
        if (p_empty=s_empty) then
        if (p_pop=s_pop) then ()
        else raise ERROR "Par_Seq: lift test failed on populated"
        else raise ERROR "Par_Seq: lift test failed on empty"
      end) ()
      
    (*Testing lifts - brief since defined directly from maps*)
    val _ = Testing.test "Par_Seq: lifts test" (fn () =>
      let
        fun lift_func "a" x = [1+x]
          | lift_func "b" x = [2+x,3+x]
          | lift_func _ _ = [];
        val p_empty = Par_Seq.list_of (Par_Seq.lifts (fn _ => (Par_Seq.of_list (fd())) oo lift_func) (Par_Seq.of_list (fd()) []) 1);
        val p_pop = Par_Seq.list_of (Par_Seq.lifts (fn _ => (Par_Seq.of_list (fd())) oo lift_func) (Par_Seq.of_list (fd()) ["a","b","c","b"]) 1);
        val s_empty = Seq.list_of (Seq.lifts (Seq.of_list oo lift_func) (Seq.of_list []) 1);
        val s_pop = Seq.list_of (Seq.lifts (Seq.of_list oo lift_func) (Seq.of_list ["a","b","c","b"]) 1)
      in
        if (p_empty=s_empty) then
        if (p_pop=s_pop) then ()
        else raise ERROR "Par_Seq: lifts test failed on populated"
        else raise ERROR "Par_Seq: lifts test failed on empty"
      end) ()
      
    (*Testing succeed*)
    val _ = Testing.test "Par_Seq: succeed test" (fn () =>
      let
        val p_res = Par_Seq.list_of (Par_Seq.succeed (fd()) 1);
        val s_res = Seq.list_of (Seq.succeed 1)
      in
        if (p_res=s_res) then ()
        else raise ERROR "Par_Seq: succeed test failed"
      end) ()
    
    (*Testing fail*)
    val _ = Testing.test "Par_Seq: fail (the method) test" (fn () =>
      let
        val p_res = Par_Seq.list_of (Par_Seq.fail (fd()) 1);
        val s_res = Seq.list_of (Seq.fail 1)
      in
        if (p_res=s_res) then ()
        else raise ERROR "Par_Seq: fail (the method) test failed"
      end) ()
      
    (*Test sequential parallel conversion*)
    val _ = Testing.test "Par_Seq: sequential to parallel conversion test" (fn () =>
      let
        (*Converts both ways a few times*)
        val p_empty = Par_Seq.list_of (
        Par_Seq.of_sequential_seq (fd()) (
        Par_Seq.sequential_seq_of (
        Par_Seq.of_sequential_seq (fd()) (
        Par_Seq.truly_sequential_seq_of (
        Par_Seq.of_list (fd()) [])))))
        val p_pop = Par_Seq.list_of (
        Par_Seq.of_sequential_seq (fd()) (
        Par_Seq.sequential_seq_of (
        Par_Seq.of_sequential_seq (fd()) (
        Par_Seq.truly_sequential_seq_of (
        Par_Seq.of_list (fd()) [1,2,3])))))
      in
        if (p_empty=[]) then
        if (p_pop=[1,2,3]) then ()
        else raise ERROR "Par_Seq: sequential to parallel failed on empty"
        else raise ERROR "Par_Seq: sequential to parallel failed on populated"
      end) ()
      
    (*Test singleton*)
    val _ = Testing.test "Par_Seq: singleton test" (fn () =>
      let
        fun p_singleton_func _ _ = Par_Seq.of_list (fd()) [[1],[2],[3]];
        fun s_singleton_func _ = Seq.of_list [[1],[2],[3]];
        val p_res = Par_Seq.list_of (Par_Seq.singleton (fd()) (p_singleton_func) "a");
        val s_res = Seq.list_of (Seq.singleton (s_singleton_func) "a")
      in
        if (p_res=s_res) then ()
        else raise ERROR "Par_Seq: singleton failed"
      end) ()
    
    (*Testing it_right*)
    val _ = Testing.test "Par_Seq: it_right test" (fn () =>
      let
        fun p_it_right_func _ (1,sq) = (Par_Seq.cons "a" sq)
          | p_it_right_func _ (_,sq) = (Par_Seq.cons "b" sq);
        fun s_it_right_func (1,sq) = (Seq.cons "a" sq)
          | s_it_right_func (_,sq) = (Seq.cons "b" sq);
        val p_empty = Par_Seq.list_of (Par_Seq.it_right (p_it_right_func) (Par_Seq.of_list (fd()) [],Par_Seq.empty (fd())));
        val p_pop = Par_Seq.list_of (Par_Seq.it_right (p_it_right_func) (Par_Seq.of_list (fd()) [],Par_Seq.empty (fd())));
        val s_empty = Seq.list_of (Seq.it_right (s_it_right_func) (Seq.of_list [],Seq.empty));
        val s_pop = Seq.list_of (Seq.it_right (s_it_right_func) (Seq.of_list [],Seq.empty))
      in
        if (p_empty=s_empty) then
        if (p_pop=s_pop) then ()
        else raise ERROR "Par_Seq: it_right populated failed"
        else raise ERROR "Par_Seq: it_right empty failed"
      end) ()
    
    (*Testing THEN*)
    val _ = Testing.test "Par_Seq: THEN test" (fn () =>
      let
        fun p_func_1 _ 1 = Par_Seq.of_list (fd()) ["a","b","a"]
          | p_func_1 _ _ = Par_Seq.of_list (fd()) [];
        fun p_func_2 _ "a" = Par_Seq.of_list (fd()) [false,true,true]
          | p_func_2 _ _ = Par_Seq.of_list (fd()) [false,true,true];
        fun s_func_1 1 = Seq.of_list ["a","b","a"]
          | s_func_1 _ = Seq.of_list [];
        fun s_func_2 "a" = Seq.of_list [false,true,true]
          | s_func_2 _ = Seq.of_list [false,true,true];
        val p_empty = Par_Seq.list_of (Par_Seq.THEN (p_func_1,p_func_2) (fd()) 2);
        val p_pop = Par_Seq.list_of (Par_Seq.THEN (p_func_1,p_func_2) (fd()) 1);
        val s_empty = Seq.list_of (Seq.THEN (s_func_1,s_func_2) 2);
        val s_pop = Seq.list_of (Seq.THEN (s_func_1,s_func_2) 1)
      in
        if (p_empty=s_empty) then
        if (p_pop=s_pop) then ()
        else raise ERROR "Par_Seq: THEN populated failed"
        else raise ERROR "Par_Seq: THEN empty failed"
      end) ()
    
    (*Testing ORELSE*)
    val _ = Testing.test "Par_Seq: ORELSE test" (fn () =>
      let
        fun p_func_1 _ 1 = Par_Seq.of_list (fd()) ["a","b","c"]
          | p_func_1 _ _ = Par_Seq.empty (fd());
        fun p_func_2 _ _ = Par_Seq.of_list (fd()) ["d"];
        fun s_func_1 1 = Seq.of_list ["a","b","c"]
          | s_func_1 _ = Seq.empty;
        fun s_func_2 _ = Seq.of_list ["d"];
        val p_first = Par_Seq.list_of (Par_Seq.ORELSE (p_func_1,p_func_2) (fd()) 1);
        val p_second = Par_Seq.list_of (Par_Seq.ORELSE (p_func_1,p_func_2) (fd()) 2);
        val s_first = Seq.list_of (Seq.ORELSE (s_func_1,s_func_2) 1);
        val s_second = Seq.list_of (Seq.ORELSE (s_func_1,s_func_2) 2)
      in
        if (p_first=s_first) then
        if (p_second=s_second) then ()
        else raise ERROR "Par_Seq: ORELSE second failed"
        else raise ERROR "Par_Seq: ORELSE first failed"
      end) ()
      
    (*Testing APPEND*)
    val _ = Testing.test "Par_Seq: ORELSE test" (fn () =>
      let
        fun p_func_1 _ _ = Par_Seq.of_list (fd()) ["a","b","c"];
        fun p_func_2 _ _ = Par_Seq.of_list (fd()) ["d"];
        fun s_func_1 _ = Seq.of_list ["a","b","c"];
        fun s_func_2 _ = Seq.of_list ["d"];
        val p_res = Par_Seq.list_of (Par_Seq.APPEND (p_func_1,p_func_2) (fd()) 1);
        val s_res = Seq.list_of (Seq.APPEND (s_func_1,s_func_2) 1)
      in
        if (p_res=s_res) then ()
        else raise ERROR "Par_Seq: APPEND test failed"
      end) ()
      
    (*Testing EVERY*)
    val _ = Testing.test "Par_Seq: EVERY test" (fn () =>
      let
        fun p_func_1 _ _ = Par_Seq.of_list (fd()) [1,2,3];
        fun p_func_2 _ _ = Par_Seq.of_list (fd()) [4,5];
        fun s_func_1 _ = Seq.of_list [1,2,3];
        fun s_func_2 _ = Seq.of_list [4,5];
        val p_empty = Par_Seq.list_of (Par_Seq.EVERY (fd()) [] 1);
        val s_empty = Seq.list_of (Seq.EVERY [] 1)
        val p_pop = Par_Seq.list_of (Par_Seq.EVERY (fd()) [p_func_1,p_func_2] 1);
        val s_pop = Seq.list_of (Seq.EVERY [s_func_1,s_func_2] 1)
      in
        if (p_empty=s_empty) then
        if (p_pop=s_pop) then ()
        else raise ERROR "Par_Seq: EVERY test failed on populated"
        else raise ERROR "Par_Seq: EVERY test failed on empty"
      end) ()
      
    (*Testing FIRST*)
    val _ = Testing.test "Par_Seq: FIRST test" (fn () =>
      let
        fun p_func_1 _ _ = Par_Seq.of_list (fd()) [];
        fun p_func_2 _ _ = Par_Seq.of_list (fd()) [4,5];
        fun s_func_1 _ = Seq.of_list [];
        fun s_func_2 _ = Seq.of_list [4,5];
        val p_empty = Par_Seq.list_of (Par_Seq.FIRST (fd()) [] 1);
        val s_empty = Seq.list_of (Seq.FIRST [] 1)
        val p_pop = Par_Seq.list_of (Par_Seq.FIRST (fd()) [p_func_1,p_func_2] 1);
        val s_pop = Seq.list_of (Seq.FIRST [s_func_1,s_func_2] 1)
      in
        if (p_empty=s_empty) then
        if (p_pop=s_pop) then ()
        else raise ERROR "Par_Seq: FIRST test failed on populated"
        else raise ERROR "Par_Seq: FIRST test failed on empty"
      end) ()
      
    (*Testing TRY*)
    val _ = Testing.test "Par_Seq: TRY test" (fn () =>
      let
        fun p_func_1 _ _ = Par_Seq.of_list (fd()) [];
        fun p_func_2 _ _ = Par_Seq.of_list (fd()) [4,5];
        fun s_func_1 _ = Seq.of_list [];
        fun s_func_2 _ = Seq.of_list [4,5];
        val p_empty = Par_Seq.list_of (Par_Seq.TRY (fd()) p_func_1 1);
        val s_empty = Seq.list_of (Seq.TRY s_func_1 1)
        val p_pop = Par_Seq.list_of (Par_Seq.TRY (fd()) p_func_2 1);
        val s_pop = Seq.list_of (Seq.TRY s_func_2 1)
      in
        if (p_empty=s_empty) then
        if (p_pop=s_pop) then ()
        else raise ERROR "Par_Seq: TRY test failed on populated"
        else raise ERROR "Par_Seq: TRY test failed on empty"
      end) ()
      
    (*Testing INTERVAL*)
    val _ = Testing.test "Par_Seq: INTERVAL test" (fn () =>
      let
        fun p_func _ x _ = Par_Seq.of_list (fd()) [x,1];
        fun s_func x _ = Seq.of_list [x,1];
        val p_empty = Par_Seq.list_of (Par_Seq.INTERVAL (fd()) p_func 2 1 0);
        val p_pop = Par_Seq.list_of (Par_Seq.INTERVAL (fd()) p_func 1 3 0);
        val s_empty = Seq.list_of (Seq.INTERVAL s_func 2 1 0);
        val s_pop = Seq.list_of (Seq.INTERVAL s_func 1 3 0)
      in
        if (p_empty=s_empty) then
        if (p_pop=s_pop) then ()
        else raise ERROR "Par_Seq: INTERVAL test failed on populated"
        else raise ERROR "Par_Seq: INTERVAL test failed on empty"
      end) ()
      
    (*Testing DETERM*)
    val _ = Testing.test "Par_Seq: DETERM test" (fn () =>
      let
        fun p_func_1 _ _ = Par_Seq.of_list (fd()) [];
        fun p_func_2 _ _ = Par_Seq.of_list (fd()) ["a","b"];
        fun s_func_1 _ = Seq.of_list [];
        fun s_func_2 _ = Seq.of_list ["a","b"];
        val p_empty = Par_Seq.list_of (Par_Seq.DETERM (fd()) p_func_1 1);
        val p_pop = Par_Seq.list_of (Par_Seq.DETERM (fd()) p_func_2 1);
        val s_empty = Seq.list_of (Seq.DETERM s_func_1 1);
        val s_pop = Seq.list_of (Seq.DETERM s_func_2 1)
      in
        if (p_empty=s_empty) then
        if (p_pop=s_pop) then ()
        else raise ERROR "Par_Seq: DETERM test failed on populated"
        else raise ERROR "Par_Seq: DETERM test failed on empty"
      end) ()
      
    (*Testing REPEAT*)
    val _ = Testing.test "Par_Seq: REPEAT test" (fn () =>
      let
        (*Sequences might be infinite/large, so only check,
        the first 5 elements...*)
        fun p_func_1 _ _ = Par_Seq.of_list (fd()) [];
        fun p_func_2 _ 1 = Par_Seq.of_list (fd()) [2,3]
          | p_func_2 _ 2 = Par_Seq.of_list (fd()) [3,4]
          | p_func_2 _ 3 = Par_Seq.of_list (fd()) [4,5]
          | p_func_2 _ 4 = Par_Seq.of_list (fd()) [5]
          | p_func_2 _ _ = Par_Seq.of_list (fd()) [];
        fun s_func_1 _ = Seq.of_list [];
        fun s_func_2 1 = Seq.of_list [2,3]
          | s_func_2 2 = Seq.of_list [3,4]
          | s_func_2 3 = Seq.of_list [4,5]
          | s_func_2 4 = Seq.of_list [5]
          | s_func_2 _ = Seq.of_list [];
        val take_no = 5;
        val p_empty = Par_Seq.list_of (Par_Seq.take take_no (Par_Seq.REPEAT (fd()) p_func_1 1));
        val p_pop = Par_Seq.list_of (Par_Seq.take take_no (Par_Seq.REPEAT (fd()) p_func_2 1));
        val s_empty = Seq.list_of (Seq.take take_no (Seq.REPEAT s_func_1 1));
        val s_pop = Seq.list_of (Seq.take take_no (Seq.REPEAT s_func_2 1))
      in
        if (p_empty=s_empty) then
        if (p_pop=s_pop) then ()
        else raise ERROR "Par_Seq: REPEAT test failed on populated"
        else raise ERROR "Par_Seq: REPEAT test failed on empty"
      end) ()
      
    (*Testing REPEAT1*)
    val _ = Testing.test "Par_Seq: REPEAT1 test" (fn () =>
      let
        (*Sequences might be infinite/large, so only check,
        the first 5 elements...*)
        fun p_func_1 _ _ = Par_Seq.of_list (fd()) [];
        fun p_func_2 _ 1 = Par_Seq.of_list (fd()) [2,3]
          | p_func_2 _ 2 = Par_Seq.of_list (fd()) [3,4]
          | p_func_2 _ 3 = Par_Seq.of_list (fd()) [4,5]
          | p_func_2 _ 4 = Par_Seq.of_list (fd()) [5]
          | p_func_2 _ _ = Par_Seq.of_list (fd()) [];
        fun s_func_1 _ = Seq.of_list [];
        fun s_func_2 1 = Seq.of_list [2,3]
          | s_func_2 2 = Seq.of_list [3,4]
          | s_func_2 3 = Seq.of_list [4,5]
          | s_func_2 4 = Seq.of_list [5]
          | s_func_2 _ = Seq.of_list [];
        val take_no = 5;
        val p_empty = Par_Seq.list_of (Par_Seq.take take_no (Par_Seq.REPEAT1 (fd()) p_func_1 1));
        val p_pop = Par_Seq.list_of (Par_Seq.take take_no (Par_Seq.REPEAT1 (fd()) p_func_2 1));
        val s_empty = Seq.list_of (Seq.take take_no (Seq.REPEAT1 s_func_1 1));
        val s_pop = Seq.list_of (Seq.take take_no (Seq.REPEAT1 s_func_2 1))
      in
        if (p_empty=s_empty) then
        if (p_pop=s_pop) then ()
        else raise ERROR "Par_Seq: REPEAT1 test failed on populated"
        else raise ERROR "Par_Seq: REPEAT1 test failed on empty"
      end) ()
in
  val _ = Testing.assert_no_failed_tests();
  val _ = "UNIT TESTS FOR PARALLEL LAZY SEQUENCE PASSED!"
  
  (*************************************************)
  (*******************SPEED TESTS*******************)
  (*************************************************)
  
  (*These are held in an additional structure since they should not be run at
  compile time*)
  
  structure MJB_Par_Seq_Speed_Test =
  struct
    structure Par_Seq = MJB_Safe_Parallel_Seq;
    structure Stopwatch = MJB_Stopwatch;
    structure Future = MJB_Simple_Future;
    structure File = MJB_File;
    
    (*A structure for handling the file used during testing*)
    structure File_Aux =
    struct
      structure File = MJB_File;
      (*The default file to use*)
      val file = Unsynchronized.ref "../../isaplib/Concurrent/test/par_seq-results.txt";
      (*Open the file and be ready to append it...*)
      val is_open = Unsynchronized.ref NONE;
      (*Get the file*)
      fun get_file() = case (!is_open) of
          NONE => raise ERROR "cannot get the file"
        | (SOME f) => f
      (*Test to see if the file is currently open*)
      fun is_file_open() = not (is_none (!is_open))
      (*Open and return the file for appending*)
      fun append() = if (is_file_open()) then get_file()
        else let val ans = File.append (!file) in
          (is_open:=SOME ans;ans) end
      (*Return the file for writing. Wipes out everything
      previously in the file*)
      fun write() = if (is_file_open()) then
        (File.close (get_file());let val ans = File.write (!file) in
          (is_open:=SOME ans;ans) end) else
          let val ans = File.write (!file) in (is_open:=SOME ans;ans) end
      (*Close the file*)
      fun close() = if (is_file_open()) then
          (File.close (get_file());is_open:=NONE;())
        else ()
      (*Remove all the contents in the file.
      This leaves the file closed.*)
      fun renew() =
        (
        if (is_file_open()) then close() else ();
        write();
        close()
        )
    end;
    
    structure Help =
    struct
      structure File = MJB_File;
      (*List helper functions*)
      
      (*Create a list of numbers between the specified range*)
      fun --- (n1,n2) = if (n1>n2) then List.rev (--- (n2,n1)) else
        if (n1=n2) then [n1] else n1::(---(n1+1,n2))
      infix ---;
      
      (*Zip two lists together...*)
      fun zip [] _ = []
        | zip _ [] = []
        | zip (x::xs) (y::ys) = (x,y)::(zip xs ys)
      
      (*Attach an element to all elements in a list*)
      fun attach [] _ = []
        | attach (x::xs) y = (x,y)::(attach xs y)
        
      (*Attach an element to all elements in a list*)
      fun attach2 [] _ = []
        | attach2 (x::xs) y = (y,x)::(attach2 xs y)
      
      (*Repeat an element as a list*)
      fun repeat_ls _ 0 = []
        | repeat_ls x n = x::(repeat_ls x (n-1))
      
      (*Compress tuples...*)
      fun compress [] = []
        | compress ((x,(y,z))::xs) = (x,y,z)::(compress xs)
        
      (*Compress tuples...*)
      fun compress2 [] = []
        | compress2 (((x,y),z)::xs) = (x,y,z)::(compress2 xs)
        
      (*Generate a list of n items given a function and a starting element*)
      fun gen_list 0 _ _ = []
        | gen_list n x f = x::(gen_list (n-1) (f x) f)
      
      (*Repeat each element in a sequence a number of times.
      e.g: duplicate 5 [1,2] => [1,1,1,1,1,2,2,2,2,2]*)
      fun duplicate _ [] = []
        | duplicate n (x::xs) = (repeat_ls x n)@(duplicate n xs)
        
      (*Create a "proportional" list, where the first number is what each
      pair of elements in the resulting list should multiply to. For example:
      
      proportional 100 [1,2,4,5,10] = [(1,100),(2,50),(4,25),(5,20),(10,10)]
      Integer lists only
      *)
      fun proportional _ [] = []
        | proportional x (y::ys) = (y,x div y)::(proportional x ys)
        
      (*This function is time consuming. How long it takes can be adjusted
      by the base and factor*)
      val base = 5000 div 160
      val factor = 1
      fun expensive () =
        let
          fun runLots 0 _ _ = ()
            | runLots n x t =
              if (x>=(base div 10) * factor) then runLots (n-1) 1 (t+1)
              else runLots (n-1) (x+1) (t+1)
        in
          runLots (factor * base) 1 1
        end
        
      (*A function which will not terminate. This is used to disable the cores
      on a sequential algorithm*)
      fun infinite () =
        infinite()
        
      (*Wrap a sequential function to make it behave more "fairly"
      This should not be used if the function contains some parallelised
      code, as it may give biased results in that case. Note that if the supposedly
      parallel algorithm runs more sequentially than it should, it will probably
      appear faster due to its ability to make use of the cores more effectively*)
      
      fun wrap_sequential sq =
        fn() =>(
        let
          val num_cores = Thread.numProcessors();
          fun gen_futures 0 = []
            | gen_futures n = let
                val f = Future.future infinite
              in
                f::(gen_futures (n-1))
              end;
          val to_cancel = gen_futures (num_cores-1);
          fun cancel_all [] = ()
            | cancel_all (f::fs) = (Future.cancel f;cancel_all fs)
        in
          (sq();cancel_all to_cancel;())
        end)
      
      (*Repeat a function expecting the unit "n" times*)
      fun repeat 0 _ = ()
        | repeat n f = (f();repeat (n-1) f)
      
      (*Creates a function of a certain grade of time consumption*)
      fun gen_func n = fn () => repeat n expensive
      
      (*Generate a random integer between two integers*)
      fun random x y = if (x>y) then random_range y x else random_range x y
      
      (*Generate a function of random hardness, with the specified integer range*)
      fun gen_random_func (low,high) = fn () => repeat (random low high) expensive
      
      (*A list of randomly hard functions, between the two specified integers*)
      fun random_list low high len = gen_list len (gen_func (random low high)) (fn _ => (gen_func (random low high)))
      
      (*Some fake future data*)
      fun fd() = MJB_Simple_Future.new_future_data NONE;
      
      (*Returns the amount of time it took f to run
      in microseconds (LargeInt)*)
      fun time f =
        let
          val sw = Stopwatch.new;
          val sw = Stopwatch.start sw;
          val _ = f();
          val (t,_) = Stopwatch.stop sw
        in
          Time.toMicroseconds t
        end
      
      (*Wrap a function so that it will accept future data*)
      val wrap_fd = (fn f => (fn _ => f))
        : ('a -> 'b) -> (MJB_Simple_Future.future_data -> 'a -> 'b)
      
      (*Time the expensive functions*)
      fun time_expensive hardness =
        let
          fun time_e [] = ()
            | time_e (h::hs) = (
              let
                val file = File_Aux.append();
                val t = time (gen_func h)
              in
              File.writeStr file ("Hardness: " ^ (Int.toString(h)) ^ " Time: " ^
              LargeInt.toString(t) ^ "\n") end;time_e hs)
        in
          (
          File_Aux.renew();
          time_e hardness;
          File_Aux.close()
          )
        end
    end;
    
    (*The tests themselves. Note that these don't attempt to compare variants
    of the existing algorithm, but just test the current implementation's
    efficiency.
    MJB: a few tests with different heuristics have been recorded in Conc vs. Seq*)
    structure Tests =
    struct
      local open Help; in
        (*For specifying hardness*)
        datatype hard = Fixed of int | Random of (int * int)
        
        (*Integer indicating both should be run together*)
        val both = 0;
        (*Integer indicating only the parallel algorithm should run*)
        val parallel_only = 1;
        (*Indicates only the sequential algorithm should run*)
        val sequential_only = 2;
        
        (*Test the parallel lazy sequence on lists with varying hardness and lengths
        potentially. The format is (length,hardness) and the pairs are supplied
        as list
        
        Hardness is supplied as a range or a fixed value. If a range is given,
        the function generated will be of random hardness.
        
        If include_filter is true, it will apply a filter to the sequence
        after one mapping of the function, and then the function will be mapped
        again after the filter. This is useful because filtering reduces
        parallelisation in the sequence, so it is an important check.
        The filtering has the same hardness as the other functions.
        The filter always returns false. The test is more to ensure that the implementation
        of filtering is efficient. By returning false, a single pull
        forces the entire sequence to be evaluated immediately.
        
        who_runs indicates whether the sequential or parallel algorithms will run
        *)
        fun test_against_generic tests include_filter who_runs =
          let
            val _ = PolyML.print("Found " ^ (Int.toString(Thread.numProcessors())) ^ " processors.");
            fun run_test len hrd = 
              let
                val file = File_Aux.append();
                (*Create the sequence*)
                val sq = Seq.of_list (repeat_ls () len);
                val par_sq = Par_Seq.of_sequential_seq (fd()) sq;
                (*Create the function*)
                val func = case (hrd) of
                     Fixed n => gen_func n
                   | Random (low,high) => gen_random_func (low,high);
                (*Time the two runs...*)
                (*val _ = if (not (who_runs=parallel_only)) then PolyML.print("Running sequential")
                        else PolyML.print("Skipping sequential");*)
                val t_seq = if (not (who_runs=parallel_only)) then time ((*wrap_sequential ( *)
                  if (include_filter) then (fn () =>
                  Seq.list_of (Seq.map func
                  (Seq.filter (fn _ => (func();false)) (Seq.map func sq))))
                  else (fn () =>
                  Seq.list_of (Seq.map func sq))(* ) *))
                  else 0;
                (*val _ = if (not (who_runs=sequential_only)) then PolyML.print("Running parallel")
                        else PolyML.print("Skipping parallel");*)
                val t_par = if (not (who_runs=sequential_only)) then time (
                  if (include_filter) then (fn () =>
                  Par_Seq.list_of (Par_Seq.map (wrap_fd func)
                  (Par_Seq.filter (wrap_fd (fn _ => (func();false))) (Par_Seq.map (wrap_fd func) par_sq))))
                  else (fn () => Par_Seq.list_of (Par_Seq.map (wrap_fd func) par_sq)))
                  else 0;
              in
                ( (*Now print the results...*)
                File.writeStr file
                ((case hrd of
                    Fixed n => "Fixed_hardness: " ^ Int.toString(n)
                  | Random (low,high) => "Random_hardness_rng: (" ^
                      Int.toString(low) ^ "," ^ Int.toString(high) ^ ")") ^ " Length: " ^ Int.toString(len) ^ 
                 (if (who_runs=sequential_only) then "" else (" Parallel: " ^ LargeInt.toString(t_par))) ^
                 (if (who_runs=parallel_only) then "" else (" Sequential: " ^ LargeInt.toString(t_seq))) ^ "\n");())
              end;
            (*Run all of the tests*)
            fun run_all [] = ()
              | run_all ((len,hrd)::ts) = (run_test len hrd;run_all ts)
          in
            (
            File_Aux.renew();
            run_all tests;
            File_Aux.close()
            )
          end
       
        (*Test the speedup of the parallel lazy sequence against lists of varying length
        but fixed hardness.
        Again, you can choose whether or not to include the filter*)
        fun test_against_lengths lengths hardness include_filter who_runs =
          test_against_generic (attach lengths (Fixed hardness)) include_filter who_runs
       
        (*Test the speedup of the parallel lazy sequence against lists of varying
        fixed hardness, but fixed length.
        You can again choose whether or not to include a filter*)
        fun test_against_hardness hardnesses length include_filter who_runs =
          test_against_generic (attach2 (List.map (fn n => Fixed n) hardnesses) length) include_filter who_runs
        
        (*Test the speedup of the parallel lazy sequence against lists
        of the length specified, with hardnesses varying between the integer
        range given for each element in the list*)
        fun test_against_random_hardness lengths (low,high) include_filter who_runs =
          test_against_generic (attach lengths (Random (low,high))) include_filter who_runs
      end;
    end;
  end;
end
