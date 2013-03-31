local
  open Testing
  structure StrTab = NameTable(structure Dom = SStrName)
  
  fun catch_coerce_fail f = (f (); false)
  handle Map.bad_coercion_exp () => true
  
  (*fun catch_multi_fail f = (f (); false)
  handle Map.multiple_values_exp () => true*)
  
  fun catch_dup_fail f = (f (); false)
  handle Map.duplicate_exp () => true
  
  (* some names *)
  val a = SStrName.mk "a"
  val b = SStrName.mk "b"
  
  val tab : int StrTab.T = StrTab.empty
  val _ = test "An empty table" (fn () => (
      assert "should have size 0" (StrTab.size tab = 0);
      assert "should report true for is_empy" (StrTab.is_empty tab);
      assert "should return NONE for get_opt" (StrTab.get_opt tab a = NONE);
    ())) ()
  
  val tab = tab |> StrTab.set (a, 42)
  
  val _ = test "A table with one element" (fn () => (
      assert "should have size 1" (StrTab.size tab = 1);
      assert "should return correct element" (StrTab.get tab a = 42);
      assert "should return SOME (elem) for get_opt" (StrTab.get_opt tab a = SOME 42);
      assert "should fail on duplicate_exp for duplicate add"
        (catch_dup_fail (fn () => tab |> StrTab.add (a,100)));
    ())) ()
  
  val tab = tab |> StrTab.set (a, 43)
  
  val _ = test "A table after override" (fn () => (
      assert "should still have size 1" (StrTab.size tab = 1);
      assert "should return new element" (StrTab.get tab a = 43);
    ())) ()
  
  val (a1,tab) = tab |> StrTab.store 44
  val tab = tab |> StrTab.store_anon 45
  
  val _ = test "A table after storage" (fn () => (
      assert "should have size 3" (StrTab.size tab = 3);
      assert "should return correct element" (StrTab.get tab a1 = 44);
    ())) ()
  
  val tab = tab |> StrTab.delete a
  
  val _ = test "A table after deletion" (fn () => (
      assert "should have size 2" (StrTab.size tab = 2);
      assert "should return NONE for deleted element" (StrTab.get_opt tab a = NONE);
    ())) ()
  
  fun di x = case SStrName.dest x of "a" => [1] | "b" => [2] | _ => []
  fun bad_di x = case SStrName.dest x of "a" => [1] | "b" => [2,3] | _ => []
  fun dom () = [a,b]
  val abs_tab = Map.mk (Map.mk_graph (dom,di), I)
  val bad_abs_tab = Map.mk (Map.mk_graph (dom,bad_di), I)
  val coerced_tab = StrTab.coerce abs_tab
  
  val _ = test "A coerced table" (fn () => (
      assert "should have size 2" (StrTab.size coerced_tab = 2);
      assert "should contain the correct values" (
        (StrTab.get coerced_tab a = 1) andalso
        (StrTab.get coerced_tab b = 2));
    ())) ()
  
  val _ = test "A bad coercion" (fn () => (
      assert "should raise bad_coercion_exp for multi-valued graph"
        (catch_coerce_fail (fn () => StrTab.coerce bad_abs_tab));
    ())) ()
  
in
  val _ = assert_no_failed_tests ()
end