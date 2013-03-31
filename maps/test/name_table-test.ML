local
  open Testing
  (* create an opaque name type *)
  structure ANames :> sig
    structure A : SSTR_NAME
  end =
  struct
    structure A = SStrName
  end
  
  open ANames
  
  structure ATab = NameTable(structure Dom = A)
  
  (* for testing renaming-style compositions *)
  structure AAInj = NameInjection(
    structure Dom = A
    structure Cod = A
  )
  
  fun catch_coerce_fail f = (f (); false)
  handle Map.bad_coercion_exp () => true
  
  (*fun catch_multi_fail f = (f (); false)
  handle Map.multiple_values_exp () => true*)
  
  fun catch_dup_fail f = (f (); false)
  handle Map.duplicate_exp () => true
  
  (* some names *)
  val (a,b,c,d) = (A.mk "a", A.mk "b", A.mk "c", A.mk "d")
  
  val tab : int ATab.T = ATab.empty
  val _ = test "An empty table" (fn () => (
      assert "should have size 0" (ATab.size tab = 0);
      assert "should report true for is_empy" (ATab.is_empty tab);
      assert "should return NONE for get_opt" (ATab.get_opt tab a = NONE);
    ())) ()
  
  val tab = tab |> ATab.set (a, 42)
  
  val _ = test "A table with one element" (fn () => (
      assert "should have size 1" (ATab.size tab = 1);
      assert "should return correct element" (ATab.get tab a = 42);
      assert "should return SOME (elem) for get_opt" (ATab.get_opt tab a = SOME 42);
      assert "should fail on duplicate_exp for duplicate add"
        (catch_dup_fail (fn () => tab |> ATab.add (a,100)));
    ())) ()
  
  val tab = tab |> ATab.set (a, 43)
  
  val _ = test "A table after override" (fn () => (
      assert "should still have size 1" (ATab.size tab = 1);
      assert "should return new element" (ATab.get tab a = 43);
    ())) ()
  
  val (a1,tab) = tab |> ATab.store 44
  val tab = tab |> ATab.store_anon 45
  
  val _ = test "A table after storage" (fn () => (
      assert "should have size 3" (ATab.size tab = 3);
      assert "should return correct element" (ATab.get tab a1 = 44);
    ())) ()
  
  val tab = tab |> ATab.delete a
  
  val _ = test "A table after deletion" (fn () => (
      assert "should have size 2" (ATab.size tab = 2);
      assert "should return NONE for deleted element" (ATab.get_opt tab a = NONE);
    ())) ()
  
  fun di x = case A.dest x of "a" => [1] | "b" => [2] | _ => []
  fun bad_di x = case A.dest x of "a" => [1] | "b" => [2,3] | _ => []
  fun dom () = [a,b]
  val abs_tab = Map.mk (Map.mk_graph (dom,di), I)
  val bad_abs_tab = Map.mk (Map.mk_graph (dom,bad_di), I)
  val coerced_tab = ATab.coerce abs_tab
  
  val _ = test "A coerced table" (fn () => (
      assert "should have size 2" (ATab.size coerced_tab = 2);
      assert "should contain the correct values" (
        (ATab.get coerced_tab a = 1) andalso
        (ATab.get coerced_tab b = 2));
    ())) ()
  
  val _ = test "A bad coercion" (fn () => (
      assert "should raise bad_coercion_exp for multi-valued graph"
        (catch_coerce_fail (fn () => ATab.coerce bad_abs_tab));
    ())) ()
  
  (*
  Keys in a table can be renamed by pre-composition with an injective map.
  
  Consider a table, and an injective function:
    tab :: a |-> 1, b |-> 2, c |-> 3
    rn :: a |-> b, b |-> a, c |-> d
  
  Then, we can rename the keys using 'rn' with (tab o rn^-1). This yields:
    tab' :: b |-> 1, a |-> 2, d |-> 3
  *)

  val tab = ATab.empty |> ATab.add (a,1) |> ATab.add (b,2) |> ATab.add (c,3)
  val rn = AAInj.empty |> AAInj.add (a,b) |> AAInj.add (b,a) |> AAInj.add (c,d)
  val tab' = ATab.compose (tab, AAInj.inverse_of rn)
  
  val _ = test "A renamed table" (fn () => (
      assert "should have size 3" (ATab.size tab' = 3);
      assert "should return correct elements"
        (ATab.get tab' b = 1 andalso ATab.get tab' a = 2 andalso ATab.get tab' d = 3);
    ())) ()
  
in
  val _ = assert_no_failed_tests ()
end