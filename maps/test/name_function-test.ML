local
  open Testing
  (* create three distinct kinds of names *)
  structure ABCNames :> sig
    structure A : SSTR_NAME
    structure B : SSTR_NAME
    structure C : SSTR_NAME
  end =
  struct
    structure A = SStrName
    structure B = SStrName
    structure C = SStrName
  end
  
  open ABCNames
  
  structure ABFn = NameFunction(
    structure Dom = A
    structure Cod = B
  )
  
  structure BCFn = NameFunction(
    structure Dom = B
    structure Cod = C
  )
  
  (* for testing renaming-style compositions *)
  structure AAInj = NameInjection(
    structure Dom = A
    structure Cod = A
  )
  
  structure BBInj = NameInjection(
    structure Dom = B
    structure Cod = B
  )
  
  
  fun catch_coerce_fail f = (f (); false)
  handle Map.bad_coercion_exp () => true
  
  fun catch_multi_fail f = (f (); false)
  handle Map.multiple_values_exp () => true
  
  fun catch_dup_fail f = (f (); false)
  handle Map.duplicate_exp () => true
  
  (* some names *)
  val (a1,a2,a3) = (A.mk "a1", A.mk "a2", A.mk "a3")
  val (b1,b2,b3) = (B.mk "b1", B.mk "b2", B.mk "b3")
  val (c1,c2,c3) = (C.mk "c1", C.mk "c2", C.mk "c3")
  
  (* naming convention: for arbitrary name-functions, use letters typical for
     functions, but doubled. e.g. ff, gg, hh *)
  val ff = ABFn.empty
  
  (*** typical mapping tests ***)
  
  val _ = test "An empty function" (fn () => (
      assert "should have size 0" (ABFn.size ff = 0);
      assert "should report true for is_empy" (ABFn.is_empty ff);
      assert "should return NONE for get_opt" (is_none (ABFn.get_opt ff a1));
    ())) ()
  
  val ff = ff |> ABFn.set (a1, b1)
  val ff = ff |> ABFn.set (a2, b2)
  val ff = ff |> ABFn.set (a3, b2)
  
  val _ = test "A function mapping three elements" (fn () => (
      assert "should have size 3" (ABFn.size ff = 3);
      assert "should have codomain size 2" (B.NSet.cardinality (ABFn.get_cod_set ff) = 2);
      assert "should return correct value for a1" (B.name_eq (ABFn.get ff a1, b1));
      assert "should return correct value for a2" (B.name_eq (ABFn.get ff a2, b2));
      assert "should return correct value for a3" (B.name_eq (ABFn.get ff a3, b2));
      assert "should return correct inverse image for b1"
        (A.NSet.eq (ABFn.inv_img ff b1) (A.NSet.of_list [a1]));
      assert "should return correct inverse image for b2"
        (A.NSet.eq (ABFn.inv_img ff b2) (A.NSet.of_list [a2,a3]));
      assert "should return SOME (elem) for get_opt"
        (B.name_eq (the (ABFn.get_opt ff a1), b1));
      assert "should fail with multiple_values_exp for inv_get(b2)"
        (catch_multi_fail (fn () => ABFn.inv_get ff b2));
      assert "should fail with multiple_values_exp for inv_get_opt(b2)"
        (catch_multi_fail (fn () => ABFn.inv_get_opt ff b2));
      assert "should fail on duplicate_exp for duplicate add"
        (catch_dup_fail (fn () => ff |> ABFn.add (a1,b3)));
    ())) ()
  
  val ff = ff |> ABFn.set (a3, b3)
  
  val _ = test "A function after override 1" (fn () => (
      assert "should still have size 3" (ABFn.size ff = 3);
      assert "should have codomain size 3" (B.NSet.cardinality (ABFn.get_cod_set ff) = 3);
      assert "should return new element" (B.name_eq (ABFn.get ff a3, b3));
      assert "should return correct inverse image for b2"
        (A.NSet.eq (ABFn.inv_img ff b2) (A.NSet.of_list [a2]));
      assert "should return correct inverse image for b3"
        (A.NSet.eq (ABFn.inv_img ff b3) (A.NSet.of_list [a3]));
    ())) ()
  
  val ff = ff |> ABFn.set (a2, b3)
  
  val _ = test "A function after override 2" (fn () => (
      assert "should still have size 3" (ABFn.size ff = 3);
      assert "should have codomain size 2" (B.NSet.cardinality (ABFn.get_cod_set ff) = 2);
      assert "should return new element" (B.name_eq (ABFn.get ff a2, b3));
      assert "should return empty inverse image for b2"
        (A.NSet.eq (ABFn.inv_img ff b2) (A.NSet.of_list []));
      assert "should return correct inverse image for b3"
        (A.NSet.eq (ABFn.inv_img ff b3) (A.NSet.of_list [a2, a3]));
    ())) ()
  
  val ff = ff |> ABFn.delete a2
  
  val _ = test "A function after deletion 1" (fn () => (
      assert "should have size 2" (ABFn.size ff = 2);
      assert "should still have codomain size 2"
        (B.NSet.cardinality (ABFn.get_cod_set ff) = 2);
      assert "should return correct inverse image for b3"
        (A.NSet.eq (ABFn.inv_img ff b3) (A.NSet.of_list [a3]));
      assert "should return NONE for deleted element" (is_none (ABFn.get_opt ff a2));
    ())) ()
  
  val ff = ff |> ABFn.delete a3
  
  val _ = test "A function after deletion 2" (fn () => (
      assert "should have size 1" (ABFn.size ff = 1);
      assert "should have codomain size 1"
        (B.NSet.cardinality (ABFn.get_cod_set ff) = 1);
      assert "should return empty inverse image for b3"
        (A.NSet.eq (ABFn.inv_img ff b3) (A.NSet.of_list []));
      assert "should return NONE for deleted element" (is_none (ABFn.get_opt ff a3));
    ())) ()
  
  (*** coercion tests ***)
  
  fun di x = case A.dest x of "a1" => [b1] | "a2" => [b2] | _ => []
  fun multi_di x = case A.dest x of "a1" => [b1] | "a2" => [b2,b3] | _ => []
  fun non_ff_di x = case A.dest x of "a1" => [b1] | "a2" => [b1] | _ => []
  fun dom () = [a1,a2]
  val abs_ff = Map.mk (Map.mk_graph (dom,di), I)
  val multi_abs = Map.mk (Map.mk_graph (dom,multi_di), I)
  val non_ff_abs = Map.mk (Map.mk_graph (dom,non_ff_di), I)
  val coerced_ff = ABFn.coerce abs_ff
  
  val _ = test "A coerced map" (fn () => (
      assert "should have size 2" (ABFn.size coerced_ff = 2);
      assert "should contain the correct values" (
        (B.name_eq (ABFn.get coerced_ff a1, b1)) andalso
        (B.name_eq (ABFn.get coerced_ff a2, b2)));
    ())) ()
  
  val _ = test "A bad coercion" (fn () => (
      assert "should raise bad_coercion_exp for multi-valued graph"
        (catch_coerce_fail (fn () => ABFn.coerce multi_abs));
    ())) ()
  
  (*** inverses and composition ***)
  (*val ff = ABFn.empty |> ABFn.add (a1,b1) |> ABFn.add (a2,b2)
  val inv_ff = BAInj.inverse_of ff
  
  val _ = test "The inverse of an ffection with two elements" (fn () => (
      assert "should have size 2" (ABFn.size ff = 2);
      assert "should return correct value for b1" (A.name_eq (BAInj.get inv_ff b1, a1));
      assert "should return correct value for b2" (A.name_eq (BAInj.get inv_ff b2, a2));
      assert "should return correct inverse-value for a1" (B.name_eq (BAInj.inv_get inv_ff a1, b1));
      assert "should return correct inverse-value for a2" (B.name_eq (BAInj.inv_get inv_ff a2, b2));
    ())) ()
  
  val comp_ff = AAInj.compose (inv_ff, ff)
  
  val _ = test "The composite of an ffection with its inverse" (fn () => (
      assert "should have size 2" (AAInj.size comp_ff = 2);
      assert "should be identity on a1" (A.name_eq (AAInj.get comp_ff a1, a1));
      assert "should be identity on a2" (A.name_eq (AAInj.get comp_ff a2, a2));
      assert "should be identity on inverse of a1" (A.name_eq (AAInj.inv_get comp_ff a1, a1));
      assert "should be identity on inverse of a2" (A.name_eq (AAInj.inv_get comp_ff a2, a2));
    ())) ()*)
  
in
  val _ = assert_no_failed_tests ()
end