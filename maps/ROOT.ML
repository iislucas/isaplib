
PolyML.Project.make "abstract_map.ML";
PolyML.Project.make "name_table.ML";
PolyML.Project.make "name_relation.ML";
PolyML.Project.make "name_function.ML";
PolyML.Project.make "name_injection.ML";

Testing.make_test "test/test_maps_util.ML";
Testing.make_test "test/name_table-test.ML";
Testing.make_test "test/name_injection-test.ML";
Testing.make_test "test/name_function-test.ML";
Testing.make_test "test/name_relation-test.ML";
