theory pure_isaplib_test
imports pure_isaplib
begin

ML_file "project/project.ML"
ML_file "project/testing.ML"
ML_file "graph/test/rgraph.ML"

ML_file "maps/test/test_maps_util.ML"
ML_file "maps/test/name_function-test.ML"
ML_file "maps/test/name_injection-test.ML"
ML_file "maps/test/name_relation-test.ML"
ML_file "maps/test/name_substitution-test.ML"
ML_file "maps/test/name_table-test.ML"

ML_file "names/test/name_brel.ML"
ML_file "names/test/nameset.ML"

end
