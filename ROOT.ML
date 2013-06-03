(* using this file should be equivalent to "make run-isap" *)

(* Boot-stapping code, to make sure project is loaded *)
use "ML-Systems/polyml.ML";
use "basics/log.ML";
use "project/project.ML";
use "project/testing.ML";

(* For debugging Project turn on logging:
   PolyML.Project.Log.level_ref := 5;
*)
PolyML.Project.set_state_unchanged ();
PolyML.Project.depend_on_files
  [(* stuff loaded by "ML-Systems/polyml.ML" *)
   "ML-Systems/polyml.ML",
   "ML-Systems/multithreading_polyml.ML",
   "ML-Systems/unsynchronized.ML",
   "General/exn.ML",
   "ML-Systems/multithreading.ML",
   "General/timing.ML",
   "ML-Systems/ml_pretty.ML",
   "ML-Systems/use_context.ML",

   (* isaplib bootsrapping: log and project *)
   "basics/log.ML",
   "project/project.ML",
   "project/testing.ML",

   (* this file *)
   "ROOT.ML"
  ];

(* from Isabelle/src/Pure/Generic *)
PolyML.Project.make "General/basics.ML";
(* from Isabelle/src/Pure *)
PolyML.Project.make "basics/library.ML";

(* Global version variable *)
val version = "Isabelle/IsaPlanner Library";

(* from Isabelle/src/Pure/Generic *)
PolyML.Project.make "General/print_mode.ML";
PolyML.Project.make "General/alist.ML";
PolyML.Project.make "General/table.ML";
PolyML.Project.make "General/properties.ML";

(* from Isabelle/src/Pure/Concurrent *)
PolyML.Project.make "Concurrent/simple_thread.ML";
PolyML.Project.make "Concurrent/synchronized.ML";

(* from Isabelle/src/Pure/Generic *)
PolyML.Project.make "General/output.ML";
PolyML.Project.make "General/timing.ML";
PolyML.Project.make "General/markup.ML";
PolyML.Project.make "General/scan.ML";
PolyML.Project.make "General/source.ML";
PolyML.Project.make "General/symbol.ML";
PolyML.Project.make "General/seq.ML";
PolyML.Project.make "General/position.ML";
PolyML.Project.make "General/symbol_pos.ML";
PolyML.Project.make "General/integer.ML";
PolyML.Project.make "General/stack.ML";
PolyML.Project.make "General/queue.ML";
PolyML.Project.make "General/heap.ML";
PolyML.Project.make "General/ord_list.ML";
PolyML.Project.make "General/balanced_tree.ML";
PolyML.Project.make "General/long_name.ML";
PolyML.Project.make "General/buffer.ML";
PolyML.Project.make "General/pretty.ML";
PolyML.Project.make "General/xml.ML";
PolyML.Project.make "General/graph.ML";
PolyML.Project.make "General/binding.ML";
PolyML.Project.make "General/path.ML";
PolyML.Project.make "General/url.ML";
PolyML.Project.make "General/file.ML";
PolyML.Project.make "General/yxml.ML";
PolyML.Project.make "General/json.ML";

(*MJB*)
Testing.make_test "General/test/json-test.ML";

(* from Isabelle/src/Pure/Concurrent; this is the main stuff loaded in
   Pure/ROOT.ML *)
PolyML.Project.make "ML/exn_properties_polyml.ML";
PolyML.Project.make "ML/ml_statistics_polyml-5.5.0.ML";
PolyML.Project.make "Concurrent/ROOT.ML";

(*MJB: simplification of the future calls.
It needs to be made here to use Synchronized and Future at the same time*)
PolyML.Project.make "General/stopwatch.ML";
PolyML.Project.make "Concurrent/future_wrapper.ML";
PolyML.Project.make "Concurrent/par_seq.ML";
PolyML.Project.make "Concurrent/compactor.ML";
PolyML.Project.make "Concurrent/par_tree.ML";

(* Other IsapLib Special stuff: names and graphs *)
PolyML.Project.use_root "names/ROOT.ML";
PolyML.Project.use_root "unif/ROOT.ML";
PolyML.Project.use_root "graph/ROOT.ML";
PolyML.Project.use_root "maps/ROOT.ML";

(* Other basic top level things *)
PolyML.Project.make "basics/collection.ML";
PolyML.Project.make "basics/polym_table.ML";


PolyML.Project.make "basics/toplevel.ML";


(* PolyML.Project.use_root "parser/ROOT.ML"; *)

(*if PolyML.Project.heap_is_newer_than_edits "heaps/all_child.heap"
then
  PolyML.Project.ensure_at_latest_heap ()
else
 (PolyML.Project.ensure_at_latest_heap ();
  PolyML.Project.save_project_child_state "heaps/all_child.heap");

if PolyML.Project.state_is_Changed ()
then TextIO.print "state changed\n"
else TextIO.print "state unchanged\n";*)

