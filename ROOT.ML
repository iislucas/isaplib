(* using this file should be equivalent to "make run-isap" *)

(* Boot-stapping code, to make sure project is loaded *)
use "ML-Systems/polyml.ML";
use "basics/log.ML";
cd "project";
use "ROOT.ML";
cd "..";

(* For debugging Project turn on logging: 
   PolyML.Project.Log.level_ref := 5; 
*)
   PolyML.Project.Log.level_ref := 5; 
PolyML.Project.set_state_unchanged ();
PolyML.Project.depend_on_files 
  [(* stuff loaded by "ML-Systems/polyml.ML" *)
   "ML-Systems/polyml.ML",
   "ML-Systems/polyml_common.ML",
   "ML-Systems/multithreading_polyml.ML",
   "ML-Systems/unsynchronized.ML",
   "ML-Systems/compiler_polyml-5.3.ML",
   "General/exn.ML",
   "ML-Systems/multithreading.ML",
   "ML-Systems/time_limit.ML",
   "General/timing.ML",
   "ML-Systems/ml_pretty.ML",
   "ML-Systems/use_context.ML",

   (* isaplib bootsrapping *)
   "basics/log.ML",
   "project/ROOT.ML",
   "project/project.ML",

   (* this file *)
   "ROOT.ML"
  ];

PolyML.Project.use_root "General/ROOT.ML";
(* PolyML.Project.use_root "Concurrent/ROOT.ML"; *)
PolyML.Project.use_root "names/ROOT.ML";
PolyML.Project.use_root "graph/ROOT.ML";
(* PolyML.Project.use_root "parser/ROOT.ML"; *)

if PolyML.Project.heap_is_newer_than_edits "heaps/all.polyml-heap" then () 
else 
 (PolyML.Project.ensure_at_latest_heap ();
  PolyML.Project.save_project_child_state "heaps/all.polyml-heap");

