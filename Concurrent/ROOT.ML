(*  Title:      Pure/Concurrent/ROOT.ML
    Author:     Makarius

Concurrency within the ML runtime.
*)
PolyML.Project.make "simple_thread.ML";
PolyML.Project.make "synchronized.ML";
PolyML.Project.make "mailbox.ML";
PolyML.Project.make "task_queue.ML";
PolyML.Project.make "future.ML";
PolyML.Project.make "par_list.ML";
if Multithreading.available then () 
else PolyML.Project.make "par_list_dummy.ML";

