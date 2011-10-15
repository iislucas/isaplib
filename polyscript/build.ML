PolyML.use "../ROOT.ML"; 
PolyML.use "polyscript.ML"; 
PolyML.fullGC (); 
PolyML.shareCommonData main; 
SaveState.saveState "../heaps/polyscript.polyml-heap"; 
PolyML.export ("bin/polyscript", (fn () => do_and_exit_or_die main));
OS.Process.exit OS.Process.success;
