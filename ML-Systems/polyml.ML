(* File to load the appropriate PolyML Compatability files *)

if (PolyML.Compiler.compilerVersion = "5.3 Enhanced Reporting Testing") then
  use "ML-Systems/polyml-5.3.ML"
else if (PolyML.rtsVersion() >= 530) then 
  use "ML-Systems/polyml-5.3.ML"
else if (PolyML.rtsVersion() >= 520) then 
  use "ML-Systems/polyml-5.2.ML"
else 
  raise Fail ("PolyML 520 or later is needed, you have PolyML:" 
              ^ (Int.toString(PolyML.rtsVersion())));
