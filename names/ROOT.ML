(* Generic Tools for namers, fresh names tables, and collections *)
(* for creating fresh names, has name suc and pred operation, 
   also nameset with ability to make fresh names. *)
(* PolyML.Project.make "dbg.ML"; *) (* function to call for debug tool *)
PolyML.Project.make "namer.ML"; 
PolyML.Project.make "namers.ML"; (* instances of namer, StrName, etc *)

PolyML.Project.make "basic_nameset.ML"; (* basic sets of names *)  
PolyML.Project.make "basic_nametab.ML"; (* name tables which provide fresh names *)
PolyML.Project.make "basic_renaming.ML"; (* renaming, based on tables and sets *)

(* generic Name structure; provies nametables, namesets and collections *)
PolyML.Project.make "basic_names.ML";
PolyML.Project.make "compound_renaming.ML"; (* renaming within datatypes *)
PolyML.Project.make "renaming.ML"; (* renamings which can be renamed *)

(* as above, but with renaming *)
PolyML.Project.make "nameset.ML"; 
PolyML.Project.make "nametab.ML"; 

(* names + renaming for them, their tables, sets, and renamings *)
PolyML.Project.make "names.ML"; 

(* Binary Relations of finite name sets: good for dependencies *)
PolyML.Project.make "name_iso.ML"; (* name iso-morphisms *)
PolyML.Project.make "name_binrel.ML"; (* bin relations on names *)
PolyML.Project.make "name_map.ML"; (* functions/mappings on names *)
PolyML.Project.make "name_amorph.ML"; (* name auto-morphisms (name iso where dom = cod) *)

(* testing *)
cd "test";
PolyML.Project.make "ROOT.ML";
cd "..";
