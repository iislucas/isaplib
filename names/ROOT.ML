(* Generic Tools for namers, fresh names tables, and collections *)
(* for creating fresh names, has name suc and pred operation, 
   also nameset with ability to make fresh names. *)
(* use "dbg.ML"; *) (* function to call for debug tool *)
use "namer.ML"; 
use "namers.ML"; (* instances of namer, StrName, etc *)

use "basic_nameset.ML"; (* basic sets of names *)  
use "basic_nametab.ML"; (* name tables which provide fresh names *)
use "renaming.ML"; (* renaming, based on tables and sets *)

(* generic Name structure; provies nametables, namesets and collections *)
use "basic_names.ML";
use "compound_renaming.ML"; (* renaming within datatypes *)

(* as above, but with renaming *)
use "nameset.ML"; 
use "nametab.ML"; 

(* names + renaming for them *)
use "names.ML"; 

(* Binary Relations of finite name sets: good for dependencies *)
use "name_iso.ML";(* name iso-morphisms *)
use "name_binrel.ML"; (* bin relations on names *)
use "name_map.ML"; (* functions/mappings on names *)
use "name_amorph.ML"; (* name auto-morphisms (name iso where dom = cod) *)

(* testing *)
cd "test";
use "ROOT.ML";
cd "..";
