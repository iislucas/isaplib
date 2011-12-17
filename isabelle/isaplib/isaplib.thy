theory isaplib
imports Main
uses
(* basic logging, testing and project tools. *)
"../../basics/log.ML" 
"../../project/project.ML" 
"../../project/testing.ML" 

(* names *)
"../../names/namer.ML" 
"../../names/namers.ML"  (* instances of namer, StrName, etc *)
"../../names/basic_nameset.ML" (* basic sets of names *)  
"../../names/basic_nametab.ML" (* name tables which provide fresh names *)
"../../names/basic_renaming.ML" (* renaming, based on tables and sets *)

(* generic Name structure; provies nametables, namesets and collections *)
"../../names/basic_name.ML"
"../../names/compound_renaming.ML" (* renaming within datatypes *)
"../../names/renaming.ML" (* renamings which can be renamed *)
(* as above, but with renaming *)
"../../names/nameset.ML" 
"../../names/nametab.ML" 

(* names + renaming for them, their tables, sets, and renamings *)
"../../names/names.ML"


(* Binary Relations of finite name sets: good for dependencies *)
"../../names/name_map.ML" (* functions/mappings on names *)
"../../names/name_inj.ML" (* name iso-morphisms *)
"../../names/name_injendo.ML" (* name auto-morphisms (name iso where dom = cod) *)
"../../names/name_binrel.ML" (* bin relations on names *)

"../../names/names_common.ML"

(* unif stuff *)
"../../unif/unif_data.ML"
"../../unif/umorph.ML"

(* graphs *)
"../../graph/pregraph.ML"
"../../graph/rgraph.ML"

(* Other basic top level things *)
"../../basics/collection.ML"
"../../basics/polym_table.ML"
"../../basics/toplevel.ML"

begin

end
