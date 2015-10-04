theory isaplib
imports Main
begin

(* basic logging, testing and project tools. *)
SML_file "../../basics/log.ML" 
SML_file "../../project/project.ML" 
SML_file "../../project/testing.ML" 

(* names *)
SML_file "../../names/namer.ML" 
SML_file "../../names/namers.ML"  (* instances of namer, StrName, etc *)
SML_file "../../names/basic_nameset.ML" (* basic sets of names *)  
SML_file "../../names/basic_nametab.ML" (* name tables which provide fresh names *)
SML_file "../../names/basic_renaming.ML" (* renaming, based on tables and sets *)

(* generic Name structure; provies nametables, namesets and collections *)
SML_file "../../names/basic_name.ML"
SML_file "../../names/compound_renaming.ML" (* renaming within datatypes *)
SML_file "../../names/renaming.ML" (* renamings which can be renamed *)
(* as above, but with renaming *)
SML_file "../../names/nameset.ML" 
SML_file "../../names/nametab.ML" 

(* names + renaming for them, their tables, sets, and renamings *)
SML_file "../../names/names.ML"


(* Binary Relations of finite name sets: good for dependencies *)
SML_file "../../names/name_map.ML" (* functions/mappings on names *)
SML_file "../../names/name_inj.ML" (* name iso-morphisms *)
SML_file "../../names/name_injendo.ML" (* name auto-morphisms (name iso where dom = cod) *)
SML_file "../../names/name_binrel.ML" (* bin relations on names *)

SML_file "../../names/names_common.ML"

(* unif stuff *)
SML_file "../../unif/unif_data.ML"
SML_file "../../unif/umorph.ML"

(* graphs *)
SML_file "../../graph/pregraph.ML"
SML_file "../../graph/rgraph.ML"

(* search *)
SML_file "../../search/gsearch.ML"
SML_file "../../search/msearch.ML"
SML_file "../../search/lsearch.ML"

SML_file "../../General/source.ML"
SML_file "../../General/json.ML"

(* Other basic top level things *)
SML_file "../../basics/collection.ML"
SML_file "../../basics/polym_table.ML"
SML_file "../../basics/toplevel.ML"

end
