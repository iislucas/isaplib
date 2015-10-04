theory isaplib
imports Main
begin

(* names *)
ML_file "../../names/namer.ML" 
ML_file "../../names/namers.ML"  (* instances of namer, StrName, etc *)
ML_file "../../names/basic_nameset.ML" (* basic sets of names *)  
ML_file "../../names/basic_nametab.ML" (* name tables which provide fresh names *)
ML_file "../../names/basic_renaming.ML" (* renaming, based on tables and sets *)

(* generic Name structure; provies nametables, namesets and collections *)
ML_file "../../names/basic_name.ML"
ML_file "../../names/compound_renaming.ML" (* renaming within datatypes *)
ML_file "../../names/renaming.ML" (* renamings which can be renamed *)
(* as above, but with renaming *)
ML_file "../../names/nameset.ML" 
ML_file "../../names/nametab.ML" 

(* names + renaming for them, their tables, sets, and renamings *)
ML_file "../../names/names.ML"


(* Binary Relations of finite name sets: good for dependencies *)
ML_file "../../names/name_map.ML" (* functions/mappings on names *)
ML_file "../../names/name_inj.ML" (* name iso-morphisms *)
ML_file "../../names/name_injendo.ML" (* name auto-morphisms (name iso where dom = cod) *)
ML_file "../../names/name_binrel.ML" (* bin relations on names *)

ML_file "../../names/names_common.ML"

(* unif stuff *)
ML_file "../../unif/unif_data.ML"
ML_file "../../unif/umorph.ML"

(* graphs *)
ML_file "../../graph/pregraph.ML"
ML_file "../../graph/rgraph.ML"

(* search *)
ML_file "../../search/gsearch.ML"
ML_file "../../search/msearch.ML"
ML_file "../../search/lsearch.ML"

(* Other basic top level things *)
ML_file "../../basics/collection.ML"
ML_file "../../basics/polym_table.ML"
SML_file "../../basics/toplevel.ML"

end
