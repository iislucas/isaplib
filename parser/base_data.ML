(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
(*  Foundational basic types for building on *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
structure BName : SSTR_NAME = SStrName;

signature BASE_DATA 
= sig
  (* one of these for each constructor of basetype *)
  val basetype_names : (string * BName.name) list
end;

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
(* Charcters are the basic data: foundations of a parser *)
structure DB_ParserBaseData 
= struct
  val basetype_names = 
       (ParserLib.base_char_ids 
          ~~ (map (BName.mk o Char.toString) ParserLib.base_chars));
end;

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
(*  Another idea for base data (currently unused):
    unit, int, string = as base data *)
structure DB_BasicBaseData 
= struct
  val unit_id = BName.mk "unit";
  val int_id = BName.mk "int";
  val string_id = BName.mk "string";

  val basetype_names = 
      [("unit", unit_id), ("int", int_id), ("string", string_id)];
end;

structure BasicBaseData : BASE_DATA = DB_BasicBaseData;
structure ParserBaseData : BASE_DATA = DB_ParserBaseData;
