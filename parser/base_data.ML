(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
(*  Foundational basic types for building on *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
signature BASE_DATA 
= sig
  type basetype

  (* one of these for each constructor of basetype *)
  val basetype_names : (string * basetype) list

  val pretty : basetype -> Pretty.T
  val print : basetype -> unit
end;

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
(* Charcters are the basic data: foundations of a parser *)
structure ParserBaseData 
= struct
  type basetype = Char.char;
  val basetype_names = (ParserLib.base_char_ids ~~ ParserLib.base_chars);
  val pretty = Pretty.str o Char.toString;
  val print = Pretty.writeln o pretty;
end;

structure BaseData : BASE_DATA = ParserBaseData;

install_pp (make_pp ["BaseData", "basetype"] 
                    (Pretty.pprint o BaseData.pretty));


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
(*  Another idea for base data (currently unused):
    unit, int, string = as base data *)
structure BasicBaseData :> BASE_DATA
= struct

datatype basetype = UnitTy
                  | IntTy
                  | StrTy;

val basetype_names = [("unit", UnitTy), ("int", IntTy), ("string", StrTy)];

fun pretty UnitTy = Pretty.str "unit"
  | pretty IntTy = Pretty.str "int"
  | pretty StrTy = Pretty.str "string";

val print = Pretty.writeln o pretty;
end;

