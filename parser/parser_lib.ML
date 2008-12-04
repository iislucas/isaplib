structure ParserLib = 
struct

(* stuff for initial table *)
fun chars_between startchar endchar = 
    (case Char.compare (startchar, endchar) of
       LESS => startchar :: chars_between (Char.succ startchar) endchar
     | _ => [endchar]);

(* little helper function to print id's nicely *)
fun id_of_str "" = "unit" (* empty string mapped to char 0 *)
  | id_of_str " " = "\" \""
  | id_of_str s = 
    let val s2 = String.toString s
    in if s2 = s then s else ("\"" ^ s2 ^ "\"") end;

val empty_str_id = id_of_str (Char.toString #"\^@"); (* empty string mapped to char 0 *)

(* all initial characters *)
val base_chars = [#"\t", #"\n"] @ (chars_between (#" ") (#"~"));
val base_char_strs = map Char.toString base_chars;
val base_char_ids = map id_of_str base_char_strs;

(* quoted characters *)
val quote_char = (#"\\");
val quoted_chars = [(#"\\"), (#"'")];
val unquoted_chars = base_chars \\ quoted_chars;

end;
