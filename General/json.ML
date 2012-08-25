signature JSON_DATA =
sig
  structure JObj : TABLE where type key = string
  datatype json = JS of string | JI of int | JF of real | JB of bool | JN
                | JARR of json list
                | JOBJ of json JObj.table
end

signature JSON =
sig
  structure Data : JSON_DATA
  type json = Data.json
  val parse : string -> json
  
  exception parse_exn of string
  exception notobj_exn of unit
  exception notfound_exn of string
  
  val empty : json
  val update : string * json -> json -> json
  val delete : string -> json -> json
  val lookup : json -> string -> json option
  val get : json -> string -> json
end

structure Json : JSON =
struct

structure Data =
struct
  structure JObj = Table(type key = string; val ord = fast_string_ord)
  datatype json = JS of string | JI of int | JF of real | JB of bool | JN
                | JARR of json list
                | JOBJ of json JObj.table
end

open Data
exception parse_exn of string
exception notobj_exn of unit
exception notfound_exn of string
val empty = JOBJ JObj.empty


(* table accessor functions *)


fun update (k, v) (JOBJ tab) = JOBJ (JObj.update (k,v) tab)
  | update _ _ = raise notobj_exn ()
fun delete k (JOBJ tab) = JOBJ (JObj.delete k tab)
  | delete _ _ = raise notobj_exn ()
fun lookup (JOBJ tab) k = (case JObj.lookup tab k of SOME v => SOME v | NONE => NONE)
  | lookup _ _ = raise notobj_exn ()
fun get obj k = case lookup obj k of SOME v => v | NONE => raise notfound_exn k


structure Parser =
struct
  datatype indicator = IND of char
  val NOOP = ()
  
  
  (*val scan_one = Scan.one*)
  
  (* standard scan repeaters *)
  val rep = Scan.repeat
  val rep1 = Scan.repeat1
  val opt = Scan.optional
  fun opt_noop s = Scan.optional s NOOP
  val unless = Scan.unless;
  fun eat scan = scan >> K NOOP
  
  (* other helpers *)
  (*val st_implode = implode (* flatten a list of strings to a string *)
  val ch_implode = String.implode (* convert a list of chars to a string *)*)
  fun pow _ 0 = 1
    | pow x 1 = x
    | pow x n = if n < 0 then raise parse_exn "negative exponent encountered" else
          (if (n mod 2 = 0) then pow (x*x) (n div 2)
                            else x * (pow (x*x) (n div 2)))
  
  
  (* Char.toString automatically inserts escape sequences, so we use implode *)
  val ch_to_st = String.implode o single
  fun st_to_ch st = case String.explode st of [c] => SOME c
                       | _ => NONE
  
  (* single-character production helpers *)
  fun ch x = Scan.one (fn c => c = x)
  
  fun ch_range (x,y) = Scan.one
    (fn c => (case st_to_ch c
                of SOME c' => the (st_to_ch x) <= c' andalso c' <= the (st_to_ch y)
                 | NONE => Scan.fail ()))
  
  fun cdec_to_int c =
    case st_to_ch c
      of SOME c' => (Char.ord c' - 48)
       | NONE => Scan.fail ()
  
  fun chex_to_int c = 
    case st_to_ch c
      of SOME c' => if #"a" < c' then Char.ord c' - 87
                    else if #"A" < c' then Char.ord c' - 55
                    else Char.ord c' - 48
      | NONE => Scan.fail ()
  
  fun dec_int digits = fold (fn c => fn i => cdec_to_int c + (10 * i)) digits 0
  fun hex_int digits = fold (fn c => fn i => chex_to_int c + (16 * i)) digits 0
  
  fun mk_float2 (num,frac) = Real.fromInt num + frac
  fun mk_float3 ((num,frac),exp) = 
    if exp < 0
    then mk_float2 (num,frac) / Real.fromInt (pow 10 (~exp))
    else mk_float2 (num,frac) * Real.fromInt (pow 10 exp) 
  
  fun hex_to_asc (((a,b),c),d) = ch_to_st (Char.chr (hex_int [a,b,c,d]))
    handle Chr => raise parse_exn "non-ascii characters not supported in strings (yet!)"
    
  
  val p_printable = ch "\t" || ch "\n" || ch "\r" || ch_range (" ", "~")
  
  (* safe characters for double-quoted string *)
  val p_safe = ch "\t" || ch "\n" || ch "\r" ||
               ch_range (" ", "!") || ch_range ("#", "[") ||
               ch_range ("]", "~")
  
  (* break characters *)
  val p_line_feed = ch "\n"
  val p_carriage_return = ch "\r" >> K "\n"
  val p_break = (p_carriage_return |-- p_line_feed) ||
                p_carriage_return ||
                p_line_feed
  
  (* whitespace characters *)
  val p_space = ch " "
  val p_tab = ch "\t"
  val p_white = p_break || p_space || p_tab
  
  (* misc characters *)
  val p_dec_digit = ch_range ("0", "9")
  val p_dec_nonzero = ch_range ("1", "9")
  val p_hex_digit = p_dec_digit || ch_range ("A", "F") || ch_range ("a", "f")
  
  fun s_wrap scanner = (rep p_white) |-- scanner --| (rep p_white)
  
  (* indicator character, wrapped in whitespace *)
  val ind = s_wrap o ch
  
  (* escape characters *)
  val p_esc_double_quote = ch "\""
  val p_esc_backslash = ch "\\"
  val p_esc_slash = ch "/"
  val p_esc_backspace = ch "b" >> K "\b"
  val p_esc_form_feed = ch "f" >> K "\f"
  val p_esc_line_feed = ch "n" >> K "\n"
  val p_esc_carriage_return = ch "r" >> K "\r"
  val p_esc_tab = ch "t" >> K "t"
  val p_esc_unicode = (p_hex_digit -- p_hex_digit -- p_hex_digit -- p_hex_digit)
                      >> hex_to_asc
  
  val p_esc_char = p_esc_double_quote || p_esc_backslash || p_esc_slash ||
                   p_esc_backspace || p_esc_form_feed || p_esc_line_feed ||
                   p_esc_carriage_return || p_esc_tab || p_esc_unicode
  
  
  
  
  (* VALUES *)
  
  (* double-quoted strings *)
  val p_double_quote_char = p_safe || (ch "\\" |-- p_esc_char)
  val p_string = (ch "\"" |-- rep p_double_quote_char --| ch "\"") >> implode
  
  (* numbers *)
  val p_exp_symb = (ch "e" || ch "E") |--
                    (opt (ch "+" >> K 1 || ch "-" >> K ~1) 1)
  val p_int = opt (ch "-" >> K ~1) 1 -- (
                 (p_dec_nonzero -- rep1 p_dec_digit) >> op:: ||
                 p_dec_digit >> single
               ) >> (fn (sgn,digits) => sgn * dec_int digits)
  val p_exp = p_exp_symb -- rep1 p_dec_digit
               >> (fn (sgn,digits) => sgn * dec_int digits)
  val p_frac = ch "." |-- rep1 p_dec_digit
                >> (fn digits =>
                      Real.fromInt (dec_int digits) /
                      Real.fromInt (pow 10 (length digits)))
  
  (* a number is interpreted as an integer, unless an exponent or decimal is present *)
  val p_number = (p_int -- opt p_frac 0.0 -- p_exp >> (JF o mk_float3)) ||
                  (p_int -- p_frac >> (JF o mk_float2)) ||
                  (p_int >> JI)
  
  val p_true  = (ch "t" |-- ch "r" |-- ch "u" |-- ch "e") >> K (JB true)
  val p_false = (ch "f" |-- ch "a" |-- ch "l" |-- ch "s" |-- ch "e") >> K (JB false)
  val p_null  = (ch "n" |-- ch "u" |-- ch "l" |-- ch "l") >> K JN
  
  fun p_value x = let
    val p_pair     = p_string --| ind ":" -- p_value
    val p_members  = p_pair ::: rep (ind "," |-- p_pair)
    val p_object   = (ind "{" |-- opt p_members [] --| ind "}")
                      >> (fn pairs => fold JObj.update pairs JObj.empty)
    val p_elements = p_value ::: rep (ind "," |-- p_value)
    val p_array    = (ind "[" |-- opt p_elements [] --| ind "]")
  in x |> (
    p_true || p_false || p_null || p_number || (p_string >> JS) ||
    (p_object >> JOBJ) || (p_array >> JARR)
  )
  end
end

fun parse s = case Parser.p_value (raw_explode s @ [Symbol.eof])
                of (json, [x]) => if x = Symbol.eof then json else
                                  raise parse_exn ("Expected: eof, got: " ^ x)
                 | (_, xs)     => raise parse_exn ("Expected: eof, got: " ^ implode xs)


(*structure Encoder =
struct
 exception notobj_exn of unit;
    
    (** escapes ", \ and \n **)
    fun escape s =
      let
        fun helper #"\"" = [#"\\", #"\""]
          | helper #"\\" = [#"\\", #"\\"]
          | helper #"\n" = [#"\\", #"n"]
          | helper x = [x]
      in
        String.implode
          (fold_rev (curry op@) (map helper (String.explode s)) [])
      end
    
    (*replaces ~ to - *)
    fun convert_minus s = String.implode
            (map (fn (#"~") => #"-" | x => x) (String.explode s))

    fun enc_name (name) = "\"" ^ (Name.string_of_name name) ^ "\":"
    and enc_value (String value) = "\"" ^ escape value ^ "\""
      | enc_value (Int value) = convert_minus (Int.toString value)
      | enc_value (Bool value) = Bool.toString value
      | enc_value (Real value) = Real.toString value
      | enc_value (List value) = enc_list value
      | enc_value (Null) = "null"
      | enc_value (obj) = (encode obj)
    and enc_list [] = "[]"
      | enc_list l =
        let
            val e = fold_rev (fn a => fn b => (enc_value a) ^ "," ^ b) l ""
        in
            "[" ^ String.substring(e, 0, (String.size e)-1) ^ "]"
        end
    and enc_1 (name, value) x = (enc_name name) ^ (enc_value value) ^ "," ^ x
    and encode (Object tab) =
        let
            val e = Tab.fold enc_1 tab "";
        in
            "{" ^ String.substring(e, 0, (String.size e)-1) ^ "}"
        end
end*)

end
