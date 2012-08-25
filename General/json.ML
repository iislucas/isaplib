signature JSON =
sig
  exception parse_exn of string
  exception notobj_exn of unit
  exception notfound_exn of string
  
  structure JObj : TABLE where type key = string
  datatype json = String of string | Int of int | Real of real | Bool of bool | Null
                | Array of json list
                | Object of json JObj.table
  
  val empty : json
  val update : string * json -> json -> json
  val delete : string -> json -> json
  val lookup : json -> string -> json option
  val get : json -> string -> json
  
  val pretty : json -> Pretty.T
  val string_of : json -> string
  val of_string : string -> json
end

structure Json : JSON =
struct

structure JObj = Table(type key = string; val ord = fast_string_ord)
datatype json = String of string | Int of int | Real of real | Bool of bool | Null
              | Array of json list
              | Object of json JObj.table

exception parse_exn of string
exception notobj_exn of unit
exception notfound_exn of string
val empty = Object JObj.empty


(* table accessor functions *)


fun update (k, v) (Object tab) = Object (JObj.update (k,v) tab)
  | update _ _ = raise notobj_exn ()
fun delete k (Object tab) = Object (JObj.delete k tab)
  | delete _ _ = raise notobj_exn ()
fun lookup (Object tab) k = (case JObj.lookup tab k of SOME v => SOME v | NONE => NONE)
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
  val p_number = (p_int -- opt p_frac 0.0 -- p_exp >> (Real o mk_float3)) ||
                  (p_int -- p_frac >> (Real o mk_float2)) ||
                  (p_int >> Int)
  
  val p_true  = (ch "t" |-- ch "r" |-- ch "u" |-- ch "e") >> K (Bool true)
  val p_false = (ch "f" |-- ch "a" |-- ch "l" |-- ch "s" |-- ch "e") >> K (Bool false)
  val p_null  = (ch "n" |-- ch "u" |-- ch "l" |-- ch "l") >> K Null
  
  fun p_value x = let
    val p_pair     = p_string --| ind ":" -- p_value
    val p_members  = p_pair ::: rep (ind "," |-- p_pair)
    val p_object   = (ind "{" |-- opt p_members [] --| ind "}")
                      >> (fn pairs => fold JObj.update pairs JObj.empty)
    val p_elements = p_value ::: rep (ind "," |-- p_value)
    val p_array    = (ind "[" |-- opt p_elements [] --| ind "]")
  in x |> (
    p_true || p_false || p_null || p_number || (p_string >> String) ||
    (p_object >> Object) || (p_array >> Array)
  )
  end
  
  fun parse s = case p_value (raw_explode s @ [Symbol.eof])
                  of (json, [x]) => if x = Symbol.eof then json else
                                    raise parse_exn ("Expected: eof, got: " ^ x)
                   | (_, xs)     => raise parse_exn ("Expected: eof, got: " ^ implode xs)
end

    
(** escapes ", \ and \n **)
fun escape s = let
  fun trans #"\"" = "\\\""
    | trans #"\\" = "\\\\"
    | trans #"\n" = "\\n"
    | trans c = String.implode [c]
in String.translate trans s
end

(*replaces ~ to - *)
fun convert_num s = let
  fun trans #"~" = "-"
    | trans #"E" = "e"
    | trans c = String.implode [c]
in String.translate trans s
end

fun pretty_q_str x = Pretty.str ("\"" ^ escape x ^ "\"")
and pretty_kv (k,v) [] = [Pretty.block [pretty_q_str k, Pretty.str " : ", pretty v]]
  | pretty_kv (k,v) pr = Pretty.block [pretty_q_str k, Pretty.str " : ", pretty v, Pretty.str ","] :: pr
and pretty (String x) = pretty_q_str x
  | pretty (Int x) = Pretty.str (convert_num (Int.toString x))
  | pretty (Real x) = Pretty.str (convert_num (Real.toString x))
  | pretty (Bool x) = Pretty.str (Bool.toString x)
  | pretty (Null) = Pretty.str "null"
  | pretty (Array xs) = Pretty.list "[" "]" (map pretty xs)
  | pretty (Object obj) = Pretty.block_enclose (Pretty.str "{", Pretty.str "}")
                        (JObj.fold pretty_kv obj [])

val string_of = Pretty.string_of o pretty
val of_string = Parser.parse

end
