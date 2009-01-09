(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  Title:      gproof/namers.ML
    Author:     Lucas Dixon, University of Edinburgh
                lucas.dixon@ed.ac.uk
                Initially written Oct 2005, Updated 10 Mar 2006        *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  DESCRIPTION:
    Basic kinds of names and their lifting into FreshNamers.

    Requirements: 
    order must be transitive, and monotomic w.r.t. succ
    pred (succ x) = x
    
    A useful problems to think of: 
    if "za" = succ("z") and "c7" < "z" then it must be that "c7" < "za"
    standard string-orders (length then alpha, and basic alpha) fail 
    for this notion of succ.

*)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)

(* string names that go up by changing ...zzz => ...aaaa, (the last n
"z"s to n +1 "a"s) and otherwise incrementing the last letter if its
alpha. If not alpha, we add an "a" to the end of it. *)

structure StrNamer 
: NAMER 
= struct

exception exp of string;

type name = string;
val pretty = Pretty.str;

fun is_alpha c = (c >= #"a" andalso c <= #"z");

(* We count the final letters as these are the digits that change
when we reach the final letter(s). eg. zzz => aaaa *)
fun count_z nzs (#"z"::t) = count_z (nzs + 1) t
  | count_z nzs l = (nzs,l);
fun count_a nas (#"a"::t) = count_a (nas + 1) t 
  | count_a nas l = (nas,l);
(* Note: only applied to alpha chars *)

(* counting for digits, currently unused: TODO: add numerical endings? *)
fun count_9 nzs (#"9"::t) = count_9 (nzs + 1) t
  | count_9 nzs l = (nzs,l);
fun count_0 nas (#"0"::t) = count_0 (nas + 1) t 
  | count_0 nas l = (nas,l);

(* adds an "a" to the end of a string if no alpha ending part, if
we get a bunch of "z"'s, then we replace them by the number + 1 of
"a"'s. Else we just inc the last char's value *)
fun alpha_succ l =
    let val (n, rest) = count_z 0 (rev l)
    in
      rev (case rest 
            of [] => List.tabulate (n + 1, K (#"a"))
             | (h::t) => (List.tabulate (n, K (#"a"))) 
                                 @ ((Char.succ h) :: t))
    end;

(* if l ends with "a"'s we replace it by one less "z"'s, else dec last
char value. If no chars, do nothing - we are allowed pred to not give
a new value *)
fun alpha_pred l =
    let val (n, rest) = count_a 0 (rev l)
    in
      rev (case rest of [] => (if n > 1 then List.tabulate (n - 1, K (#"z")) 
                                  else [])
                         | (h :: t) => (List.tabulate (n, K (#"z")))
                                       @ ((Char.pred h) :: t))
    end;

(* accumulate the alpha chars at the start of a given string: we use
this on a string that has been reversed so that we get all the alpha
chars at the end of the string. *)
fun alpha_revstart a [] = (a,[]) (* all alpha num *)
  | alpha_revstart a (l as (h :: t)) = 
    if is_alpha h then alpha_revstart (h::a) t
    else (a, l);
(* apply a function to the alpha-end part of a string *)
fun appto_alpha_ending f cl =
    let val (ending,revrest) = alpha_revstart [] (rev cl)
    in (rev revrest) @ (f ending) end;

val suc = String.implode 
          o (appto_alpha_ending alpha_succ) 
          o String.explode;

fun pred "" = NONE 
	| pred s = SOME ((String.implode 
										o (appto_alpha_ending alpha_pred) 
										o String.explode) s);

(* break a string into it's alpha ending and whatever before it *)
fun nonalpha_alpha_pair s = 
    let val (alphaending, revstart) = 
            alpha_revstart [] (rev (String.explode s))
    in (String.implode (rev revstart), String.implode alphaending) end;


val string_ord = String.compare;
fun len_based_string_ord (s1, s2) =
  (case Library.int_ord (size s1, size s2) 
    of EQUAL => string_ord (s1, s2) | ord => ord);

(* compares a string based on the alpha ending, but first use standard
fast_string check on non-alpha start. *)
fun ord (a,b) = 
    let val (a1,a2) = nonalpha_alpha_pair a
      val (b1,b2) = nonalpha_alpha_pair b
    in 
      (* Use length based ord for postfix part which must respect Suc
         operations, and ordinary left-to-right comparison for the
         string prefix so that prefixes look alphabetical.
      *)
      case string_ord (a1, b1)
       of EQUAL => len_based_string_ord (a2,b2)
        | x => x
    end;

fun eq n1n2 = (case String.compare n1n2 of EQUAL => true | _ => false)

end; (* structure *)





(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
structure StrIntNamer : NAMER
  = struct
  type name = string * int 
  fun suc (x,i) = (x, i + 1);
  fun pred (x,0) = NONE
    | pred (x,i) = SOME (x,i - 1);
  val ord = Library.prod_ord Library.fast_string_ord Library.int_ord;
  fun pretty (n,i) = 
      Pretty.block 
        ([Pretty.str n]
         @ (if i <> 0 then [Pretty.str (Int.toString i)] else []));
  fun eq n1n2 = (case ord n1n2 of EQUAL => true | _ => false);
end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
structure IntNamer : NAMER
  = struct
  type name = int 
  fun suc x = x + 1;
  fun pred 0 = NONE
		| pred x = SOME (x - 1);
  val ord = Library.int_ord;
  fun pretty x = Pretty.str (Int.toString x);
  fun eq n1n2 = (case ord n1n2 of EQUAL => true | _ => false);
end;
