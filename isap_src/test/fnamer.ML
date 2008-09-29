local
structure StrFNamer = FNamerFun(StrNamer);
structure N = StrFNamer;

structure N2 = FNamerFun(StrIntNamer);

structure N3 = FNamerFun(IntNamer);

in 

val _ = 
    let 
(* 
 install_pp (make_pp ["N", "T"] (Pretty.pprint o N.pretty_struct));
*)

fun assert_correct m = StrFNamer.assert_correct m;

(* --- basic tests --- *)
val m = N.empty;
val m = m |> N.add "z" |> assert_correct;
val m = m |> N.add "aa" |> assert_correct;
val m = m |> N.add "ab" |> assert_correct;
val m = m |> N.add "za" |> assert_correct;
val m = m |> N.add "zb" |> assert_correct;
val m = m |> N.add "zc" |> assert_correct;
val m2 = m |> N.delete "zb" |> assert_correct;
val m2 = m |> N.delete "aa" |> assert_correct;
val m2 = m2 |> N.delete "ab" |> assert_correct;
val m2 = m2 |> N.delete "zb" |> assert_correct;
val m2 = m2 |> N.delete "zc" |> assert_correct;
val m2 = m2 |> N.delete "z" |> assert_correct;
val m2 = m2 |> N.delete "za" |> assert_correct;
val m2 = m2 |> N.delete "za" |> assert_correct;
val m = m |> N.add_range ("ac","f") |> assert_correct;
val m = m |> N.add_range ("a","c") |> assert_correct;
val m = m |> N.add_range ("bb","cc") |> assert_correct;
val m = m |> N.add_range ("dd","ee") |> assert_correct;
val m = m |> N.add_range ("ef","zb") |> assert_correct;


(* --- Test from parser code --- *)
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
    in if s2 = s then  s else ("\"" ^ s2 ^ "\"") end;

val empty_str_id = id_of_str (Char.toString #"\^@"); (* empty string mapped to char 0 *)

(* all initial characters *)
val base_chars = [#"\t", #"\n"] @ (chars_between (#" ") (#"~"));
val base_char_strs = map Char.toString base_chars;
val base_char_ids = map id_of_str base_char_strs;

(* quoted characters *)
val quote_char = (#"\\");
val quoted_chars = [(#"\\"), (#"'")];
val unquoted_chars = base_chars \\ quoted_chars;

val basetype_names = (base_char_ids ~~ base_chars);

val big_table = 
    fold 
      (N.assert_correct oo N.add)
      base_char_ids
      N.empty;


(* --- Bugs found and cleared up... lets not make them again... *)


val tr2 = 
    StrFNamer.empty 
      |> StrFNamer.add_range ("conjEm","conjEn")
      |> StrFNamer.add_range ("aw","bk")
      |> StrFNamer.add "exEh"
      |> StrFNamer.add_range ("g","j")
      |> StrFNamer.add "exEf"
      |> StrFNamer.add_range ("conjEj","conjEk")
      |> StrFNamer.add_range ("conjEg","conjEh")
      |> assert_correct;
val _ = StrFNamer.print_full tr2;

(* causes major rebalance... *)
val tr3 = tr2 |> StrFNamer.add "conjEl";



(* --- intersection test 1 --- *)
val m = N.empty;
val m = m |> N.add "a" |> assert_correct;
val m = m |> N.add "b" |> assert_correct;

val m2 = N.empty;
val m2 = m2 |> N.add "a" |> assert_correct;
val m2 = m2 |> N.add "c" |> assert_correct;

val _ = N.intersect m m2;
val _ = N.intersect m2 m;


(* --- intersection test 2 --- *)
val m = N2.empty;
val m = m |> N2.add ("C",15) |> N2.assert_correct;
val m = m |> N2.add ("C",13) |> N2.assert_correct;

val m2 = N2.empty;
val m2 = m2 |> N2.add ("C",14) |> N2.assert_correct;
val m2 = m2 |> N2.add ("C",15) |> N2.assert_correct;

val _ = N2.intersect m m2;
val _ = N2.intersect m2 m;


(* --- union_merge test 1 --- *)
val s = N3.empty
      |> N3.add_range (96,101)
      |> N3.add_range (116,118)
      |> N3.add 112
      |> N3.add 110;

val s2 = s |> N3.add_range (111,111) |> N3.assert_correct;

val s2 = N3.empty |> N3.add_range (111,111);

val (v as (b,t)) = (111,111);
open N3;
val (tr as Br (d,v2 as (b2,t2),l,r)) = s;


N3.union_merge s2 s |> N3.assert_correct;;

in Pretty.writeln (Pretty.str "FNameTab tests passed.") end (* big let *)
handle _ => raise ERROR "test/fnamer.ML: test failed! horrible monsters!";

end; (* local *)






(* debugging ....


val tr = 
    StrFNamer.empty 
      |> StrFNamer.add_range ("conjEm","conjEn")
      |> StrFNamer.add_range ("aw","bk")
      |> StrFNamer.add "exEh"
      |> StrFNamer.add_range ("g","j")
      |> StrFNamer.add "exEf"
      |> StrFNamer.add_range ("conjEj","conjEk")
      |> StrFNamer.add_range ("conjEg","conjEh")
      |> assert_correct;
val _ = StrFNamer.print_full tr;

open StrFNamer;

val s = "conjEl";
val (s as (sp,sx,ss)) = (pred_or_same s, s, N.suc s);

val tr3 = tr |> StrFNamer.add' s;

val tr2 = tr |> StrFNamer.add "conjEl";



val s= n;
val s = (N.pred_or_same s, s, N.N.suc s);
val (s as (sp,sx,ss)) = s;
val (tr as N.Br (d,v as (b,t),l,r)) = tr;
N.print_full tr;

val (tr as N.Br (d,v as (b,t),l,r)) = r;


N.print_full (N.add' s tr);


N.N.ord (sx,b);
N.N.ord(ss,b);
N.N.ord (sp,t);

N.print_full l;
N.print_full (N.add' s l);
val newl = N.add' s l;
N.print_full newl;

val (tr as N.Br(_,v, l as N.Br(ld,lv,ll,lr), r as N.Br(rd,rv,rl,rr))) =
rebalance(N.Br (N.br_depth newl r,(b,t),newl,r));

N.print_full (N.Br(rd + 1,v,lr,r));

N.add' s l;

N.print_full (N.rebalance (N.add' s r));
N.print_full ((N.add' s r));


N.N.ord (sx," ");
N.N.ord ("$","\" \"");

*)
