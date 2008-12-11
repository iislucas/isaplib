local 

structure IntStrBinxRel = BinRelFun(structure A = IntName and B = StrName)
structure BinRel = IntStrBinxRel

in val _ = let

val rel0 = BinRel.empty;
val rel = rel0 |> (BinRel.add_many (IntName.NSet.of_list [1,2,3]) 
                                   (StrName.NSet.of_list ["a","b","c"]));
val _ = BinRel.print rel;

val rel = rel |> (BinRel.add_toA 5 (StrName.NSet.of_list ["a","b"]));
val _ = BinRel.print rel;

val rel = rel |> (BinRel.renameA 5 4);
val _ = BinRel.print rel;

val rel = rel |> (BinRel.del_many (IntName.NSet.of_list [1,2,4]) 
                                  (StrName.NSet.of_list ["b","c"]));
val _ = BinRel.print rel;

val rel = rel |> (BinRel.del_many (IntName.NSet.of_list [1]) 
                                  (StrName.NSet.of_list ["a"]));
val _ = BinRel.print rel;


in 
  Pretty.writeln (Pretty.str "binrel.ML: tests passed.") (* big let *)
end (* let *)
    handle _ => raise ERROR "test/binrel.ML: test failed! horrible monsters!";
end
