val SOME (n,md,dcx) = (System.use "exdata.ML"; NONE) handle Data.bug_exp x => SOME x;
open DB_Data;

    PolyML.exception_trace (fn () =>          case md of MaybeAnds etab => 
                     ((SubTySet.empty, dcx2), Ands etab)
                   | MaybeOrs ctab => 
                     (CnTab.fold 
                        (fn (c,n2) => fn (subs, dcx') => 
                         if (dname_eq n n2) then (subs, dcx')
                         else
                           (subs |> SubTySet.union1 (SubTyPos (n,[c]))
                                 |> SubTySet.union_set 
                                 (inc1_subpos_set c (get_subs dcx' n2)), 
                            dcx' 
                              |> add_parent_and_supers_in_dcx
                              (n, inc1_suppos_set c supers2) n2
                        ))
                        ctab (SubTySet.empty, dcx2),
                      Ors ctab));