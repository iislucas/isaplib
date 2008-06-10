
(* consider symstate of id i in parser state st *)
fun consider i st = 
    let 
      val s = get_symstate st i
      val lnds = SymState.get_lnds s
      val rnds = SymState.get_rnds s
      val st' = remove_from_open i st;
    in
      case s of
        SymState.Top _ =>
        if parsed_everything st s then 
          Final ((i,s),st')
        else (* note: do not delete if we make new top states *)
          Progress (st' |> consider_parents_of_top (i,s))
      | SymState.Left e => 
        Progress (st' |> ITab.N.fold (consider_right_of_leftedge (i,s)) rnds
                      |> (AdjNds.foldall
                            (link_left_of_dropped_to_newrights i rnds)
                            (SymState.get_lnds s))
                      |> remove_symstate (i,s) (* remove i (now merged) *)
                 )
      | SymState.Right _ =>
        Progress (st' |> ITab.N.fold (consider_left_of_rightedge (i,s)) lnds
                      |> (AdjNds.foldall
                            (link_right_of_dropped_to_newlefts i lnds)
                            (SymState.get_rnds s))
                      |> remove_symstate (i,s) (* remove i (now merged) *)
                 )
      | SymState.Middle _ =>
        let val (new_left_nodes, st') = 
                (st',ITab.N.empty) 
                  |> (ITab.N.fold (consider_left_of_middle (i,s)) lnds)
                  |> 
            val (new_right_nodes, st') = 
                (st',ITab.N.empty) 
                  |> (ITab.N.fold (consider_left_of_middle (i,s)) lnds)
        in
          Progress 
            (st' |> |> (AdjNds.foldall
                            (link_right_of_dropped_to_newlefts i lnds)
                            (SymState.get_rnds s))
                 |> remove_symstate (i,s) (* remove i (now merged) *)
          )
    end;

fun prune_ands_given_end_left dcx sym =
  SymState.update_pandpset 
  (Data.AndSet.filter 
    (fn andpos => 
      let val f = Data.field_of_andpos andpos
          val p = Data.parentn_of_andpos andpos
          val p_dtyp = Data.get_datatyp dcx p
      in case p_dtyp of 
           Data.Ands etab => 
           (case EnTab.N.next_smaller (EnTab.get_names etab) f
             of NONE => true (* we are left-most pos *)
              | SOME _ => false (* more things to the left*) )
         | _ => raise bad_state_exp "and-parent was not an and-datatype."
      end)) sym;


(* take everything left of i point to i2  *)
(* implicit: droppedi newrights oldlefti st => st' *)
val link_left_of_dropped_to_newright  = 
    update_sym o SymState.update_lnds oo AdjNds.replace1_for_many;
(* implicit: droppedi newlefts oldrighti st => st' *)
val link_right_of_dropped_to_newleft  = 
    update_sym o SymState.update_rnds oo AdjNds.replace1_for_many;

fun merge_adj_states_dropping_left (i,s) (i2,s2) st =
    AdjNds.foldall 
      (update_sym (SymState.update_lnds (AdjNds.replace1_for_many i ))
      (SymState.get_lnds s);

fun merge_adj_states_dropping_right (i,s) (i2,s2) st =
    AdjNds.foldall 
    (update_sym (SymState.update_lnds (AdjNds.replace1_for_many i ))
                (SymState.get_lnds s);



(* assume "(i,s)" is a right-edge, "lid" is the id of node to it's
left *)
fun consider_left_of_rightedge (i,s) lid st = 
    let val s2 = get_symstate st lid in
      case s2 of 
        SymState.Top _ => st
      | SymState.Right _ => st
      | SymState.Middle midstatus2 => 
        if (UnderData.dname_eq (get_parent s) (get_parent s2)) then 
          if UnderData.ename_eq 
               (SymState.get_max_left_elem s) 
               (SymState.nextrelem_of_midstatus midstatus2) then 
            
            merge_states (i,s) (lid,s2) st  (* !!! dir of merge? *)
          else drop_adj_link (lid,s2) (i,s) st 
        else st (* wait until s0 or s2 or other gets to top *)          
      | SymState.Left _ => 
       if (UnderData.dname_eq (get_parent s) (get_parent s2)) then 
         if UnderData.ename_eq 
              (SymState.get_max_right_elem s) 
              (SymState.next_elem_of_endstatus endstatus2) then 
           merge_states_dropping_left (i,s) (lid,s2) st  (* !!! dir of merge? *)
         else drop_adj_link (lid,s2) (i,s) st 
       else st (* wait until s0 or s2 or other gets to top *)
    end;


fun consider_right_of_leftedge (i,s) rid st = 
    let val s2 = get_symstate st lid in
      case s2 of 
        SymState.Top _ => st
      | SymState.Left _ => st
      | SymState.Middle midstatus2 => 
        if (UnderData.dname_eq (get_parent s) (get_parent s2)) then 
          if UnderData.ename_eq 
               (SymState.get_max_right_elem s) 
               (SymState.nextlelem_of_midstatus midstatus2) then 
            merge_states (i,s) (rid,s2) st(* !!! dir of merge? *)
          else drop_adj_link (i,s) (rid,s2) st 
        else st (* wait until s0 or s2 or other gets to top *)          
      | SymState.Right _ => 
       if (UnderData.dname_eq (get_parent s) (get_parent s2)) then 
         if UnderData.ename_eq 
              (SymState.get_max_left_elem s) 
              (SymState.next_elem_of_endstatus endstatus2) then 
           merge_states (i,s) (rid,s2) st  (* !!! dir of merge? *)
         else drop_adj_link (i,s) (rid,s2) st 
       else st (* wait until s0 or s2 or other gets to top *)
    end;


fun consider_left_of_middle (i,s) st lid = 
fun consider_right_of_middle (i,s) st rid = 


(* 
fun prune_ands_given_waiting_left lsym sym =
  let 
    val left_joins = 
  
fun refine_left st sym = 
  (case (SymState.get_left sym) 
    of SymState.End => 
       ParserState.set_sym
         (prune_ands_given_end_left sym)
     | SymState.Waiting lid => 
       let val lsym = ParserState.get_symstate st lid
       in 
         prune_ands_given_waiting_left lsym sym
       end)

fun refine id st = 
  let 
    val _ = if not (ParserState.is_open st id) then 
      raise state_not_open_exp "refine" else ();
    val st = ParserState.get_symstate st id
  in
    
  end;
*)


(* given a state as first argument and another symstate to it's right, think
   about what to do, (updating the snd argument). *)
(* raise an exception? should we ever be doing this? *)
fun consider_leftof (lid, (s as SymState.RightEdge _)) (rid,s2) st = 
    check_right_after_left lid rid st 
    (* add or-list of possible and-locations of s *)
  | consider_leftof (s as SymState.Top ) s2 st = 
    (* implies: check left top before right *)
    check_right_after_left lid rid st 
    (* the posible merge case: s is LeftEdge or Middle *)
  | consider_leftof s s2 st = 
    (case s2 of 
       SymState.LeftEdge _ =>
       check_left_after_right lid rid st (* not now: wait until s2 is top *)
     | SymState.RightEdge endstatus2 => 
       (* check the same parent *)
       if (UnderData.dname_eq (get_parent s) (get_parent s2)) then 
         let val s2leftel = 
         in (* check element-wise adjacent *)
           if UnderData.ename_eq 
                (SymState.get_max_right_elem s) 
                (SymState.next_elem_of_endstatus endstatus2) then 
                Merged (merge_states s s2) (* merge! *)
           else 
             not_compatible lid rid st
         end
       else  (* wait until s0 or s2 or other gets to top *)
         check_after_either_moveup lid rid st
     | SymState.Top _ => 
       check_left_after_right lid rid st
     | (SymState.Middle midstatus2) => 
       (* check the same parent *)
       if (UnderData.dname_eq (get_parent s) (get_parent s2)) then 
         let val s2leftel = 
         in (* check element-wise adjacent *)
           if UnderData.ename_eq 
                (SymState.get_max_right_elem s) 
                (SymState.nextlelem_of_midstatus midstatus2) then 
                Merged (merge_states s s2) (* merge! *)
           else 
             not_compatible lid rid st
         end
       else  (* wait until s0 or s2 or other gets to top *)
         check_after_either_moveup lid rid st
     );


fun mk_nextnds  = 
    let 
      val next_elem = SymState.nextelem_of_endstatus endstatus
      val 
    
| RightEdge of endstatus (* on right of a dname *)
| Middle of midstatus
| Top of intype;
*)
