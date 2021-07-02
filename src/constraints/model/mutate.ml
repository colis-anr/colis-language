open Common
open Process_atom

let get_reachable_from_v v = 
  let v_reach = ref VSet.empty in
  v_reach := VSet.add v (!v_reach);
  let rec get_reach (v)=
    let ll = FMap.bindings ((find_node v).feat) in
      let rec helper ll =
        match ll with
        |[] -> ()
        |(_,v2)::t ->v_reach := VSet.add v2 (!v_reach);
                      get_reach (v2);
                      helper t
      in helper ll
  in get_reach v;(!v_reach)
 
let mutate (clau:clause) (num:int) (rootb)= 
  let clau = ref clau in
  let v_reach = ref (get_reachable_from_v rootb) in
  let v_max_old = !v_max in
  let rec add_noise x safety = 
    match (x,safety) with
    |(x,safety) when (x > num) || (safety>(num*10)) -> (!clau)
    |(x,safety) -> let v1 = !v_min + Random.int (!v_max - !v_min) in
        if (not (VSet.mem v1 !v_reach))then add_noise x (safety+1)
        else if ((v1<=v_max_old)&&(((find_node v1).fen_p) || ((find_node v1).kind = Reg))) then add_noise x (safety+1)
        else if((Random.int 10) < 8) then
          (v_max := !v_max + 1;
          let f_new = "GenFto"^(string_of_int !v_max) in
          v_all := VSet.add !v_max !v_all;
          v_reach := VSet.add !v_max !v_reach;
          var_map := VarMap.add !v_max (empty_node !v_max) (!var_map);
          clau := Pos(Feat(v1,f_new,!v_max)) :: (!clau);
          add_feat_to_node (Feat(v1,f_new,!v_max));
          add_noise (x+1) (safety+1))
        else 
          (let f_new = "GenFAbs"^(string_of_int (Random.int !v_max)) in
          var_map := VarMap.add !v_max (empty_node !v_max) (!var_map);
          clau:= Pos(Abs(v1,f_new)) :: (!clau);
          add_abs_to_node (Abs(v1,f_new));
          add_noise (x+1) (safety+1))
  in add_noise 1 1
