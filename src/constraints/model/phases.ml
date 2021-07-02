open Common
open Process_atom
open Mutate
open Print

let is_feature_in_cwd (cwd) (f)= 
  let cwd_l = list_remove "" (String.split_on_char '/' cwd) in
  let rec helper = function
    |[] -> false
    |h::_ when (h=f) -> true
    |_::t -> helper t
  in
  helper cwd_l

let dissolve_all () =
  let var_map_l = VarMap.bindings !var_map in
  let rec helper var_map_l  = 
    match var_map_l with 
    |[] -> ()
    |(_,n_1)::t -> disolve_node n_1 ;
                     helper t
  in 
  helper var_map_l 

let rec create_empty_var_map clause  = 
  match clause with 
  |[] -> var_map := VarMap.add 0 (empty_node 0) (!var_map);   (*Var 0 is used to respresent absent mapping*)
        ()
  |Pos Feat(v1,f,v2)::t| Neg Feat(v1,f,v2)::t -> 
                       var_map := VarMap.add v1 (empty_node v1) (!var_map); 
                       var_map := VarMap.add v2 (empty_node v2) (!var_map); 
                       fBigSet := FSet.add f (!fBigSet);
                       create_empty_var_map t  

  |Pos Abs (v1,f)::t| Neg Abs (v1,f)::t -> 
                    var_map := VarMap.add v1 (empty_node v1) (!var_map);
                    fBigSet := FSet.add f (!fBigSet);
                    create_empty_var_map t 

  |Pos Eqf (v1,fl,v2)::t| Neg Eqf (v1,fl,v2)::t ->
                    var_map := VarMap.add v1 (empty_node v1) (!var_map);
                    var_map := VarMap.add v2 (empty_node v2) (!var_map);
                    fBigSet := FSet.union (FSet.of_list fl) (!fBigSet);
                    create_empty_var_map t 

  |Pos Eq(v1,v2)::t| Neg Eq(v1,v2)::t -> 
                    var_map := VarMap.add v1 (empty_node v1) (!var_map);
                    var_map := VarMap.add v2 (empty_node v2) (!var_map);
                    create_empty_var_map t 

  |Pos Sim (v1,fl,v2)::t| Neg Sim (v1,fl,v2)::t->
                    var_map := VarMap.add v1 (empty_node v1) (!var_map);
                    var_map := VarMap.add v2 (empty_node v2) (!var_map);
                    fBigSet := FSet.union (FSet.of_list fl) (!fBigSet);
                    create_empty_var_map t 

  |Pos Fen(v1,fl)::t| Neg Fen(v1,fl)::t -> 
                    var_map := VarMap.add v1 (empty_node v1) (!var_map);
                    fBigSet := FSet.union (FSet.of_list fl) (!fBigSet);
                    create_empty_var_map t 
  |Pos Kind(v1,_)::t| Neg Kind(v1,_)::t -> var_map := VarMap.add v1 (empty_node v1) (!var_map);
                    create_empty_var_map t  
  |Pos Maybe(v1,f,v2)::t| Neg Maybe(v1,f,v2)::t -> 
                       var_map := VarMap.add v1 (empty_node v1) (!var_map); 
                       var_map := VarMap.add v2 (empty_node v2) (!var_map); 
                       fBigSet := FSet.add f (!fBigSet);
                       create_empty_var_map t   

let rec clause_phase_I (clau:clause) =
    match clau with 
    |[] -> ()
    |Pos Eqf(v1,fl,v2)::t -> add_equal_to_node (Eqf(v1,fl,v2));
                         clause_phase_I t
    |Pos Sim(v1,fl,v2)::t -> add_sim_to_node (Sim(v1,fl,v2));
                         clause_phase_I t
    |Pos Fen(v1,fl)::t -> add_fen_to_node (Fen(v1,fl));
                      clause_phase_I t
    |Pos Kind(v1,k)::t -> add_kind_to_node (Kind(v1,k));
                      clause_phase_I t
    |Neg Kind(v1,k)::t -> let k  = if(k = Dir) then Reg else if(k= Reg) then Dir else Other in
                          add_kind_to_node (Kind(v1,k));  
                      clause_phase_I t
    | _ :: t -> clause_phase_I t

let rec clause_phase_II (clau:clause) =
    match clau with 
    |[] -> ()
    |Pos Eq(v1,v2)::t -> let new_node = node_union (find_node v1) (find_node v2) in
                     var_map := VarMap.add v1 new_node !var_map;
                     var_map := VarMap.add v2 new_node !var_map;
                     clause_phase_II t
    | _ :: t -> clause_phase_II t

let rec clause_phase_III (clau:clause) =
    match clau with 
    |[] -> ()
    |Pos Feat(v1,f,v2)::t -> add_feat_to_node (Feat(v1,f,v2));
                      clause_phase_III t
    |Pos Abs (v1,f)::t -> add_abs_to_node (Abs(v1,f));
                      clause_phase_III t
    |Pos Maybe (v1,f,v2)::t -> (if(((Random.int 10) < 7) || (is_feature_in_cwd cwd_s f)) then
                              add_feat_to_node (Feat(v1,f,v2))
                              else add_abs_to_node (Abs(v1,f)));
                      clause_phase_III t
    | _ :: t -> clause_phase_III t

let rec clause_phase_IV (clau:clause) =
    match clau with 
    |[] -> ()
    |Neg Feat(v1,f,v2)::t -> no_feat_abs_to_node (Feat(v1,f,v2));
                      clause_phase_IV t
    |Neg Abs (v1,f)::t -> no_feat_abs_to_node (Abs(v1,f));
                      clause_phase_IV t
    | _ :: t -> clause_phase_IV t

let rec clause_phase_V (clau:clause) =
    match clau with 
    |[] -> ()
    |Neg Fen(v1,fl)::t -> not_Fen_transform (Fen(v1,fl));
                      clause_phase_V t
    |Neg Eqf(v1,fl,v2)::t -> if(v1=v2) then failwith "Clash: x =/=F x" 
                             else not_eq_sim_transform (Eqf(v1,fl,v2));
                                 clause_phase_V t
    |Neg Eq(v1,v2)::t -> if(v1=v2) then failwith "Clash: x =/= x" 
                         else not_eq_sim_transform (Eqf(v1,FSet.elements !fBigSet,v2));
                              clause_phase_V t
    |Neg Sim(v1,fl,v2)::t -> if(v1=v2) then failwith "Clash: x ~/~F x" 
                             else not_eq_sim_transform (Sim(v1,fl,v2));
                             clause_phase_V t
    | _ :: t -> clause_phase_V t

let set_v_max_all () = 
  let rec helper vm_l = 
    match vm_l with
    |[]-> ()
    |(v,_)::t-> v_all := VSet.add v !v_all;
         v_max := if(!v_max < v)then v else !v_max;
         v_min := if((!v_min > v)&&(v<>0))then v else !v_min;
         helper t
  in 
  helper (VarMap.bindings !var_map)

let reintializ_ref roota rootb =
  var_map := VarMap.empty;
  fBigSet := FSet.empty;
  v_all := VSet.empty;
  v_max := 0; 
  v_min := max_int;
  print_collect := "";
  var_map := VarMap.add roota (empty_node roota) (!var_map);
  var_map := VarMap.add rootb (empty_node rootb) (!var_map) 

let engine (clau_1:clause) ?(m = false) ?(p = true) ?(m_v = 10) ?(rootb = 1) ?(roota = 1)() =
  reintializ_ref roota rootb;
  (*let clau_1 = (if (m) then (mutate clau_1 m_v rootb) else clau_1) in
  let _ = (if(m&&p)then (Format.printf "Mutant Clause :";print_clause clau_1) else ()) in*)
  
  create_empty_var_map clau_1; 
  clause_phase_I clau_1;
  set_v_max_all ();
  clause_phase_II clau_1;
  clause_phase_III clau_1;
  clause_phase_IV clau_1;
  clause_phase_V clau_1;
  dissolve_all ();
  let clau_1 = (if (m) then (mutate clau_1 m_v rootb) else clau_1) in
  let _ = (if(m&&p)then (Printf.fprintf out_f_l "Mutant Clause :";Format.printf "Mutant Clause :";print_clause clau_1) else ()) in
  dissolve_all ()
  (*var_map_display !var_map*)
  (*execute !mkdir*)




(*engine clau_1 ~m:true ();;*)