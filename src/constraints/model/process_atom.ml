open Common

let fresh =
  let i = ref 0 in
  fun () -> incr i; !i  

let add_abs_to_node atom  = 
  match atom with
  | Abs (v1,f) -> let v1_node = find_node v1 in 
          let absent_var = 0 in 
          if ((FMap.mem f v1_node.feat)&&(FMap.find_opt f v1_node.feat <> Some 0)) then failwith "Clash: tring to create x[f]abs when x[f]y exists"    
          else
          let new_node = {v1_node with feat = FMap.add f absent_var v1_node.feat} in
             let rec helper vl =
                match vl with 
                |[]-> ()
                |v1::t -> var_map := (VarMap.add v1 new_node !var_map);
                    helper t
             in
             (helper (VSet.elements new_node.var_l))
          (*All mappings from f to 0 are considered absent*)
  |_ -> failwith "add_abs_to_node is only for Abs"

let add_feat_to_node atom = 
  match atom with
  | Feat(v1,f,v2) when v2 = 0 -> add_abs_to_node (Abs(v1,f))
  | Feat(v1,f,v2) -> 
             let v1_node = find_node v1 in
             if(v1_node.kind = Reg) then failwith ("Clash a Reg cant have feature mappings V:"^(string_of_int v1))
             else if (FMap.find_opt f v1_node.feat  = Some 0) then failwith ("Clash: tring to create %d[%s]%d when x[f]abs exists"^(string_of_int v1)^f^(string_of_int v2))
             else if (v1_node.fen_p && not(FSet.mem f v1_node.fen)) then failwith "Clash: tring to create x[f]y when x[F] exists and f does not belong to F"
             else 
             let new_node = {v1_node with feat = FMap.add f v2 v1_node.feat} in
             let rec helper  vl =
                match vl with 
                |[]-> () 
                |v1::t -> var_map := (VarMap.add v1 new_node (!var_map));
                    helper t
             in
             (helper (VSet.elements new_node.var_l))
  |_ -> failwith "add_feat_to_node is only for Feat"

let rec no_feat_abs_to_node atom =
  match atom with
  |Feat(v1,f,v2) -> let v1_node = find_node v1 in
                    if((FMap.find_opt  f v1_node.feat) = (Some v2))
                    then failwith "Clash x[f]y and -x[f]y both present"
                    else
                    let new_node = {v1_node with notfeat =  (f,v2)::v1_node.notfeat} in
                    let rec helper  vl =
                       match vl with 
                       |[]-> () 
                       |v1::t -> var_map := (VarMap.add v1 new_node (!var_map));
                           helper t
                    in
                    (helper (VSet.elements new_node.var_l))

  |Abs(v1,f) -> no_feat_abs_to_node (Feat(v1,f,0));
                if((FMap.find_opt  f (find_node v1).feat) = None) then
                    let v_new = ((VarMap.cardinal !var_map) + 3) in
                    var_map := VarMap.add v_new (empty_node v_new) !var_map;
                    add_feat_to_node (Feat (v1,f,v_new))  (*BUG : ADD A FEATURE to make sure not absent*)
                else ()
  |_ -> failwith "no_feat_abs_to_node is only for Feat"

let add_kind_to_node atom =
  match atom with
  | Kind(v1,k) -> let v1_node = find_node v1 in
                  let fen_n = if(k = Reg)then FSet.of_list ["RegEmpty62o"] else v1_node.fen in
                  var_map := (VarMap.add v1 {v1_node with kind = k; fen = fen_n} !var_map); 
                  ()
  |_ -> failwith "add_equal_to_node is only for kind"

let add_equal_to_node atom = 
  match atom with
  | Eqf (v1,fl,v2) -> let v1_node = find_node v1 in
                      let v2_node = find_node v2 in
            let fl_v = (FSet.of_list fl,v2) in        
            var_map := (VarMap.add v1 {v1_node with equality = fl_v :: v1_node.equality} !var_map);
            let fl_v = (FSet.of_list fl,v1) in  
            var_map := (VarMap.add v2 {v2_node with equality = fl_v :: v2_node.equality} !var_map);
            ()
  |_ -> failwith "add_equal_to_node is only for Eqf"

let add_sim_to_node atom = 
  match atom with
  | Sim (v1,fl,v2) -> let v1_node = find_node v1 in
                      let v2_node = find_node v2 in
            let fl_v = (FSet.of_list fl,v2) in        
            var_map := (VarMap.add v1 {v1_node with sim = fl_v :: v1_node.sim} !var_map);
            let fl_v = (FSet.of_list fl,v1) in  
            var_map := (VarMap.add v2 {v2_node with sim = fl_v :: v2_node.sim} !var_map);
            ()
  |_ -> failwith " add_sim_to_node is only for Sim"

let add_fen_to_node atom = 
  match atom with
  | Fen (v1,fl) -> let v1_node = find_node v1 in  
          let fl = FSet.of_list fl in
          let fen_new = (if(not v1_node.fen_p)then fl else (FSet.inter fl v1_node.fen)) in
          var_map := (VarMap.add v1 {v1_node with fen = fen_new; fen_p = true} !var_map);
          ()
  |_ -> failwith "add_fen_to_node is only for Fen"
(*
let z_eq_update vz v_l eq= 
  let vz_node = (find_node vz) in
  let vzeq = vz_node.equality in
  let rec helper ll vze =
    match ll with 
    |[]-> vze
    |(fl,v)::t -> if(List.mem v v_l) then
                  helper t (list_remove (fl,v) vze)
                  else helper t vze
  in 
  let n_eq =  (eq,List.hd v_l)::(helper vzeq vzeq) in
  var_map := (VarMap.add vz {vz_node with equality = n_eq} !var_map);
  ()

let z_sim_update vz v_l sim= 
  let vz_node = (find_node vz) in
  let vzeq = vz_node.sim in
  let rec helper ll vze =
    match ll with 
    |[]-> vze
    |(fl,v)::t -> if(List.mem v v_l) then
                  helper t (list_remove (fl,v) vze)
                  else helper t vze
  in 
  let n_sim =  (sim,List.hd v_l)::(helper vzeq vzeq) in
  var_map := (VarMap.add vz {vz_node with sim = n_sim} !var_map);
  ()
*)
let eq_union eq1 eq2 = 
  let n_eq = ref eq1 in
  let rec helper1 l1 = 
      match l1 with
      | [] -> !n_eq
      | (l,v)::t -> let rec helper2 l2 =
                        match l2 with
                        |[]-> n_eq := (l,v)::!n_eq;
                                      ()
                        |(l2,v2)::t2 -> if(v=v2) then
                                        n_eq :=((FSet.union l2 l),v)::(list_remove (l2,v2) !n_eq) 
                                        else helper2 t2  (*BUG IMPLEMENT  z_eq_update vz vx vy eq*)
                    in
                    helper2 eq1;
                    helper1 t
  in helper1 eq2

let sim_union sim1 sim2 = 
  let n_sim = ref sim1 in
  let rec helper1 l1 = 
      match l1 with
      | [] -> !n_sim
      | (l,v)::t -> let rec helper2 l2 =
                        match l2 with
                        |[]-> n_sim := (l,v)::!n_sim;()
                        |(l2,v2)::t2 -> if(v=v2) then
                                        n_sim :=((FSet.inter l2 l),v)::(list_remove (l2,v2) !n_sim)
                                        else helper2 t2 (*IMPLEMENT  z_sim_update vz vx vy eq*)
                    in
                    helper2 sim1;
                    helper1 t
  in helper1 sim2

let kind_union k1 k2 =
  match(k1,k2) with
  |(Unknown,Unknown)-> Unknown
  |(Unknown,k) -> k
  |(k,Unknown) -> k
  |(k,k3) when (k=k3) -> k
  | _ -> failwith "Kind miss match during union"

let fen_inter n1 n2 = 
  match(n1.fen_p,n2.fen_p)with
  |(true,true)-> FSet.inter n1.fen n2.fen
  |(true,false)-> n1.fen
  |(false,true)-> n2.fen
  |(false,false)-> n1.fen


let node_union (n1:node) (n2:node):node = (*Do a clash check maybe*)
  let nf_var_l = VSet.union n1.var_l n2.var_l in
  let nf_equality = eq_union n1.equality n2.equality in
  let nf_sim = sim_union n1.sim n2.sim in 
  let nf_fen = fen_inter n1 n2 in
  let nf_fen_p = n1.fen_p || n2.fen_p in
  let nf_notfeat = n1.notfeat@n2.notfeat in
  let nf_id = "" in (*union occurs before we set the ids*)
  let nf_kind = kind_union n1.kind n2.kind in

  let merge _ n1 n2 = if(n1=n2) then Some n1 (*n1 and n2 are either the same value or are eqivalent*)
            else Some n1 (*Add a clash for if n1 and n2 are not equivalent*)
  in  
  let nf_feat = FMap.union merge n1.feat n2.feat in

  ({var_l = nf_var_l;feat = nf_feat;notfeat = nf_notfeat; equality = nf_equality;sim = nf_sim;fen=nf_fen;fen_p = nf_fen_p; id = nf_id; kind = nf_kind})




let add_equal_to_node_ALL atom = 
  match atom with
  | Eq (v1,v2) -> let v1_node = find_node v1 in
          let v2_node = find_node v2 in
          let new_node = node_union v1_node v2_node in        
          var_map := VarMap.add v1 new_node !var_map;
          var_map := VarMap.add v2 new_node !var_map;
          ()

  |_ -> failwith "add_equal_to_node is only for Eq"




let invertRes (x:bool) (y:bool) = if(y)then (not x) else x
(*For equality f needs to exist in F and for sim no exist so use this*)
let contents z = 
   match z with
   |Some c -> c
   |None -> failwith "It was None"

let rec find_feat_link_opt (n:node) (f:feature) (vlist): var option=
  let var_l = VSet.elements n.var_l in
  let vlist = vlist@var_l in
  match (FMap.find_opt f n.feat) with
  | Some x -> add_feat_to_node (Feat((List.hd var_l),f,x)); 
              Some x 

  | None ->let rec helper1 l_l (flip:bool) : var option =
           match l_l with
           |[] -> None   
           |(l,v)::t when (not (List.mem v vlist)) -> 
                  let v_node = find_node v in
                  if (invertRes (FSet.mem f l) flip)then
                    begin
                      let x = (find_feat_link_opt v_node  f vlist) in 
                      if(x <> None)then
                        begin
                          let x = contents x in
                          add_feat_to_node (Feat((List.hd var_l),f,x)); (*Try Abs(v,f,x) if error *)
                          Some x 
                        end
                      else helper1 t flip
                    end
                  else helper1 t flip

            |_::t -> helper1 t flip
           in
         let x = helper1 n.equality false in (*Check indirectly trhough equality*)
         if(x = None) then (helper1 n.sim  true) (*Check indirectly trhough sim*)
         else x

(*Think about the clash condition where x =F y and x =G z and f belongs to both F,G but y[f] and g[f] are differnt*)

let disolve_node (n:node)= 
  let l = FSet.elements !fBigSet in
  let rec helper l =
    match l with
    |[] -> ()
    |f::t -> let _ = find_feat_link_opt n f [] in
             helper t
  in
  helper l 

let not_Fen_transform atom  =
  match atom with
  |Fen(v1,fl) ->  let v1_node = find_node v1 in
                  let all_f = FSet.elements (FSet.diff !fBigSet (FSet.of_list fl)) in 
                  let helper2 () = 
                        if(not v1_node.fen_p) then
                          begin
                            let (new_f:feature) = "GeneratedF"^ string_of_int (fresh ()) in
                            v_max :=  !v_max + 1; (*Make sure max has already been stored*)
                            var_map := VarMap.add !v_max (empty_node !v_max) !var_map;
                            add_feat_to_node (Feat (v1,new_f,!v_max));
                            fBigSet := FSet.add new_f !fBigSet;
                            ()
                          end
                        else failwith "not_Fen_transform: [Clash]It is not possible to satisfy it"
                  in
                  let rec helper l  =
                    match l with
                    |[] -> helper2 ()
                    |f::t -> let x = find_feat_link_opt v1_node f [] in
                             if(x = None) then helper t 
                             else ()
                  in
                  helper all_f
  |_ -> failwith "not_Fen_transform is only for Fen"

let is_allowed (atom):bool =
  match atom with 
  |Feat(v1,f,v2)-> not (List.mem (f,v2) (find_node v1).notfeat)
  |Abs(v1,f) ->  not (List.mem (f,0) (find_node v1).notfeat)
  |_ -> failwith "is_allowed: is only for Feat and Abs"


            
let not_eq_sim_transform  atom =
  let getValF  = match atom with 
               |Eqf(v1,fl,v2) -> (v1,fl,v2,false)
               |Sim(v1,fl,v2) -> let fl = FSet.elements(FSet.diff !fBigSet (FSet.of_list fl)) in
                                (v1,fl,v2,true)
               |_ -> failwith "node_not_eq_sim_transform is only for Eqf and Sim"
  in
  let (v1,all_f,v2,isSim) = getValF in
  let v1_node = find_node v1 in
  let v2_node = find_node v2 in
  let info = ref [] in
  let helper4 () = if((not v1_node.fen_p)&&(isSim))then  
                  begin
                    let (new_f:feature) = "GeneratedF"^ string_of_int (fresh ()) in
                    v_max :=  !v_max + 1;
                    var_map := VarMap.add !v_max (empty_node !v_max) (!var_map);
                    add_feat_to_node (Feat (v1,new_f,!v_max));
                    add_abs_to_node (Abs (v2,new_f));
                    fBigSet := FSet.add new_f !fBigSet;
                    ()
                  end
                else if((not v2_node.fen_p)&&(isSim)) then
                  begin
                    let (new_f:feature) = "GeneratedF"^ string_of_int (fresh ()) in
                    v_max :=  !v_max + 1;
                    var_map := VarMap.add !v_max (empty_node !v_max) (!var_map);
                    add_feat_to_node (Feat (v2,new_f,!v_max));
                    add_abs_to_node (Abs (v1,new_f));
                    fBigSet := FSet.add new_f !fBigSet;
                    ()
                  end
                else failwith "node_not_eq_transform : Could not find a way to satisfy it"
    in
  let rec helper3 info_s=
        match info_s with 
        |[] -> helper4 () (*For sim we can still add a new feature*)
        |(f1,None,None)::t -> if (((not v1_node.fen_p)||(FSet.mem f1 v1_node.fen))&&(is_allowed (Abs (v2,f1)) )) then 
                              (v_max :=  !v_max + 1;
                              var_map := VarMap.add !v_max (empty_node !v_max) (!var_map);
                              add_feat_to_node (Feat (v1,f1,!v_max));
                              add_abs_to_node (Abs (v2,f1));
                              ())
                           else if(((not v2_node.fen_p)||(FSet.mem f1 v2_node.fen))&&(is_allowed (Abs (v1,f1)) )) then 
                              (v_max :=  !v_max + 1;
                              var_map := VarMap.add !v_max (empty_node !v_max) (!var_map);
                              add_feat_to_node (Feat (v2,f1,!v_max));
                              add_abs_to_node (Abs (v1,f1));
                              ())
                           else helper3 t
        |_::t -> helper3 t
        in
  let rec helper2 info_s =
        match info_s with 
        |[] -> helper3 !info
        |(f1,Some _, None)::t ->  if(is_allowed (Abs (v2,f1))) then helper2 t
                                    else add_abs_to_node (Abs (v2,f1))
                                    
        |(f1,None, Some _)::t ->  if(is_allowed (Abs (v1,f1))) then helper2 t
                                    else add_abs_to_node (Abs (v1,f1)) 
                                    
        |_::t -> helper2 t

        in 
  let rec helper1 all_f  = 
      match all_f with 
      |[]-> helper2 !info
      |f::t -> let x = (find_feat_link_opt v1_node f [] ) in
               let y = (find_feat_link_opt v2_node f [] ) in
               info := (f,x,y)::!info;
               match !info with
               |[]-> failwith "Not a possible match" 
               |(_,Some v_1,Some v_2)::_ -> if(v_1=v_2) then helper1 t
                                            else ()
               |_::_ -> helper1 t  
      in
    helper1 all_f 
