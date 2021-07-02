open Common

let get_id_feat_str path =
 (let tmp_file = Filename.temp_file "" ".txt" in
 let _ = Sys.command @@ "ls -i "^path^" >" ^ tmp_file in
 let chan = open_in tmp_file in
 let s = really_input_string chan (in_channel_length chan) in
 close_in chan;
 s)

let get_id_map path =
    let s = get_id_feat_str path in
    let s = String.split_on_char '\n' s in
    let map = ref FMap.empty in
    let rec helper = function
      |[]-> !map
      |h::t when h <> ""-> let ll = String.split_on_char ' ' h in
               map := FMap.add (List.nth ll 1) (List.nth ll 0) !map;
               helper t
      |_::t -> helper t
    in helper s

let add_id_node v id_i = 
    let v_node = find_node v in
    let new_node = {v_node with id = id_i} in
    let rec helper vl =
       match vl with 
       |[]-> ()
       |v1::t -> var_map := (VarMap.add v1 new_node !var_map);
           helper t
    in
    (helper (VSet.elements new_node.var_l))

let set_same_id v1 v2 s=
   let v1_id = (find_node v1).id in
   let v2_id = (find_node v2).id in
   (match (v1_id,v2_id) with 
   |("","")-> ()
   |("",id2)-> add_id_node v1 id2
   |(id1,"")-> add_id_node v2 id1
   |(id1,id2) when id1 <> id2-> 
              Printf.fprintf out_f_l "%s" ("\nDifferent ID of V"^(string_of_int v1)^" and V"^(string_of_int v2)^" on "^s);
              Format.printf "%s" ("\nDifferent ID of V"^(string_of_int v1)^" and V"^(string_of_int v2)^" on "^s);()
   | _ -> ())
 
let rec dissolve_id_sim (clau:clause) =
    match clau with 
    |[] -> ()
    |Pos Sim(v1,_,v2)::t -> set_same_id v1 v2 "SIM";
                            dissolve_id_sim t
    | _::t -> dissolve_id_sim t

let rec dissolve_id_eqf (clau:clause) =
    match clau with 
    |[] -> ()
    |Pos Eqf(v1,_,v2)::t -> set_same_id v1 v2 "EQF";
                            dissolve_id_sim t
    | _::t -> dissolve_id_eqf t

let rec set_id (v) (path)=
  let ll = FMap.bindings ((find_node v).feat) in
  if((ll=[])||(v=0)) then ()
  else
    ( let id_map = get_id_map path in
      let rec helper ll =
      match ll with
      |[] -> ()
      |(_,v2)::t when v2 = 0 -> helper t
      |(f2,v2)::t -> try(
                     add_id_node v2 (FMap.find f2 id_map);
                     set_id (v2) (path^"/"^f2) ;
                     helper t)
                    with Not_found-> Printf.fprintf out_f_l "Not_Found during check_id(%d,%s,%d)\n" v f2 v2;
                    Format.printf "Not_Found during check_id(%d,%s,%d)\n" v f2 v2
    in helper ll)

let rec check_id (v) (path)=
  let ll = FMap.bindings ((find_node v).feat) in
  if((ll=[])||(v=0)) then
      ()
  else
    ( let id_map = get_id_map path in
      let rec helper ll =
      match ll with
      |[] -> ()
      |(_,v2)::t when v2 = 0 -> helper t
      |(f2,v2)::t -> try(
                     let v2_id = (find_node v2).id in
                     if ((v2_id = (FMap.find f2 id_map))||(v2_id = "")) then
                     (check_id (v2) (path^"/"^f2) ;
                     helper t)
                     else 
                     ( Printf.fprintf out_f_l "%s" ("ID Mismatch f: "^f2^" , v1: "^(string_of_int v)^" , v2: "^(string_of_int v2)^ ", v2_id(stored): "^v2_id^", v2_id(FS): "^(FMap.find f2 id_map)^"\n");
                     Format.printf "%s" ("ID Mismatch f: "^f2^" , v1: "^(string_of_int v)^" , v2: "^(string_of_int v2)^ ", v2_id(stored): "^v2_id^", v2_id(FS): "^(FMap.find f2 id_map)^"\n");
                     check_id (v2) (path^"/"^f2);
                     helper t))
                    with Not_found-> Printf.fprintf out_f_l "Not_Found during check_id(%d,%s,%d)\n" v f2 v2;
                    Format.printf "Not_Found during check_id(%d,%s,%d)\n" v f2 v2
    in helper ll)
