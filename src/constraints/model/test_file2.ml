open Model_ref

let get_id_feat_str path =
 let tmp_file = Filename.temp_file "" ".txt" in
 let _ = Sys.command @@ "ls -i "^path^" >" ^ tmp_file in
 let chan = open_in tmp_file in
 let s = really_input_string chan (in_channel_length chan) in
 close_in chan;
 s

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

let rec set_id (v) (v_cycle) (path)=
  let ll = FMap.bindings ((find_node v).feat) in
  if(List.mem v v_cycle)then failwith "Cycle Clash"
  else if((ll=[])||(v=0)) then ()
  else
    ( let id_map = get_id_map path in
      let rec helper ll =
      match ll with
      |[] -> ()
      |(_,v2)::t when v2 = 0 -> helper t
      |(f2,v2)::t -> let v2_node = find_node v2 in
                     let new_node = {v2_node with id = (FMap.find f2 id_map)} in
                     let rec helper2 vl =
                        match vl with 
                        |[]-> ()
                        |v1::t -> var_map := (VarMap.add v1 new_node !var_map);
                            helper2 t
                     in
                     (helper2 (VSet.elements new_node.var_l));
                     set_id (v2) (v::v_cycle) (path^"/"^f2) ;
                     helper t
    in helper ll)

let rec check_id (v) (v_cycle) (path)=
  let ll = FMap.bindings ((find_node v).feat) in
  if(List.mem v v_cycle)then failwith "Cycle Clash"
  else if((ll=[])||(v=0)) then
      ()
  else
    ( let id_map = get_id_map path in
      let rec helper ll =
      match ll with
      |[] -> ()
      |(_,v2)::t when v2 = 0 -> helper t
      |(f2,v2)::t -> let v2_id = (find_node v2).id in
                     if ((v2_id = (FMap.find f2 id_map))||(v2_id = "")) then
                     (check_id (v2) (v::v_cycle) (path^"/"^f2) ;
                     helper t)
                     else failwith ("ID Mismatch f: "^f2^" , v1: "^(string_of_int v)^" , v2: "^(string_of_int v2)^ ", v2: "^v2_id)
    in helper ll)

let rec mkdir_from_path path_list =
  match path_list with 
  |[] -> ()
  |(h,_)::t -> let h = "mkdir -p "^h in
           Format.printf "%s\n" h ;ignore (Sys.command h); mkdir_from_path t

let rec check_path path_list =
  match path_list with 
  |[] -> true
  |(h,f)::t when f = "" -> 
                Format.printf "check : %s\n" h ;
                if(Sys.file_exists h)then check_path t else false
  |(h,f)::t->
                let h2 = (h^"/"^f) in
                Format.printf "check : %s\t" h ;
                Format.printf "check Abs : %s\n" h2 ;
                if((Sys.file_exists h) && (not (Sys.file_exists h2)))then 
                   check_path t else false

let test_files_1_2 ()=
  let l = get_unreachable () in
  let rec helper l count = 
    match l with 
    |[root_before;root_after] -> 
                      if(count = 3) then failwith "Test Fail"
                      else
                        create_TR ();
                        clean_TR ();
                        paths:= [];
                        get_path root_before [] "." "";
                        mkdir_from_path (!paths);
                        set_id root_before [] ".";
                        shell_script ();
                        paths:= [];
                        get_path root_after [] "." ""; 
                        if(check_path (!paths)) then 
                            (Format.printf "CHECK SUCCESS - Phase 1\n\n";
                            (*check_id root_after [] "."; change to check_id*)
                            Format.printf "CHECK SUCCESS - Phase 2")
                        else 
                        (Format.printf "Failure\n") (*(helper [root_before;root_after] (count+1)))*)
                     
    |_ -> failwith "Not exactly 2 unreachable"
  in helper l 1