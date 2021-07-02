open Common
open Inode

(* DEPRICATED
let get_vBigSet () =
    let ll = VarMap.bindings !var_map in
    let rec helper ll =
      match ll with
      |[] -> []
      |(v,_)::t -> v::(helper t)
    in helper ll

let get_unreachable () =
  let vBigSet = ref (get_vBigSet ())in
  let ll = VarMap.bindings !var_map in
  let rec helper1 ll =
    match ll with
    |[] -> (list_remove 0 !vBigSet)
    |(_,v_node)::t -> let l = FMap.bindings (v_node.feat) in
                      let rec helper2 l =
                        match l with
                        |[] -> helper1 t
                        |(_,v2)::t2-> vBigSet := (list_remove v2 !vBigSet);
                                   helper2 t2
                      in helper2 l
  in helper1 ll
*)

let rec get_path (v) (v_cycle) (path) (f)=
  let ll = FMap.bindings ((find_node v).feat) in
  if(List.mem v v_cycle)then failwith "Cycle Clash"
  else if((ll=[])||(v=0)) then
      paths := (path,f,v)::(!paths)
  else
    (let rec helper ll =
      match ll with
      |[] -> ()
      |(f2,v2)::t when v2 = 0 -> 
                     get_path (v2) (v::v_cycle) (path) (f2);
                     helper t
      |(f2,v2)::t -> if((find_node v2).kind = Reg) then
                        (get_path (v2) (v::v_cycle) (path) (f2);
                        helper t)
                     else   
                        (get_path (v2) (v::v_cycle) (path^"/"^f2) ("");
                        helper t)
    in helper ll)

let rec mkdir_from_path path_list =
  match path_list with 
  |[] -> ()
  |(h,f,v)::t when ((v <> 0) && (f <> "")) -> 
           let h1 = "mkdir -p "^h in
           let h2 = "touch "^(h^"/"^f) in
           print_collect := !print_collect^h1^"\n"^h2^"\n" ;ignore (Sys.command h1);
           ignore (Sys.command h2); 
           mkdir_from_path t

  |(h,_,_)::t -> let h = "mkdir -p "^h in
           print_collect := !print_collect^h^"\n" ;ignore (Sys.command h); 
           mkdir_from_path t

let path_exists p = (Sys.command("test -e "^p)=0)
let file_exists p = (Sys.command("test -f "^p)=0)

let rec check_path path_list =
  match path_list with 
  |[] -> true
  |(h,f,_)::t when f = "" -> 
                print_collect := !print_collect^"check : "^h^"\n" ;
                if(path_exists h)then check_path t else false
  |(h,f,v)::t when v = 0->
                let h2 = (h^"/"^f) in
                print_collect := !print_collect^"check : "^h^"\t" ;
                print_collect := !print_collect^"check Abs : "^h2^"\n" ;
                if((path_exists h) && (not (path_exists h2)))then 
                check_path t else false
  |(h,f,_)::t -> print_collect := !print_collect^"check Reg : "^(h^"/"^f)^"\n" ;
              if(file_exists (h^"/"^f))then check_path t else false




(*FOR PC USE BELOW*)
(*
let safe_dir =  "/media/ap/New Volume/IIIT Kalyani/Internships/Feature Tree Logic/Reverse/ADifferentWay/Test region/InnerTR/Inner2TR/Inner3TR"
let create_TR () =
  ignore (Sys.chdir safe_dir);
  ignore (Sys.command "mkdir ./TR");
  Sys.chdir("./TR");() *)

(*FOR DOCKER USE BELOW*)

let safe_dir =  cwd_s

let create_TR () =
  ignore (Sys.command ("mkdir -p "^safe_dir));
  ignore (Sys.chdir safe_dir);
  ignore (Sys.command ("mkdir -p ./TR"^safe_dir));
  Sys.chdir("./TR");() 

let shell_script cmd =
  ignore (Sys.command ("mkdir -p ."^safe_dir));
  print_collect := !print_collect^cmd^"\n" ;
  if(Sys.command cmd = 0)then true else false

(*
let shell_script cmd =
  print_collect := !print_collect^cmd^"\n" ;
  if(Sys.command cmd = 0)then true else false
*)
let clean_TR () = 
  Sys.chdir("..");
  ignore (Sys.command "rm -r ./TR/*");
  Sys.chdir("./TR");()

(*DEPRICATED:without ID check
  let test_files ()=
  let l = get_unreachable () in
  let rec helper l count = 
    match l with 
    |[root_after;root_before] -> 
                      if(count = 3) then failwith "Test Fail"
                      else
                        create_TR ();
                        clean_TR ();
                        paths:= [];
                        get_path root_before [] "." "";
                        mkdir_from_path (!paths);
                        shell_script ();
                        paths:= [];
                        get_path root_after [] "." ""; 
                        if(check_path (!paths)) then Format.printf "CHECK SUCCESS"
                        else 
                        (Format.printf "Failure\n" ;(helper [root_before;root_after] (count+1)))
                     
    |_ -> failwith "Not exactly 2 unreachable"
  in helper l 1*)

let test_files_1_2 (root_before) (root_after) (clau) (is_error) (cmd) (print_b) =
    let _ = if(VarMap.find_opt root_before !var_map)=None 
            then var_map := VarMap.add root_before (empty_node root_before) (!var_map) else () in
    let _ = if(VarMap.find_opt root_after !var_map)=None 
            then var_map := VarMap.add root_after (empty_node root_after) (!var_map) else () in
    create_TR ();
    clean_TR ();
    paths:= [];
    get_path root_before [] "." "";
    mkdir_from_path (!paths);
    set_id root_before ".";
    if(shell_script cmd <> is_error) then
        (paths:= [];
        get_path root_after [] "." ""; 
        if(check_path (!paths)) then 
            let _ = if(print_b) then (Printf.fprintf out_f_l "%s" (!print_collect);Format.printf "%s" (!print_collect) ) else () in
            print_collect := "";
            (Printf.fprintf out_f_l "\t\t***PATH CHECK SUCCESS***\n";
            Format.printf "\t\t***PATH CHECK SUCCESS***\n";
            Printf.fprintf out_f_l "%s" "\t\t\tID Dissolve Repot\nEquality(*) Dissolve Error:\t";
            Format.printf "%s" "\t\t\tID Dissolve Repot\nEquality(*) Dissolve Error:\t";
            check_id root_after ".";

            Printf.fprintf out_f_l "%s" "SIM(F) Dissolve Error:\t";
            Format.printf "%s" "SIM(F) Dissolve Error:\t";
            dissolve_id_sim clau;
            check_id root_after ".";

            Printf.fprintf out_f_l "%s" "Equality(F) Dissolve Error:\t";
            Format.printf "%s" "Equality(F) Dissolve Error:\t";
            dissolve_id_eqf clau;
            check_id root_after ".";
            )
        else 
        (Printf.fprintf out_f_l "%s \t\t-----PATH CHECK FAILURE-----\n" (!print_collect);
        Format.printf "%s \t\t-----PATH CHECK FAILURE-----\n" (!print_collect)) )
    
    else (Printf.fprintf out_f_l "%s %s" (!print_collect) (if(is_error)then "\nCMD does not give an error(But it should)\n" else "\nCMD gives an error\n");
    Format.printf "%s %s" (!print_collect) (if(is_error)then "\nCMD does not give an error(But it should)\n" else "\nCMD gives an error\n"))                
