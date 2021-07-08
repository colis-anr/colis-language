open Convert
open Phases
open File_system
open Common
open Print

let if_mutate = false
let if_print_detail = true
let cwd = Colis_constraints.Path.normalize (Colis_constraints.Path.from_string (cwd_s))
(*
let cwd = [] (*apply cmd from root dir*)
*)


let () =
   List.iter Colis.SymbolicUtility.Mixed.register [
     (module Colis__Basics.True) ;
     (module Colis__Basics.Colon) ;
     (module Colis__Basics.False) ;
     (module Colis__Basics.Echo) ;
     (module Colis__Cp) ;
     (module Colis__Rm) ;
     (module Colis__Touch) ;
     (module Colis__Mkdir) ;
     (module Colis__Mv) ;
     (module Colis__Test) ;
     (module Colis__Test.Bracket) ;
   ]

let printStdout stdO =
  Printf.fprintf out_f_l "%s" (Colis__.Semantics__Buffers.Stdout.to_string stdO);
  Format.printf "%s" (Colis__.Semantics__Buffers.Stdout.to_string stdO)


let rec run_model (res_l:(Colis.SymbolicUtility.Mixed.state *
            bool Colis__Semantics__Result.result)
           list) (print_b:bool) (num:int) (cmd_mod) (mutate:bool)= 
  match res_l with
  | [] -> ()
  | (state_,Ok x)::t ->
                  let (out_fs:Colis__.SymbolicUtility.Mixed.filesystem) = state_.filesystem in
                  printStdout state_.stdout ;
                  let s_c = match out_fs with Constraints r -> r.clause | _ -> failwith "not a good fs" in
                  let rootb = match out_fs with Constraints r -> r.root0 | _ -> failwith "not a good fs" in 
                  let roota = match out_fs with Constraints r -> r.root | _ -> failwith "not a good fs" in  
                  let rootb = match rootb with | Some v -> v |None -> failwith "no root before" in
                  let s_c = Colis_constraints_efficient.sat_conj_to_literals (s_c) in
                  let s_c = List.of_seq s_c in
                  let s_c = clause_to_clause s_c in
                  let _ = if(print_b) then
                  ( Printf.fprintf out_f_l "\n\n\n\tClause %d [RootB: %d ;RootA: %d; isError: %b] : \n"(num) (var_to_int rootb) (var_to_int roota) (not x);
                    Format.printf "\n\n\n\tClause %d [RootB: %d ;RootA: %d; isError: %b] : \n"(num) (var_to_int rootb) (var_to_int roota) (not x);                 
                  
                  print_clause (s_c)) else (Printf.fprintf out_f_l "\n\n[MUTATION:%b]Clause %d: \n"(mutate)(num);Format.printf "\n\n[MUTATION:%b]Clause %d: \n"(mutate)(num)) in
                  engine (s_c) ~m:mutate ~p:print_b ~rootb:(var_to_int rootb) ~roota:(var_to_int roota)();
                  test_files_1_2 (var_to_int rootb) (var_to_int roota) (s_c) (not x) (cmd_mod) (print_b); 
                  run_model t print_b (num+1) cmd_mod mutate
  | _::t -> Printf.fprintf out_f_l "\n\n\tClause %d : Incomplete\n"(num);
            Format.printf "\n\n\tClause %d : Incomplete\n"(num);
            run_model t print_b (num+1) cmd_mod mutate

let isRel s = (let p = String.sub (String.trim(s)) 0 1 in not (p = "/"))
let isOpt s = (let p = String.sub (String.trim(s)) 0 1 in ((p = "-")||(String.trim(s)="!")))

let split_cmd (cmd) =
      let sl = list_remove "" (String.split_on_char ' ' cmd) in
      let rec helper stl =
        match stl with
         |[]-> ([],[])
         |h::t -> let h_mod = (if(not (isOpt h))then
                            (if(isRel h) then "."^cwd_s^"/"^h 
                            else "./"^h)
                          else h) in     (*CHANGE HERE*)
                  let (hl_1,hl_2) = helper t in
                  (h::hl_1,h_mod::hl_2)
      in
      (List.hd sl,helper (List.tl sl))

let get_result (cmd) ?(m = false) ?(p = true) () = 
    
    let (utility_context_:Colis__Semantics__UtilityContext.utility_context) = {
      cwd = cwd;
      env = Colis__.Env.SMap.empty;
      args = [];
    } in   
    let (utility_name,(args,args_mod)) = split_cmd (cmd) in  
    let cmd_mod = utility_name^" "^(String.concat " " args_mod) in 
    let utility_ = Colis.SymbolicUtility.Mixed.call (utility_name) (utility_context_) (args) in  
    let root_v = (Colis_constraints_common__Var.fresh ()) in
    let (initial_fs:Colis__.SymbolicUtility.Mixed.filesystem) = Constraints {
          root = root_v;
          clause = Colis_constraints_efficient.true_sat_conj;
          root0 = Some root_v;
        } in
    let (initial_state:Colis__.SymbolicUtility.Mixed.state) =  {
           filesystem = initial_fs;
           stdin = [];
           stdout = Colis__.Semantics__Buffers.Stdout.empty;
           log = Colis__.Semantics__Buffers.Stdout.empty;
         } in
    let _ = Printf.fprintf out_f_l "\nCMD: %s" (cmd);
            Format.printf "\nCMD: %s" (cmd);
            Printf.fprintf out_f_l "\nCMD_Mod: %s" (cmd_mod);
            Format.printf "\nCMD_Mod: %s" (cmd_mod) in

    let result_list = try utility_ initial_state with 
                    e -> 
                    let msg = Printexc.to_string e in
                    Printf.fprintf out_f_l "\nEXCEPTION: [%s]" msg;
                    Format.printf "\nEXCEPTION: [%s]" msg;
                    [initial_state,Incomplete] in

    let _ =  Printf.fprintf out_f_l "\nNo of Clauses : %d" (List.length result_list);
          Format.printf "\nNo of Clauses : %d" (List.length result_list) in
    let _ = run_model result_list p 1 cmd_mod m in 
    ()

let rec loop_cmd (cmd_l) ?(m = false) ?(p = true) ()=
  match cmd_l with
  |[] -> ()
  |h::t ->  Printf.fprintf out_f_l "-------------------------------------------------------------------------";
            Format.printf "-------------------------------------------------------------------------";
            get_result h ~m:m ~p:p (); 
            loop_cmd t ~m:m ~p:p ()

let read_file filename = 
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done; !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines ;;

let cmd_file = "cmd.dat"
(*m-> boolean specifying if mutuate; p->boolean specifying if print detail*)
let _ =  Printf.fprintf out_f_l "\t\tMUTATION OFF\n"
let _ = loop_cmd (read_file cmd_file) ~m:if_mutate ~p:if_print_detail ()

(*For single cmd (use for debugging)
let cmd = "mkdir ./a/b"
let _ = get_result cmd ~m:true ~p:true ()
*)