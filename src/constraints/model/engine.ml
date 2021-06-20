(*open Model_ref*)
(*open Path*)

(*let cwd = "/media/ap/New Volume/IIIT Kalyani/Internships/Feature Tree Logic/Reverse/ADifferentWay/Test region/InnerTR/Inner2TR/Inner3TR"
let cwd = Colis_constraints.Path.normalize (Colis_constraints.Path.from_string (cwd))*)
let cwd = [] (*When CWD is set to something else it gives an error*)
(*
type utility_context = {
  cwd : Colis_constraints.Path.normal;
  env : string Colis__.Env.SMap.t;
  args : string list;
}

type filesystem = {
root : Colis_constraints.Var.t;
clause : Colis_constraints.Clause.sat_conj;
root0 : Colis_constraints.Var.t option;
}

type state =  {
           filesystem : filesystem;
           stdin : Colis__.Semantics__Buffers.Stdin.t;
           stdout : Colis__.Semantics__Buffers.Stdout.t;
           log : Colis__.Semantics__Buffers.Stdout.t;
         }

type utility = state -> (state * bool Colis__.Semantics__Result.result) list
*)
(*Doc : Colis->Language->Nat*)
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

let feat_to_string (x:Colis_constraints_common.Feat.t):string = Colis_constraints_common.Feat.to_string x

let var_to_int (x:Colis_constraints_common.Var.t):int =
  let rec helper in_s out_s=
    if((String.length in_s) < 3) then
      int_of_string out_s
    else
    let ch = String.sub in_s 0 3 in
    let digit =
      (match ch with
       | "₀" -> "0"
       | "₁" -> "1"
       | "₂" -> "2"
       | "₃" -> "3"
       | "₄" -> "4"
       | "₅" -> "5"
       | "₆" -> "6"
       | "₇" -> "7"
       | "₈" -> "8"
       | "₉" -> "9"
       | _ -> assert false) in
    let in_s = String.sub in_s 3 ((String.length in_s) - 3) in
    helper in_s (out_s^digit)
  in 
  (helper (Colis_constraints_common.Var.to_string x) "")

let fset_to_fset (x:Colis_constraints_common.Feat.Set.t): string list = 
  let lis = Colis_constraints_common.Feat.Set.elements x in
  let rec helper = function
   |[]-> []
   |h::t -> (feat_to_string h)::helper t
  in (helper lis)

let kind_to_kind (x:Colis_constraints_common.Kind.t): Model_ref.kindt =
  match x with
  | Dir -> Dir
  | Reg -> Reg
  | Char | Sock | Pipe | Symlink | Block -> Other

let atom_to_Atom (x: Colis_constraints_common.Atom.t): Model_ref.atom =
  match x with
  | Eq(v1,v2) -> Eq(var_to_int v1,var_to_int v2)
  | Feat(v1,f,v2) -> Feat (var_to_int v1,feat_to_string f,var_to_int v2)
  | Abs(v1,f) -> Abs(var_to_int v1,feat_to_string f)
  | Kind(v1,k) -> Kind(var_to_int v1,(kind_to_kind k))
  | Fen(v1,f) -> Fen(var_to_int v1,fset_to_fset f)
  | Sim(v1,f,v2) -> Sim(var_to_int v1,fset_to_fset f,var_to_int v2)

let rec literal_to_Literal (x: Colis_constraints_common.Literal.t list): Model_ref.literal list =
  match x with
  | [] -> []
  | Pos a::t -> Pos (atom_to_Atom a):: literal_to_Literal t
  | Neg a::t -> Neg (atom_to_Atom a):: literal_to_Literal t

let printStdout stdO =
  Format.printf "%s" (Colis__.Semantics__Buffers.Stdout.to_string stdO)


let rec run_model (res_l:(Colis.SymbolicUtility.Mixed.state *
            bool Colis__Semantics__Result.result)
           list) (print_b:bool) (num:int) (cmd)= 
  match res_l with
  | [] -> false
  | (state_,Ok x)::t ->
                  let (out_fs:Colis__.SymbolicUtility.Mixed.filesystem) = state_.filesystem in
                  printStdout state_.stdout ;
                  let s_c = match out_fs with Constraints r -> r.clause | _ -> failwith "not a good fs" in
                  let rootb = match out_fs with Constraints r -> r.root0 | _ -> failwith "not a good fs" in 
                  let roota = match out_fs with Constraints r -> r.root | _ -> failwith "not a good fs" in  
                  let rootb = match rootb with | Some v -> v |None -> failwith "no root before" in
                  let s_c = Colis_constraints_efficient.sat_conj_to_literals (s_c) in
                  let s_c = List.of_seq s_c in
                  let s_c = literal_to_Literal s_c in
                  let _ = if(print_b) then
                  (Format.printf "\n\n\n\tClause %d [RootB: %d ;RootA: %d; isError: %b] : \n"(num) (var_to_int rootb) (var_to_int roota) (not x);                 
                  Model_ref.print_clause (s_c)) else (Format.printf "\n\nClause %d:"(num)) in
                  Model_ref.engine (s_c) ~m:true ~p:print_b ~rootb:(var_to_int rootb) ();
                  Test_file2.test_files_1_2 (var_to_int rootb) (var_to_int roota) (s_c) (not x) (cmd) (print_b); 
                  run_model t print_b (num+1) cmd
  | _::t -> Format.printf "\n\n\tClause %d : Incomplete\n"(num);
            run_model t print_b (num+1) cmd

let get_result (cmd) (print_b:bool) = 
    
    let split_cmd (cmd) =
      let sl = Model_ref.list_remove "" (String.split_on_char ' ' cmd) in
      (List.hd sl,List.tl sl) in
    
    let (utility_context_:Colis__Semantics__UtilityContext.utility_context) = {
      cwd = cwd;
      env = Colis__.Env.SMap.empty;
      args = [];
    } in   
    let (utility_name,args) = split_cmd (cmd) in   
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
    let _ = Format.printf "\nCMD: %s" (cmd) in

    let result_list = try utility_ initial_state with 
                    e -> 
                    let msg = Printexc.to_string e in
                    Format.printf "\nEXCEPTION: [%s]" msg;
                    [initial_state,Incomplete] in
    let _ = Format.printf "\nNo of Clauses : %d" (List.length result_list) in
    let _ = run_model result_list print_b 1 cmd (*False-> less print*) in 
    ()

let rec loop_cmd (cmd_l) =
  match cmd_l with
  |[] -> ()
  |h::t ->  Format.printf "-------------------------------------------------------------------------";
            get_result h false;
            loop_cmd t

let cmd_file = "cmd.dat"

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

let _ = loop_cmd (read_file cmd_file)

(* For single cmd (use for debugging)
let cmd = "touch ./a/b/../c/d/./e"
let _ = get_result cmd true(*False-> less print*)
*)
