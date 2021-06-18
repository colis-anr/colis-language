(*open Model_ref*)
(*open Path*)

let cwd = ref []
let cwd_update ()= cwd := Colis_constraints.Path.normalize (Colis_constraints.Path.from_string ( Sys.getcwd ()));()

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

let (utility_context_:Colis__Semantics__UtilityContext.utility_context) = {
  cwd = !cwd;
  env = Colis__.Env.SMap.empty;
  args = [];
}

let utility_name = "mkdir"
let args = ["./a/b/../c/d/./e"]
let cmd = "mkdir ./a/b/../c/d/./e"

let utility_ = Colis.SymbolicUtility.Mixed.call (utility_name) (utility_context_) (args)

let root_v = (Colis_constraints_common__Var.fresh ()) (*Replace by fresh*)

let (initial_fs:Colis__.SymbolicUtility.Mixed.filesystem) = Constraints {
      root = root_v;
      clause = Colis_constraints_efficient.true_sat_conj;
      root0 = Some root_v;
    }
    
let (initial_state:Colis__.SymbolicUtility.Mixed.state) =  {
       filesystem = initial_fs;
       stdin = [];
       stdout = Colis__.Semantics__Buffers.Stdout.empty;
       log = Colis__.Semantics__Buffers.Stdout.empty;
     }

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


let result_list = utility_ initial_state


let printStdout stdO =
  Format.printf "%s" (Colis__.Semantics__Buffers.Stdout.to_string stdO)


let rec run_model (res_l:(Colis.SymbolicUtility.Mixed.state *
            bool Colis__Semantics__Result.result)
           list) = 
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
                  Format.printf "\n\n\n\tOutput Clause [RootB: %d ;RootA: %d; isError: %b] : \n" (var_to_int rootb) (var_to_int roota) (not x);
                  let s_c = literal_to_Literal s_c in
                  Model_ref.print_clause (s_c);
                  Model_ref.engine (s_c) ~m:true ();
                  Test_file2.test_files_1_2 (var_to_int rootb) (var_to_int roota) (s_c) (not x) (cmd); 
                  run_model t
  | _::t -> run_model t

let _ = Format.printf "\nNo of Clauses : %d\n" (List.length result_list)
let _ = run_model result_list

