(*open Model_ref*)
(*open Path*)

let cwd = ref []
let cwd_update ()= cwd := Colis_constraints.Path.normalize (Colis_constraints.Path.from_string ( Sys.getcwd ()));()
let _ = Format.printf "Hello World\n"

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
     (module Colis__.Basics.True) ;
     (module Colis__.Basics.Colon) ;
     (module Colis__.Basics.False) ;
     (module Colis__.Basics.Echo) ;
     (module Colis__.Cp) ;
     (module Colis__.Mkdir) ;
     (module Colis__.Mv) ;
   ]

let (utility_context_:Colis__Semantics__UtilityContext.utility_context) = {
  cwd = !cwd;
  env = Colis__.Env.SMap.empty;
  args = [];
}

let utility_name = "mv"
let args = ["./a/b";"./c"]

let utility_ = Colis.SymbolicUtility.Constraints.call (utility_name) (utility_context_) (args)

let root_v = 1 (*Replace by fresh*)

let (initial_fs:Colis__.SymbolicUtility.Constraints.filesystem) = {
      root = (Colis_constraints_common__Var.fresh ());
      clause = Colis_constraints_efficient.true_sat_conj;
      root0 = Some (Colis_constraints_common__Var.fresh ());
    }
    
let (intial_state:Colis__.SymbolicUtility.Constraints.state) =  {
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


let result_list = utility_ intial_state

(*
let printStdout stdO =
  Format.printf "%s" (Colis__.Semantics__Buffers.Stdout.to_string stdO)


let rec run_model (res_l:(Colis.SymbolicUtility.Constraints.state *
            bool Colis__Semantics__Result.result)
           list) = 
  match res_l with
  | [] -> false
  | (state_,r)::t when (r = Ok true) -> (*Assuming r is a simple bool for now*)
                  let out_fs = state_.filesystem in
                  printStdout state_.stdout ;
                  let s_c = out_fs.clause in
                  let s_c = Colis_constraints_efficient.sat_conj_to_literals (s_c) in
                  let s_c = List.of_seq s_c in
                  Model_ref.engine s_c;
                  if(test_file out_fs.root out_fs.root0)then true
                  else run_model t
  | _::t -> run_model t
  *)