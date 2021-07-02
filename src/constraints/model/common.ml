let cwd_s = "/tmp/InnerTR/Inner2TR/Inner3TR"
type feature = string
let compare = compare

module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module Feat = struct
  type t = feature
  let compare = compare
end

module FSet = Set.Make(Feat)
module FMap = Map.Make(Feat)


type var = int
let compare2 = compare


module Var = struct
  type t = var
  let compare = compare2
end

module VarMap = Map.Make(Var)
module VSet = Set.Make(Var)

type kindt = Reg | Dir | Other | Unknown (*Unknown will be treated as Dir*)

type node = { var_l: VSet.t;
			 feat: var FMap.t;
       notfeat: (feature*var) list ; (*Not using Map as -x[f]y,-x[f]z can exists together*)
			 equality: (FSet.t*var) list; 
			 sim: (FSet.t*var) list;
       fen_p: bool;
			 fen : FSet.t; (*empty signifies no Fen specified so all allowed*)
       id : string;
       kind: kindt;
			 }

type fT = |Leaf
          |Node of (var * (fT FMap.t))

type atom =
  | Eq of var * var
  | Eqf of var * feature list * var
  | Feat of var * feature * var
  | Abs of var * feature
  | Fen of var * feature list
  | Sim of var * feature list * var
  | Kind of var * kindt
  | Maybe of var * feature * var (*Unimplemented*)

type var_map_type = node VarMap.t

type literal =
 |Pos of atom
 |Neg of atom

type clause = literal list

let (var_map:var_map_type ref) = ref VarMap.empty
let fBigSet = ref FSet.empty
let paths = ref []
let v_all = ref VSet.empty
let v_max = ref 0 
let v_min = ref max_int
let print_collect = ref "" 
let file1 = "print.dat"
let out_f_l = open_out file1 

let close_file () = close_out out_f_l

let rec list_remove ele = function
  |[] -> []
  |h::t when (h=ele) -> list_remove ele t 
  |h::t -> h::list_remove ele t

let find_node v1 = 
  let a = VarMap.find_opt v1 !var_map in
  match a with
  | None -> failwith ("Could not find Var"^(string_of_int v1))
  | Some nod -> nod 

let empty_node v:node = {var_l = VSet.of_list [v];feat = FMap.empty;equality = [];notfeat=[];sim = [];fen = FSet.empty;fen_p=false;id = "";kind = Unknown}

