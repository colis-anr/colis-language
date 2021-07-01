val cwd_s : string
type feature = string

val compare : feature -> feature -> int

module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module Feat : OrderedType with type t = feature

module FSet : Set.S with type elt = feature 
module FMap : Map.S with type key = feature


type var = int
val compare2 : var -> var -> int
val equal2 : var -> var -> bool
module Var: OrderedType with type t = var

module VarMap : Map.S with type key = var
module VSet : Set.S with type elt = var


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

val var_map:var_map_type ref
val fBigSet: FSet.t ref
val paths: (string*feature*var) list ref 
val v_all: VSet.t ref
val v_max: var ref
val print_collect: string ref
val file1 : string 
val out_f_l: out_channel

val close_file: unit-> unit

val list_remove : 'a -> 'a list -> 'a list

val find_node: var -> node
val empty_node: var -> node