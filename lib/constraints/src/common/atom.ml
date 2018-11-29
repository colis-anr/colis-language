let fpf = Format.fprintf

type t =
  | Eq of Var.t * Var.t
  | Feat of Var.t * Feat.t * Var.t
  | Abs of Var.t * Feat.t
  | Kind of Var.t * Kind.t
  | Fen of Var.t * Feat.Set.t
  | Sim of Var.t * Feat.Set.t * Var.t
[@@deriving eq, ord]

let pp fmt = function
  | Eq (x, y) ->
     fpf fmt "%a = %a" Var.pp x Var.pp y
  | Feat (x, f, y) ->
     fpf fmt "%a[%a]%a" Var.pp x Feat.pp f Var.pp y
  | Abs (x, f) ->
     fpf fmt "%a[%a]↑" Var.pp x Feat.pp f
  | Kind (x, k) ->
     fpf fmt "%a(%a)" Kind.pp k Var.pp x
  | Fen (x, fs) ->
     fpf fmt "%a[%a]" Var.pp x Feat.Set.pp fs
  | Sim (x, fs, y) ->
     fpf fmt "%a ~%a %a" Var.pp x Feat.Set.pp fs Var.pp y
