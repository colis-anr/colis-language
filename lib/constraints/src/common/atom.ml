let fpf = Format.fprintf

type t =
  | Eq of Var.t * Var.t
  | Feat of Var.t * Feat.t * Var.t
  | Abs of Var.t * Feat.t
  | Kind of Var.t * Kind.t
  | Fen of Var.t * Feat.Set.t
  | Sim of Var.t * Feat.Set.t * Var.t
[@@deriving ord]

let compare a1 a2 =
  match a1, a2 with
  | Eq (x1, y1), Eq (x2, y2)
  | Sim (x1, _, y1), Sim (x2, _, y2) ->
     let (x1, y1) =
       if Var.compare x1 y1 < 0 then
         (x1, y1)
       else
         (y1, x1)
     in
     let (x2, y2) =
       if Var.compare x2 y2 < 0 then
         (x2, y2)
       else
         (y2, x2)
     in
     (
       match Var.compare x1 x2 with
       | 0 -> Var.compare y1 y2
       | n -> n
     )
  | _ -> compare a1 a2

let equal a1 a2 = compare a1 a2 = 0

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
