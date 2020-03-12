open Colis_constraints_common open Atom open Literal

type t = Conj.t

let true_ = (Var.Set.empty, Literal.Set.empty)

let quantify_over x (e, c) =
  let x' = Var.fresh () in
  [Var.Set.add x' e,
   Rules.replace_in_literal_set ~var:x ~by:x' c]
  |> Engine.normalize
  |> Engine.simplify

let add l (e, c) =
  [e, Literal.Set.add l c]
  |> Engine.normalize

let  eq x y = add (Pos (Eq (x, y)))
let neq x y = add (Neg (Eq (x, y)))

let  feat x f y = add (Pos (Feat (x, f, y)))
let nfeat x f y = add (Neg (Feat (x, f, y)))

let  abs x f = add (Pos (Abs (x, f)))
let nabs x f = add (Neg (Abs (x, f)))

let  maybe _x _f _y = assert false
let nmaybe _x _f _y = assert false

let  fen x fs = add (Pos (Fen (x, fs)))
let nfen x fs = add (Neg (Fen (x, fs)))

let  sim x fs y = add (Pos (Sim (x, fs, y)))
let nsim x fs y = add (Neg (Sim (x, fs, y)))

let  kind k x = add (Pos (Kind (x, k)))
let nkind k x = add (Neg (Kind (x, k)))

let with_shadow_variables f = f ()

let pp = Conj.pp
let pp_as_dot = Conj.pp_as_dot
