open Constraints_common open Atom open Literal

type t = Conj.t

let true_ = (Var.Set.empty, Literal.Set.empty)

let quantify_over x (e, c) = Engine.normalize [Var.Set.add x e, c]
let add l (e, c) = Engine.normalize [e, Literal.Set.add l c]

let eq x y = add (Pos (Eq (x, y)))
let neq x y = add (Neg (Eq (x, y)))
let feat x f y = add (Pos (Feat (x, f, y)))
let nfeat x f y = add (Neg (Feat (x, f, y)))
let abs x f = add (Pos (Abs (x, f)))
let nabs x f = add (Neg (Abs (x, f)))
let reg x = add (Pos (Reg x))
let nreg x = add (Neg (Reg x))
let dir x = add (Pos (Dir x))
let ndir x = add (Neg (Dir x))
let fen x fs = add (Pos (Fen (x, fs)))
let nfen x fs = add (Neg (Fen (x, fs)))
let sim x fs y = add (Pos (Sim (x, fs, y)))
let nsim x fs y = add (Neg (Sim (x, fs, y)))

let pp = Conj.pp
