type t =
  | Pos of Atom.t
  | Neg of Atom.t
[@@deriving eq, ord]

let pp fmt = function
  | Pos a -> Atom.pp fmt a
  | Neg a -> Format.fprintf fmt "¬ %a" Atom.pp a

module Self = struct
  type s = t
  type t = s

  let compare = compare
  let equal = equal
end

module Set = Set.Make(Self)
