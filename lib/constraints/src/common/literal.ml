let fpf = Format.fprintf

type t =
  | Pos of Atom.t
  | Neg of Atom.t
[@@deriving eq, ord]

let pp fmt = function
  | Pos a -> Atom.pp fmt a
  | Neg a -> fpf fmt "¬ %a" Atom.pp a

module Self = struct
  type s = t
  type t = s

  let compare = compare
  let equal = equal
end

module Set = struct
  include Set.Make(Self)

  let pp fmt fs =
    match elements fs with
    | [] -> ()
    | f :: fs -> pp fmt f;
                 List.iter (fpf fmt " ∧ %a" pp) fs
end
