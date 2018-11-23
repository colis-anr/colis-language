type t = string
[@@deriving show]

let from_string s = s
let to_string s = s

let pp_print = Format.pp_print_string

let compare = compare
let equal f1 f2 = compare f1 f2 = 0

module Self = struct
  type s = t
  type t = s
  let compare = compare
end

module Set = Set.Make(Self)
module Map = Map.Make(Self)
