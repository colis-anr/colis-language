type t = string

let from_string s = s
let to_string s = s

let pp_print = Format.pp_print_string

module Self = struct
  type s = t
  type t = s
  let compare = compare
end

module Set = Set.Make(Self)
module Map = Map.Make(Self)
