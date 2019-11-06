type t = string
[@@deriving yojson]

let pp = Format.pp_print_string

let from_string s = s
let to_string s = s

let compare = compare
let equal f1 f2 = compare f1 f2 = 0

module Self = struct
  type s = t type t = s

  let compare = compare

  let to_yojson = to_yojson
  let of_yojson = of_yojson

  let pp = pp
end

module Set = Derivable.MakeSet(Self)
module Map = Derivable.MakeMap(Self)
