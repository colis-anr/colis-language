type t = string

let pp = Format.pp_print_string

let from_string s = s
let to_string s = s

let compare = compare
let equal f1 f2 = compare f1 f2 = 0

module Self = struct
  type s = t
  type t = s
  let compare = compare
end

module Set = struct
  include Set.Make(Self)

  let pp fmt fs =
    let fpf = Format.fprintf in
    match elements fs with
    | [] ->
       fpf fmt "∅"
    | f :: fs ->
       fpf fmt "{";
       pp fmt f;
       List.iter (fpf fmt ", %a" pp) fs;
       fpf fmt "}"
end

module Map = Map.Make(Self)
