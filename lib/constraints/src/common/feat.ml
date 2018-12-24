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

  let pp_gen ~open_ ~close ~empty fmt fs =
    let fpf = Format.fprintf in
    match elements fs with
    | [] ->
       fpf fmt "%s" empty
    | f :: fs ->
       fpf fmt "%s" open_;
       pp fmt f;
       List.iter (fpf fmt ", %a" pp) fs;
       fpf fmt "%s" close
  (* FIXME: this is here because of Atom.pp that uses @@deriving. But this should move. *)

  let pp fmt fs =
    pp_gen ~open_:"{" ~close:"}" ~empty:"∅" fmt fs
end

module Map = Map.Make(Self)
