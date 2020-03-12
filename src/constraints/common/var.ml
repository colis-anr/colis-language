type t = int
[@@deriving yojson]

let to_string v =
  let number = string_of_int v in
  let buf = Buffer.create 8 in
  String.iter
    (fun c -> Buffer.add_string buf
        (match c with
         | '0' -> "₀"
         | '1' -> "₁"
         | '2' -> "₂"
         | '3' -> "₃"
         | '4' -> "₄"
         | '5' -> "₅"
         | '6' -> "₆"
         | '7' -> "₇"
         | '8' -> "₈"
         | '9' -> "₉"
         | _ -> assert false))
    number;
  Buffer.contents buf

let pp fmt v =
  Format.pp_print_string fmt (to_string v)

let fresh =
  let free = ref 0 in
  fun () -> incr free; !free

let compare = compare
let equal = (=)

module Self = struct
  type s = t
  type t = s
  let compare = compare
  let pp = pp
  let to_yojson = to_yojson
  let of_yojson = of_yojson
end

module Set = Derivable.MakeSet(Self)
module Map = Derivable.MakeMap(Self)
