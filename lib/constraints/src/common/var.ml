type t =
  { id : int ;
    hint : string option }

let show v =
  let hint = match v.hint with None -> "?" | Some h -> h in
  let number =
    let number = string_of_int v.id in
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
  in
  hint ^ number

let pp fmt v =
  Format.pp_print_string fmt (show v)

let fresh =
  let free = ref 0 in
  fun ?hint () ->
  incr free;
  { id = !free ; hint }

let compare v1 v2 = Pervasives.compare v1.id v2.id
let equal v1 v2 = compare v1 v2 = 0

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
       fpf fmt "[]"
    | f :: fs ->
       fpf fmt "[";
       pp fmt f;
       List.iter (fpf fmt ", %a" pp) fs;
       fpf fmt "]"
end

module Map = Map.Make(Self)
