type t =
  { id : int ;
    hint : string option }

let hint v = v.hint

let to_string v =
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
  Format.pp_print_string fmt (to_string v)

let fresh ?hint =
  let free = ref 0 in
  fun () ->
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
    match elements fs with
    | [] -> ()
    | f :: fs -> pp fmt f;
                 List.iter (Format.fprintf fmt ", %a" pp) fs
end

module Map = Map.Make(Self)
