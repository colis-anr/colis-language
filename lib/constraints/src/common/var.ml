type t =
  { id : int ;
    hint : string option }

let show v =
  let hint = match v.hint with None -> "_" | Some h -> h in
  hint ^ string_of_int v.id

let pp fmt v =
  Format.pp_print_string fmt (show v)

let fresh =
  let free = ref 0 in
  fun ?hint ?hintf ?hintv ?hintp () ->
  let hint =
    match hint, hintf, hintv, hintp with
    | Some s, _, _, _ -> Some s
    | _, Some f, _, _ -> Some (Feat.to_string f)
    | _, _, Some v, _ -> v.hint
    | _, _, _, Some _p -> assert false
    | _ -> None
  in
  incr free;
  { id = !free ; hint }

let compare v1 v2 = Pervasives.compare v1.id v2.id
let equal v1 v2 = compare v1 v2 = 0

module Self = struct
  type s = t
  type t = s
  let compare = compare
end

module Set = Set.Make(Self)
module Map = Map.Make(Self)
