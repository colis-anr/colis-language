type t =
  { id : int ;
    hint : string option }

let fresh =
  let free = ref 0 in
  fun ?hint ?hintf () ->
  let hint =
    match hint, hintf with
    | Some hint, _ -> Some hint
    | _, Some hintf -> Some (Feat.to_string hintf)
    | _ -> None
  in
  incr free;
  { id = !free ; hint }
