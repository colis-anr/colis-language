type t =
  { id : int ;
    hint : string option }

let fresh =
  let free = ref 0 in
  fun ?hint ?hintf ?hintv () ->
  let hint =
    match hint, hintf, hintv with
    | Some hint, _, _ -> Some hint
    | _, Some hintf, _ -> Some (Feat.to_string hintf)
    | _, _, Some hintv -> hintv.hint
    | _ -> None
  in
  incr free;
  { id = !free ; hint }
