type t =
  { id : int ;
    hint : string option }

let fresh =
  let free = ref 0 in
  fun ?hint ?hintf ?hintv ?hintp () ->
  let hint =
    match hint, hintf, hintv, hintp with
    | Some s, _, _, _ -> Some s
    | _, Some f, _, _ -> Some (Feat.to_string f)
    | _, _, Some v, _ -> v.hint
    | _, _, _, Some p ->
       let (_, f) = Path.split_last p in
       Some (Feat.to_string f)
    | _ -> None
  in
  incr free;
  { id = !free ; hint }
