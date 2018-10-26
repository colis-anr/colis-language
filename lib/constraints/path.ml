type t = Feat.t list

let empty = []

let rec split_last = function
  | [] -> failwith "split_last"
  | [e] -> [], e
  | h::t ->
     let t',e = split_last t in
     h::t',e

let from_string s =
  String.split_on_char '/' s
  |> List.map Feat.from_string
