type t = Feat.t list

let empty = []

let to_list p = p

let split_first = function
  | [] -> failwith "split_first"
  | h::t -> (h, t)

let rec split_last = function
  | [] -> failwith "split_last"
  | [e] -> [], e
  | h::t ->
     let t',e = split_last t in
     h::t',e

let from_string s =
  String.split_on_char '/' s
  |> List.map Feat.from_string
