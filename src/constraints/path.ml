type t = Feat.t list

let rec split_last = function
  | [] -> failwith "split_last"
  | [e] -> [], e
  | h::t ->
     let t',e = split_last t in
     h::t',e
