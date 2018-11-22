type component =
  | Up
  | Here
  | Down of Feat.t

let component_from_string = function
  | ".." -> Up
  | "." -> Here
  | s -> Down (Feat.from_string s) (*FIXME: check validity*)

type t = component list

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
  |> List.map component_from_string

let normalize_syntactically p =
  let rec aux q p =
    match q, p with
    |      _,            [] -> []
    |      q, (Down f) :: p -> aux (f :: q) p
    |      q,  Here    :: p -> aux q p
    |     [],  Up      :: p -> aux [] p
    | _ :: q,  Up      :: p -> aux q p
  in
  aux [] p
