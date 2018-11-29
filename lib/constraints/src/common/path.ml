open OptionMonad

type comp =
  | Up
  | Here
  | Down of Feat.t

let comp_from_string = function
  | ".." -> Up
  | "." -> Here
  | s -> Down (Feat.from_string s) (*FIXME: check validity*)

let comp_to_string = function
  | Up -> ".."
  | Here -> "."
  | Down f -> Feat.to_string f

type rel = comp list

let empty_rel = []

let split_first_rel = function
  | [] -> None
  | h::t -> Some (h, t)

let rec split_last_rel = function
  | [] -> None
  | [e] -> Some ([], e)
  | h::t ->
     match split_last_rel t with
     | None -> assert false
     | Some (t, e) -> Some (h::t, e)

let to_string_rel r =
  List.map comp_to_string r
  |> String.concat "/"

type t = Abs of rel | Rel of rel

let split_last = function
  | Abs q ->
     split_last_rel q >>= fun (q, e) ->
     Some (Abs q, e)
  | Rel q ->
     split_last_rel q >>= fun (q, e) ->
     Some (Rel q, e)

let from_string s =
  String.split_on_char '/' s
  |> function
    | [] -> failwith "Path.from_string"
    | "" :: p -> Abs (List.map comp_from_string p)
    | p -> Rel (List.map comp_from_string p)

let to_string = function
  | Abs q -> "/" ^ to_string_rel q
  | Rel q -> to_string_rel q

let pp fmt q =
  Format.fprintf fmt "%s" (to_string q)

let rel = function
  | Abs q -> q
  | Rel q -> q

let concat p q =
  match p with
  | Abs p -> Abs (p @ q)
  | Rel p -> Rel (p @ q)

let normalize p =
  let rec normalize q p =
    match q, p with
    | q, [] -> List.rev q
    | q, Down f :: p -> normalize (f :: q) p
    | q, Here :: p -> normalize q p
    | [], Up :: p -> normalize [] p
    | _::q, Up :: p -> normalize q p
  in
  match p with
  | Abs p -> normalize [] p
  | Rel _ -> failwith "Path.normalize"
