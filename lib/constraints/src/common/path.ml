type comp =
  | Up
  | Here
  | Down of Feat.t

(* let comp_from_string = function
 *   | ".." -> Up
 *   | "." -> Here
 *   | s -> Down (Feat.from_string s) (\*FIXME: check validity*\) *)

type rel = comp list

let empty_rel = []

let split_first_rel = function
  | [] -> None
  | h::t -> Some (h, t)

type t = Abs of rel | Rel of rel

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
