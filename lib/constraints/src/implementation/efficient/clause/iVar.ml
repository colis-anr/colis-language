type t = int

module Map = Map.Make(struct type t = int let compare = compare end)

type 'a gen = Son of t | Ancestor of 'a
type 'a map = 'a gen Map.t

let empty_map = Map.empty

(* FIXME, remove: *)
let _ = Son 1
let _ = Ancestor ()

let rec repr m x =
  match Map.find x m with
  | Ancestor _ -> x
  | Son y -> repr m y

let equal m x y =
  repr m x = repr m y

let rec set_equal m x y merger =
  match Map.find x m, Map.find y m with
  | Ancestor ax, Ancestor ay ->
    m
    |> Map.add x (Son y)
    |> Map.add y (Ancestor (merger ax ay))
  | Ancestor _, Son fy ->
    set_equal m x fy merger
  | Son fx, Ancestor _ ->
    set_equal m fx y merger
  | Son fx, Son fy ->
    set_equal m fx fy merger

let iter m f =
  Map.iter
    (fun x v ->
       match v with
       | Ancestor i -> f x i
       | _ -> ())
    m

let rec get m x =
  match Map.find x m with
  | Ancestor v -> v
  | Son y -> get m y

let rec set m x v =
  match Map.find x m with
  | Ancestor _ -> Map.add x (Ancestor v) m
  | Son y -> set m y v

open Constraints_common

type globals = t Var.Map.t

let empty_globals = Var.Map.empty

let fresh =
  let i = ref 0 in
  fun () -> incr i; !i

let internalise v m =
  match Var.Map.find_opt v m with
  | Some i -> (i, m)
  | None ->
    let i = fresh () in
    (i, Var.Map.add v i m)

let quantify_over v m =
  Var.Map.remove v m
