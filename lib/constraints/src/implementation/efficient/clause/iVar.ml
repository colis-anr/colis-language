type t = int

let eq = (=)
let lt = (<)

let pp = Format.pp_print_int

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

let iter_sons m f =
  Map.iter
    (fun x v ->
       match v with
       | Son y -> f x y
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

let add m x v =
  Map.add x (Ancestor v) m

open Constraints_common

type globals = t Var.Map.t

let empty_globals = Var.Map.empty

let get_global globals x =
  Var.Map.bindings globals
  |> List.find_opt (fun (_, y) -> eq x y)
  |> function None -> None | Some (gx, _) -> Some gx

let iter_globals globals f =
  Var.Map.iter f globals

let fresh_counter = ref 0

let fresh info empty_info =
  incr fresh_counter;
  (!fresh_counter, add info !fresh_counter empty_info)

let internalise v globals info empty_info =
  match Var.Map.find_opt v globals with
  | Some i -> (i, globals, info)
  | None ->
    let (i, info) = fresh info empty_info in
    (i, Var.Map.add v i globals, info)

let quantify_over v m =
  Var.Map.remove v m
