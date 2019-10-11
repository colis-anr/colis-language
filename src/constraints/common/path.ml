type comp =
  | Up
  | Here
  | Down of Feat.t

type rel = comp list

type normal = Feat.t list

type t = Abs of rel | Rel of rel

let comp_from_string = function
  | ".." -> Up
  | "." -> Here
  | s -> Down (Feat.from_string s) (*FIXME: check validity*)

let comp_to_string = function
  | Up -> ".."
  | Here -> "."
  | Down f -> Feat.to_string f

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

let split_last = function
  | Abs q ->
    BatOption.bind
      (split_last_rel q)
      (fun (q, e) -> Some (Abs q, e))
  | Rel q ->
    BatOption.bind
      (split_last_rel q)
      (fun (q, e) -> Some (Rel q, e))

let from_string s =
  String.split_on_char '/' s
  |> function
    | [] -> failwith "Path.from_string"
    | "" :: p -> Abs (List.map comp_from_string p)
    | p -> Rel (List.map comp_from_string p)

let strip_trailing_slashes s =
  let sl = String.split_on_char '/' s in
  let rec remove_empty sl =
          match sl with 
          | "" :: sl' -> remove_empty sl'
          | _ -> sl
   in
   String.concat (String.make 1 '/') (List.rev (remove_empty sl)) 

let rel_to_string ?(abs=false) r =
  List.map comp_to_string r
  |> String.concat "/"
  |> (if abs then (^) "/" else fun x -> x)

let to_string = function
  | Abs q -> rel_to_string ~abs:true q
  | Rel q -> rel_to_string ~abs:false q

let normal_to_string p =
  p
  |> List.map Feat.to_string
  |> String.concat "/"
  |> (^) "/"

let pp fmt q =
  Format.fprintf fmt "%s" (to_string q)

let rel = function
  | Abs q -> q
  | Rel q -> q

let concat p q =
  match q with
  | Abs q -> q
  | Rel q -> List.map (fun f -> Down f) p @ q

let normalize ?(cwd=[]) p =
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
  | Rel p -> normalize cwd p

let rec check_normal_rel = function
  | [] -> []
  | Down f :: p -> f :: check_normal_rel p
  | _ -> raise (Invalid_argument "check_normal")

let check_normal = function
  | Abs p -> check_normal_rel p
  | Rel _ -> raise (Invalid_argument "Path.check_normal")

let rec check_prefix np nq =
  match np, nq with
  | [], [] -> false (* strict prefix *)
  |  _, [] -> false
  | [], _  -> true
  | f1 :: p1, f2 :: p2 -> 
    (Feat.equal f1 f2) && (check_prefix p1 p2)

