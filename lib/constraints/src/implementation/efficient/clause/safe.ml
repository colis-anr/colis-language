open Constraints_common
open Core

let eq _x _y _c = assert false

let feat _x _f _y = assert false
let nfeat _x _f _y = assert false

let abs _x _f = assert false
let nabs _x _f = assert false

let fen _x _fs = assert false
let nfen _x _fs = assert false

let sim _x _fs _y = assert false
let nsim _x _fs _y = assert false

let update_info x c f =
  let info = IVar.get c.info x in
  f info
  |> List.map
    (fun info ->
       { c with info = IVar.set c.info x info })

let  kind k x c =
  update_info x c @@ fun info ->
  (
    match info.kind, k with
    | Any       , Kind.Dir -> [Dir empty_dir]
    | Any       , _        -> [Pos k]
    | Neg kinds , _
      when not (List.mem k kinds) -> [Pos k]
    | Neg _     , _        -> []
    | Pos kind  , k
      when Kind.equal kind k -> [Pos kind]
    | Pos _     , _        -> []
    | Dir dir   , Kind.Dir -> [Dir dir]
    | Dir _     , _        -> []
  )
  |> List.map
    (fun kind -> { info with kind })

let rec insert_sorted cmp e = function
  | [] -> [e]
  | h :: q ->
    match cmp h e with
    | c when c < 0 -> h :: insert_sorted cmp e q
    | c when c = 0 -> h :: q
    | _            -> e :: h :: q

let rec find_smallest_diff cmp l1 l2 =
  match l1, l2 with
  | [], [] -> failwith "find_smallest_diff"
  | [], h2 :: _ -> h2
  | h1 :: _, [] -> h1
  | h1 :: q1, h2 :: q2 ->
    match cmp h1 h2 with
    | c when c < 0 -> h1
    | c when c > 0 -> h2
    | _ -> find_smallest_diff cmp q1 q2

let nkind k x c =
  update_info x c @@ fun info ->
  (
    match info.kind, k with
    | Any       , _        -> [Neg [k]]
    | Neg kinds , _        ->
      (
        let kinds = insert_sorted Kind.compare k kinds in
        if List.length kinds = Kind.nb_all - 1 then
          [Pos (find_smallest_diff Kind.compare kinds Kind.all)]
        else
          [Neg kinds]
      )
    | Pos kind  , _
      when Kind.equal kind k -> []
    | Pos kind  , _        -> [Pos kind]
    | Dir _     , Kind.Dir -> []
    | Dir dir   , _        -> [Dir dir]
  )
  |> List.map
    (fun kind -> { info with kind })
