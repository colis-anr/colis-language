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

(* FIXME: new kinds can remove negative information! *)

let set_empty_dir_and_suck_nabs info =
  let feats =
    List.fold_left
      (fun feats f ->
         Feat.Map.add f Exists feats)
      Feat.Map.empty
      info.nabs
  in
  { info with
    nfeats = [] ;
    kind = Dir { fen = false ; sims = [] ; feats } }

let set_pos_and_empty_negs _info k =
  { nfeats = [] ;
    nabs = [] ;
    nfens = [] ;
    nsims = [] ; (* FIXME: we need to remove the refl versions. *)
    kind = Pos k }

let set_ndir_and_empty_negs _info =
  { nfeats = [] ;
    nabs = [] ;
    nfens = [] ;
    nsims = [] ; (* FIXME: we need to remove the refl versions. *)
    kind = Neg [Dir] }

let kind k x c =
  update_info x c @@ fun info ->
  match k with
  | Kind.Dir ->
    (
      match info.kind with
      | Any -> [set_empty_dir_and_suck_nabs info]
      | Neg kinds when List.mem Kind.Dir kinds -> []
      | Neg _ -> [set_empty_dir_and_suck_nabs info]
      | Pos _ -> []
      | Dir _ -> [info]
    )
  | _ ->
    (
      match info.kind with
      | Any -> [set_pos_and_empty_negs info k]
      | Neg kinds when List.mem k kinds -> []
      | Neg _ -> [set_pos_and_empty_negs info k]
      | Pos kind when Kind.equal k kind -> [info]
      | Pos _ -> []
      | Dir _ -> []
    )

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
  match k with
  | Kind.Dir ->
    (
      match info.kind with
      | Any -> [set_ndir_and_empty_negs info]
      | Neg kinds ->
        let kinds = insert_sorted Kind.compare Kind.Dir kinds in
        if List.length kinds = Kind.nb_all - 1 then
          [set_pos_and_empty_negs info (find_smallest_diff Kind.compare kinds Kind.all)]
        else
          [{info with kind = Neg kinds}]
      | Pos _ -> [info]
      | Dir _ -> []
    )
  | _ ->
    (
      match info.kind with
      | Any ->  assert (Kind.nb_all > 2); [{info with kind = Neg [k]}]
      | Neg kinds ->
        let kinds = insert_sorted Kind.compare k kinds in
        if List.length kinds = Kind.nb_all - 1 then
          match find_smallest_diff Kind.compare kinds Kind.all with
          | Kind.Dir -> [set_empty_dir_and_suck_nabs info]
          | kind -> [set_pos_and_empty_negs info kind]
        else
          [{info with kind = Neg kinds}]
      | Pos kind when Kind.equal kind k -> []
      | Pos _ -> [info]
      | Dir _ -> [info]
    )
