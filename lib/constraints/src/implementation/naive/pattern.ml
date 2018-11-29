open Constraints_common

module List = struct
  include List

  let map_filter f l =
    l
    |> fold_left (fun l x -> match f x with None -> l | Some y -> y :: l) []
    |> rev

  let map_flatten f l =
    map f l |> flatten
end

type atom =
  | Eq of Metavar.t * Metavar.t
  | Feat of Metavar.t * Metavar.t * Metavar.t
  | Abs of Metavar.t * Metavar.t
  | Kind of Metavar.t * Metavar.t
  | Fen of Metavar.t * Metavar.t
  | Sim of Metavar.t * Metavar.t * Metavar.t

let match_atom (pa : atom) (a : Atom.t) : Assign.t list =
  match pa, a with
  | Eq (mx, my), Eq (x, y) ->
     [Assign.from_lists ~vars:[mx, x; my, y] ();
      Assign.from_lists ~vars:[mx, y; my, x] ()]
  | Feat (mx, mf, my), Feat (x, f, y) ->
     [Assign.from_lists ~vars:[mx, x; my, y] ~feats:[mf, f] ()]
  | Abs (mx, mf), Abs (x, f) ->
     [Assign.from_lists ~vars:[mx, x] ~feats:[mf, f] ()]
  | Kind (mx, mk), Kind (x, k) ->
     [Assign.from_lists ~vars:[mx, x] ~kinds:[mk, k] ()]
  | Fen (mx, mfs), Fen (x, fs) ->
     [Assign.from_lists ~vars:[mx, x] ~feat_sets:[mfs, fs] ()]
  | Sim (mx, mfs, my), Sim (x, fs, y) ->
     [Assign.from_lists ~vars:[mx, x; my, y] ~feat_sets:[mfs, fs] ();
      Assign.from_lists ~vars:[mx, y; my, x] ~feat_sets:[mfs, fs] ()]
  | _ ->
     []

type literal =
  | Pos of atom
  | Neg of atom

let match_literal (pl : literal) (l : Literal.t) : Assign.t list =
  match pl, l with
  | Pos pa, Pos a -> match_atom pa a
  | Neg pa, Neg a -> match_atom pa a
  | _ -> []

let rec match_literal_l pl ls =
  match ls with
  | [] -> []
  | l :: ls ->
     List.map (fun affect -> (affect, ls)) (match_literal pl l)
     @ List.map (fun (affect, ls) -> (affect, l :: ls)) (match_literal_l pl ls)

let%test _ =
  let mx = Metavar.fresh () in
  let mg = Metavar.fresh () in
  let x = Var.fresh () in
  let y = Var.fresh () in
  let f = Feat.from_string "f" in
  let g = Feat.from_string "g" in
  match_literal_l
    (Pos (Abs (mx, mg)))
    [Pos (Feat (x, f, y)); Pos (Abs (x, g))]
  =
    [Assign.from_lists ~vars:[mx, x] ~feats:[mg, g] (),
     [Pos (Feat (x, f, y))]]

let rec match_ pls ls =
  let open OptionMonad in
  match pls with
  | [] -> [Assign.empty, ls]
  | pl :: pls' ->
     match_literal_l pl ls
     |> List.map_flatten (fun (aff1, ls1) ->
            match_ pls' ls1
            |> List.map_filter (fun (aff2, ls2) ->
                   Assign.merge aff1 aff2 >>= fun aff ->
                   Some (aff, ls2)))

let find ?(pred=(fun _ -> true)) pls ls =
  Literal.Set.elements ls
  |> match_ pls
  |> List.find_opt (fun (aff, _) -> pred aff)
  |> function
    | None -> None
    | Some (aff, ls) -> Some (aff, Literal.Set.of_list ls)

let mem ?pred pls ls =
  find ?pred pls ls <> None
