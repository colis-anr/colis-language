open Colis_constraints_common

type atom =
  | Eq of Metavar.t * Metavar.t
  | Feat of Metavar.t * Metavar.t * Metavar.t
  | Abs of Metavar.t * Metavar.t
  | Kind of Metavar.t * Metavar.t
  | Fen of Metavar.t * Metavar.t
  | Sim of Metavar.t * Metavar.t * Metavar.t

let match_atom (pa : atom) (a : Atom.t) : Assign.t option list =
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
  | _ -> []

let match_atom pa a : Assign.t Seq.t =
  match_atom pa a
  |> Seq.from_list
  |> Seq.filter_map (fun x -> x)

type literal =
  | Pos of atom
  | Neg of atom

let match_literal (pl : literal) (l : Literal.t) : Assign.t Seq.t =
  match pl, l with
  | Pos pa, Pos a -> match_atom pa a
  | Neg pa, Neg a -> match_atom pa a
  | _ -> Seq.empty

let rec match_literal_l pl ls =
  match ls with
  | [] -> Seq.empty
  | l :: ls ->
     Seq.concat
       (Seq.map (fun a -> (a, ls)) (match_literal pl l))
       (Seq.map (fun (a, ls) -> (a, l::ls)) (match_literal_l pl ls))

let rec match_ pls ls =
  match pls with
  | [] -> Seq.return (Assign.empty, Literal.Set.of_list ls)
  | pl :: pls' ->
     match_literal_l pl ls
     |> Seq.flat_map
          (fun (a1, ls1) ->
            match_ pls' ls1
            |> Seq.filter_map
                 (fun  (a2, ls2) ->
                  BatOption.bind
                    (Assign.merge a1 a2)
                    (fun a -> Some (a, ls2))))

let find_all ?(pred=(fun _ _ -> true)) pls (es, ls) =
  match_ pls (Literal.Set.elements ls)
  |> Seq.filter_map (fun (a, c) ->
         if pred a (es, c) then
           Some (a, (es, c))
         else
           None)
