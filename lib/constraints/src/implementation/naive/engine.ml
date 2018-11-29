open Constraints_common

let (x, y, z) = Metavar.fresh3 ()
let (f, g) = Metavar.fresh2 ()
let (fs, gs) = Metavar.fresh2 ()

let apply_rule_on_disj (name, rule) disj =
  let (changes, disj) =
    List.fold_left
      (fun (changes, disj) conj ->
        match rule conj with
        | None -> (changes, conj :: disj)
        | Some disj' ->
           Log.debug (fun m -> m "Rule %s applied" name);
           (true, disj' @ disj))
      (false, [])
      disj
  in
  if changes then Some disj else None

let apply_rules_on_disj rules disj =
  let (changes, disj) =
    List.fold_left
      (fun (changes, disj) rule ->
        match apply_rule_on_disj rule disj with
        | None -> (changes, disj)
        | Some disj -> (true, disj))
      (false, disj)
      rules
  in
  if changes then Some disj else None

let is_atom_about xs = function
  | Atom.Abs (a, _) | Kind (a, _) | Fen (a, _) ->
     Var.Set.mem a xs
  | Eq (a, b) | Feat (a, _, b) | Sim (a, _, b) ->
     Var.Set.mem a xs || Var.Set.mem b xs

let is_literal_about xs = function
  | Literal.Pos a | Neg a -> is_atom_about xs a

let remove_literals_about_in_literal_set xs =
  Literal.Set.filter (fun l -> not (is_literal_about xs l))

let rec normalize limit d =
  Log.debug (fun m -> m "%a" Conj.pp_disj d);
  assert (limit >= 0);
  match apply_rules_on_disj Rules.all d with
  | None ->
     d
  | Some d ->
     normalize (limit-1) d

let normalize ?(limit=50) (disj : Conj.disj) : Conj.disj =
  Log.debug (fun m -> m "Normalizing");
  let disj' = normalize limit disj in
  let disj' =
    List.map
      (fun (es, c) ->
        let xs =
          Rules.accessibility c
          |> List.filter (fun (x, ys) ->
                 Var.Set.mem x es && Var.Set.subset ys es)
          |> List.map (fun (x, _) -> x)
          |> Var.Set.of_list
        in
        (Var.Set.diff es xs, remove_literals_about_in_literal_set xs c))
      disj'
  in
  Log.debug (fun m -> m "Normal form reached");
  disj'
