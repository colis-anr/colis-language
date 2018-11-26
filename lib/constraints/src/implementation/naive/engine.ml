open Constraints_common

let (x, y, z) = Metavar.fresh3 ()
let (f, g) = Metavar.fresh2 ()
let (fs, gs) = Metavar.fresh2 ()

let apply_rule_on_conj (name, rule) conj =
  match rule conj with
  | None -> None
  | Some [] ->
     (
       Log.debug (fun m -> m "Clash %s applied" name);
       Some []
     )
  | Some [conj'] ->
     if Conj.equal conj' conj then
       None
     else
       (
         Log.debug (fun m -> m "Rule %s applied" name);
         Some [conj']
       )
  | Some disj' ->
     Log.debug (fun m -> m "Rule %s applied" name);
     assert (List.for_all ((<>) conj) disj');
     Some disj'

let apply_rule_on_disj rule disj =
  let (changes, disj) =
    List.fold_left
      (fun (changes, disj) conj ->
        match apply_rule_on_conj rule conj with
        | None -> (changes, conj :: disj)
        | Some disj' -> (true, disj' @ disj))
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

let rec normalize limit d =
  Log.debug (fun m -> m "Normalizing:@\n%a" Conj.pp_disj d);
  assert (limit >= 0);
  match apply_rules_on_disj Rules.all d with
  | None ->
     Log.debug (fun m -> m "Normal form reached");
     d
  | Some d ->
     normalize (limit-1) d

let normalize ?(limit=10) disj =
  normalize limit disj
