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

(* About this assertion.

   TL;DR: No rule should produce a disjunction where one of the
   conjunctions is the same as the input conjunction. This assertion
   should never fail.

   The IJCAR'18 paper says the following:

   "We say that such a rule left => right applies to a clause c if the
   transformation yields a formula which is different from c."

   This is where it becomes annoying. The easy case is when the rule
   transforms a conjunction into an other conjunction. In that case,
   we just check the equality of the conjunctions (modulo
   associativity and commutativity, but since the literals are in a
   set, it's fine).

   The paper is not really explicit in the case where a rule yields a
   disjunction. We most likely want that none of the output
   conjunctions is equal to the input one, because that would create
   an infinite loop. However, when it is the case, should we consider
   that that is an error or that the rule simply does not apply?

   "Luckily", there is no rule that may produce this, so we have this
   "safe" assertion (We would rather have the program stop here than
   continue with a wrong constraints solver). *)

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
