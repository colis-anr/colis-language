open Colis_constraints
open Clause

type sat = Sat | Unsat
type 'a test =
  { formula : 'a ;
    expectation : sat }

let tests =
  let x = Var.fresh () in
  (* let x' = Var.fresh () in *)
  let y = Var.fresh () in
  let z = Var.fresh () in

  let f = Feat.from_string "f" in
  let g = Feat.from_string "g" in

  [
    (* { expectation = Sat ;
       formula = exists @@ fun y' -> feat x f y & nempty y & feat x' f y' & fen y' (Feat.Set.singleton g) & sim1 x g x' } ; *)
    (* { expectation = Unsat ;
       formula = feat x f y & sim x Feat.Set.empty x' & nfeat x' f y } ; *)
    (* { expectation = Unsat ;
       formula = nsim x Feat.Set.empty y & ndir x & ndir y } ; *)
    (* { expectation = Sat ;
       formula = sim1 x g y & fen x (Feat.Set.singleton f) & nfen y (Feat.Set.singleton f) } ; *)
    (* { expectation = Unsat ;
       formula = sim x Feat.Set.empty y & feat x f y } ; *)
    (* { expectation = Sat ;
       formula = fen x (Feat.Set.of_list [f; g]) & nsim x Feat.Set.empty y & abs y f & abs y g } ; *)
    (* { expectation = Unsat ;
       formula = sim1 x f y & sim1 y g z & nsim x Feat.Set.empty z & empty z & abs x f & abs x g } ; *)
    { expectation = Unsat ;
      formula = feat x f y & abs y f & exists @@ fun y' -> feat x f y' & feat y' f z } ;
    (* { expectation = Sat ;
       formula = sim1 x f y & nsim x Feat.Set.empty z & abs x f & abs z f & fen y (Feat.Set.singleton g) } ; *)
    (* { expectation = Unsat ;
       formula = fen x (Feat.Set.singleton f) & abs x f & nsim x Feat.Set.empty z & empty z } ; *)
    { expectation = Sat ;
      formula = feat x f y & feat x f z & fen y Feat.Set.empty } ;
    { expectation = Unsat ;
      formula = feat x f y & feat x f z & fen y Feat.Set.empty & nabs z g } ;
    { expectation = Unsat ;
      formula = feat x f y & feat x f z & dir y & ndir z } ;
    { expectation = Sat ;
      formula = feat x f y & exists @@ fun x -> abs x f }
  ]

let run_tests ~engine_name () =
  let res =
    List.fold_left
      (fun (res, i) test ->
         Log.info (fun m -> m "Running test %d (%s expected)." i
                      (match test.expectation with Sat -> "Sat" | _ -> "Unsat"));
         (
           match add_to_sat_conj test.formula true_sat_conj, test.expectation with
           | [], Unsat | _::_, Sat ->
             (
               Log.info (fun m -> m "Test %d succeeded" i);
               res + 1
             )
           | ((_::_) as disj), Unsat ->
             (
               Log.warn (fun m -> m "Test %d failed" i);
               Log.info (fun m -> m "Should be unsat, got@\n%a" (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\nor@\n") pp_sat_conj) disj);
               res
             )
           | [], Sat ->
             (
               Log.warn (fun m -> m "Test %d failed" i);
               Log.info (fun m -> m "Should be sat.");
               res
             )
         ),
         (i + 1))
      (0, 1)
      tests
    |> fst
  in
  let total = List.length tests in
  Format.eprintf "Tests for %s: %d / %d@\n@." engine_name res total;
  res >= total

let () =
  run_tests ~engine_name:"efficient" ()
  |> fun b -> exit (if b then 0 else 1)
